{-# Language CPP #-}
{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module Sheets
  ( Field(..)
  , Table(..)
  , rows
  , columns
  , split
  , counter
  , see
  , blank
  , (*.)
  , Layout(..)
  , horizontal
  , renderTable
  , renderLayout
  , render
  , html
  , style )
  where
-- base
import Control.Monad
import Data.String
-- mtl
import Control.Monad.State
-- Text
import Data.Text (Text)
import Data.Text (pack, empty)
import qualified Data.Text.IO as T
-- lens
import Control.Lens
-- blaze-html
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as T
import Text.Blaze.Html5.Attributes (charset, class_)
import Text.Blaze.Html.Renderer.Pretty
-- provided by cabal
#ifdef MIN_VERSION_base(0,0,0)
import Paths_sheets
#endif

data Field m a = Field
  { _classes :: [String]
  , _field :: a -> m Text
  }
makeLenses ''Field

data Table m a = Table
  { _title  :: Maybe Text
  , _fields :: [Field m a]
  , _items  :: [a]
  }
makeLenses ''Table 

-- | Monadically traverse the rows in a table.
rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table _ cs is) = forM is $ \i -> forM cs (($ i) . view field) >>= fn

-- | Monadically traverse the columns in a table.
columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table _ cs is) = forM cs $ \(Field _ c) -> forM is c >>= fn

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a -> [Table m a]
split n (Table t fs is) = Table t fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking _ [] = []
  taking n x = let (a, b) = splitAt n x in a : taking n b
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x
counter l = Field ["count"] . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | A column just showing the 'Text' in the row.
see :: Monad m => Field m Text
see = Field [] $ return . id

-- | An empty column.
blank :: Monad m => Field m a
blank = Field [] $ \_ -> return empty

-- | Add a class to a 'Field'.
(*.) :: Field m a -> String -> Field m a
(*.) f s = classes <>~ [s] $ f

data Layout a
  = Column [Either a (Layout a)]
  | Adjacent [Either a (Layout a)]
  deriving
  ( Eq
  , Ord
  , Show
  )

-- | Split a table horizontally.
horizontal :: Int -> Table m a -> Layout (Table m a)
horizontal n = Adjacent . map Left . split n

-- | Render a table simply to html.
renderTable :: Monad m => Table m t -> m Html
renderTable t = do
  x <- flip rows t $ return . T.tr . mapM_ (T.td . toMarkup)
  return . T.table $ do
    T.colgroup $ cols t
    sequence_ x
  where 
    cols :: Monad m => Table m a -> Html
    cols = mapMOf_ (fields . traverse) toCol where
      toCol :: Monad m => Field m a -> Html
      toCol f = let s = fromString . unwords $ view classes f
        in T.col ! class_ s

-- | Render a 'Layout' to html.
renderLayout :: Monad m => (a -> m Html) -> Layout a -> m Html
renderLayout f (Column cs) = liftM sequence_ $
  forM cs $ either f (renderLayout f)
renderLayout f (Adjacent as) = liftM sequence_ $ forM as $
  liftM ((! class_ "_adjacent") . T.div) . either f (renderLayout f)

-- | Render a 'Layout' of 'Table' as a full document, given some css.
render :: Monad m => Text -> Layout (Table m t) -> m Html
render css = liftM (template css) . renderLayout renderTable where
  -- | Basic html to wrap something in.
  template :: Text -> Html -> Html
  template css t = T.docTypeHtml $ do
    T.head $ do
      T.meta ! charset "utf-8"
      T.style $ toMarkup css
    T.body $ t

-- | Render a 'Layout' of 'Table' as a full document and save it
-- to some file path.
html :: Monad m => Text -> FilePath -> Layout (Table m t) -> m (IO ())
html css fp = liftM (writeFile fp . renderHtml) . render css

-- | The default stylesheet.
style :: IO Text
style = path >>= T.readFile where
  -- Stupid hack so that we can still run this file in ghci; otherwise
  -- we wouldn't be able to find the Paths_sheets module.
#ifdef MIN_VERSION_base(0,0,0)
  path = getDataFileName "style.css"
#else
  path = return "data/style.css"
#endif
