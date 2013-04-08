{-# Language CPP #-}
{-# Language Rank2Types #-}
{-# Language OverloadedStrings #-}
module Sheets
  ( Field(..)
  , Table(..)
  , rows
  , columns
  , split
  , counter
  , see
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
-- mtl
import Control.Monad.State
-- Text
import Data.Text (Text)
import Data.Text (pack)
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
  { field :: a -> m Text
  }

data Table m a = Table
  { fields :: [Field m a]
  , items  :: [a]
  }

-- | Monadically traverse the rows in a table.
rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table cs is) = forM is $ \i -> forM cs (($ i) . field) >>= fn

-- | Monadically traverse the columns in a table.
columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table cs is) = forM cs $ \(Field c) -> forM is c >>= fn

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a -> [Table m a]
split n (Table fs is) = Table fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking _ [] = []
  taking n x = let (a, b) = splitAt n x in a : taking n b
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x
counter l = Field . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | A column just showing the 'Text' in the row.
see :: Monad m => Field m Text
see = Field $ return . id

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
renderTable = liftM (T.table . sequence_) .
  rows (return . T.tr . mapM_ (T.td . toMarkup))

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
style :: IO FilePath
-- Stupid hack so that we can still run this file in ghci; otherwise
-- we wouldn't be able to find the Paths_sheets module.
#ifdef MIN_VERSION_base(0,0,0)
style = getDataFileName "style.css"
#else
style = return "data/style.css"
#endif
