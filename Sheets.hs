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
  , split'
  , counter
  , see
  , blank
  , (*.)
  , Layout(..)
  , horizontal
  , renderTable
  , renderLayout
  , renderWhole
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
import Text.Blaze.Html5.Attributes (charset, class_, colspan)
import Text.Blaze.Html.Renderer.Pretty
-- provided by cabal
#ifdef MIN_VERSION_base(0,0,0)
import Paths_sheets
#endif

data Field m a = Field
  { _classes :: [String]
  , _label :: Maybe Text
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
columns fn (Table _ cs is) = forM cs $ \(Field _ _ c) -> forM is c >>= fn

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a -> [Table m a]
split n (Table t fs is) = Table t fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking _ [] = []
  taking n x = let (a, b) = splitAt n x in a : taking n b

-- | Split a table into a table with twice as many fields and
-- half as many items.
split' :: Table m b -> Table m (b, b)
split' (Table t fs is) = Table t
  (map (over field (. fst)) fs ++ map (over field (. snd)) fs)
  . uncurry zip . splitAt (length is `div` 2) $ is
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x
counter l = Field ["count"] Nothing . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | A column just showing the 'Text' in the row.
see :: Monad m => Field m Text
see = Field [] Nothing $ return . id

-- | An empty column.
blank :: Monad m => Field m a
blank = Field [] Nothing $ \_ -> return empty

-- | Add a class to a 'Field'.
(*.) :: Field m a -> String -> Field m a
(*.) f s = classes <>~ [s] $ f

-- | Label a field.
(!.) :: Field m a -> Text -> Field m a
(!.) f n = label .~ Just n $ f

-- | Set the monadic function of a field.
($.) :: Field m a -> (b -> n Text) -> Field n b
($.) f g = field .~ g $ f

-- | Set the monadic function of a field from an ordinary function.
(%.) :: Monad n => Field m a -> (b -> Text) -> Field n b
(%.) f g = field .~ (return . g) $ f

data Layout a
  = Column [Either a (Layout a)]
  | Adjacent [Either a (Layout a)]
  deriving
  ( Eq
  , Ord
  , Show
  )

-- | Split a table horizontally into tables with the given
-- number of columns.
horizontal :: Int -> Table m a -> Layout (Table m a)
horizontal n = Adjacent . map Left . split n

-- | Split a table vertically into tables with the given
-- number of columns.
vertical :: Int -> Table m a -> Layout (Table m a)
vertical n = Column . map Left . split n

-- | Render a table simply to html.
renderTable :: Monad m => Table m t -> m Html
renderTable t = do
  x <- flip rows t $ return . T.tr . mapM_ (T.td . toMarkup)
  return . T.table $ do
    T.colgroup $ cols t
    case (view title t) of
      Nothing -> return ()
      Just tt -> T.th (toMarkup tt) ! colspan
        (fromString . show . length $ view fields t)
    sequence_ x
  where 
    cols :: Monad m => Table m a -> Html
    cols = mapMOf_ (fields . traverse) toCol where
      toCol :: Monad m => Field m a -> Html
      toCol f = let s = view classes f in
        if null s then T.col
          else T.col ! class_ (fromString . unwords $ s)

-- | Render a 'Layout' to html.
renderLayout :: Monad m => (a -> m Html) -> Layout a -> m Html
renderLayout f (Column (Left a : [])) = f a
renderLayout f (Column cs) = liftM sequence_ $
  forM cs $ either f (renderLayout f)
renderLayout f (Adjacent (Left a : [])) = f a
renderLayout f (Adjacent as) = liftM sequence_ $ forM as $
  liftM ((! class_ "_adjacent") . T.div) . either f (renderLayout f)

-- | Render a 'Layout' of 'Table' as a full document, given some css.
renderWhole :: Monad m => Text -> Layout (Table m t) -> m Html
renderWhole css = liftM (template css) . renderLayout renderTable where
  -- | Basic html to wrap something in.
  template :: Text -> Html -> Html
  template css t = T.docTypeHtml $ do
    T.head $ do
      T.meta ! charset "utf-8"
      T.style $ toMarkup css
    T.body $ t

-- | The default stylesheet.
style :: IO FilePath
style = path where
  -- Stupid hack so that we can still run this file in ghci; otherwise
  -- we wouldn't be able to find the Paths_sheets module.
#ifdef MIN_VERSION_base(0,0,0)
  path = getDataFileName "data/style.css"
#else
  path = return "data/style.css"
#endif
