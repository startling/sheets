{-# Language CPP #-}
{-# Language OverloadedStrings #-}
module Sheets.Html where
-- base
import Control.Monad
import Data.String
-- text
import Data.Text (Text)
-- blaze-html
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as T
import Text.Blaze.Html5.Attributes (charset, class_, colspan)
import Text.Blaze.Html.Renderer.Pretty
-- lens
import Control.Lens
-- sheets
import Sheets
-- provided by cabal
#ifdef MIN_VERSION_base(0,0,0)
import Paths_sheets
#endif

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
    case traverse (view label) . view fields $ t of
      Nothing -> return ()
      Just ls -> T.tr (forM_ ls $ T.td . toMarkup) ! class_ "labels"
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
