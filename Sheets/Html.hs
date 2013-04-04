module Sheets.Html where
-- base
import Control.Monad
-- text
import Data.Text (Text)
-- blaze-html
import Text.Blaze.Html
import Text.Blaze.Html5
-- sheets
import Sheets

renderRows :: [[Text]] -> Html
renderRows t = table $ forM_ t renderRow where
  renderRow c = tr . forM_ c $ td . toMarkup

renderTable :: Monad m => Table m t -> m Html
renderTable t = renderRows `liftM` asRows t
