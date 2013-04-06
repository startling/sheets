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

renderTable :: Monad m => Table m t -> m Html
renderTable = liftM (table . sequence_) . rows (return . renderRow)
  where
    renderRow :: [Text] -> Html
    renderRow = tr . mapM_ (td . toMarkup)
    
