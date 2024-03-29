{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}
-- base
import Data.Maybe
-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- transformers
import Control.Monad.IO.Class
-- mtl
import Control.Monad.Reader
import Control.Monad.State
-- optparse-applicative
import Options.Applicative
-- blaze-html
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
-- lens
import Control.Lens hiding (argument)
-- sheets
import Sheets hiding (title)
import Sheets.Html

-- TODO: real csv

data Configuration = Configuration
  { _fragment   :: Bool
  , _stylesheet :: FilePath
  , _cols       :: Int
  , _count      :: Bool
  , _title      :: Maybe String
  , _input      :: FilePath
  , _output     :: Maybe FilePath
  } deriving
  (
  )
makeLenses ''Configuration

options :: IO (Parser Configuration)
options = style >>= \css -> return $
  Configuration
    <$> switch 
      ( long "fragment"
     <> short 'f'
     <> help "Create only a fragment, i.e. not a full HTMl document."
      )
    <*> strOption
      ( long "stylesheet"
     <> short 's'
     <> metavar "file"
     <> value css
     <> help "The source file of an alternative stylesheet to embed in the document."
      )
    <*> option
       ( long "columns"
      <> short 'c'
      <> metavar "n"
      <> value 50
      <> help "Split the table into columns of length n."
       )
    <*>
      ( toggle <$>
        switch
          ( long "no-counter"
         <> short 'n'
         <> help "Do not include the counter on the left-hand side of the table."
          )
      )
    <*>
      ( optional $
        strOption
          ( long "title"
         <> short 't'
         <> help "A title for the table."
         <> metavar "string"
          )
       )
    <*> argument Just
      ( metavar "input"
     <> help "Input file."
      )
    <*>
      ( optional $
        argument Just
        ( metavar "output"
       <> help "File to output HTML to."
        )
      )
  where
    toggle :: Bool -> Bool
    toggle True = False
    toggle False = True

parser :: IO (ParserInfo Configuration)
parser = options >>= \o -> return $ info (helper <*> o)
   ( progDesc "Create nice html tables out of plaintext."
  <> header "sheets"
  <> fullDesc
   )
  
make ::
  ( MonadIO m
  , MonadState Int n
  , MonadReader Configuration m
  ) => m (Layout (Table n Text Text))
make = horizontal `liftM` view cols `ap` do
  co <- view count
  tt <- view title
  ls <- T.lines `liftM` (view input >>= liftIO . T.readFile)
  return $ flip (Table $ fmap T.pack tt) ls $
    if co then [counter id, see] else [see]

compile ::
  ( Monad n
  , MonadIO m
  , MonadReader Configuration m
  ) => (Layout (Table n x Html)) -> m (n Html)
compile l = view fragment >>=
  \fp -> if fp then return $ renderLayout renderTable l
    else flip renderWhole l `liftM`
      (view stylesheet >>= (liftIO . T.readFile))

display :: (MonadIO m, MonadReader Configuration m) => String -> m ()
display x = view output >>= \o -> liftIO $ case o of
  Nothing -> putStrLn x
  Just fp -> writeFile fp x

run :: (MonadIO m, MonadReader Configuration m) => m ()
run = ((renderHtml . flip evalState 0)
  `liftM` (liftM (fmap (fmap toMarkup)) make >>= compile)) >>= display

main :: IO ()
main = parser >>= execParser >>= runReaderT run
