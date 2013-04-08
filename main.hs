{-# Language CPP #-}
{-# Language OverloadedStrings #-}
module Main where
-- base
import Control.Applicative
import Data.Monoid
import System.Environment
-- filepath
import System.FilePath
-- mtl
import Control.Monad.State
-- lens
import Control.Lens
-- text
import Data.Text (Text)
-- sheets
import Sheets
-- either
import Control.Monad.Trans.Either
-- bytestring
import qualified Data.ByteString as B
-- utf8-string
import Data.ByteString.UTF8 (fromString)
-- blaze-builder
import Blaze.ByteString.Builder
-- heist
import Heist
import Heist.Interpreted
-- provided by cabal
#ifdef MIN_VERSION_base(0, 0, 0)
import Paths_sheets (getDataFileName)
#endif

-- If this is being built by cabal, get the cabal-installed data files;
-- otherwise, just stick an empty list in there.
otherTemplates :: IO [FilePath]
#ifdef MIN_VERSION_base(0, 0, 0)
otherTemplates = return `liftM` getDataFileName ""
#else
otherTemplates = return []
#endif

forTable :: (Monad m, Monad n) =>
  (([Text] -> Splice n) -> b -> m [[a]]) -> b -> m [a]
forTable tr t = liftM concat $ flip tr t $ \t -> runChildrenWith
  [ ("per-cell", liftM concat $ mapM perCell t)
  ] where
    perCell :: Monad n => Text -> HeistT n n Template
    perCell t = runChildrenWithText [("cell-text", t)]

withTable :: Monad n => Table (HeistT n n) t -> Splice n
withTable t = runChildrenWith
  [ ("per-row", forTable rows t )
  , ("per-column", forTable columns t)
  ]

example :: Monad m => Table m Text
example = Table
  [ Column return
  , Column return
  ]
  $ return
  [ "a"
  , "b"
  , "c"
  ]

config :: MonadIO m => [FilePath] -> EitherT [String] IO (HeistConfig m)
config ds = (`fmap` foldM combine mempty ds) $ HeistConfig
  (
    [ ("example-table", withTable example)
    ] ++ defaultInterpretedSplices
  ) defaultLoadTimeSplices [] [] where
    combine a b = mappend a `liftM` loadTemplates b

-- Configure and render in a heist state, given some directories
-- that a templates are in and the name of the template.
render :: [String] -> String -> EitherT [String] IO ()
render d f = do
  h <- config d >>= initHeist
  m <- lift . renderTemplate h . fromString $ f
  maybe (left ["Template not found: " ++ f])
    (lift . toByteStringIO B.putStr . fst) m

main :: IO ()
main = eitherT showErrors return $ lift otherTemplates >>=
  \ot -> arguments >>=  uncurry (render . (: ot)) where
  -- Separate the first command-line argument into the
  -- directory and the extension-less name of the template.
  arguments :: EitherT [String] IO (String, String)
  arguments = lift getArgs >>= \x -> case x of
    [] -> left ["No file specified."]
    (a:_) -> right (takeDirectory a, takeBaseName a)
  -- Display the errors we've gotten.
  showErrors :: [String] -> IO ()
  showErrors ss = putStrLn "Found the following errors:"
    >> forM_ ss (putStrLn . ("--> " ++))

-- TODO: clean up code
-- TODO: list files
-- TODO: per-header
-- TODO: easy table rendering
-- TODO: more modular withTable
