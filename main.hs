{-# Language OverloadedStrings #-}
module Main where
-- base
import Control.Applicative
import Data.Monoid
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
-- xmlhtml
import Text.XmlHtml
-- bytestring
import qualified Data.ByteString as B
-- blaze-builder
import Blaze.ByteString.Builder
-- heist
import Heist
import Heist.Interpreted
--

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

config :: MonadIO m => EitherT [String] IO (HeistConfig m)
config = (`fmap` loadTemplates ".") $ HeistConfig
  (
    [ ("example-table", withTable example)
    ] ++ defaultInterpretedSplices
  ) defaultLoadTimeSplices [] []

main = eitherT showErrors return $ config >>= initHeist >>= go where
  showErrors :: [String] -> IO ()
  showErrors ss = putStrLn "Found the following errors:"
    >> forM_ ss (putStrLn . ("--> " ++))
  go :: HeistState IO -> EitherT [String] IO ()
  go hs = lift (renderTemplate hs "sheet")
    >>= maybe (left ["Couldn't find template."]) (lift . output)
  output :: (Builder, MIMEType) -> IO ()
  output (b, _) = toByteStringIO B.putStr b

-- TODO: clean up code
-- TODO: more templates? add just a file to a templaterepo?
-- TODO: list files
-- TODO: per-header
-- TODO: easy table rendering
-- TODO: more modular withTable
