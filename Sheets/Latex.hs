{-# Language OverloadedStrings #-}
module Sheets.Latex
  ( renderTable
  , module Text.LaTeX.Base
  ) where
-- base
import Data.List
import Data.String
import Control.Applicative
import Control.Monad
-- lens
import Control.Lens hiding ((&))
-- HaTeX
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Commands
-- Sheets
import Sheets as S

multicolumn :: LaTeXC l => Int -> [TableSpec] -> l -> l
multicolumn n c = liftL $ \l -> TeXComm "multicolumn"
  [ FixArg $ rendertex n
  , FixArg . TeXRaw $ renderAppend c
  , FixArg l
  ]

renderTable :: Monad m => Table m a LaTeX -> LaTeXT m ()
renderTable t = let cs = length $ view fields t in
  tabular Nothing (specs cs) $ do
    hline
    case view S.title t of
      Nothing -> return ()
      Just x -> do 
        multicolumn cs
          [ VerticalLine, CenterColumn, VerticalLine ]
          . fromLaTeX $ x
        lnbk
        hline
    case traverse (fmap fromLaTeX . view S.label) . view fields $ t of
      Nothing -> return ()
      Just [] -> lnbk >> hline
      Just (l : ls) -> foldl (&) l ls >> lnbk >> hline
    flip rows (S.transform lift t) $ \x
      -> (>> (lnbk >> hline)) $ case map fromLaTeX x of
        [] -> return ()
        (x : xs) -> foldl (&) x xs
    return ()
  where
    specs :: Int -> [TableSpec]
    specs n = (:) VerticalLine . concatMap (: [VerticalLine])
      $ replicate n CenterColumn
