{- tex-join-bib
Gregory W. Schwartz

Blah's the blah in the blah.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Data.Maybe (fromMaybe)
import Options.Generic
import qualified Data.Text as T
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP

-- Local
import Tex.Join

-- | Command line arguments
data Options = Options { input  :: [T.Text]
                               <?> "(FILE) Input tex files. A list, so for example: `-i a.tex -i b.tex -i c.tex`. The global bibliography should already be setup with biblatex, i.e. in the preamble: \\usepackage{biblatex} and \\addbibresource{/path/to/global_references.bib}. This path will automatically be replaced with the combined bib. Order matters in terms of reference numbering."
                       , output :: Maybe T.Text
                               <?> "([out.pdf] | FILE) The output joined compiled pdf. Will also output FILE_references.bib, FILE_references.tex, and FILE_references.pdf to customize later if needed."
                       }
               deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = firstLetter }

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers

main :: IO ()
main = do
  opts <- getRecord "tex-join-bib, Gregory W. Schwartz.\
                    \ Compiles each tex separately with a joined bibliography.\
                    \ Order matters in terms of reference numbering.\
                    \ Works by replacing \
                    \ \\addbibresource{/path/to/global_bibliography.bib} with\
                    \ the new combined bibliography, then inserting \\nocite{*}."

  let files = fmap FP.fromText . unHelpful . input $ opts
      out =
        FP.fromText . fromMaybe "out.pdf" . unHelpful . output $ opts

  joinTex out files

  return ()
