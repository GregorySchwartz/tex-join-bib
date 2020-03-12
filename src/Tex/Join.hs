{- Tex.Join
Gregory W. Schwartz

Collects the functions pertaining to the joining and compiling of separate tex
files.
-}

{-# LANGUAGE OverloadedStrings #-}

module Tex.Join
  ( joinTex
  ) where

-- Remote
import System.Environment (getArgs)
import System.IO (openTempFile, hClose)
import Turtle
import qualified Control.Concurrent.Async as A
import qualified Control.Foldl as Fold
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Filesystem.Path as FP

-- Local


newtype BibFile = BibFile { unBibFile :: FP.FilePath }

-- | Export the file to latex. Not used due as there is no simple way to have
-- custom configuration seemingly.
exportFileOrg :: FP.FilePath -> IO ()
exportFileOrg file = procs
                       "emacs"
                       [ format fp file
                       , "--batch"
                       , "-l", "~/.emacs.d/init.el"
                       , "--eval", "(run-hooks 'emacs-startup-hook)"
                       , "--eval", "(setq org-latex-logfiles-extensions (quote (\"lof\" \"lot\" \"tex~\" \"aux\" \"idx\" \"out\" \"toc\" \"nav\" \"snm\" \"vrb\" \"dvi\" \"fdb_latexmk\" \"blg\" \"brf\" \"fls\" \"entoc\" \"ps\" \"spl\" \"bbl\" \"xml\" \"run.xml\" \"blg\" \"cb\" \"cb2\")))"
                       , "--eval", "(org-mode)"
                       , "--eval", "(org-latex-export-to-pdf)"
                       , "--kill"
                       ]
                       mempty

-- | Export the file to latex.
exportFileTex :: FP.FilePath -> IO ()
exportFileTex file = procs
                       "latexmk"
                       [ "-pdf"
                       , format fp file
                       ]
                       mempty

-- | Get the biber bib file from the tex file.
runBiber :: FP.FilePath -> IO ()
runBiber file = procs
                  "biber"
                  [ format fp . FP.dropExtension $ file
                  , "--output-format", "bibtex"
                  ]
                  mempty

-- | Get biber output name.
toBiberFile :: FP.FilePath -> FP.FilePath
toBiberFile =
  fromText . flip mappend "_biber.bib" . format fp . FP.dropExtension

-- | Replace a bibresource line.
replaceBibresource :: T.Text -> Pattern T.Text
replaceBibresource bib = (text "addbibresource{" <> plus (notChar '}'))
                      *> pure ("addbibresource{" <> bib)

-- | Re-export with new bib, returning pdf.
finalExport :: BibFile -> FP.FilePath -> IO [FP.FilePath]
finalExport (BibFile bib) file = reduce Fold.list $ do
  tmp <- mktempfile "." (format fp file)

  output tmp
    . sed (text "\\begin{document}" *> pure "\\begin{document}\n\\nocite{*}")
    . sed (replaceBibresource $ format fp bib)
    . input
    $ file

  liftIO $ exportFileTex tmp

  -- Clean up
  procs "latexmk" ["-c", format fp tmp] mempty

  return $ FP.replaceExtension tmp "pdf"

-- | Re-export with new bib, returning pdf.
bibliographyExport :: BibFile -> IO ()
bibliographyExport (BibFile bib) = sh $ do
  let bibliographyFile = FP.replaceExtension bib "tex"
      contents = [ "\\documentclass{article}"
                 , "\\usepackage[utf8]{inputenc}"
                 , "\\usepackage[T1]{fontenc}"
                 , "\\usepackage{helvet}"
                 , "\\renewcommand{\\familydefault}{\\sfdefault}"
                 , "\\usepackage{microtype}"
                 , "\\usepackage[left=0.5in, right=0.5in, top=0.5in, bottom=0.5in]{geometry}"
                 , "\\usepackage[citestyle=nature,bibstyle=nature,date=year,firstinits=true,sorting=none,url=false,doi=false,isbn=false,eprint=false,maxbibnames=5]{biblatex}"
                 , "\\addbibresource{" <> format fp bib <> "}"
                 , "\\begin{document}"
                 , "\\nocite{*}"
                 , "\\printbibliography"
                 , "\\end{document}"
                 ]

  output bibliographyFile . toLines . select $ contents

  liftIO $ exportFileTex bibliographyFile

  -- Clean up
  procs "latexmk" ["-c", format fp bibliographyFile] mempty

joinTex :: FP.FilePath -> [FP.FilePath] -> IO ()
joinTex out files = sh $ do
  -- Export files
  liftIO $ A.mapConcurrently_ exportFileTex files

  -- Run biber on files
  liftIO $ A.mapConcurrently_ runBiber files

  -- Combine files, order is important.
  let bibFile = BibFile
              . fromText
              . (<> "_references.bib")
              . format fp
              . FP.dropExtension
              $ out
  output (unBibFile bibFile) . cat . fmap (input . toBiberFile) $ files

  -- Export final files
  pdfs <- fmap mconcat . liftIO . A.mapConcurrently (finalExport bibFile) $ files

  -- Combine into a single pdf
  procs "pdfjam"
    ( ["--paper", "letterpaper"]
   <> fmap (format fp) pdfs
   <> ["-o", format fp out]
    )
    mempty

  -- Export bibliography
  liftIO $ bibliographyExport bibFile

  -- Cleanup
  rmFiles <- fmap (Set.toList . Set.fromList . mconcat)
           . reduce Fold.list
           $ mapM (flip find "." . contains . text . format fp . FP.dropExtension) pdfs
  mapM_ rm rmFiles
