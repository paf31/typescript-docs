-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) DICOM Grid Inc. 2013
-- License     :  MIT
--
-- Maintainer  :  Phillip Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Language.TypeScript
import Language.TypeScript.Docs
import System.Console.CmdTheLine
import Control.Applicative
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (pathSeparator)
import System.FilePath.Find
import qualified System.IO.UTF8 as U

docGen :: [FilePath] -> Maybe FilePath -> IO ()
docGen inputFiles outputFile = do
  tds <- fmap concat $ mapM U.readFile inputFiles
  case convertDeclarationsToHtml tds of
    Left err -> do
      U.putStrLn err
      exitFailure
    Right html -> do
      case outputFile of
        Just path -> U.writeFile path html
        Nothing -> U.putStrLn html
      exitSuccess

inputFiles :: Term [FilePath]
inputFiles = nonEmpty $ posAny [] $ posInfo
     { posDoc = "The input .d.ts file" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output .html file" }

term :: Term (IO ())
term = docGen <$> inputFiles <*> outputFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "typescript-docs"
  , version  = "0.0.1"
  , termDoc  = "Converts TypeScript Declaration files into HTML format"
  }

main = run (term, termInfo)
