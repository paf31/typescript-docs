-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Docs.Comments
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

module Language.TypeScript.Docs.Comments (
    appendComments
) where

import Data.Data
import Data.Generics
import Text.Parsec
import Text.Parsec.Pos
import qualified Data.Map as M
import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad (when, (>=>))
import Language.TypeScript
import Data.List (intercalate, break)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (isSpace)
import System.IO.Unsafe (unsafePerformIO)

findComments :: String -> (Int, Int) -> CommentPlaceholder
findComments s pos@(line, col) =
  let
    before = unlines $ toString (line - 1) col $ lines s
  in
    maybe (Left pos) (Right . parseComment . getContent) (findCommentBlock before)
  where
    findCommentBlock s = do
      let reversed = reverse s
      s1 <- findEndOfComment reversed
      findStartOfComment [] s1
    findEndOfComment (c:s) | isSpace c = findEndOfComment s
    findEndOfComment ('/':'*':s) = Just s
    findEndOfComment _ = Nothing
    findStartOfComment s (c:'*':'*':'/':_) | isSpace c = Just s
    findStartOfComment s (c:'*':'/':_) = Nothing
    findStartOfComment s (c:s1) = findStartOfComment (c:s) s1
    findStartOfComment s [] = Nothing

toString line col (s:_)  | line <= 0 = [take (col - 1) s]
toString line _ []       | line <= 0 = []
toString line col (s:ss) = s:toString (line - 1) col ss

getContent :: String -> [String]
getContent = map dropStar . lines
  where
    dropStar :: String -> String
    dropStar ('*':c:s) | isSpace c = s
    dropStar ('*':s) = s
    dropStar (c:s) | isSpace c = dropStar s
    dropStar s = s

parseComment :: [String] -> Comment
parseComment = fst . foldr go (Comment [] [], True)
  where
    go ('@':rest) (comment, _) =
      let
        (key, value) = break (== ' ') rest
      in
        (comment { commentOther = (key, dropWhile isSpace value):commentOther comment}, True)
    go line (comment, _) | all isSpace line = (comment, True)
    go line (comment, True) =
      (comment { commentText = line:commentText comment }, False)
    go line (comment@Comment{ commentText = init:tail }, False) =
      (comment { commentText = (line ++ " " ++ init):tail }, False)

appendComments :: String -> [DeclarationElement] -> [DeclarationElement]
appendComments source = everywhere (mkT appendComments')
  where
    appendComments' (Left pos) = findComments source pos
    appendComments' r = r
