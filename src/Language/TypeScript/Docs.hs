-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Docs
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

module Language.TypeScript.Docs (
   convertDeclarationsToHtml,
   module Language.TypeScript.Docs.Comments,
   module Language.TypeScript.Docs.Html
) where

import Language.TypeScript.Docs.Comments
import Language.TypeScript.Docs.Html

import Language.TypeScript.Parser

import Text.Parsec
import Text.Blaze.Html.Renderer.String
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

convertDeclarationsToHtml :: String -> Either String String
convertDeclarationsToHtml dts = do
  decls <- either (Left . show) Right $ parse declarationSourceFile "TypeScript Declaration File" dts
  return . renderHtml . generateDocument $ appendComments dts decls
