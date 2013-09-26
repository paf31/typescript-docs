-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Docs.Html
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

{-# LANGUAGE OverloadedStrings #-}

module Language.TypeScript.Docs.Html (
    generateDocument
) where

import Language.TypeScript.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..), Monoid)
import Data.Either (rights)
import Data.List (sortBy, groupBy, intercalate, intersperse)
import Data.String (IsString(..))
import Control.Monad (MonadPlus(..), when, unless)
import qualified Data.Map as M
import Data.Function (on)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

header :: Int -> H.Html -> H.Html
header 1 = H.h1
header 2 = H.h2
header 3 = H.h3
header 4 = H.h4
header 5 = H.h5
header _ = H.h6

topLevelInterfaces :: [DeclarationElement] -> [(Maybe Exported, Interface)]
topLevelInterfaces =
  sortBy (comparing $ (\(_, Interface _ name _ _ _) -> name))
  . map collect
  . groupBy ((==) `on` (\(_, Interface _ name _ _ _) -> name))
  . mapMaybe toInterface
  where
  toInterface (InterfaceDeclaration com e (Interface com1 name tps exts body)) = Just (e, Interface (com <.> Just com1) name tps exts body)
  toInterface _ = Nothing
  collect = foldl1 (\(e1, Interface com1 name tps exts (TypeBody body1))
                     (e2, Interface com2 _ _ _ (TypeBody body2)) ->
                     (e1 `mplus` e2, Interface (com1 <..> Just com2) name tps exts (TypeBody $ body1 ++ body2)))

ambientDeclarations :: [DeclarationElement] -> [(Maybe Exported, Ambient, CommentPlaceholder)]
ambientDeclarations = mapMaybe toAmbient
  where
  toAmbient (AmbientDeclaration com e a) = Just (e, a, com)
  toAmbient _ = Nothing

lift :: (Monoid m) => (a -> m) -> Maybe a -> m
lift f Nothing = mempty
lift f (Just a) = f a

surround :: H.Html -> H.Html -> H.Html -> H.Html
surround open close inside = open >> inside >> close

parens = surround (syntax "(") (syntax ")")
angles = surround (syntax "<") (syntax ">")
braces = surround (syntax "{") (syntax "}")
squares = surround (syntax "[") (syntax "]")

renderComment :: CommentPlaceholder -> H.Html
renderComment (Left _) = mempty
renderComment (Right comments) = do
  flip mapM_ (text comments) $ \line ->
    H.p ! A.class_ "comments" $ H.toHtml line
  let groups = groupBy ((==) `on` fst) (other comments)
  flip mapM_ groups $ \all@((key,_):_) -> do
    H.p $ H.strong $ H.toHtml key
    H.ul $ mapM_ (H.li . renderCommentKeyValuePair) all

renderCommentKeyValuePair :: (String, String) -> H.Html
renderCommentKeyValuePair ("see", value) = linkToBookmark (splitOn "." value) $ H.toHtml value
renderCommentKeyValuePair (_, value) = H.toHtml value

intersperseM :: (Functor m, Monad m) => m b -> [m a] -> m [a]
intersperseM sep as = fmap rights $ sequence $ intersperse (fmap Left sep) (map (fmap Right) as)

intersperseM_ :: (Functor m, Monad m) => m b -> [m a] -> m ()
intersperseM_ sep = fmap (const ()) . intersperseM sep

sp :: H.Html
sp = preEscapedToHtml ("&nbsp;" :: String)

withClass :: String -> String -> H.Html
withClass _class text = H.span ! A.class_ (fromString _class) $ H.toHtml text

keyword = withClass "keyword"
ident = withClass "identifier"
syntax = withClass "syntax"
literal = withClass "literal"

comma = syntax ","
colon = syntax ":"

bookmark :: [String] -> H.Html
bookmark names = H.a ! A.name (fromString $ intercalate "-" names) $ mempty

linkToBookmark ::  [String] -> H.Html -> H.Html
linkToBookmark names = H.a ! A.href (fromString ('#' : intercalate "-" names))

renderExported :: Exported -> H.Html
renderExported _ = keyword "export" >> sp

renderStatic :: Static -> H.Html
renderStatic _ = keyword "static" >> sp

renderOptional :: Optional -> H.Html
renderOptional _ = syntax "?"

renderPublicOrPrivate :: PublicOrPrivate -> H.Html
renderPublicOrPrivate Public = keyword "public" >> sp
renderPublicOrPrivate Private = keyword "private" >> sp

renderStringOrNumber :: StringOrNumber -> H.Html
renderStringOrNumber String = keyword "string"
renderStringOrNumber Number = keyword "number"

(<.>) :: CommentPlaceholder -> Maybe CommentPlaceholder -> CommentPlaceholder
_ <.> (Just r@(Right _)) = r
r@(Right _) <.> _ = r
l <.> _ = l

(<..>) :: CommentPlaceholder -> Maybe CommentPlaceholder -> CommentPlaceholder
Right l <..> (Just (Right r)) = Right $ l `mappend` r
l <..> r = l <.> r

renderInterface :: [String] -> Int -> (Maybe Exported, Interface) -> H.Html
renderInterface moduleName level (e, Interface com name typeParameters extends body) = do
  bookmark $ moduleName ++ [name]
  header level $ ident name
  H.div ! A.class_ "interface" $ do
    H.p ! A.class_ "class_decl mono" $ do
      lift renderExported e
      keyword "interface"
      sp
      ident name
      (lift $ angles . intersperseM_ (comma >> sp) . map (renderTypeParameter moduleName)) typeParameters
      lift (\e -> sp >> keyword "extends" >> sp >> intersperseM_ (comma >> sp) (map (renderTypeReference moduleName) e)) extends
    renderComment com
    renderTypeBody moduleName (H.ul . mapM_ H.li) body

predefinedTypeToString :: PredefinedType -> String
predefinedTypeToString AnyType = "any"
predefinedTypeToString NumberType = "number"
predefinedTypeToString BooleanType = "boolean"
predefinedTypeToString StringType = "string"
predefinedTypeToString VoidType = "void"

renderTypeName :: [String] -> TypeName -> H.Html
renderTypeName moduleName (TypeName Nothing name) =
  linkToBookmark (moduleName ++ [name]) $ ident name
renderTypeName _ (TypeName (Just m@(ModuleName moduleName)) name) = do
  renderModuleName m
  syntax "."
  linkToBookmark (moduleName ++ [name]) $ ident name

renderModuleName :: ModuleName -> H.Html
renderModuleName (ModuleName parts) = linkToBookmark parts $ ident $ intercalate "." parts

renderTypeBody :: [String] -> ([H.Html] -> H.Html) -> TypeBody -> H.Html
renderTypeBody moduleName wrap (TypeBody members) = wrap $ map (renderTypeMember moduleName) members

renderParameterListAndReturnType :: [String] -> ParameterListAndReturnType -> H.Html
renderParameterListAndReturnType moduleName (ParameterListAndReturnType gens args ret) = do
  (lift $ angles . intersperseM_ comma . map (renderTypeParameter moduleName)) gens
  (parens . intersperseM_ (comma >> sp) . map (renderParameter moduleName)) args
  lift (renderTypeAnnotation moduleName) ret

renderTypeAnnotation :: [String] -> Type -> H.Html
renderTypeAnnotation moduleName ty = do
  syntax ":"
  sp
  renderType moduleName ty

renderParameter :: [String] -> Parameter -> H.Html
renderParameter moduleName (RequiredOrOptionalParameter publicOrPrivate name opt ty) = do
  lift renderPublicOrPrivate publicOrPrivate
  ident name
  lift renderOptional opt
  lift (renderTypeAnnotation moduleName) ty
renderParameter moduleName (RestParameter name ty) = do
  syntax "..."
  ident name
  lift (renderTypeAnnotation moduleName) ty

renderTypeMember :: [String] -> (CommentPlaceholder, TypeMember) -> H.Html
renderTypeMember moduleName (com, mem) = do
  H.span ! A.class_ "mono" $ renderTypeMember' mem
  renderComment com
  where
  renderTypeMember' (PropertySignature name opt ty) = do
    ident name
    lift renderOptional opt
    colon
    sp
    lift (renderType moduleName) ty
  renderTypeMember' (MethodSignature name opt prt) = do
    ident name
    lift renderOptional opt
    renderParameterListAndReturnType moduleName prt
  renderTypeMember' (CallSignature prt) = renderParameterListAndReturnType moduleName prt
  renderTypeMember' (ConstructSignature gens args ret) = do
    (lift $ angles . intersperseM_ (comma >> sp) . map (renderTypeParameter moduleName)) gens
    (parens . intersperseM_ (comma >> sp) . map (renderParameter moduleName)) args
    lift (renderTypeAnnotation moduleName) ret
  renderTypeMember' (TypeIndexSignature index) = renderIndexSignature moduleName index

renderIndexSignature :: [String] -> IndexSignature -> H.Html
renderIndexSignature moduleName (IndexSignature name key ret) = do
  squares $ do
    ident name
    colon
    renderStringOrNumber key
  renderTypeAnnotation moduleName ret

renderTypeParameter :: [String] -> TypeParameter -> H.Html
renderTypeParameter moduleName (TypeParameter name ext) = do
  ident name
  flip lift ext $ \ty -> do
    keyword "extends"
    sp
    renderType moduleName ty

renderTypeReference :: [String] -> TypeRef -> H.Html
renderTypeReference moduleName (TypeRef name args) = do
  renderTypeName moduleName name
  (lift $ angles . intersperseM_ (comma >> sp) . map (renderType moduleName)) args

renderType :: [String] -> Type -> H.Html
renderType _ (Predefined pre) = keyword $ predefinedTypeToString pre
renderType moduleName (TypeReference tr) = renderTypeReference moduleName tr
renderType moduleName (ObjectType body) = renderTypeBody moduleName (braces . intersperseM_ (comma >> sp)) body
renderType moduleName (ArrayType ty) = renderType moduleName ty >> syntax "[]"
renderType moduleName (FunctionType gens args ret) = do
  (lift $ angles . intersperseM_ (comma >> sp) . map (renderTypeParameter moduleName)) gens
  (parens . intersperseM_ (comma >> sp) . map (renderParameter moduleName)) args
  renderTypeAnnotation moduleName ret
renderType moduleName (ConstructorType gens args ret) = do
  keyword "new"
  sp
  (lift $ angles . intersperseM_ (comma >> sp) . map (renderTypeParameter moduleName)) gens
  (parens . intersperseM_ (comma >> sp) . map (renderParameter moduleName)) args
  renderTypeAnnotation moduleName ret

renderAmbientClassBodyElement :: [String] -> (CommentPlaceholder, AmbientClassBodyElement) -> H.Html
renderAmbientClassBodyElement moduleName (com, el) = do
  H.span ! A.class_ "mono" $ renderAmbientClassBodyElement' el
  renderComment com
  where
  renderAmbientClassBodyElement' (AmbientConstructorDeclaration args) = do
    keyword "constructor"
    (parens . intersperseM_(comma >> sp) . map (renderParameter moduleName)) args
  renderAmbientClassBodyElement' (AmbientMemberDeclaration publicOrPrivate static name (Left ty)) = do
    lift renderPublicOrPrivate publicOrPrivate
    lift renderStatic static
    keyword "var"
    sp
    ident name
    lift (renderTypeAnnotation moduleName) ty
  renderAmbientClassBodyElement' (AmbientMemberDeclaration publicOrPrivate static name (Right prt)) = do
    lift renderPublicOrPrivate publicOrPrivate
    lift renderStatic static
    keyword "function"
    sp
    ident name
    renderParameterListAndReturnType moduleName prt
  renderAmbientClassBodyElement' (AmbientIndexSignature indexSignature) = renderIndexSignature moduleName indexSignature

ambientVariableDeclarations :: [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> [(Maybe Exported, String, Maybe Type, CommentPlaceholder)]
ambientVariableDeclarations = mapMaybe p
  where
  p (e, AmbientVariableDeclaration com name ty, com1) = Just (e, name, ty, com <.> com1)
  p _ = Nothing

renderAmbientVariableDeclaration :: [String] -> (Maybe Exported, String, Maybe Type, CommentPlaceholder) -> H.Html
renderAmbientVariableDeclaration moduleName (e, name, ty, com) = do
  do
    H.span ! A.class_ "mono" $ do
      lift renderExported e
      keyword "var"
      sp
      ident name
      lift (renderTypeAnnotation moduleName) ty
    renderComment com

ambientFunctionDeclarations :: [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> [(Maybe Exported, String, ParameterListAndReturnType, CommentPlaceholder)]
ambientFunctionDeclarations = mapMaybe p
  where
  p (e, AmbientFunctionDeclaration com name ps, com1) = Just (e, name, ps, com <.> com1)
  p _ = Nothing

renderAmbientFunctionDeclaration :: [String] -> (Maybe Exported, String, ParameterListAndReturnType, CommentPlaceholder) -> H.Html
renderAmbientFunctionDeclaration moduleName (e, name, prt, com) =
  do
    H.span ! A.class_ "mono" $ do
      lift renderExported e
      keyword "function"
      sp
      ident name
      renderParameterListAndReturnType moduleName prt
    renderComment com

ambientClassDeclarations :: [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> [(Maybe Exported, String, Maybe [TypeParameter], Maybe [TypeRef], Maybe [TypeRef], [(CommentPlaceholder, AmbientClassBodyElement)], CommentPlaceholder)]
ambientClassDeclarations = mapMaybe p
  where
  p (e, AmbientClassDeclaration com name tps exts imps els, com1) = Just (e, name, tps, exts, imps, els, com <.> com1)
  p _ = Nothing

renderAmbientClassDeclaration :: [String] -> Int -> (Maybe Exported, String, Maybe [TypeParameter], Maybe [TypeRef], Maybe [TypeRef], [(CommentPlaceholder, AmbientClassBodyElement)], CommentPlaceholder) -> H.Html
renderAmbientClassDeclaration moduleName level (e, name, gens, exts, imps, els, com) = do
  bookmark $ moduleName ++ [name]
  header level $ do
    lift renderExported e
    H.toHtml name
  H.div ! A.class_ "class" $ do
    renderComment com
    H.p ! A.class_ "class_decl mono" $ do
      keyword "class"
      sp
      ident name
      (lift $ angles . intersperseM_ (comma >> sp) . map (renderTypeParameter moduleName)) gens
      lift (\e -> sp >> keyword "extends" >> sp >> intersperseM_ (comma >> sp) (map (renderTypeReference moduleName) e)) exts
      lift (\e -> sp >> keyword "implements" >> sp >> intersperseM_ (comma >> sp) (map (renderTypeReference moduleName) e)) imps
    H.ul $ mapM_ (H.li . renderAmbientClassBodyElement moduleName) els

ambientInterfaceDeclarations :: [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> [(Maybe Exported, Interface)]
ambientInterfaceDeclarations = mapMaybe p
  where
  p (e, AmbientInterfaceDeclaration (Interface com a b c d), com1) = Just (e, Interface (com <.> com1) a b c d)
  p _ = Nothing

renderAmbientInterfaceDeclaration :: [String] -> Int -> (Maybe Exported, Interface) -> H.Html
renderAmbientInterfaceDeclaration moduleName level = renderInterface moduleName level

ambientEnumDeclarations :: [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> [(Maybe Exported, String, [(String, Maybe Integer)], CommentPlaceholder)]
ambientEnumDeclarations = mapMaybe p
  where
  p (e, AmbientEnumDeclaration com name els, com1) = Just (e, name, els, com <.> com1)
  p _ = Nothing

renderAmbientEnumDeclaration :: [String] -> (Maybe Exported, String, [(String, Maybe Integer)], CommentPlaceholder) -> H.Html
renderAmbientEnumDeclaration moduleName (e, name, els, comments) =
  H.div ! A.class_ "enum" $ do
    bookmark $ moduleName ++ [name]
    renderComment comments
    lift renderExported e
    keyword "enum"
    sp
    ident name
    H.ul $ flip mapM_ els $ \(name, value) -> H.li ! A.class_ "mono" $ do
      ident name
      flip lift value $ \val -> do
        sp
        syntax "="
        sp
        literal (show val)

ambientModuleDeclarations :: [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> [(Maybe Exported, String, [Ambient], CommentPlaceholder)]
ambientModuleDeclarations =
  sortBy (comparing $ \(_, name, _, _) -> name)
  . map collect
  . groupBy ((==) `on` \(_, name, _, _) -> name)
  . mapMaybe p
  where
  p (e, AmbientModuleDeclaration com names ambs, com1) = Just (e, intercalate "." names, ambs, com <.> com1)
  p (e, AmbientExternalModuleDeclaration com name ambs, com1) = Just (e, name, ambs, com <.> com1)
  p _ = Nothing
  collect = foldl1 (\(e1, name, els1, com1) (e2, _, els2, com2) -> (e1 `mplus` e2, name, els1 ++ els2, com1 <..> Just com2))

renderAmbientModuleDeclaration :: [String] -> Int -> (Maybe Exported, String, [Ambient], Maybe CommentPlaceholder) -> H.Html
renderAmbientModuleDeclaration moduleName level (e, name, els, comments) =
  H.div ! A.class_ "section" $ do
    bookmark moduleName
    header level $ H.toHtml $ "Module " ++ (intercalate "." moduleName)
    lift renderComment comments
    renderAmbientDeclarations moduleName (level + 1) $ map (\a -> (e, a, Nothing)) els

renderAmbientDeclarations :: [String] -> Int -> [(Maybe Exported, Ambient, Maybe CommentPlaceholder)] -> H.Html
renderAmbientDeclarations moduleName level ds = do
  let vars       = ambientVariableDeclarations ds
  let functions  = ambientFunctionDeclarations ds
  let classes    = ambientClassDeclarations ds
  let interfaces = ambientInterfaceDeclarations ds
  let enums      = ambientEnumDeclarations ds
  let modules    = ambientModuleDeclarations ds
  section H.ul H.li "Variables"  vars       (renderAmbientVariableDeclaration moduleName)
  section H.ul H.li "Functions"  functions  (renderAmbientFunctionDeclaration moduleName)
  section id   id   "Classes"    classes    (renderAmbientClassDeclaration moduleName (level + 1))
  section id   id   "Interfaces" interfaces (renderAmbientInterfaceDeclaration moduleName (level + 1))
  section id   id   "Enums"      enums      (renderAmbientEnumDeclaration moduleName)
  section id   id   "Modules"    modules    $ \m@(a, name, b, c) -> renderAmbientModuleDeclaration (moduleName ++ [name]) (level + 1) (a, name, b, Just c)
  where
    section :: (H.Html -> H.Html) -> (H.Html -> H.Html) -> String -> [a] -> (a -> H.Html) -> H.Html
    section outer inner name xs render =
      unless (null xs) $ H.div ! A.class_ "section" $ do
        header level $ H.toHtml name
        outer $ mapM_ (inner . render) xs

generateDocument :: [DeclarationElement] -> H.Html
generateDocument ds = do
  H.docType
  H.html $ do
    H.head (H.title mempty)
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "style.css"
    H.body $ do
      let interfaces = topLevelInterfaces ds
      unless (null interfaces) $ do
        header 1 $ H.toHtml ("Interfaces" :: String)
        mapM_ (renderInterface [] 2) interfaces
      let declarations = ambientDeclarations ds
      unless (null interfaces) $ do
        header 1 $ H.toHtml ("Declarations" :: String)
        renderAmbientDeclarations [] 2 (map (\(a, b, c) -> (a, b, Just c)) declarations)
