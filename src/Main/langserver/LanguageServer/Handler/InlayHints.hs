------------------------------------------------------------------------------
-- Copyright 2023, Tim Whiting
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer.Handler.InlayHints (inlayHintsHandler) where

import Debug.Trace
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))
import Data.Maybe (mapMaybe)
import Data.List(intersperse,nubBy)
import Common.File(startsWith)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Lens as J
import Lib.PPrint
import Common.Name
import Common.Range
import Type.Pretty (ppType, Env (..), defaultEnv, ppScheme)
import Kind.ImportMap (ImportMap)
import Compiler.Compile (Module(..), Loaded (..))
import Compiler.Module (modLexemes)
import Compiler.Options (prettyEnvFromFlags, Flags)
import Syntax.RangeMap (NameInfo (..), RangeInfo (..), rangeMapFindIn, lexemesFromPos)
import Syntax.Lexeme (Lexeme (..), Lex (..))
import Language.LSP.Server (Handlers, sendNotification, requestHandler)
import LanguageServer.Monad (LSM, getLoaded, getLoadedModule, getFlags, getInlayHintOptions, InlayHintOptions (..), getLoadedLatest)
import LanguageServer.Conversions (fromLspPos, toLspRange, toLspPos, fromLspRange)
import LanguageServer.Handler.Hover (formatRangeInfoHover)


-- The LSP handler that provides inlay hints (inline type annotations etc)
inlayHintsHandler :: Handlers LSM
inlayHintsHandler = requestHandler J.SMethod_TextDocumentInlayHint $ \req responder -> do
  let J.InlayHintParams prog doc rng = req ^. J.params
      uri = doc ^. J.uri
      normUri = J.toNormalizedUri uri
  options <- getInlayHintOptions
  newRng <- liftIO $ fromLspRange normUri rng
  loaded <- getLoadedLatest normUri -- don't trust outdated versions
  flags <- getFlags
  let rsp = do -- maybe monad
        l <- loaded
        let lm = loadedModule l
        rmap <- modRangeMap lm
        -- trace (show $ rangeMapFindIn True newRng rmap) $ return ()
        let env = (prettyEnvFromFlags flags){
                      context = modName lm,
                      importsMap = loadedImportMap l,
                      showFlavours=False
                      -- fullNames=showFullQualifiers options
                   }
        let hints = concatMap (createInlayHints options env lm) $ rangeMapFindIn True {-for inlay hints-} newRng rmap
        let hintsDistinct = nubBy (\h1 h2 -> h1 ^. J.position == h2 ^. J.position) hints
        return hintsDistinct
  case rsp of
    Nothing  -> responder $ Right $ J.InR J.Null
    Just rsp -> responder $ Right $ J.InL rsp


-- | Create inlay hints at some token
createInlayHints :: InlayHintOptions -> Env -> Module -> (Range, RangeInfo) -> [J.InlayHint]
createInlayHints opts env mod (rng, rinfo)
  = concat [
      guard showFullQualifiers    $ qualifierHint env (modName mod) lexemes rng rinfo,
      guard showInferredTypes     $ typeHint env lexemes rng rinfo,
      guard showImplicitArguments $ implicitsHint env lexemes rng rinfo
    ]
  where
    guard flag action
      = if flag opts then action else []

    lexemes
      = lexemesFromPos (rangeEnd rng) (modLexemes mod)


-- | Show type hint
typeHint :: Env -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]
typeHint env lexemes range rinfo
  = case rinfo of
      Id qname (NIValue _ tp _doc isAnnotated) _idocs isDef | isDef && not isAnnotated && not hasAnnot
        -> -- trace ("typeHint: " ++ show qname ++ ": " ++ show tp) $
           let s   = show (ppScheme env tp)
               fmt = " : " ++ (if s `startsWith` "->" then drop 3 s else s)  -- for `-> eff res` types
           in [newInlayHint range fmt J.InlayHintKind_Type False]
      _ -> []
  where
    hasAnnot = case lexemes of
                 Lexeme _ (LexKeyword ":" _) : _      -> True
                 _ : Lexeme _ (LexKeyword ":" _) : _  -> True
                 _                                    -> False

-- | Show qualified name
qualifierHint :: Env -> Name -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]
qualifierHint env modName lexemes rng rinfo
  = case rinfo of
      Id qname _ _ _  | qname /= unqualifyFull qname
        -> -- trace ("qualifierHint: " ++ show rinfo ++ ": " ++ show (take 1 lexemes)) $
           let qual = getQualifier qname
           in if null qual then [] else [newInlayHint (rangeJustBefore rng) qual J.InlayHintKind_Type False]
      _ -> []
  where
    getQualifier qname
      = case lexemes of
          (Lexeme rng (LexId name)):_ | unqualifyFull qname == unqualifyFull name
            -> missingQualifier modName name qname
          (Lexeme rng (LexOp name)):_ | unqualifyFull qname == unqualifyFull name
            -> let qual = missingQualifier modName name qname
               in if null qual then "" else " " ++ qual
          (Lexeme rng (LexIdOp name)) :_  | unqualifyFull qname == unqualifyFull name
            -> let qual = missingQualifier modName name qname
               in if null qual then "" else " " ++ qual
          _ -> ""

-- | Show implicit arguments
implicitsHint :: Env -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]
implicitsHint env lexemes rng rinfo
  = case rinfo of
      Id _qname (NIValue _ _tp _doc _isannot) implicitDocs _isDef  | not (null implicitDocs)
        -> -- trace ("implicitsHint: " ++ show rinfo ++ ": " ++ show (take 4 lexemes)) $
           [format implicitDocs]
      _ -> []
  where
    format :: [Doc] -> J.InlayHint
    format docs
      = let (crange,adjust) = finalCallRange lexemes
            fmt = show $ adjust $ hcat (intersperse comma docs)
        in newInlayHint crange fmt J.InlayHintKind_Parameter False


finalCallRange :: [Lexeme] -> (Range, Doc -> Doc)
finalCallRange (lex1:lexes)
  = if isDotFunction
      then (erange,parens)
      else (findCallEnd lexes 0, case lexes of
                                   Lexeme _ (LexSpecial "("):Lexeme _ (LexSpecial ")"):_
                                     -> id
                                   _ -> (\doc -> comma <.> doc))
  where
    erange
      = endOfRange (getRange lex1)

    isDotFunction
      = case lexes of
          (Lexeme _ (LexKeyword "." _)):_ -> True
          (Lexeme _ (LexSpecial ";")):_   -> True
          (Lexeme _ LexInsSemi):_         -> True
          (Lexeme _ LexInsLCurly):_       -> True
          _ -> case lex1 of
                 Lexeme _ (LexOp _) -> True  -- add parenthesis
                 _                  -> False

    findCallEnd lexemes depth
      = case lexemes of
          Lexeme r (LexSpecial "("):rst -> findCallEnd rst (depth + 1)
          Lexeme r (LexSpecial ")"):rst
            -> if depth > 1 then findCallEnd rst (depth - 1)
                            else rangeJustBefore r
          lexeme : rst -> findCallEnd rst depth
          []           -> erange


-- | Create a new inlay hint
newInlayHint :: Range -> String -> J.InlayHintKind -> Bool -> J.InlayHint
newInlayHint rng fmt kind padding
  = let posEnd   = rangeEnd rng
        position = toLspPos posEnd{posColumn = posColumn posEnd + 1}
        text     = T.pack fmt
    in J.InlayHint position (J.InL text) (Just kind)
             (Just [J.TextEdit (J.Range position position) text])
             Nothing (Just padding) (Just padding) Nothing

