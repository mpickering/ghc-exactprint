{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.GHC.ExactPrint
        ( -- * Relativising
          relativiseApiAnns
        , relativiseApiAnnsWithComments
        , Anns
        , Comment

        -- * Printing
        , exactPrintWithAnns
        , exactPrint

        -- * Utility
        , Parser
        , parseModule
        , parseExpr
        , parseImport
        , parseType
        , parseDecl
        , parsePattern
        , parseStmt
        , parseWith

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Print

import Language.Haskell.GHC.ExactPrint.Preprocess

import GHC.Paths (libdir)

import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified RdrHsSyn as GHC ( checkPattern )
import qualified OrdList as OL

import qualified Data.Map as Map
import Control.Monad (void)


runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.RdrName))
parseFile = runParser GHC.parseModule

parseWith :: GHC.P w
          -> String
          -> IO (Either (GHC.SrcSpan, String) (GHC.ApiAnns, w))
parseWith f s =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- GHC.getSessionDynFlags
      void $ GHC.setSessionDynFlags dflags
      return $
        -- AZ: It may be worth passing in a parameter for the filename instead of "<interactive>".
        --     This allows multiple changes to co-exist as having guaranteed different spans.
        case runParser f dflags "<Interactive>" s of
          GHC.PFailed ss m -> Left (ss, GHC.showSDoc dflags m)
          GHC.POk (mkApiAnns -> apianns) pmod   -> Right (apianns, pmod)

type Parser a = String -> IO (Either (GHC.SrcSpan, String)
                                     (GHC.ApiAnns, a))

parseExpr :: Parser (GHC.LHsExpr GHC.RdrName)
parseExpr = parseWith GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GHC.RdrName)
parseImport = parseWith GHC.parseImport

parseType :: Parser (GHC.LHsType GHC.RdrName)
parseType = parseWith GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GHC.RdrName)
parseDecl = parseWith (head . OL.fromOL <$> GHC.parseDeclaration)

parseStmt :: Parser (GHC.ExprLStmt GHC.RdrName)
parseStmt = parseWith GHC.parseStatement

-- Interim, see D1005
-- will not parse bang patterns properly
parsePattern :: Parser (GHC.LPat GHC.RdrName)
parsePattern = parseWith (GHC.parseExpression >>= GHC.checkPattern GHC.empty)






initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
  (dflags2, _, _)
    <- GHC.parseDynamicFilePragma dflags1 src_opts
  void $ GHC.setSessionDynFlags dflags2
  return dflags2

parseModule :: FilePath -> IO (Either (GHC.SrcSpan, String) (GHC.ApiAnns, (GHC.Located (GHC.HsModule GHC.RdrName))))
parseModule file =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags file
      (fileContents, _) <-
        if False
          then do
            contents <- getPreprocessedSrcDirect file
            cppComments <- getCppTokensAsComments file
            return (contents,cppComments)
          else do
            txt <- GHC.liftIO $ readFile file
            let (contents1,lp) = stripLinePragmas txt
            return (contents1,lp)
      return $
        case parseFile dflags file fileContents of
          GHC.PFailed ss m -> Left $ (ss, (GHC.showSDoc dflags m))
          GHC.POk (mkApiAnns -> apianns) pmod   -> Right $ (apianns, pmod)


mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate = (Map.fromListWith (++) . GHC.annotations $ pstate
                   , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : (GHC.annotations_comments pstate)))


