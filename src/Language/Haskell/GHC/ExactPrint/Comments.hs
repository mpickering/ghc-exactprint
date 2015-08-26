{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}
module Language.Haskell.GHC.ExactPrint.Comments (balanceComments) where

import qualified SrcLoc as GHC
import SrcLoc (SrcSpan, isSubspanOf)
import ApiAnnotation
import Data.Data
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types
import Control.Monad.State
import qualified Data.Generics as SYB
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

-- |
-- If the comment trails a node we associate it with the innermost,
-- rightmost node which ends before it on the same line.
--
-- If the comment is on a new line then we associate it with the
-- immediately following node.
--
balanceComments :: Data ast => GHC.Located ast -> Anns -> Anns
balanceComments ast as =
  execState (SYB.everywhereM (return `SYB.ext2M` reallocate) ast) as
  where
    reallocate :: (Data a, Data b) => GHC.GenLocated a b -> State Anns (GHC.GenLocated a b)
    reallocate l@(GHC.L ss body) =
      case cast ss of
        Nothing -> return l
        Just newL -> do
            let cn = CN . show . toConstr $ body
                ak = AnnKey newL cn
                Ann{..} = fromMaybe annNone (Map.lookup ak as)
            newPrior <- reallocateCs annPriorComments
            -- Alter as it may change during realloc
            modify (Map.adjust (\a -> a {annPriorComments = newPrior}) ak )
            return l

    reallocateCs :: [(Comment, DeltaPos)] -> State Anns [(Comment, DeltaPos)]
    reallocateCs cs = (++ toKeep) . catMaybes <$> mapM moveComment toMove
      where
        (toMove, toKeep) = span (\(c, DP (l,_)) -> l == 0 && isNothing (commentOrigin c)) cs


    moveComment :: (Comment, DeltaPos) -> State Anns (Maybe (Comment, DeltaPos))
    moveComment com@(c,_) =
      case execState (collect After c ast) Nothing of
        Nothing -> return (Just com)
        Just ak@(AnnKey ss _) ->

          -- Only move if actually on the same line, not following some
          -- cruft
          if fst (ss2posEnd ss) == fst (ss2pos (commentIdentifier c))
              then do
                -- Check that we are not going to move it outside the
                -- parent
                    let parent = findEnclosure (commentIdentifier c) ast
                    traceShowM parent
                    if maybe True (ss `isSubspanOf`) parent
                      then do
                        Just ann <- gets (Map.lookup ak)
                        let badTrailing = case annsDP ann of
                                            [] -> False
                                            (last -> (c,_)) -> c `elem` [AnnSemiSep, G AnnComma]
                        if badTrailing
                          then return (Just com)
                          else do
                            modify (Map.adjust (\a -> a {annFollowingComments = annFollowingComments a ++ [com]}) ak)
                            return Nothing
                      else return (Just com)
              else return (Just com)


-- Find the smallest span for which the specified span is a subspan
findEnclosure :: (Data a, Typeable a) => SrcSpan -> a -> Maybe SrcSpan
findEnclosure target ast = execState (SYB.everywhereM (return `SYB.ext2M` checkEnclosed) ast) Nothing
  where
    checkEnclosed :: (Data a, Data b) => GHC.GenLocated a b -> State (Maybe SrcSpan) (GHC.GenLocated a b)
    checkEnclosed l@(GHC.L ss body) =
      case cast ss of
        Nothing -> return l
        Just ss -> if target `isSubspanOf` ss
                    then do
                          modify (\mp ->
                                    case mp of
                                      Nothing -> Just ss
                                      Just curParent ->
                                        if ss `isSubspanOf` curParent
                                          then Just ss
                                          else Just curParent)
                          return l
                    else return l





collect :: (Data a,Typeable a) => ComInfoLocation -> Comment -> a -> State (Maybe AnnKey) a
collect loc' c ast =
  SYB.everywhereM (return `SYB.ext2M` collectOne) ast
  where
    collectOne :: (Data a, Data b) => GHC.GenLocated a b -> State (Maybe AnnKey) (GHC.GenLocated a b)
    collectOne l@(GHC.L ss body) =
      case cast ss of
        Nothing -> return l
        Just newL -> do
            let cn = CN . show . toConstr $ body
                ak = AnnKey newL cn
            when (commentLocated loc' newL c) (do
              (modify (maybe (Just ak)
                            (\oldak@(AnnKey oldL _) ->
                             Just (if (test loc' oldL newL)
                                      then  traceShow (loc', "Updating", ak) ak
                                      else  oldak)))))
            return l

test After = testBefore Biggest
test Before = testAfter

-- Locate with the previous biggest closest thing
testAfter old new =
                     (srcSpanStartLine new < srcSpanStartLine old) ||
                      (srcSpanStartLine new == srcSpanStartLine old &&
                       srcSpanStartColumn new < srcSpanStartColumn old) ||
                      (GHC.srcSpanStart new == GHC.srcSpanStart old &&
                        old `isSubspanOf` new)


-- Smallest preceeding element

data Size = Biggest | Smallest

testBefore size old new =
                     (srcSpanEndLine new > srcSpanEndLine old) ||
                      (srcSpanEndLine new == srcSpanEndLine old &&
                       srcSpanEndColumn new > srcSpanEndColumn old)
--                      (GHC.srcSpanEnd new == GHC.srcSpanEnd old &&
--                        new `isSubspanOf` old)
--  where
--    (old, new) = case size of
--                   Biggest -> (s1, s2)
--                   Smallest -> (s2, s1)


-- | Is the comment after the node?
commentLocated :: ComInfoLocation -> SrcSpan -> Comment -> Bool
commentLocated loc' ss (Comment _ c _) =
  spanTest loc' ss c

data ComInfoLocation = Before | After deriving (Show)

-- | For @After@, does the first span end before the second starts?
-- For @Before@, does the first span start after the second ends?
spanTest :: ComInfoLocation -> SrcSpan -> SrcSpan -> Bool
spanTest loc' first second =
  (srcSpanStartLine after > srcSpanEndLine before) ||
  ((srcSpanStartLine after == srcSpanEndLine before) &&
   (srcSpanStartColumn after > srcSpanEndColumn before))
  where (before,after) =
          case loc' of
            After -> (first,second)
            Before -> (second,first)

