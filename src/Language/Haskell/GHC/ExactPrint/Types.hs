{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.GHC.ExactPrint.Types
  ( -- * Core Types
   Anns
  , emptyAnns
  , Annotation(..)
  , annNone

  , KeywordId(..)
  , Comment(..)
  -- * Positions
  , Pos
  , DeltaPos(..)
  , deltaRow, deltaColumn
  -- * AnnKey
  , AnnKey(..)
  , mkAnnKey
  , AnnConName(..)
  , annGetConstr
  -- * Internal Types
  , LayoutStartCol(..)
  , declFun

  ) where

import Data.Data (Data, Typeable, toConstr,cast)

import qualified DynFlags       as GHC
import qualified GHC
import qualified Outputable    as GHC

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data Comment = Comment
    {
      commentContents   :: !String -- ^ The contents of the comment including separators
    , commentIdentifier :: !GHC.SrcSpan -- ^ Needed to uniquely identify two comments with the same contents
    , commentOrigin     :: !(Maybe GHC.AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Eq,Show,Typeable,Data, Ord)

instance GHC.Outputable Comment where
  ppr x = GHC.text (show x)

type Pos = (Int,Int)

-- | A relative positions, row then column
newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

deltaRow, deltaColumn :: DeltaPos -> Int
deltaRow (DP (r, _)) = r
deltaColumn (DP (_, c)) = c


-- | Marks the start column of a layout block.
newtype LayoutStartCol = LayoutStartCol { getLayoutStartCol :: Int }
  deriving (Eq, Num)

instance Show LayoutStartCol where
  show (LayoutStartCol sc) = "(LayoutStartCol " ++ show sc ++ ")"


annNone :: Annotation
annNone = Ann (DP (0,0)) [] [] [] Nothing Nothing

data Annotation = Ann
  {
    -- The first three fields relate to interfacing up into the AST
    annEntryDelta      :: !DeltaPos
    -- ^ Offset used to get to the start of the SrcSpan, from whatever the prior
    -- output was, including all annPriorComments (field below).
  , annPriorComments   :: ![(Comment,  DeltaPos)]
    -- ^ Comments coming after the last non-comment output of the preceding
    -- element but before the SrcSpan being annotated by this Annotation. If
    -- these are changed then annEntryDelta (field above) must also change to
    -- match.
  , annFollowingComments   :: ![(Comment,  DeltaPos)]
    -- ^ Comments coming after the last output for the element subject to this
    -- Annotation. These will only be added by AST transformations, and care
    -- must be taken not to disturb layout of following elements.

  -- The next three fields relate to interacing down into the AST
  , annsDP             :: ![(KeywordId, DeltaPos)]
    -- ^ Annotations associated with this element.
  , annSortKey         :: !(Maybe [GHC.SrcSpan])
    -- ^ Captures the sort order of sub elements. This is needed when the
    -- sub-elements have been split (as in a HsLocalBind which holds separate
    -- binds and sigs) or for infix patterns where the order has been
    -- re-arranged. It is captured explicitly so that after the Delta phase a
    -- SrcSpan is used purely as an index into the annotations, allowing
    -- transformations of the AST including the introduction of new Located
    -- items or re-arranging existing ones.
  , annCapturedSpan    :: !(Maybe AnnKey)
    -- ^ Occasionally we must calculate a SrcSpan for an unlocated list of
    -- elements which we must remember for the Print phase. e.g. the statements
    -- in a HsLet or HsDo. These must be managed as a group because they all
    -- need eo be vertically aligned for the Haskell layout rules, and this
    -- guarantees this property in the presence of AST edits.

  } deriving (Typeable,Eq)

instance Show Annotation where
  show (Ann dp comments fcomments ans sk csp)
    = "(Ann (" ++ show dp ++ ") " ++ show comments ++ " "
        ++ show fcomments ++ " "
        ++ show ans ++ " " ++ showGhc sk ++ " "
        ++ showGhc csp ++ ")"


-- | This structure holds a complete set of annotations for an AST
type Anns = Map.Map AnnKey Annotation

emptyAnns :: Anns
emptyAnns = Map.empty

-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey GHC.SrcSpan AnnConName
                  deriving (Eq, Ord)

-- More compact Show instance
instance Show AnnKey where
  show (AnnKey ss cn) = "AnnKey " ++ showGhc ss ++ " " ++ show cn

mkAnnKey :: (Data a) => GHC.Located a -> AnnKey
mkAnnKey (GHC.L l a) = AnnKey l (annGetConstr a)

-- Holds the name of a constructor
data AnnConName = CN { unConName :: String }
                 deriving (Eq,Ord)

-- More compact show instance
instance Show AnnConName where
  show (CN s) = "CN " ++ show s

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

-- | The different syntactic elements which are not represented in the
-- AST.
data KeywordId = G GHC.AnnKeywordId  -- ^ A normal keyword
               | AnnSemiSep          -- ^ A seperating comma
               | AnnComment Comment
               | AnnString String    -- ^ Used to pass information from
                                     -- Delta to Print when we have to work
                                     -- out details from the original
                                     -- SrcSpan.
               | AnnUnicode GHC.AnnKeywordId -- ^ Used to indicate that we should print using unicode syntax if possible.
               deriving (Eq,Ord)

instance Show KeywordId where
  show (G gc)          = "(G " ++ show gc ++ ")"
  show AnnSemiSep      = "AnnSemiSep"
  show (AnnComment dc) = "(AnnComment " ++ show dc ++ ")"
  show (AnnString s)   = "(AnnString " ++ s ++ ")"
  show (AnnUnicode gc) = "(AnnUnicode " ++ show gc ++ ")"

-- ---------------------------------------------------------------------

instance GHC.Outputable KeywordId where
  ppr k     = GHC.text (show k)

instance GHC.Outputable (AnnConName) where
  ppr tr     = GHC.text (show tr)

instance GHC.Outputable Annotation where
  ppr a     = GHC.text (show a)

instance GHC.Outputable AnnKey where
  ppr a     = GHC.text (show a)

instance GHC.Outputable DeltaPos where
  ppr a     = GHC.text (show a)

-- ---------------------------------------------------------------------

declFun :: (forall a . Data a => GHC.Located a -> b) -> GHC.LHsDecl GHC.RdrName -> b
declFun f (GHC.L l de) =
  case de of
      GHC.TyClD d       -> f (GHC.L l d)
      GHC.InstD d       -> f (GHC.L l d)
      GHC.DerivD d      -> f (GHC.L l d)
      GHC.ValD d        -> f (GHC.L l d)
      GHC.SigD d        -> f (GHC.L l d)
      GHC.DefD d        -> f (GHC.L l d)
      GHC.ForD d        -> f (GHC.L l d)
      GHC.WarningD d    -> f (GHC.L l d)
      GHC.AnnD d        -> f (GHC.L l d)
      GHC.RuleD d       -> f (GHC.L l d)
      GHC.VectD d       -> f (GHC.L l d)
      GHC.SpliceD d     -> f (GHC.L l d)
      GHC.DocD d        -> f (GHC.L l d)
      GHC.RoleAnnotD d  -> f (GHC.L l d)
#if __GLASGOW_HASKELL__ < 711
      GHC.QuasiQuoteD d -> f (GHC.L l d)
#endif


-- ---------------------------------------------------------------------

-- Duplicated here so it can be used in show instances
showGhc :: (GHC.Outputable a) => a -> String
showGhc = GHC.showPpr GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------

