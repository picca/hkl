{-# LANGUAGE CPP #-}

module Hkl.PyFAI.PoniExt
       ( PoniExt(..)
       , Pose
       , setPose
       , Hkl.PyFAI.PoniExt.flip
       ) where

import Data.Vector.Storable
import Hkl.PyFAI.Poni
import Hkl.MyMatrix
import Numeric.LinearAlgebra

#if !MIN_VERSION_hmatrix(0, 17, 0)
tr:: Matrix t -> Matrix t
tr = trans
#endif

-- | Types

type Pose = MyMatrix Double

data PoniExt = PoniExt { poniExtPoni :: Poni
                       , poniExtPose :: Pose
                       } deriving (Show)


setPoniEntryPose :: MyMatrix Double -> MyMatrix Double -> PoniEntry -> PoniEntry
setPoniEntryPose mym1 mym2 e = e { poniEntryRot1 = new_rot1
                                 , poniEntryRot2 = new_rot2
                                 , poniEntryRot3 = new_rot3
                                 }
  where
    rot1 = poniEntryRot1 e
    rot2 = poniEntryRot2 e
    rot3 = poniEntryRot3 e
    rotations = Prelude.map (uncurry fromAxisAndAngle)
                [ (Data.Vector.Storable.fromList [0, 0, 1], rot3)
                , (Data.Vector.Storable.fromList [0, 1, 0], rot2)
                , (Data.Vector.Storable.fromList [1, 0, 0], rot1)]
    -- M1 . R0 = R1
    r1 = Prelude.foldl (<>) (ident 3) rotations -- pyFAIB
    -- M2 . R0 = R2
    -- R2 = M2 . M1.T . R1
    r2 = Prelude.foldl (<>) m2 [tr m1, r1]
    (new_rot1, new_rot2, new_rot3) = toEulerians r2

    (MyMatrix _ m1) = changeBase mym1 PyFAIB
    (MyMatrix _ m2) = changeBase mym2 PyFAIB

setPose :: PoniExt -> Pose -> PoniExt
setPose (PoniExt p1 mym1) mym2 = PoniExt p2 mym2
  where
    p2 = Prelude.map (setPoniEntryPose mym1 mym2) p1

flip :: PoniExt -> PoniExt
flip p = p { poniExtPoni = [flipPoniEntry e | e <- (poniExtPoni p)] }
