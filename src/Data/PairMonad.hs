
{-# LANGUAGE
    CPP,
    StandaloneDeriving,
    DeriveFoldable,
    DeriveTraversable #-}

module Data.PairMonad where

import Control.Applicative

-- #ifdef __HAS_LENS__
import Control.Lens()
-- #endif __HAS_LENS__

import Data.Monoid
import Data.Foldable
import Data.Traversable

-- #ifndef __HAS_LENS__
-- deriving instance Foldable ((,) o)
-- deriving instance Traversable ((,) o)
-- #endif __HAS_LENS__
