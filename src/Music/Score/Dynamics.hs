
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides functions for manipulating dynamics.
--
-------------------------------------------------------------------------------------

module Music.Score.Dynamics (

        -- * Dynamic type functions
        Dynamic,
        -- SetDynamic,
        -- DynamicLensLaws',
        -- DynamicLensLaws,

        -- * Accessing dynamics
        HasDynamics(..),
        HasDynamic(..),
        HasDynamics',
        HasDynamic',
        dynamic',
        dynamics',

        -- * Manipulating dynamics
        Level,
        Attenuable,
        louder,
        softer,
        level,
        compressor,
        fadeIn,
        fadeOut,

        DynamicT(..),

        -- * Context
        -- TODO move
        Ctxt,
        vdynamic,
        addDynCon,

  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Comonad
import           Control.Lens                  hiding (Level, transform)
import           Control.Monad
import           Data.AffineSpace
import           Data.Foldable
import           Data.Functor.Couple
import           Data.Functor.Context
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace              hiding (Sum)

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo
import           Music.Score.Internal.Util     (through)
import           Music.Time
import           Music.Time.Internal.Transform


-- |
-- Dynamics type.
--
type family Dynamic (s :: *) :: *

-- |
-- Dynamic type.
--
type family SetDynamic (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single dynamic.
--
class (HasDynamics s t a b) => HasDynamic s t a b where

  -- | Access a single dynamic.
  dynamic :: Lens s t a b


-- type DynamicLensLaws' s t a b = (
--   Dynamic (SetDynamic a s) ~ a,
--   SetDynamic (Dynamic t) s ~ t,
--   SetDynamic a (SetDynamic b s) ~ SetDynamic a s
--   )
-- 
-- type DynamicLensLaws s t = DynamicLensLaws' s t (Dynamic s) (Dynamic t)

-- |
-- Class of types that provide a dynamic traversal.
--
class (
  Transformable (Dynamic s), a ~ Dynamic s,
  Transformable (Dynamic t), b ~ Dynamic t
  -- SetDynamic (Dynamic t) s ~ t
  -- DynamicLensLaws s t
  ) => HasDynamics s t a b where

  -- | Access all dynamics.
  dynamics :: Traversal s t a b

type HasDynamic'  s a = HasDynamic  s s a a
type HasDynamics' s a = HasDynamics s s a a

-- | Access a single dynamic.
dynamic' :: HasDynamic' s a => Lens' s a
dynamic' = dynamic

-- | Access all dynamics.
dynamics' :: HasDynamics' s a => Traversal' s a
dynamics' = dynamics

#define PRIM_DYNAMIC_INSTANCE(TYPE)       \
                                          \
type instance Dynamic TYPE = TYPE;        \
type instance SetDynamic a TYPE = a;      \
                                          \
instance (Transformable a, a ~ Dynamic a, SetDynamic TYPE a ~ TYPE) \
  => HasDynamic TYPE a TYPE a where {            \
  dynamic = ($)              } ;          \
                                          \
instance (Transformable a, a ~ Dynamic a, SetDynamic TYPE a ~ TYPE) \
  => HasDynamics TYPE a TYPE a where {           \
  dynamics = ($)               } ;        \

PRIM_DYNAMIC_INSTANCE(())
PRIM_DYNAMIC_INSTANCE(Bool)
PRIM_DYNAMIC_INSTANCE(Ordering)
PRIM_DYNAMIC_INSTANCE(Char)
PRIM_DYNAMIC_INSTANCE(Int)
PRIM_DYNAMIC_INSTANCE(Integer)
PRIM_DYNAMIC_INSTANCE(Float)
PRIM_DYNAMIC_INSTANCE(Double)

type instance Dynamic (c,a)               = Dynamic a
type instance SetDynamic b (c,a)          = (c,SetDynamic b a)
type instance Dynamic [a]                 = Dynamic a
type instance SetDynamic b [a]            = [SetDynamic b a]

type instance Dynamic (Maybe a)           = Dynamic a
type instance SetDynamic b (Maybe a)      = Maybe (SetDynamic b a)
type instance Dynamic (Either c a)        = Dynamic a
type instance SetDynamic b (Either c a)   = Either c (SetDynamic b a)

type instance Dynamic (Note a)            = Dynamic a
type instance SetDynamic b (Note a)       = Note (SetDynamic b a)
type instance Dynamic (Delayed a)         = Dynamic a
type instance SetDynamic b (Delayed a)    = Delayed (SetDynamic b a)
type instance Dynamic (Stretched a)       = Dynamic a
type instance SetDynamic b (Stretched a)  = Stretched (SetDynamic b a)

type instance Dynamic (Voice a)       = Dynamic a
type instance SetDynamic b (Voice a)  = Voice (SetDynamic b a)
type instance Dynamic (Chord a)       = Dynamic a
type instance SetDynamic b (Chord a)  = Chord (SetDynamic b a)
type instance Dynamic (Track a)       = Dynamic a
type instance SetDynamic b (Track a)  = Track (SetDynamic b a)
type instance Dynamic (Score a)       = Dynamic a
type instance SetDynamic b (Score a)  = Score (SetDynamic b a)

instance HasDynamic a b da db => HasDynamic (c, a) (c, b) da db where
  dynamic = _2 . dynamic

instance HasDynamics a b da db => HasDynamics (c, a) (c, b) da db where
  dynamics = traverse . dynamics

instance HasDynamics a b da db => HasDynamics [a] [b] da db where
  dynamics = traverse . dynamics

instance HasDynamics a b da db => HasDynamics (Maybe a) (Maybe b) da db where
  dynamics = traverse . dynamics

instance HasDynamics a b da db => HasDynamics (Either c a) (Either c b) da db where
  dynamics = traverse . dynamics



instance (HasDynamics a b da db) => HasDynamics (Note a) (Note b) da db where
  dynamics = _Wrapped . whilstL dynamics

instance (HasDynamic a b da db) => HasDynamic (Note a) (Note b) da db where
  dynamic = _Wrapped . whilstL dynamic


instance (HasDynamics a b da db) => HasDynamics (Delayed a) (Delayed b) da db where
  dynamics = _Wrapped . whilstLT dynamics

instance (HasDynamic a b da db) => HasDynamic (Delayed a) (Delayed b) da db where
  dynamic = _Wrapped . whilstLT dynamic


instance (HasDynamics a b da db) => HasDynamics (Stretched a) (Stretched b) da db where
  dynamics = _Wrapped . whilstLD dynamics

instance (HasDynamic a b da db) => HasDynamic (Stretched a) (Stretched b) da db where
  dynamic = _Wrapped . whilstLD dynamic


instance HasDynamics a b da db => HasDynamics (Voice a) (Voice b) da db where
  dynamics = traverse . dynamics

instance HasDynamics a b da db => HasDynamics (Track a) (Track b) da db where
  dynamics = traverse . dynamics

instance HasDynamics a b da db => HasDynamics (Chord a) (Chord b) da db where
  dynamics = traverse . dynamics

instance (HasDynamics a b da db) => HasDynamics (Score a) (Score b) da db where
  dynamics =
    _Wrapped . _2   -- into NScore
    . _Wrapped
    . traverse
    . _Wrapped      -- this needed?
    . whilstL dynamics

type instance Dynamic      (Behavior a) = Behavior a
type instance SetDynamic b (Behavior a) = b

instance (
  Transformable a, Transformable b, b ~ Dynamic b, SetDynamic (Behavior a) b ~ Behavior a) => HasDynamics (Behavior a) b (Behavior a) b where
  dynamics = ($)

instance (
  Transformable a, Transformable b, b ~ Dynamic b, SetDynamic (Behavior a) b ~ Behavior a) => HasDynamic (Behavior a) b (Behavior a) b where
  dynamic = ($)

type instance Dynamic (Couple c a)        = Dynamic a
type instance SetDynamic g (Couple c a)   = Couple c (SetDynamic g a)
type instance Dynamic (TremoloT a)        = Dynamic a
type instance SetDynamic g (TremoloT a)   = TremoloT (SetDynamic g a)
type instance Dynamic (TextT a)           = Dynamic a
type instance SetDynamic g (TextT a)      = TextT (SetDynamic g a)
type instance Dynamic (HarmonicT a)       = Dynamic a
type instance SetDynamic g (HarmonicT a)  = HarmonicT (SetDynamic g a)
type instance Dynamic (TieT a)            = Dynamic a
type instance SetDynamic g (TieT a)       = TieT (SetDynamic g a)
type instance Dynamic (SlideT a)          = Dynamic a
type instance SetDynamic g (SlideT a)     = SlideT (SetDynamic g a)


instance (HasDynamics a b da db) => HasDynamics (Couple c a) (Couple c b) da db where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b da db) => HasDynamic (Couple c a) (Couple c b) da db where
  dynamic = _Wrapped . dynamic


instance (HasDynamics a b da db) => HasDynamics (TremoloT a) (TremoloT b) da db where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b da db) => HasDynamic (TremoloT a) (TremoloT b) da db where
  dynamic = _Wrapped . dynamic


instance (HasDynamics a b da db) => HasDynamics (TextT a) (TextT b) da db where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b da db) => HasDynamic (TextT a) (TextT b) da db where
  dynamic = _Wrapped . dynamic


instance (HasDynamics a b da db) => HasDynamics (HarmonicT a) (HarmonicT b) da db where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b da db) => HasDynamic (HarmonicT a) (HarmonicT b) da db where
  dynamic = _Wrapped . dynamic


instance (HasDynamics a b da db) => HasDynamics (TieT a) (TieT b) da db where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b da db) => HasDynamic (TieT a) (TieT b) da db where
  dynamic = _Wrapped . dynamic


instance (HasDynamics a b da db) => HasDynamics (SlideT a) (SlideT b) da db where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b da db) => HasDynamic (SlideT a) (SlideT b) da db where
  dynamic = _Wrapped . dynamic



-- |
-- Associated interval type.
--
type Level a = Diff (Dynamic a)

-- |
-- Class of types that can be transposed.
--
type Attenuable a
  = (HasDynamics a a (Dynamic a) (Dynamic a),
     VectorSpace (Level a), AffineSpace (Dynamic a),
     {-IsLevel (Level a), -} IsDynamics (Dynamic a))

-- |
-- Transpose up.
--
louder :: Attenuable a => Level a -> a -> a
louder a = dynamics %~ (.+^ a)

-- |
-- Transpose down.
--
softer :: Attenuable a => Level a -> a -> a
softer a = dynamics %~ (.-^ a)

-- |
-- Transpose down.
--
volume :: (Num (Dynamic t), HasDynamics s t a b, Dynamic s ~ Dynamic t) => Dynamic t -> s -> t
volume a = dynamics *~ a

-- |
-- Transpose down.
--
level :: Attenuable a => Dynamic a -> a -> a
level a = dynamics .~ a

compressor :: Attenuable a =>
  Dynamic a           -- ^ Threshold
  -> Scalar (Level a) -- ^ Ratio
  -> a
  -> a
compressor = error "Not implemented: compressor"

--
-- TODO non-linear fades etc
--

-- |
-- Fade in.
--
fadeIn :: (
  HasPosition a, HasDynamics a a da da, 
  Dynamic a ~ Behavior c, Fractional c
  ) => Duration -> a -> a
fadeIn d x = x & dynamics *~ (_onset x >-> d `transform` unit)

-- |
-- Fade in.
--
fadeOut :: (
  HasPosition a, HasDynamics a a da da, 
  Dynamic a ~ Behavior c, Fractional c
  ) => Duration -> a -> a
fadeOut d x = x & dynamics *~ (d <-< _offset x `transform` rev unit)



newtype DynamicT n a = DynamicT { getDynamicT :: (n, a) }
  deriving (Eq, Ord, Show, Typeable, Functor,
    Applicative, Monad, Comonad, Transformable, Monoid, Semigroup)

instance (Monoid n, Num a) => Num (DynamicT n a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Monoid n, Fractional a) => Fractional (DynamicT n a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Monoid n, Floating a) => Floating (DynamicT n a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acos

instance (Monoid n, Enum a) => Enum (DynamicT n a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . extract

instance (Monoid n, Bounded a) => Bounded (DynamicT n a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- instance (Monoid n, Num a, Ord a, Real a) => Real (DynamicT n a) where
--     toRational = toRational . extract
--
-- instance (Monoid n, Real a, Enum a, Integral a) => Integral (DynamicT n a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . extract

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (DynamicT p a) where
  type Unwrapped (DynamicT p a) = (p, a)
  _Wrapped' = iso getDynamicT DynamicT

instance Rewrapped (DynamicT p a) (DynamicT p' b)


type instance Dynamic (DynamicT p a) = p
type instance SetDynamic p' (DynamicT p a) = DynamicT p' a

instance (Transformable p, Transformable p') => HasDynamic (DynamicT p a) (DynamicT p' a) p p' where
  dynamic = _Wrapped . _1

instance (Transformable p, Transformable p') => HasDynamics (DynamicT p a) (DynamicT p' a) p p' where
  dynamics = _Wrapped . _1


deriving instance (IsPitch a, Monoid n) => IsPitch (DynamicT n a)
deriving instance (IsInterval a, Monoid n) => IsInterval (DynamicT n a)

instance (IsDynamics n, Monoid a) => IsDynamics (DynamicT n a) where
    fromDynamics l = DynamicT (fromDynamics l, mempty)

deriving instance Reversible a => Reversible (DynamicT p a)

instance (Tiable n, Tiable a) => Tiable (DynamicT n a) where
  toTied (DynamicT (d,a)) = (DynamicT (d1,a1), DynamicT (d2,a2))
    where
      (a1,a2) = toTied a
      (d1,d2) = toTied d

-- JUNK

-- |
-- View just the dynamices in a voice.
--
vdynamic :: ({-SetDynamic (Dynamic t) s ~ t,-} HasDynamic a a da da, HasDynamic a b da db) 
  => Lens (Voice a) (Voice b) (Voice (Dynamic a)) (Voice (Dynamic b))

vdynamic = lens (fmap $ view dynamic) (flip $ zipVoiceWithNoScale (set dynamic))
-- vdynamic = through dynamic dynamic

addDynCon :: (
  HasPhrases s t a b, HasDynamic a a da da, HasDynamic a b da db, 
  Dynamic a ~ d, Dynamic b ~ Ctxt d
  ) => s -> t
addDynCon = over (phrases.vdynamic) withContext

