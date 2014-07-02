
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
{-# LANGUAGE FunctionalDependencies     #-}
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
-- Provides functions for manipulating pitch.
--
-------------------------------------------------------------------------------------


module Music.Score.Pitch (
        
        -- * Pitch type functions
        Pitch,
        -- SetPitch,
        Interval,
        
        -- * Accessing pitch
        HasPitches(..),
        HasPitch(..),
        -- ** Simple versions
        HasPitches',
        HasPitch',
        pitch',
        pitches',
        
        -- * Converting pitch to container
        fromPitch',
        
        -- * Manipulating pitch
        Transposable,
        up,
        down,
        above,
        below,
        inv,
        invertPitches,
        octavesUp,
        octavesDown,
        octavesAbove,
        octavesBelow,
        fifthsUp,
        fifthsDown,
        fifthsAbove,
        fifthsBelow,
        -- ** Utility
        _15va,
        _8va,
        _8vb,
        _15vb,

        -- * Inspecting pitch
        highestPitch,
        lowestPitch,
        meanPitch,

        -- * Intervals
        augmentIntervals,

        -- TODO pitchIs, to write filter pitchIs ... etc
        -- TODO gliss etc

  ) where

import           Control.Applicative
import           Control.Lens                  hiding (above, below, transform)
import           Control.Monad                 (MonadPlus (..), ap, join, liftM,
                                                mfilter)
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable                 (Foldable)
import           Data.Functor.Couple
import qualified Data.List                     as List
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Traversable              (Traversable)
import           Data.Typeable
import           Data.VectorSpace              hiding (Sum)

import           Music.Pitch.Literal
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo
import           Music.Score.Phrases
import           Music.Time
import           Music.Time.Internal.Transform

-- |
-- This type fuction is used to retrive the /pitch type/ for a given concrete type.
--
-- For types representing pitch, it is generally 'Identity', i.e
--
-- @
-- Pitch Integer ~ Integer
-- Pitch Double ~ Double
-- @
--
-- and so on.
--
-- For containers, 'Pitch' provides a morphism:
--
-- @
-- 'Pitch' (c,a)             ~ 'Pitch' a
-- 'Pitch' [a]               ~ 'Pitch' a
-- 'Pitch' ('Note' a)          ~ 'Pitch' a
-- 'Pitch' ('Delayed' a)       ~ 'Pitch' a
-- 'Pitch' ('Stretched' a)     ~ 'Pitch' a
-- 'Pitch' ('Voice' a)         ~ 'Pitch' a
-- 'Pitch' ('Chord' a)         ~ 'Pitch' a
-- 'Pitch' ('Track' a)         ~ 'Pitch' a
-- 'Pitch' ('Score' a)         ~ 'Pitch' a
-- @
--
type family Pitch (s :: *) :: *

-- |
-- This type fuction is used to retrive the /pitch type/ for a given concrete type.
--
-- For types representing pitch, it is generally 'Constant', i.e
--
-- @
-- SetPitch a Double ~ a
-- SetPitch a Integer ~ a
-- @
--
-- For containers, 'Pitch' provides a morphism:
--
-- @
-- 'SetPitch' b (c,a)          ~ (c, 'SetPitch' b a)
-- 'SetPitch' b [a]            ~ ['SetPitch' b a]
-- 'SetPitch' g ('Note' a)       ~ Note ('SetPitch' g a)
-- 'SetPitch' g ('Delayed' a)    ~ Delayed ('SetPitch' g a)
-- 'SetPitch' g ('Stretched' a)  ~ Stretched ('SetPitch' g a)
-- 'SetPitch' g ('Voice' a)      ~ 'Voice' ('SetPitch' g a)
-- 'SetPitch' g ('Chord' a)      ~ 'Chord' ('SetPitch' g a)
-- 'SetPitch' g ('Track' a)      ~ 'Track' ('SetPitch' g a)
-- 'SetPitch' g ('Score' a)      ~ 'Score' ('SetPitch' g a)
-- @
--
type family SetPitch (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single pitch.
--
class HasPitches s t a b => HasPitch s t a b | s -> a, {-t -> b, -}s b -> t{-, t a -> s-} where

  -- | Access the pitch.
  --
  --   As this is a 'Traversal', you can use all combinators from the lens package,
  --   for example:
  --
  --   @
  --   'view' 'pitch' :: HasPitch a a
  --   @
  --
  --   @
  --   'over' 'pitch'         :: HasPitch' a => a -> Pitch a
  --   'pitch' %~ 'succ'      :: HasPitch' a => a -> a
  --   'pitch' +~ 2         :: (HasPitch' a, Num (Pitch a)) => a -> a
  --   'pitch' .~ c         :: (HasPitch' a, IsPitch a) => a -> a
  --   @
  --
  pitch :: Lens s t a b

-- |
-- Class of types that provide a pitch traversal.
--
class (Transformable (Pitch s), a ~ Pitch s,
       Transformable (Pitch t), b ~ Pitch t) => HasPitches s t a b | s -> a, {-t -> b, -}s b -> t{-, t a -> s-} where

  -- | Access all pitches.
  --
  --   As this is a 'Traversal', you can use all combinators from the lens package,
  --   for example:
  --
  --   @
  --   'lengthOf' 'pitches' :: HasPitches a a => a -> Int
  --   @
  --
  --   @
  --   'toListOf' 'pitches' :: HasPitches' a => a -> Pitch a
  --   @
  --
  --   @
  --   'over' 'pitches' :: HasPitches a b => a -> b
  --   @
  --
  pitches :: Traversal s t a b

type HasPitch' s a = HasPitch s s a a

type HasPitches' s a = HasPitches s s a a


-- |
-- Pitch type.
--
pitch' :: HasPitch' s a => Lens' s a
pitch' = pitch
{-# INLINE pitch' #-}

-- |
-- Pitch type.
--
pitches' :: HasPitches' s a => Traversal' s a
pitches' = pitches
{-# INLINE pitches' #-}


-- TODO flip name of this and Literal.fromPitch (or call that fromPitchL)
fromPitch' :: (HasPitches' a a, IsPitch a) => Pitch a -> a
fromPitch' x = c & pitches' .~ x
{-# INLINE fromPitch' #-}


#define PRIM_PITCH_INSTANCE(TYPE)       \
                                        \
type instance Pitch TYPE = TYPE;        \
                                        \
instance (Transformable a, a ~ Pitch a) \
  => HasPitch TYPE a TYPE a where {            \
  pitch = ($)              } ;          \
                                        \
instance (Transformable a, a ~ Pitch a) \
  => HasPitches TYPE a TYPE a where {          \
  pitches = ($)              } ;        \


PRIM_PITCH_INSTANCE(())
PRIM_PITCH_INSTANCE(Bool)
PRIM_PITCH_INSTANCE(Ordering)
PRIM_PITCH_INSTANCE(Char)
PRIM_PITCH_INSTANCE(Int)
PRIM_PITCH_INSTANCE(Integer)
PRIM_PITCH_INSTANCE(Float)
PRIM_PITCH_INSTANCE(Double)


type instance Pitch (c,a)               = Pitch a
type instance SetPitch b (c,a)          = (c,SetPitch b a)
type instance Pitch [a]                 = Pitch a
type instance SetPitch b [a]            = [SetPitch b a]

type instance Pitch (Maybe a)           = Pitch a
type instance SetPitch b (Maybe a)      = Maybe (SetPitch b a)
type instance Pitch (Either c a)        = Pitch a
type instance SetPitch b (Either c a)   = Either c (SetPitch b a)

type instance Pitch (Note a)            = Pitch a
type instance SetPitch b (Note a)       = Note (SetPitch b a)
type instance Pitch (Delayed a)         = Pitch a
type instance SetPitch b (Delayed a)    = Delayed (SetPitch b a)
type instance Pitch (Stretched a)       = Pitch a
type instance SetPitch b (Stretched a)  = Stretched (SetPitch b a)

type instance Pitch (Voice a)       = Pitch a
type instance SetPitch b (Voice a)  = Voice (SetPitch b a)
type instance Pitch (Chord a)       = Pitch a
type instance SetPitch b (Chord a)  = Chord (SetPitch b a)
type instance Pitch (Track a)       = Pitch a
type instance SetPitch b (Track a)  = Track (SetPitch b a)
type instance Pitch (Score a)       = Pitch a
type instance SetPitch b (Score a)  = Score (SetPitch b a)

instance HasPitch a b p q => HasPitch (c, a) (c, b) p q where
  pitch = _2 . pitch
instance HasPitches a b p q => HasPitches (c, a) (c, b) p q where
  pitches = traverse . pitches

instance (HasPitches a b p q) => HasPitches (Note a) (Note b) p q where
  pitches = _Wrapped . whilstL pitches
instance (HasPitch a b p q) => HasPitch (Note a) (Note b) p q where
  pitch = _Wrapped . whilstL pitch

instance (HasPitches a b p q) => HasPitches (Delayed a) (Delayed b) p q where
  pitches = _Wrapped . whilstLT pitches
instance (HasPitch a b p q) => HasPitch (Delayed a) (Delayed b) p q where
  pitch = _Wrapped . whilstLT pitch

instance (HasPitches a b p q) => HasPitches (Stretched a) (Stretched b) p q where
  pitches = _Wrapped . whilstLD pitches
instance (HasPitch a b p q) => HasPitch (Stretched a) (Stretched b) p q where
  pitch = _Wrapped . whilstLD pitch

instance HasPitches a b p q => HasPitches (Maybe a) (Maybe b) p q where
  pitches = traverse . pitches

instance HasPitches a b p q => HasPitches (Either c a) (Either c b) p q where
  pitches = traverse . pitches

instance HasPitches a b p q => HasPitches [a] [b] p q where
  pitches = traverse . pitches

instance HasPitches a b p q => HasPitches (Voice a) (Voice b) p q where
  pitches = traverse . pitches

instance HasPitches a b p q => HasPitches (Track a) (Track b) p q where
  pitches = traverse . pitches

instance HasPitches a b p q => HasPitches (Chord a) (Chord b) p q where
  pitches = traverse . pitches

instance (HasPitches a b p q) => HasPitches (Score a) (Score b) p q where
  pitches =
    _Wrapped . _2   -- into NScore
    . _Wrapped
    . traverse
    . _Wrapped      -- this needed?
    . whilstL pitches


type instance Pitch (Sum a) = Pitch a
type instance SetPitch b (Sum a) = Sum (SetPitch b a)

instance HasPitches a b p q => HasPitches (Sum a) (Sum b) p q where
  pitches = _Wrapped . pitches

type instance Pitch      (Behavior a) = Behavior a
type instance SetPitch b (Behavior a) = b

instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitches (Behavior a) b (Behavior a) b where
  pitches = ($)
instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitch (Behavior a) b (Behavior a) b where
  pitch = ($)

type instance Pitch (Couple c a)        = Pitch a
type instance SetPitch g (Couple c a)   = Couple c (SetPitch g a)
type instance Pitch (TremoloT a)        = Pitch a
type instance SetPitch g (TremoloT a)   = TremoloT (SetPitch g a)
type instance Pitch (TextT a)           = Pitch a
type instance SetPitch g (TextT a)      = TextT (SetPitch g a)
type instance Pitch (HarmonicT a)       = Pitch a
type instance SetPitch g (HarmonicT a)  = HarmonicT (SetPitch g a)
type instance Pitch (TieT a)            = Pitch a
type instance SetPitch g (TieT a)       = TieT (SetPitch g a)
type instance Pitch (SlideT a)          = Pitch a
type instance SetPitch g (SlideT a)     = SlideT (SetPitch g a)

instance (HasPitches a b p q) => HasPitches (Couple c a) (Couple c b) p q where
  pitches = _Wrapped . pitches
instance (HasPitch a b p q) => HasPitch (Couple c a) (Couple c b) p q where
  pitch = _Wrapped . pitch

instance (HasPitches a b p q) => HasPitches (TremoloT a) (TremoloT b) p q where
  pitches = _Wrapped . pitches
instance (HasPitch a b p q) => HasPitch (TremoloT a) (TremoloT b) p q where
  pitch = _Wrapped . pitch

instance (HasPitches a b p q) => HasPitches (TextT a) (TextT b) p q where
  pitches = _Wrapped . pitches
instance (HasPitch a b p q) => HasPitch (TextT a) (TextT b) p q where
  pitch = _Wrapped . pitch

instance (HasPitches a b p q) => HasPitches (HarmonicT a) (HarmonicT b) p q where
  pitches = _Wrapped . pitches
instance (HasPitch a b p q) => HasPitch (HarmonicT a) (HarmonicT b) p q where
  pitch = _Wrapped . pitch

instance (HasPitches a b p q) => HasPitches (TieT a) (TieT b) p q where
  pitches = _Wrapped . pitches
instance (HasPitch a b p q) => HasPitch (TieT a) (TieT b) p q where
  pitch = _Wrapped . pitch

instance (HasPitches a b p q) => HasPitches (SlideT a) (SlideT b) p q where
  pitches = _Wrapped . pitches
instance (HasPitch a b p q) => HasPitch (SlideT a) (SlideT b) p q where
  pitch = _Wrapped . pitch


-- |
-- Associated interval type.
--
type Interval a = Diff (Pitch a)

-- |
-- Class of types that can be transposed, inverted and so on.
--
type Transposable a
  = (HasPitches a a (Pitch a) (Pitch a),
     VectorSpace (Interval a), AffineSpace (Pitch a),
     IsInterval (Interval a), IsPitch (Pitch a),
     Num (Scalar (Interval a)))

-- |
-- Transpose up.
--
up :: Transposable a => Interval a -> a -> a
up v = pitches %~ (.+^ v)

-- |
-- Transpose down.
--
down :: Transposable a => Interval a -> a -> a
down v = pitches %~ (.-^ v)

-- |
-- Add the given interval above.
--
above :: (Semigroup a, Transposable a) => Interval a -> a -> a
above v x = x <> up v x

-- |
-- Add the given interval below.
--
below :: (Semigroup a, Transposable a) => Interval a -> a -> a
below v x = x <> down v x

inv :: Transposable a => Pitch a -> a -> a
inv = invertPitches
{-# DEPRECATED inv "Use 'invertPitches'" #-}

-- |
-- Invert pitches.
--
invertPitches :: Transposable a => Pitch a -> a -> a
invertPitches p = pitches %~ reflectThrough p

-- |
-- Transpose up by the given number of octaves.
--
octavesUp :: Transposable a => Scalar (Interval a) -> a -> a
octavesUp n = up (_P8^*n)

-- |
-- Transpose down by the given number of octaves.
--
octavesDown :: Transposable a => Scalar (Interval a) -> a -> a
octavesDown n = down (_P8^*n)

-- |
-- Add the given octave above.
--
octavesAbove :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
octavesAbove n = above (_P8^*n)

-- |
-- Add the given octave below.
--
octavesBelow :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
octavesBelow n = below (_P8^*n)

-- |
-- Transpose up by the given number of fifths.
--
fifthsUp :: Transposable a => Scalar (Interval a) -> a -> a
fifthsUp n = up (_P8^*n)

-- |
-- Transpose down by the given number of fifths.
--
fifthsDown :: Transposable a => Scalar (Interval a) -> a -> a
fifthsDown n = down (_P8^*n)

-- |
-- Add the given octave above.
--
fifthsAbove :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
fifthsAbove n = above (_P8^*n)

-- |
-- Add the given octave below.
--
fifthsBelow :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
fifthsBelow n = below (_P8^*n)

-- | Shorthand for @'octavesUp' 2@.
_15va :: Transposable a => a -> a
_15va = octavesUp 2

-- | Shorthand for @'octavesUp' 1@.
_8va :: Transposable a => a -> a
_8va  = octavesUp 1

-- | Shorthand for @'octavesDown' 1@.
_8vb :: Transposable a => a -> a
_8vb  = octavesDown 1

-- | Shorthand for @'octavesDown' 2@.
_15vb :: Transposable a => a -> a
_15vb = octavesDown 2



-- |
-- Return the highest pitch in the given music.
--
highestPitch :: (HasPitches' a p, Ord (Pitch a)) => a -> Pitch a
highestPitch = maximum . toListOf pitches'

-- |
-- Return the lowest pitch in the given music.
--
lowestPitch :: (HasPitches' a p, Ord (Pitch a)) => a -> Pitch a
lowestPitch = maximum . toListOf pitches'

-- |
-- Return the mean pitch in the given music.
--
meanPitch :: (HasPitches' a p, Fractional (Pitch a)) => a -> Pitch a
meanPitch = mean . toListOf pitches'
  where
    mean x = fst $ foldl (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x


augmentIntervals :: (HasPhrases' s a, Transposable a) => Interval a -> s -> s
augmentIntervals x = over phrases (augmentIntervals' x)

augmentIntervals' :: Transposable a => Interval a -> Voice a -> Voice a
augmentIntervals' = error "Not implemented: augmentIntervals"
-- TODO generalize to any type where we can traverse phrases of something that has pitch


-- TODO augment/diminish intervals (requires phrase traversal)
-- TODO rotatePitch (requires phrase traversal)
-- TODO invert diatonically

