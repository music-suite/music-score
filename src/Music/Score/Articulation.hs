
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
-- Provides functions for manipulating articulation.
--
-------------------------------------------------------------------------------------

module Music.Score.Articulation (

        -- ** Articulation type functions
        Articulation,
        -- SetArticulation,
        Accentuation,
        Separation,
        Articulated(..),

        -- ** Accessing articulation
        HasArticulations(..),
        HasArticulation(..),
        HasArticulations',
        HasArticulation',
        articulation',
        articulations',

        -- * Manipulating articulation
        -- ** Accents
        accent,
        marcato,
        accentLast,
        marcatoLast,
        accentAll,
        marcatoAll,

        -- ** Phrasing and separation
        staccatissimo,
        staccato,
        separated,
        portato,
        legato,
        legatissimo,

        tenuto,
        spiccato,

        -- * Articulation transformer
        ArticulationT(..),
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens                  hiding (above, below, transform)
import           Control.Lens.Cons.Middle
import           Data.AffineSpace
import           Data.Foldable
import           Data.Functor.Couple
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace              hiding (Sum)

import           Music.Score.Part
import           Music.Time
import           Music.Time.Internal.Transform

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo




-- |
-- Articulations type.
--
type family Articulation (s :: *) :: *

-- |
-- Articulation type.
--
type family SetArticulation (b :: *) (s :: *) :: *

-- type ArticulationLensLaws' s t a b = (
--   Articulation (SetArticulation a s) ~ a,
--   SetArticulation (Articulation t) s ~ t,
--   SetArticulation a (SetArticulation b s) ~ SetArticulation a s
--   )
-- 
-- type ArticulationLensLaws s t = ArticulationLensLaws' s t (Articulation s) (Articulation t)

-- |
-- Class of types that provide a single articulation.
--
class (HasArticulations s t a b) => HasArticulation s t a b where

  -- | Articulation type.
  articulation :: Lens s t a b

-- |
-- Class of types that provide a articulation traversal.
--
class (Transformable (Articulation s), Articulation s ~ a,
       Transformable (Articulation t), Articulation t ~ b) => HasArticulations s t a b where

  -- | Articulation type.
  articulations :: Traversal s t a b

type HasArticulation'  s a = HasArticulation  s s a a
type HasArticulations' s a = HasArticulations s s a a

-- |
-- Articulation type.
--
articulation' :: HasArticulation' s a => Lens' s a
articulation' = articulation

-- |
-- Articulation type.
--
articulations' :: HasArticulation' s a => Traversal' s a
articulations' = articulations

#define PRIM_ARTICULATION_INSTANCE(TYPE)       \
                                               \
type instance Articulation TYPE = TYPE;        \
type instance SetArticulation a TYPE = a;      \
                                               \
instance (Transformable a, a ~ Articulation a, SetArticulation TYPE a ~ TYPE) \
  => HasArticulation TYPE a TYPE a where {            \
  articulation = ($)              } ;          \
                                               \
instance (Transformable a, a ~ Articulation a, SetArticulation TYPE a ~ TYPE) \
  => HasArticulations TYPE a TYPE a where {           \
  articulations = ($)               } ;        \

PRIM_ARTICULATION_INSTANCE(())
PRIM_ARTICULATION_INSTANCE(Bool)
PRIM_ARTICULATION_INSTANCE(Ordering)
PRIM_ARTICULATION_INSTANCE(Char)
PRIM_ARTICULATION_INSTANCE(Int)
PRIM_ARTICULATION_INSTANCE(Integer)
PRIM_ARTICULATION_INSTANCE(Float)
PRIM_ARTICULATION_INSTANCE(Double)


type instance Articulation (c,a)              = Articulation a
type instance SetArticulation b (c,a)         = (c,SetArticulation b a)
type instance Articulation [a]                = Articulation a
type instance SetArticulation b [a]           = [SetArticulation b a]

type instance Articulation (Maybe a)          = Articulation a
type instance SetArticulation b (Maybe a)     = Maybe (SetArticulation b a)
type instance Articulation (Either c a)       = Articulation a
type instance SetArticulation b (Either c a)  = Either c (SetArticulation b a)

type instance Articulation (Note a) = Articulation a
type instance SetArticulation g (Note a) = Note (SetArticulation g a)
type instance Articulation (Delayed a) = Articulation a
type instance SetArticulation g (Delayed a) = Delayed (SetArticulation g a)
type instance Articulation (Stretched a) = Articulation a
type instance SetArticulation g (Stretched a) = Stretched (SetArticulation g a)

type instance Articulation (Voice a) = Articulation a
type instance SetArticulation b (Voice a) = Voice (SetArticulation b a)
type instance Articulation (Chord a) = Articulation a
type instance SetArticulation b (Chord a) = Chord (SetArticulation b a)
type instance Articulation (Track a) = Articulation a
type instance SetArticulation b (Track a) = Track (SetArticulation b a)
type instance Articulation (Score a) = Articulation a
type instance SetArticulation b (Score a) = Score (SetArticulation b a)


instance HasArticulation a b aa ab => HasArticulation (c, a) (c, b) aa ab where
  articulation = _2 . articulation

instance HasArticulations a b aa ab => HasArticulations (c, a) (c, b) aa ab where
  articulations = traverse . articulations

instance HasArticulations a b aa ab => HasArticulations [a] [b] aa ab where
  articulations = traverse . articulations

instance HasArticulations a b aa ab => HasArticulations (Maybe a) (Maybe b) aa ab where
  articulations = traverse . articulations

instance HasArticulations a b aa ab => HasArticulations (Either c a) (Either c b) aa ab where
  articulations = traverse . articulations



instance (HasArticulations a b aa ab) => HasArticulations (Note a) (Note b) aa ab where
  articulations = _Wrapped . whilstL articulations

instance (HasArticulation a b aa ab) => HasArticulation (Note a) (Note b) aa ab where
  articulation = _Wrapped . whilstL articulation

instance (HasArticulations a b aa ab) => HasArticulations (Delayed a) (Delayed b) aa ab where
  articulations = _Wrapped . whilstLT articulations

instance (HasArticulation a b aa ab) => HasArticulation (Delayed a) (Delayed b) aa ab where
  articulation = _Wrapped . whilstLT articulation

instance (HasArticulations a b aa ab) => HasArticulations (Stretched a) (Stretched b) aa ab where
  articulations = _Wrapped . whilstLD articulations

instance (HasArticulation a b aa ab) => HasArticulation (Stretched a) (Stretched b) aa ab where
  articulation = _Wrapped . whilstLD articulation


instance HasArticulations a b aa ab => HasArticulations (Voice a) (Voice b) aa ab where
  articulations = traverse . articulations

instance HasArticulations a b aa ab => HasArticulations (Chord a) (Chord b) aa ab where
  articulations = traverse . articulations

instance HasArticulations a b aa ab => HasArticulations (Track a) (Track b) aa ab where
  articulations = traverse . articulations

instance HasArticulations a b aa ab => HasArticulations (Score a) (Score b) aa ab where
  articulations =
    _Wrapped . _2   -- into NScore
    . _Wrapped
    . traverse
    . _Wrapped      -- this needed?
    . whilstL articulations

type instance Articulation (Couple c a)        = Articulation a
type instance SetArticulation g (Couple c a)   = Couple c (SetArticulation g a)
type instance Articulation (TremoloT a)        = Articulation a
type instance SetArticulation g (TremoloT a)   = TremoloT (SetArticulation g a)
type instance Articulation (TextT a)           = Articulation a
type instance SetArticulation g (TextT a)      = TextT (SetArticulation g a)
type instance Articulation (HarmonicT a)       = Articulation a
type instance SetArticulation g (HarmonicT a)  = HarmonicT (SetArticulation g a)
type instance Articulation (TieT a)            = Articulation a
type instance SetArticulation g (TieT a)       = TieT (SetArticulation g a)
type instance Articulation (SlideT a)          = Articulation a
type instance SetArticulation g (SlideT a)     = SlideT (SetArticulation g a)

instance (HasArticulations a b aa ab) => HasArticulations (Couple c a) (Couple c b) aa ab where
  articulations = _Wrapped . articulations
instance (HasArticulation a b aa ab) => HasArticulation (Couple c a) (Couple c b) aa ab where
  articulation = _Wrapped . articulation

instance (HasArticulations a b aa ab) => HasArticulations (TremoloT a) (TremoloT b) aa ab where
  articulations = _Wrapped . articulations
instance (HasArticulation a b aa ab) => HasArticulation (TremoloT a) (TremoloT b) aa ab where
  articulation = _Wrapped . articulation

instance (HasArticulations a b aa ab) => HasArticulations (TextT a) (TextT b) aa ab where
  articulations = _Wrapped . articulations
instance (HasArticulation a b aa ab) => HasArticulation (TextT a) (TextT b) aa ab where
  articulation = _Wrapped . articulation

instance (HasArticulations a b aa ab) => HasArticulations (HarmonicT a) (HarmonicT b) aa ab where
  articulations = _Wrapped . articulations
instance (HasArticulation a b aa ab) => HasArticulation (HarmonicT a) (HarmonicT b) aa ab where
  articulation = _Wrapped . articulation

instance (HasArticulations a b aa ab) => HasArticulations (TieT a) (TieT b) aa ab where
  articulations = _Wrapped . articulations
instance (HasArticulation a b aa ab) => HasArticulation (TieT a) (TieT b) aa ab where
  articulation = _Wrapped . articulation

instance (HasArticulations a b aa ab) => HasArticulations (SlideT a) (SlideT b) aa ab where
  articulations = _Wrapped . articulations
instance (HasArticulation a b aa ab) => HasArticulation (SlideT a) (SlideT b) aa ab where
  articulation = _Wrapped . articulation


type family Accentuation (a :: *) :: *
type family Separation (a :: *) :: *

type instance Accentuation () = ()
type instance Separation   () = ()
type instance Accentuation (a, b) = a
type instance Separation   (a, b) = b


-- |
-- Class of types that can be transposed, inverted and so on.
--
class (
  Fractional (Accentuation a),
  Fractional (Separation a),
  AffineSpace (Accentuation a),
  AffineSpace (Separation a)
  ) => Articulated a where
    accentuation :: Lens' a (Accentuation a)
    separation   :: Lens' a (Separation a)


-- TODO move
instance Num () where
  _ + _ = ()
  _ - _ = ()
  _ * _ = ()
  signum _ = ()
  abs _ = ()
  fromInteger _ = ()

instance Fractional () where
  _ / _ = ()
  fromRational _ = ()

instance VectorSpace () where
  type Scalar () = ()
  _ *^ _ = ()

instance AffineSpace () where
  type Diff () = ()
  _ .-. _ = ()
  _ .+^ _ = ()

instance Articulated () where
  accentuation = id
  separation   = id

instance (AffineSpace a, AffineSpace b, Fractional a, Fractional b) => Articulated (a, b) where
  accentuation = _1'
  separation   = _2'

_1' :: Lens' (a, b) a
_1' = _1

_2' :: Lens' (a, b) b
_2' = _2
  


accent :: (HasPhrases' s b, HasArticulations' b ab, Articulation b ~ a, Articulated a) => s -> s
accent = set (phrases . _head . articulations . accentuation) 1

marcato :: (HasPhrases' s b, HasArticulations' b ab, Articulation b ~ a, Articulated a) => s -> s
marcato = set (phrases . _head . articulations . accentuation) 2

accentLast :: (HasPhrases' s b, HasArticulations' b ab, Articulation b ~ a, Articulated a) => s -> s
accentLast = set (phrases . _last . articulations . accentuation) 1

marcatoLast :: (HasPhrases' s b, HasArticulations' b ab, Articulation b ~ a, Articulated a) => s -> s
marcatoLast = set (phrases . _last . articulations . accentuation) 2

accentAll :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
accentAll = set (articulations . accentuation) 1

marcatoAll :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
marcatoAll = set (articulations . accentuation) 2



tenuto :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
tenuto = id

spiccato :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
spiccato = id

legatissimo :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
legatissimo = set (articulations . separation) (-2)

legato :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
legato = set (articulations . separation) (-1)

separated :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
separated = set (articulations . separation) 0

portato :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
portato = set (articulations . separation) 0.5

staccato :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
staccato = set (articulations . separation) 1

staccatissimo :: (HasArticulations' s as, Articulation s ~ a, Articulated a) => s -> s
staccatissimo = set (articulations . separation) 2




newtype ArticulationT n a = ArticulationT { getArticulationT :: (n, a) }
  deriving (
    Eq, Ord, Show, Typeable, Functor, Applicative, Monad, 
    Comonad, Transformable, Monoid, Semigroup
    )

instance (Monoid n, Num a) => Num (ArticulationT n a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Monoid n, Fractional a) => Fractional (ArticulationT n a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Monoid n, Floating a) => Floating (ArticulationT n a) where
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

instance (Monoid n, Enum a) => Enum (ArticulationT n a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . extract

instance (Monoid n, Bounded a) => Bounded (ArticulationT n a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- instance (Monoid n, Num a, Ord a, Real a) => Real (ArticulationT n a) where
--     toRational = toRational . extract
--
-- instance (Monoid n, Real a, Enum a, Integral a) => Integral (ArticulationT n a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . extract

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (ArticulationT p a) where
  type Unwrapped (ArticulationT p a) = (p, a)
  _Wrapped' = iso getArticulationT ArticulationT

instance Rewrapped (ArticulationT p a) (ArticulationT p' b)

type instance Articulation (ArticulationT p a) = p
type instance SetArticulation p' (ArticulationT p a) = ArticulationT p' a

instance (Transformable p, Transformable p') 
    => HasArticulation (ArticulationT p a) (ArticulationT p' a) p p' where
  articulation = _Wrapped . _1

instance (Transformable p, Transformable p') 
    => HasArticulations (ArticulationT p a) (ArticulationT p' a) p p' where
  articulations = _Wrapped . _1

deriving instance (IsPitch a, Monoid n) => IsPitch (ArticulationT n a)
deriving instance (IsInterval a, Monoid n) => IsInterval (ArticulationT n a)
deriving instance Reversible a => Reversible (ArticulationT p a)

instance (Tiable n, Tiable a) => Tiable (ArticulationT n a) where
  toTied (ArticulationT (d,a)) = (ArticulationT (d1,a1), ArticulationT (d2,a2))
    where
      (a1,a2) = toTied a
      (d1,d2) = toTied d
