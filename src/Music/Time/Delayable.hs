
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving #-} 

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides delayable values.
--
-------------------------------------------------------------------------------------

module Music.Time.Delayable (
        -- * Delayable class
        Delayable(..),
        move,
        moveBack,     
        
        -- ** Utility functions
        delayTime,
  ) where

import Control.Arrow
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Time

-- |
-- Delayable values. 
-- 
class Delayable a where

    -- |
    -- Delay a value.
    -- 
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration -> a -> a

instance Delayable Time where
    delay n = (.+^ n)

instance Delayable (Time -> a) where
    delay n = (. delay (negateV n))

instance Delayable a => Delayable [a] where
    delay n = fmap (delay n)

instance Delayable (Time, a) where
    delay n (t, a) = (t .+^ n, a)

instance Delayable (Time, Duration, a) where
    delay n (t, d, a) = (t .+^ n, d, a)


-- |
-- Move a score forward in time. Equivalent to 'delayTime.
--
-- > Duration -> Score a -> Score a
--
move            :: (Delayable a) =>
                Duration -> a -> a

-- |
-- Move a score backward in time. Negated verison of 'delayTime
--
-- > Duration -> Score a -> Score a
--
moveBack        :: Delayable a =>
                Duration -> a -> a

-- |
-- Delay relative to 'origin'. Provided for situations when you have a value that
-- should forward based on the distance between some time @t@ and the origin, but
-- it does not necessarily have a start time.
--
-- > Time -> Score a -> Score a
--
delayTime       :: Delayable a => 
                Time   -> a -> a

move            = delay
moveBack t      = delay (negateV t)
delayTime t     = delay (t .-. origin)

