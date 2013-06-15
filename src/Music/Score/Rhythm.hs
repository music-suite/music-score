
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    GeneralizedNewtypeDeriving,
    ScopedTypeVariables #-}

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
-------------------------------------------------------------------------------------

module Music.Score.Rhythm (
        -- * Rhythm type
        Rhythm(..),

        -- * Quantization
        quantize,
        dotMod,
  ) where

import Data.Semigroup
import Control.Applicative
import Control.Monad
import Control.Monad.Plus
import Data.Maybe
import Data.Either
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio
import Data.VectorSpace

import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

import Music.Time
import Music.Score.Ties

type Dur = DurationT

data Rhythm a
    = Beat       Dur a                          -- d is divisible by 2
    | Group      [Rhythm a]                     -- normal note sequence
    | Dotted     Int (Rhythm a)                 -- n > 0.
    | Tuplet     Dur (Rhythm a)                 -- d is an emelent of 'kTupletMods'.
    deriving (Eq, Show, Functor, Foldable)

getBeatValue :: Rhythm a -> a
getBeatValue (Beat d a) = a
getBeatValue _          = error "getBeatValue: Not a beat"

getBeatDuration :: Rhythm a -> Dur
getBeatDuration (Beat d a) = d
getBeatDuration _          = error "getBeatValue: Not a beat"

instance Semigroup (Rhythm a) where
    (<>) = mappend

-- Catenates using 'Group'. Nested groups are unwrapped.
instance Monoid (Rhythm a) where
    mempty = Group []
    Group as `mappend` Group bs   =  Group (as <> bs)
    r        `mappend` Group bs   =  Group ([r] <> bs)
    Group as `mappend` r          =  Group (as <> [r])

instance AdditiveGroup (Rhythm a) where
    zeroV   = error "No zeroV for (Rhythm a)"
    (^+^)   = error "No ^+^ for (Rhythm a)"
    negateV = error "No negateV for (Rhythm a)"

instance VectorSpace (Rhythm a) where
    type Scalar (Rhythm a) = Dur
    a *^ Beat d x = Beat (a*d) x

instance Stretchable Rhythm where
    a `stretch` Beat d x = Beat (a*d) x

instance AdditiveGroup a => AdditiveGroup [a] where
    zeroV = error ""
    negateV = error ""
    (^+^) = error ""
instance VectorSpace a => VectorSpace [a] where
    type Scalar [a] = Scalar a
    a *^ xs = fmap (a*^) xs

Beat d x `subDur` d' = Beat (d-d') x
b `divDur` d = recip d `stretch` b

type instance Time Rhythm = TimeT

instance HasDuration Rhythm where
    duration (Beat d _)        = d
    duration (Dotted n a)      = duration a * dotMod n
    duration (Tuplet c a)      = duration a * c
    duration (Group as)        = sum (fmap duration as)

-- |
-- Returns the factor by which a note with the given number of dots should be stretched.
-- 
-- /Warning/ This function fails given @n < 1@.
-- 
-- > fmap dotMod [1..] = [3/2,7/4,15/8,31/16..]
--
dotMod :: Int -> Dur
dotMod n 
    | n < 1     = error "dotMod: n < 1"
    | otherwise = dotMods !! pred n
    where
        dotMods = zipWith (/) (fmap pred $ drop 2 times2) (drop 1 times2)
        times2  = iterate (* 2) 1


-- TODO break out parameters

-- Allowed tuplets
-- We only use proper fractions at the moment.
kTupletMods :: [Dur]
kTupletMods = [2/3, 4/5, {-4/6,-} 4/7, 8/9]

-- Allowed maximum tuple depth.
-- A value of 1 disallows /nested/ tuplets, while a value of 0 disallows tuplets alltogether.
kMaxTupleDepth :: Int
kMaxTupleDepth = 1

-- Maximum number of beat divisions
-- This can be quite high, but is needed to terminate the new parser
kMaxDepth :: Int
kMaxDepth = 2

-- Allowed maximum number of dots.
kMaxDots :: Int
kMaxDots = 2



data RState = RState {
        _timeMul     :: Dur, -- time modification; notatedDur * _timeMul = actualDur
        _timeAdd     :: Dur, -- time subtraction (in bound note)
        _tupleDepth  :: Int,
        _depth       :: Int
    }

instance Monoid RState where
    mempty = RState { 
        _timeMul     = 1, 
        _timeAdd     = 0, 
        _tupleDepth  = 0,
        _depth       = 0
    }
    a `mappend` _ = a

timeMul :: (Dur -> Dur) -> RState -> RState
timeMul f (RState tm ts td d) = RState (f tm) ts td d

timeAdd :: (Dur -> Dur) -> RState -> RState
timeAdd f (RState tm ts td d) = RState tm (f ts) td d

tupleDepth :: (Int -> Int) -> RState -> RState
tupleDepth f (RState tm ts td d) = RState tm ts (f td) d

depth :: (Int -> Int) -> RState -> RState
depth f (RState tm ts td d) = RState tm ts td (f d)

-- |
-- A @RhytmParser a b@ converts [(Dur, a)] to b.
type RhythmParser a b = Parsec [(Dur, a)] RState b

quantize :: Tiable a => [(Dur, a)] -> Either String (Rhythm a)
quantize = quantize' (atEnd rhythm)

testQuantize :: RhythmParser () b -> [Dur] -> Either String b
testQuantize p = quantize' (atEnd p) . fmap (\x->(x,()))

quantize' :: Tiable a => RhythmParser a b -> [(Dur, a)] -> Either String b
quantize' p = left show . runParser p mempty ""







-- Matches a 2-based rhytm group (such as a 4/4 or 2/4 bar)
rhythm2 :: Tiable a => RhythmParser a (Rhythm a)
rhythm2 = checkDepth $ (mzero
    <|> dur 1
    <|> addLevel rhythm2)              
    -- <|> (group $ fmap (withTimeMul $ 1/2) $ [rhythm2, rhythm2])
    -- <|> try (seq2 rhythm2 rhythm2)
    -- <|> try (seq2 rhythm2 rhythm3)
    -- <|> try (rhythm2 >> rhythm2 >> rhythm2) -- syncopation etc
    -- <|> (tuplet rhythm2)                   -- fixme should recur on 2 or 3

-- Matches a 2-based rhytm group (such as a 3/4 or 3/8 bar)
rhythm3 :: Tiable a => RhythmParser a (Rhythm a)
rhythm3 = mzero
    <|> dur 1.5
    -- <|> try (seq2 rhythm2 rhythm2)         -- long-short or short-long
    -- <|> try (seq3 rhythm3 rhythm3 rhythm3) -- for 9/8
    -- <|> try (seq3 rhythm2 rhythm2 rhythm2) -- hemiola
    -- <|> (tuplet rhythm2)                   -- fixme should recur on 2 or 3

-- seq2 p q = do
--     a <- p
--     b <- q
--     return $ Group [a,b]
-- seq3 p q x = do
--     a <- p
--     b <- q
--     c <- x
--     return $ Group [a,b,c]
group :: [RhythmParser a (Rhythm a)] -> RhythmParser a (Rhythm a)
group ps = do
    as <- Prelude.sequence ps
    return $ Group as





-- Matches any rhythm
rhythm :: Tiable a => RhythmParser a (Rhythm a)
rhythm = Group <$> many1 (rhythm' <|> bound)

rhythmNoBound :: Tiable a => RhythmParser a (Rhythm a)
rhythmNoBound = Group <$> many1 rhythm'

rhythm' :: Tiable a => RhythmParser a (Rhythm a)
rhythm' = mzero
    <|> beat
    <|> dotted
    <|> tuplet rhythmNoBound















-- (notatedDur + timeAdd) * timeMul = actualDur
-- notatedDur = actualDur / timeMul - timeAdd

-- 1/2+(1/8*2/3)


-- Matches exactly the given duration (modified by context).
dur :: Tiable a => Dur -> RhythmParser a (Rhythm a)
dur d' = do
    state <- getState
    let tm = _timeMul state
    let ts = _timeAdd state
    (\d -> (d `divDur` tm) `subDur` ts) <$> matchBeat (\d _ ->
        d - ts > 0
        &&
        d' == (d / tm) - ts
        )

-- Matches a beat divisible by 2 (modified by context)
beat :: Tiable a => RhythmParser a (Rhythm a)
beat = do
    state <- getState
    let tm = _timeMul state
    let ts = _timeAdd state
    (\d -> (d `divDur` tm) `subDur` ts) <$> matchBeat (\d _ ->
        d - ts > 0
        &&
        isDivisibleBy 2 ((d / tm) - ts)
        ) 

-- | Matches a dotted rhythm
dotted :: Tiable a => RhythmParser a (Rhythm a)
dotted = msum . fmap dotted' $ [1..kMaxDots]

dotted' :: Tiable a => Int -> RhythmParser a (Rhythm a)
dotted' n = fmap (Dotted n) $ withTimeMul (dotMod n) beat

-- | Matches a bound rhythm
bound :: Tiable a => RhythmParser a (Rhythm a)
bound = bound' (1/2)

bound' :: Tiable a => Dur -> RhythmParser a (Rhythm a)
bound' d = do
    a <- withTimeAdd d beat
    let (b,c) = toTied $ getBeatValue a
    return $ Group [Beat (getBeatDuration a) $ b, Beat (1/2) $ c]

-- | Matches a tuplet, recurring on the given parser.
tuplet :: Tiable a => RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
tuplet p = msum . fmap (tuplet' p) $ kTupletMods

-- tuplet' 2/3 for triplet, 4/5 for quintuplet etc
tuplet' :: Tiable a => RhythmParser a (Rhythm a) -> Dur -> RhythmParser a (Rhythm a)
tuplet' p d = do
    state <- getState
    onlyIf (_tupleDepth state < kMaxTupleDepth) $ do
        fmap (Tuplet d) (withTimeMul d . addTupletLevel $ p)

addTupletLevel :: RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
addTupletLevel = withState (tupleDepth succ) (tupleDepth pred)

addLevel :: RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
addLevel = withState (depth succ) (depth pred)

checkTupleDepth :: RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
checkTupleDepth p = do
    state <- getState
    onlyIf (_tupleDepth state < kMaxTupleDepth) p

checkDepth :: RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
checkDepth p = do
    state <- getState
    onlyIf (_depth state < kMaxDepth) p

withTimeMul :: Dur -> RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
withTimeMul d = withState (timeMul (* d)) (timeMul (/ d))

withTimeAdd :: Dur -> RhythmParser a (Rhythm a) -> RhythmParser a (Rhythm a)
withTimeAdd d = withState (timeAdd (+ d)) (timeAdd (subtract d))


-------------------------------------------------------------------------------------

-- Matches a (duration, value) pair iff the predicate beats, returns beat
matchBeat :: Tiable a => (Dur -> a -> Bool) -> RhythmParser a (Rhythm a)
matchBeat p = tokenPrim show next test
    where
        show x        = ""
        next pos _ _  = updatePosChar pos 'x'
        test (d,x)    = if p d x then Just (Beat d x) else Nothing


-- -- | Similar to 'many1', but tries longer sequences before trying one.
-- many1long :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
-- many1long p = try (many2 p) <|> fmap return p
-- 
-- -- | Similar to 'many1', but applies the parser 2 or more times.
-- many2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
-- many2 p = do { x <- p; xs <- many1 p; return (x : xs) }

withState :: Monad m => (u -> u) -> (u -> u) -> ParsecT s u m b -> ParsecT s u m b
withState f g p = do { modifyState f; a <- p; modifyState g; return a }

-- |
-- Succeed only if the entire input is consumed.
--
atEnd :: RhythmParser a b -> RhythmParser a b
atEnd p = do
    x <- p
    notFollowedBy' anyToken' <?> "end of input"
    return x
    where
        notFollowedBy' p = try $ (try p >> unexpected "") <|> return ()
        anyToken'        = tokenPrim (const "") (\pos _ _ -> pos) Just

onlyIf :: MonadPlus m => Bool -> m a -> m a
onlyIf p = mfilter (const p)

logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)

isDivisibleBy :: Dur -> Dur -> Bool
isDivisibleBy n = (== 0.0) . snd . properFraction . logBaseR (toRational n) . toRational


left f (Left x)  = Left (f x)
left f (Right y) = Right y
