{-# LANGUAGE RecursiveDo #-}

module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe

type Time = Integer

type Stream a = Time -> a

type StreamGen a = Time -> a

delay :: a -> Stream a -> StreamGen (Stream a)
delay x s t_start t_sample
  | t_start == t_sample = x
  | t_start < t_sample = s (t_sample - 1)
  | otherwise = error "Premature sample!"

start :: StreamGen (Stream a) -> Stream a
start g = g 0

generator :: Stream (StreamGen a) -> Stream a
generator g t_sample = g t_sample t_sample

stateful :: a -> (a -> a) -> StreamGen (Stream a)
stateful x_0 f = mfix $ \str -> delay x_0 (f <$> str)

streamTest :: Integer -> StreamGen (Stream a) -> [a]
streamTest dur g = map (start g) [0..dur]

countdown :: String -> Int -> StreamGen (Stream (String, Maybe Int))
countdown name t = do
  let tick prev = do {t <- prev; guard (t > 0); return (t - 1)}
  timer <- stateful (Just t) tick
  return ((,) name <$> timer)

timerSource :: [(String, Int, Int)] -> StreamGen (Stream [Stream (String, Maybe Int)])
timerSource ts = do
  let gen t = mapM (uncurry countdown) newTimers
        where newTimers = [(n, v) | (n, v, st) <- ts, st == t]
  cnt <- stateful 0 (+1)
  return $ generator (gen <$> cnt)

collection :: Stream [Stream a] -> (a -> Bool) -> StreamGen (Stream [a])
collection source isAlive = mdo
  coll <- liftA2 (++) source <$> delay [] coll'
  let collWithVals = zip <$> (sequence =<< coll) <*> coll
      collWithVals' = filter (isAlive . fst) <$> collWithVals
      coll' = map snd <$> collWithVals'
  return $ map fst <$> collWithVals'

timers :: [(String, Int, Int)] -> StreamGen (Stream [(String, Int)])
timers timerData = do
  src <- timerSource timerData
  getOutput <$> collection src (isJust . snd)
    where getOutput = fmap (map (\(name, Just val) -> (name, val)))
