module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

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
