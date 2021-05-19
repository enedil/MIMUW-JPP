{-# LANGUAGE RankNTypes #-}
module Tracing (trace, traceShow, bug) where

import qualified Debug.Trace
debug :: Bool
debug = False

trace :: String -> b -> b
trace x y = if debug then Debug.Trace.trace x y else y

traceShow :: Show a => a -> b -> b
traceShow a = trace $ show a

bug :: forall b. String -> b
bug msg = trace ("bug: " ++ msg) undefined
