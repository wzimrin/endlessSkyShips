{-# LANGUAGE NoImplicitPrelude #-}
module Debug ( trace
             , log
             ) where

import Prelude hiding (log)
import qualified Debug.Trace as Trace

showTraces :: Bool
showTraces = False

trace :: String -> t -> t
trace str val = if showTraces
  then Trace.trace str val
  else val

log :: String -> IO ()
log str = if showTraces
  then putStrLn str
  else return ()
