module Control.Pipe.Exception (
  catch,
  throw,
  onException,
  finally,
  bracket
  ) where

import qualified Control.Exception as E
import Control.Pipe.Common
import Prelude hiding (catch)

catch :: (Monad m, E.Exception e)
      => Pipe a b m r
      -> (e -> Pipe a b m r)
      -> Pipe a b m r
catch p h = catchP p $ \e -> case E.fromException e of
  Nothing -> throwP e
  Just e' -> h e'

throw :: (Monad m, E.Exception e) => e -> Pipe a b m r
throw = throwP . E.toException

onException :: Monad m
            => Pipe a b m r
            -> Pipe a b m s
            -> Pipe a b m r
onException p w = catchP p $ \e -> w >> throw e

finally :: Monad m
        => Pipe a b m r
        -> m s
        -> Pipe a b m r
finally p w = do
  r <- onException p (ensure w)
  ensure w
  return r

bracket :: Monad m
        => m r
        -> (r -> m y)
        -> (r -> Pipe a b m x)
        -> Pipe a b m x
bracket open close run = do
  r <- liftP Masked open
  finally (run r) (close r)
