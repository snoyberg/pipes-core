module Control.Pipe.Exception (
  throw,
  catch,
  bracket,
  finally,
  onException,
  ) where

import qualified Control.Exception as E
import Control.Pipe.Common
import Prelude hiding (catch)

-- | Catch an exception within the 'Pipe' monad.
--
-- This function takes a 'Pipe', runs it, and if an exception is raised it
-- executes the handler, passing it the value of the exception.  Otherwise, the
-- result is returned as normal.
--
-- For example, given a 'Pipe':
--
-- > reader :: Pipe () String IO ()
--
-- we can use 'catch' to resume after an exception. For example:
--
-- > safeReader :: Pipe () (Either SomeException String) IO ()
-- > safeReader = catch (reader >+> pipe Right) $ \e -> do
-- >   yield $ Left e
--
-- Note that there is no guarantee that the handler will actually be executed,
-- as any action in a 'Pipe': 'Pipe's at either side can terminate before the
-- handler has a chance to be executed.
--
-- It is therefore common to use 'ensure' within an exception handler to
-- perform cleanup or finalization of resources.  However, we recommend using
-- 'finally' or 'bracket' for such use cases.
catch :: (Monad m, E.Exception e)
      => Pipe a b m r               -- ^ The 'Pipe' to run
      -> (e -> Pipe a b m r)        -- ^ The handler function
      -> Pipe a b m r
catch p h = catchP p $ \e -> case E.fromException e of
  Nothing -> throwP e
  Just e' -> h e'

-- | Throw an exception within the 'Pipe' monad.
--
-- An exception thrown with 'throw' can be caught by 'catch' with any base
-- monad.
--
-- If the exception is not caught in the 'Pipeline' at all, it will be rethrown
-- as a normal Haskell exception when using 'runPipe'.  Note that 'runPurePipe'
-- returns the exception in an 'Either' value, instead.
throw :: (Monad m, E.Exception e) => e -> Pipe a b m r
throw = throwP . E.toException

-- | Like 'finally', but only performs the final action if there was an
-- exception raised by the 'Pipe'.
onException :: Monad m
            => Pipe a b m r
            -> Pipe a b m s
            -> Pipe a b m r
onException p w = catchP p $ \e -> w >> throw e

-- | A specialized variant of 'bracket' with just a computation to run
-- afterwards.
finally :: Monad m
        => Pipe a b m r
        -> m s
        -> Pipe a b m r
finally p w = do
  r <- onException p (ensure w)
  ensure w
  return r

-- | Allocate a resource within the base monad, run a pipe, then ensure the
-- resource is released.
--
-- The typical example is reading from a file:
--
-- > bracket
-- >   (openFile "filename" ReadMode)
-- >   hClose
-- >   (\handle -> do
-- >       line <- lift $ hGetLine handle
-- >       yield line
-- >       ...)
bracket :: Monad m
        => m r
        -> (r -> m y)
        -> (r -> Pipe a b m x)
        -> Pipe a b m x
bracket open close run = do
  r <- liftP Masked open
  finally (run r) (close r)
