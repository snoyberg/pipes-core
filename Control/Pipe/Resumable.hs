{-# LANGUAGE FlexibleContexts #-}

module Control.Pipe.Resumable where

import Control.Pipe.Common

import Control.Exception (Exception)
import Control.Exception.Lifted (catch)
import Control.Monad.Trans.Control
import Prelude hiding (catch)

resumableJust :: (MonadBaseControl IO m, Exception e)
              => Pipe a b m r
              -> Pipe a (Either e b) m (Maybe r)
resumableJust = resumable . fmap Just

resumable :: (MonadBaseControl IO m, Exception e)
          => Pipe a b m (Maybe r)
          -> Pipe a (Either e b) m (Maybe r)
resumable (Pure r) = return r
resumable (Yield x c) = yield (Right x) >> resumable c
resumable (Await k) = tryAwait >>= \x -> resumable (k x)
resumable (M m) = M $
  catch (fmap resumable m) $ \e ->
    return $ do
      yield (Left e)
      return Nothing

(>?>) :: (MonadBaseControl IO m, Exception e)
      => Pipe (Either e a) b m (Maybe r)
      -> Pipe (Either e b) c m (Maybe r)
      -> Pipe (Either e a) c m (Maybe r)
p1 >?> p2 = resumable p1 >+> p2
