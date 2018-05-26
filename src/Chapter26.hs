module Chapter26 where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (Reader, ReaderT(..))
import Data.Functor.Identity (Identity)

-- EitherT

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m
      => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT mef) <*> (EitherT mea) =
    EitherT $ (<*>) <$> mef <*> mea

instance Monad m
      => Monad (EitherT e m) where
  return = pure

  (EitherT mea) >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left e -> return (Left e)
        Right a -> runEitherT (f a)

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) =
  EitherT $ swapEither <$> mea

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = do
  v <- mab
  case v of
    Left a -> f a
    Right b -> g b

-- StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m
      => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ (fmap . fmap) f' sma
    where
      f' (a, s) = (f a, s)

instance Monad m
      => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT smf) <*> (StateT sma) =
    StateT $ \s -> do
      (f, s') <- smf s
      (a, s'') <- sma s'
      return (f a, s'')

instance Monad m
      => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f =
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

-- Wrap It Up

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded =
  MaybeT $
  ExceptT $
  ReaderT $
  fmap return (const (Right (Just 1)))

-- Lift More

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    v <- ma
    return (v, s)

-- MonadIO Instances

instance (MonadIO m)
      => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance (MonadIO m)
      => MonadIO (StateT s m) where
  liftIO = lift . liftIO

-- Chapter Exercises

-- Write the code

rDec :: Num a => Reader a a
rDec = ReaderT $ \r ->
  return (r - 1)

rDec' :: Num a => Reader a a
rDec' = ReaderT $ return . (+) (-1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \r ->
  return $ show r

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn $ "Hi: " ++ show r
  return $ r + 1

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
