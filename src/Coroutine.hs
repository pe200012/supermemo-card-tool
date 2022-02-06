{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coroutine where

import           Control.Monad.Cont
import           Control.Monad.Identity         ( Identity(runIdentity) )
import           Data.Bifunctor                 ( bimap )

-- | @CoroutineT m result yield arg@ represents a coroutine that runs in monad @m@, yields type @yield@,
--  receives @arg@ when resuming and returns type @result@ when completed
newtype CoroutineT result arg m yield = Co { unCoroutineT :: ContT result m (Either result (yield, arg -> CoroutineT result arg m yield )) }

type Coroutine result arg = CoroutineT result arg Identity

instance Functor (CoroutineT result arg m) where
    fmap f (Co m) = Co (ContT (\c -> runContT m (c . fmap (bimap f (\ct -> fmap f . ct)))))

instance Applicative (CoroutineT result arg m) where
    pure x = Co (ContT (\c -> c (Right (x, const (pure x)))))
    (Co f) <*> Co x = Co
        (ContT
            (\c -> runContT
                x
                (\case
                    Left  result        -> c $ Left result
                    Right (ya, argToCa) -> runContT
                        f
                        (\case
                            Left  result            -> c $ Left result
                            Right (aTob, argToaTob) -> c $ Right (aTob ya, ((<*>) . argToaTob) <*> argToCa)
                        )
                )
            )
        )

instance Monad (CoroutineT result arg m) where
    (Co x) >>= f = Co
        (ContT
            (\c -> runContT
                x
                (\case
                    Left  result  -> c $ Left result
                    Right (a, cc) -> runContT
                        (unCoroutineT (f a))
                        (\case
                            Left  result  -> c $ Left result
                            Right (b, c2) -> c $ Right (b, c2)
                        )
                )
            )
        )

instance MonadTrans (CoroutineT result arg) where
    lift m = x where x = Co $ ContT $ \c -> m >>= \a -> c $ Right (a, const x)

instance MonadIO m => MonadIO (CoroutineT result arg m) where
    liftIO io = x where x = Co (ContT (\c -> liftIO io >>= \a -> c $ Right (a, const x)))

runCoroutineT :: Monad m => CoroutineT result arg m yield -> (yield -> m arg) -> m result
runCoroutineT (Co ct) f = runContT ct $ \case
    Left  r         -> return r
    Right (y, cont) -> f y >>= (`runCoroutineT` f) . cont

runCoroutine :: Coroutine c arg yield -> (yield -> arg) -> c
runCoroutine co f = runIdentity (runCoroutineT co (pure . f))

