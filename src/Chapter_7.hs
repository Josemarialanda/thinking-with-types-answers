{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module Chapter_7 where

import Data.Foldable (asum)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Typeable ( Typeable, cast )
import Data.IORef ( writeIORef, readIORef, newIORef, IORef ) 
import System.IO.Unsafe (unsafePerformIO)

data Any where
    Any :: a -> Any

elimEnay :: (forall a. a -> r) -> Any -> r
elimEnay f (Any a) = f a

{- exercise 7.1-i
Are functions of type forall a. a -> r interesting? Why or why not?

Not really, because we can't do anything with the type variable a, since
that Any doesn't store any information about it. Thus the only thing we 
can do is return any value polymorphic value of type r with no relation
to the type variable a.
-}

notInteresting :: r ~ String => r
notInteresting = elimEnay f (Any "hello")
    where f a = "not interesting"

data HashShow where
    HashShow :: Show t => t -> HashShow

{-
instance Show HashShow where
    show (HashShow s) = "HashShow " ++ show s
-}

{- exercise 7.1-ii
What happens to this instance if you remove the Show t =>
constraint from HashShow?

It turns into Data Any, which is not a valid instance of Show,
since there is no way to get the Show instance from the type variable t.
-}

elimHasShow :: (forall a. Show a => a -> r) -> HashShow -> r
elimHasShow f (HashShow s) = f s

{- exercise 7.1-iii
Write the Show instance for HasShow in terms of elimHasShow.
-}

instance Show HashShow where
    show = elimHasShow (("HashShow " ++) . show)

data Dynamic where
    Dynamic :: Typeable a => a -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => Dynamic 
                                               -> Dynamic 
                                               -> (a -> b -> r)
                                               -> Maybe Dynamic
liftD2 d1 d2 f = do
    a <- fromDynamic @a d1
    b <- fromDynamic @b d2
    return $ Dynamic @r $ f a b

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int    @Int    a b (+)
    , liftD2 @String @Int    a b $ \strA intB -> strA ++ show intB
    , liftD2 @Int    @String a b $ \intA strB -> show intA ++ strB
    ]

-- Factoring out the pattern

data Has (c :: Type -> Constraint) where
    Has :: c a => a -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

-- We can implement and Dynamic as type synonyms

-- type HasShow = Has Show
-- type HasEq   = Has Eq

newtype ST s a  = ST { unsafeRunST :: a }

instance Functor (ST s) where
    fmap f (ST !a) = ST (f a)

instance Applicative (ST s) where
    pure = ST
    ST !f <*> (ST !a) = ST (f a)

instance Monad (ST s) where
    return = pure
    ST !a >>= f = f a

newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref (f a)

runST :: forall a. (forall s. ST s a) -> a
runST !st = unsafeRunST st

safeExample :: ST s String
safeExample = do
    ref <- newSTRef "hello"
    modifySTRef ref (++ " world")
    readSTRef ref

runSafeExample :: String
runSafeExample = runST safeExample