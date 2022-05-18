> module Chapter_3 where

Exercise 3-i
Which of these types are Functors? Give instances for the ones
that are.

Note: Functor instances can only be covariant

> newtype T1 a = T1 (Int -> a)
> -- covariant
> instance Functor T1 where
>   fmap f (T1 a) = T1 $ f . a

> -- contravariant
> newtype T2 a = T2 (a -> Int)

> -- invariant
> newtype T3 a = T3 (a -> a)

> -- contravariant
> newtype T4 a = T4 ((Int -> a) -> Int)

> newtype T5 a = T5 ((a -> Int) -> Int)
> 
> instance Functor T5 where
>   fmap f (T5 a) = T5 (\g -> a $ g . f)