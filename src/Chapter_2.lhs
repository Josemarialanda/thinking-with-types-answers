> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GADTs #-}


> module Chapter_2 where

> import GHC.TypeLits

Exercise 2.1.3-i
If Show Int has kind CONSTRAINT, what's the kind of Show?
ANSWER: TYPE -> CONSTRAINT

Exercise 2.1.3-ii
What is the kind of Functor?
ANSWER: (TYPE -> TYPE) -> CONSTRAINT

Exercise 2.1.3-iii
What is the kind of Monad?
ANSWER: (TYPE -> TYPE) -> CONSTRAINT

Exercise 2.1.3-iv
What is the kind of MonadTrans?
ANSWER: ((TYPE -> TYPE) -> TYPE -> TYPE) -> CONSTRAINT

Exercise 2.4-i
Write a closed type family to compute Not.

> type family Not (x :: Bool) :: Bool where
>   Not 'True  = 'False
>   Not 'False = 'True