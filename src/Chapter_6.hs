{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Chapter_6 where

test2 :: forall a b. (a -> b) -> (forall c. c -> a) -> b
test2 f g = undefined

{-
NOTE: A function gains higher rank every time a
forall quantiﬁer exists on the left-side of a function arrow.

But aren’t forall quantiﬁers always on the left-side
of a function arrow? 

While it might seem that way, this is merely a quirk of
Haskell’s syntax. Because the forall quantiﬁer binds more loosely
than the arrow type (->), the everyday type of id ,

forall a. a -> a

has some implicit parentheses. When written in full:

forall a. (a -> a)

it’s easier to see that the arrow is in fact captured by the forall.
Compare this to a rank-n type with all of its implicit parentheses
inserted:

forall r. ((forall a. (a -> r)) -> r)

Here we can see that indeed the forall a. is to the left of a
function arrow -- the outermost one.

And so, the rank of a function is simply the number of arrows
its deepest forall is to the left of.
-}

{- exercise 6.3-i
what is the rank of Int -> forall a. a -> a?
Hint: try adding the explicit parentheses.

(Int -> (forall a. (a -> a)))

rank 1
-}

{- -- exercise 6.3-ii
What is the rank of (a -> b) -> (forall c. c -> a) -> b?
Hint: recall that the function arrow is right-associative, 
so a -> b -> c is actually parsed as a -> (b -> c).

((a -> b) -> ((forall c. (c -> a)) -> b))

rank 2
-}

{- exercise 6.3-iii
What is the rank of ((forall x. m x -> b (z m x)) -> b (z m a)) - > m a?
Believe it or not, this is a real type signature we had to
write back in the bad old days before MonadUnliftIO!

rank-3
-}

newtype Cont a = Cont { runCont :: forall r. (a -> r) -> r }

{- exercise 6.4-i
Provide a Functor instance for Cont. Hint: use lots of type holes,
and an explicit lambda whenever looking for a function type.
The implementation is sufficiently difficult that trying to write
it point-free will be particularly mind-bending.
-}

instance Functor Cont where

{- exercise 6.4-ii  
Provide the Applicative instances for Cont.
-}

instance Applicative Cont where


{- exercise 6.4-iii 
Provide the Monad instances for Cont
-}

instance Monad Cont where

{- exercise 6.4-iv 
There is also a monad transformer version of Cont. Implement it.


-}

ret :: a -> (a -> r) -> r
ret val = \out -> out val

twoC :: (a ~ Integer) => (a -> r) -> r
twoC = ret 2

helloC :: (a ~ String) => (a -> r) -> r
helloC = ret "hello"

bind :: ((a -> r) -> r) -> (a -> (b -> r)) -> (b -> r)
inCont `bind` fn = \out -> inCont (\inContVal -> (fn inContVal) out)
         
fourC :: (a ~ Integer) => (a -> r) -> r
fourC = twoC `bind` \two -> ret (two*2)

badC :: p -> String
badC = \out -> "boom!"

twoBadC :: (String -> String) -> String
twoBadC = twoC `bind` \two ->
            badC `bind` \hello ->
              ret $ show two++hello