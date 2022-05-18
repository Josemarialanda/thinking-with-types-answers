> module Chapter_1 where

exercise 1.2-i

Determine the cardinality of Either Bool (Bool, Maybe Bool) -> Bool

ANSWER: Let a = Either Bool (Bool, Maybe Bool) and
            b = Bool

        Then, we know that |a| = |Either Bool (Bool, Maybe Bool)| 
                               = |Bool| + |(Bool, Maybe Bool)|
                               = 2 + 2*(1+2)
                               = 2 + 2*3
                               = 2 + 6
                               = 8 and that
                           |b| = |Bool| = 2

        So, |a->b| = |Either Bool (Bool, Maybe Bool) -> Bool|
                   = |b|^|a|
                   = 2^8
                   = 256


exercise 1.4-i

Use Curry-Howard to prove the exponent law that a^b * a^c = a^(b+c).
That is, provide a function of the type (b -> a) -> (c -> a) -> Either b c -> a
and one of (Either b c -> a) -> (b -> a, c -> a).

ANSWER:

> exercise_14i_to :: (b -> a) -> (c -> a) -> Either b c -> a
> exercise_14i_to f _ (Left  b) = f b
> exercise_14i_to _ g (Right c) = g c

> exercise_14i_from :: (Either b c -> a) -> (b -> a, c -> a)
> exercise_14i_from f = (f . Left, f . Right)

exercise 1.4-ii

Prove (a*b)^c = a^c * b^c

ANSWER: The proof boils down to finding functions of the types:

(c -> (a,b)) -> (c -> a, c -> b) and
(c -> a, c -> b) -> (c -> (a,b))

> exercise_14ii_to :: (c -> (a,b)) -> (c -> a, c -> b)
> exercise_14ii_to f = (fst . f, snd . f)

> exercise_14ii_from :: (c -> a, c -> b) -> (c -> (a,b))
> exercise_14ii_from (f,g) c = (f c, g c)

exercise 1.4-iii

Give a proof of (a^b)^c = a^(b*c). Does it remind you of anything from Prelude?

ANSWER: The proof boils down to finding functions of the types:

(c -> (b -> a)) -> ((b,c) -> a) and
((b,c) -> a) -> (c -> (b -> a))

> exercise_14iii_to :: (c -> (b -> a)) -> ((b,c) -> a)
> exercise_14iii_to f (b,c) = uncurry f (c,b)

> exercise_14iii_from :: ((b,c) -> a) -> (c -> (b -> a))
> exercise_14iii_from f c b = curry f b c