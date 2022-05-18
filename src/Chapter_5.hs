{-#LANGUAGE ConstraintKinds #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE FlexibleInstances #-}

module Chapter_5 where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

{-
instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (x :# xs) == (y :# ys) = x == y && xs == ys
-}

-- exercise 5.3-i
-- implement Ord for HList

{-
instance Ord (HList '[]) where
  compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare (x :# xs) (y :# ys) = case compare x y of
    EQ -> compare xs ys
    c -> c
-}

-- exercise 5.3-ii
-- implement Show for HList

type family All (c :: Type -> Constraint) 
                (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- exercise 5.3-iii
-- rewrite the ord and show instances in terms of All

instance (All Eq ts, All Ord ts)=> Ord (HList ts) where
  compare HNil HNil = EQ
  compare (x :# xs) (y :# ys) = case compare x y of
    EQ -> compare xs ys
    c -> c

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (x :# xs) = show x ++ " :# " ++ show xs