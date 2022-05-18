{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Chapter_10 where

import Prelude hiding (fst)
import Data.Kind (Constraint, Type)

{- Exercise 10.1-i
Defunctionalize listToMaybe :: [a] -> Maybe a.
-}

data ListToMaybe a = ListToMaybe [a]

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe xs) = case xs of       
        []     -> Nothing 
        a : as -> Just a

{- Exercise 10.2-i
Defunctionalize listToMaybe at the type-level.
-}

type Exp a = a -> Type
type family Eval' (e :: Exp a) :: a

data ListToMaybe' :: [a] -> Exp (Maybe a)
type instance Eval' (ListToMaybe' '[])      = 'Nothing
type instance Eval' (ListToMaybe' (a ': _)) = 'Just a

{- Exercise 10.2-ii
Defunctionalize foldr :: (a -> b -> b) -> b -> [a] -> b.
-}

data Foldr :: (a -> b -> b) -> b -> [a] -> Exp b
type instance Eval' (Foldr _ b '[])       = b
type instance Eval' (Foldr f b (a ': as)) = Eval' (Foldr f (f a b) as)

{- Exercise 10.4-i
Write a promoted functor instance for tuples.
-}

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval' (Map f '(a,b)) = '(a, Eval' (f b))