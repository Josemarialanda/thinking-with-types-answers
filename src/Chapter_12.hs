{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}


module Chapter_12 where

import GHC.TypeLits(
    Symbol, 
    TypeError, 
    ErrorMessage(
        Text, 
        (:<>:), 
        ShowType, 
        (:$$:)
    ) 
 )
import Data.Kind (Constraint)
import Fcf
    ( Eval,
      type (<=<),
      TyEq,
      Fst,
      type (=<<),
      FromMaybe,
      FindIndex,
      Map )
import Chapter_11 (UniqueKey, Key, OpenProduct (OpenProduct), Any (Any), LookupType)
import qualified Data.Vector as V

instance (TypeError (Text "Attempting to show a function of type '"
          :<>: ShowType (a->b)
          :<>: Text "'"
          :$$: Text "Did you forget to apply an argument?"))
          => Show (a->b) where
              show = undefined 

{- FindElem with generic error messages
type FindElem (key :: k) (ts :: [k]) = 
    FromMaybe Stuck =<< FindIndex (TyEq key) ts
-}

{- FindEleme with custom error messages
type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k])
    where FriendlyFindElem f t ts = FromMaybe (TypeError
                ('Text "Attempted to call `friendlyPrj' to produce a `"
                 ':<>: 'ShowType (f t)
                 ':<>: 'Text "'."
                 ':$$: 'Text "But the OpenSum can only contain one of:"
                 ':$$: 'Text " "
                 ':<>: 'ShowType ts)) =<< FindIndex (TyEq t) ts
-}

insert :: Eval (UniqueKey key ts) ~ 'True 
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f ('(key,t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type family RequireUniqueKey 
    (result :: Bool) 
    (key :: Symbol) 
    (t :: k) 
    (ts :: [(Symbol, k)]) :: Constraint where
        RequireUniqueKey 'True key t ts = ()
        RequireUniqueKey 'False key t ts = 
            TypeError (
                'Text "Attempting to add a field named `" 
                 ':<>: 'Text key
                 ':<>: 'Text "' with type "
                 ':<>: 'ShowType t
                 ':<>: 'Text " to an OpenProduct."
                 ':$$: 'Text "But the OpenProduct already has a field `"
                 ':<>: 'Text key
                 ':<>: 'Text "' with type "
                 ':<>: 'ShowType (LookupType key ts)
                 ':$$: 'Text "Consider using `update' "
                 ':<>: 'Text "instead of `insert'.")

betterInsert :: RequireUniqueKey (Eval (UniqueKey key ts)) key t ts
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f ('(key,t) ': ts)
betterInsert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

{- Exercise 12-i
Add helpful type errors to OpenProduct’s update and delete
functions.
-}

type family FriendlyFindElem (f :: Symbol) (key :: Symbol) (ts :: [(Symbol, k)]) where
  FriendlyFindElem f key ts =
    Eval
      ( FromMaybe
          ( TypeError
              ( 'Text "Attempted to call `" ':<>: 'Text f ':<>: 'Text "` with key `" ':<>: 'Text key ':<>: 'Text "`."
                  ':$$: 'Text "But the OpenProduct only has keys:"
                  ':$$: 'Text " " ':<>: ShowList (Eval (Map Fst ts))
              )
          )
          =<< FindIndex (TyEq key <=< Fst) ts
      )

{- Exercise 12-ii
Write a closed type family of kind [K] → ERRORMESSAGE that pretty
prints a list. Use it to improve the error message from
FriendlyFindElem.
-}

type family ShowList (ts :: [k]) where
  ShowList '[]        = Text ""
  ShowList (a ': '[]) = ShowType a
  ShowList (a ': as)  = ShowType a ':<>: Text ", " ':<>: ShowList as

{- Exercise 12-iii
See what happens when you directly add a TypeError to the
context of a function (eg. foo :: TypeError ... => a). What
happens? Do you know why?

It automatically sees the error and thus is thrown
foo :: TypeError ('Text "error msg") => a
-}