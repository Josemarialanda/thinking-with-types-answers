{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chapter_13 where

import GHC.Generics
import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Functor.Yoneda
import Data.Functor.Day.Curried
import Control.Monad.Codensity

data Maybe' a = Nothing' | Just' a

toCanonical :: Maybe' a -> Either () a
toCanonical Nothing'  = Left ()
toCanonical (Just' a) = Right a

fromCanonical :: Either () a -> Maybe' a
fromCanonical (Left _)  = Nothing'
fromCanonical (Right a) = Just' a

{-
class Generic a where
    type Rep a :: Type -> Type
    from :: a -> Rep a x
    to   :: Rep a x -> a
-}

class GEq a where
    geq :: a x -> a x -> Bool

instance GEq U1 where
  geq _ _ = True

instance GEq V1 where
    geq _ _ = True

instance Eq a => GEq (K1 _1 a) where
    geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L1 a1) (L1 a2) = geq a1 a2
    geq (R1 b1) (R1 b2) = geq b1 b2
    geq _ _             = False

instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq a => GEq (M1 _x _y a) where
    geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool 
genericEq a b = geq (from a) (from b)

data Foo a b c = F0
               | F1 a 
               | F2 b c
    deriving (Generic, MyEq)

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
    (==) = genericEq

{- Exercise 13.2-i
Provide a generic instance for the Ord class.
-}

class GOrd a where
    gord :: a x -> a x -> Ordering

instance GOrd U1 where
    gord _ _ = EQ

instance GOrd V1 where
    gord _ _ = EQ

instance Ord a => GOrd (K1 _1 a) where
    gord (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
    gord (L1 a1) (L1 a2) = gord a1 a2
    gord (R1 b1) (R1 b2) = gord b1 b2
    gord (L1 _) (R1 _)   = LT
    gord (R1 _) (L1 _)   = GT

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
    gord (a1 :*: b1) (a2 :*: b2) = gord a1 a2 <> gord b1 b2

instance GOrd a => GOrd (M1 _x _y a) where
    gord (M1 a1) (M1 a2) = gord a1 a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering 
genericOrd a b = gord (from a) (from b)


{- Exercise 13.2-ii
Use GHC.Generics to implement the function exNihilo :: Maybe a.
This function should give a value of Just a if a has exactly one
data constructor which takes zero arguments. Otherwise, 
exNihilo should return Nothing.
-}

class GexNihilo a where
    gexNihilo :: Maybe (a x)

instance GexNihilo U1 where
    gexNihilo = Just U1

instance GexNihilo V1 where
    gexNihilo = Nothing

instance GexNihilo (K1 _1 a) where
    gexNihilo = Nothing

instance GexNihilo (a :+: b) where
    gexNihilo = Nothing

instance GexNihilo (a :*: b) where
  gexNihilo = Nothing

instance GexNihilo a => GexNihilo (M1 _x _y a) where
  gexNihilo = fmap M1 gexNihilo -- TODO: What's going on here?

exNihilo :: forall a. (Generic a, GexNihilo (Rep a)) => Maybe a
exNihilo = to <$> gexNihilo @(Rep a)

class MyEq a where
    eq :: a -> a -> Bool 
    default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool 
    eq a b = geq (from a) (from b)

-- Using Generic Metadata

-- the following Haskell type
data Person = Person
    { name        :: String
    , age         :: Int
    , phone       :: Maybe String
    , permissions :: [Bool]
    }
    deriving (Generic)

-- would be described in JSON Schema as:

{-

{ "title": "Person"
, "type": "object"
, "properties":
    { "name": { "type": "string" }
    , "age": { "type": "integer" }
    , "phone": { "type": "string" }
    , "permissions": { "type": "array", "items": { "type": "boolean" }}
    }
, "required": ["name" , "age", "permissions"]
}

-}

class GSchema (a :: Type -> Type) where
    gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family ToJSONType (a :: Type) :: Symbol where
    ToJSONType Int     = "integer"
    ToJSONType Integer = "integer"
    ToJSONType Float   = "number" 
    ToJSONType Double  = "number"
    ToJSONType String  = "string"
    ToJSONType Bool    = "boolean"
    ToJSONType [a]     = "array"
    ToJSONType a       = TypeName a

type family RepName (x :: Type -> Type) :: Symbol where
    RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
    TypeName t = RepName (Rep t)

makeTypeObj :: forall a. KnownSymbol (ToJSONType a) => Value
makeTypeObj = object ["type" .= String (pack . symbolVal $ Proxy @(ToJSONType a))]

makePropertyObj :: forall name. (KnownSymbol name) => Value -> Value
makePropertyObj v = object ["type" .= String (pack . symbolVal $ Proxy @name) , "properties" .= v]

instance (KnownSymbol nm, KnownSymbol (ToJSONType a))
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 a)) where                      
    gschema = do emitRequired @nm
                 return $ makePropertyObj @nm (makeTypeObj @a)
    {-# INLINE gschema #-}     
 
instance (GSchema f, GSchema g) => GSchema (f :*: g) where 
    gschema = mergeObjects <$> gschema @f
                           <*> gschema @g
    {-# INLINE gschema #-}     

instance (TypeError ('Err.Text "JSON Schema does not support sum types"))
    => GSchema (f :+: g) where
    gschema = error "JSON SChema does not support sum types"
    {-# INLINE gschema #-}     

instance GSchema a => GSchema (M1 C _1 a) where
    gschema = gschema @a
    {-# INLINE gschema #-}

instance (GSchema a, KnownSymbol nm)
    => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
    gschema = do
        sch <- gschema @a
        return $ object
            [ "title" .= (String . pack . symbolVal $ Proxy @nm)
            , "type"  .= String "object"
            , "properties" .= sch
            ]
    {-# INLINE gschema #-}

{-# INLINE schema #-}
schema :: forall a. (GSchema (Rep a), Generic a) => Value
schema = let (v,reqs) = runWriter $ gschema @(Rep a)
          in mergeObjects v $ object 
                ["required" .= 
                    Array (fromList $ String <$> reqs)
                ]

instance {-# OVERLAPPING #-} (KnownSymbol nm, KnownSymbol (ToJSONType a))
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe a))) where
    gschema = pure . makePropertyObj @nm $ makeTypeObj @a
    {-# INLINE gschema #-}

instance {-# OVERLAPPING #-} ( KnownSymbol nm
                             , KnownSymbol (ToJSONType [a])
                             , KnownSymbol (ToJSONType a))
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [a])) where
    gschema = do
        emitRequired @nm 
        let innerType = object ["items" .= makeTypeObj @a]
        pure . makePropertyObj @nm
             . mergeObjects innerType
             $ makeTypeObj @[a]
    {-# INLINE gschema #-}

instance {-# OVERLAPPING #-} KnownSymbol nm
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String)) where
    gschema = do
        emitRequired @nm 
        pure . makePropertyObj @nm
             $ makeTypeObj @String
    {-# INLINE gschema #-}

example :: IO ()
example = pp $ schema @Person 
    where pp = LC8.putStrLn . encodePretty @Value

-- Kan Extensions

{-

newtype Yoneda' f a
  = Yoneda
      { runYoneda :: forall b. (a -> b) -> f b
      }

-- Data.Functor.Yoneda == forall f. Functor f => f a
-- Data.Functor.Day.Curried == forall f. Applicative f => f a
-- Control.Monad.Codensity == forall f. Monad f => f a
instance Functor (Yoneda f) where
  fmap f (Yoneda y) = Yoneda $ \k -> y (k . f)

-}