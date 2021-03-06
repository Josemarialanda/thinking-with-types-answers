{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Chapter_9 where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )

data a :<< b
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type
    format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
    type Printf text = String
    format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text)=> HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a
    format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a) 

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
    type Printf (param :<< a) = param -> Printf a
    format s _ param = format (s <> show param) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
    type Printf (String :<< a) = String -> Printf a
    format s _ string = format (s <> string) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format "" 

test :: String
test = printf (Proxy @("The answer to " :<< Int :<< "+" :<< Int :<< " is " :<< Int :<< "")) 1 1 (1+1)