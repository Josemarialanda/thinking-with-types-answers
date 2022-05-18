{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter_8 where 

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))

{- Exercise 8.2-i
What is the role signature of Either a b?

Either representational representational
-}

{- Exercise 8.2-ii
What is the role signature of Proxy a?

Proxy phantom
-}

type role BST nominal
data BST v = Empty 
           | Branch (BST v) v (BST v)