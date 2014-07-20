{-# LANGUAGE StandaloneDeriving #-}
module Orphans where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

import Data.Hashable
import Web.Twitter.Types

deriving instance Read BoundingBox
deriving instance Read Entities
deriving instance (Read a) => Read (Entity a)
deriving instance Read HashTagEntity
deriving instance Read MediaEntity
deriving instance Read MediaSize
deriving instance Read Place
deriving instance Read Status
deriving instance Read URLEntity
deriving instance Read User
deriving instance Read UserEntity

-- Ord is excessive but enables the lazy way of doing it
instance (Read k, Read v, Ord k, Hashable k) => Read (HashMap.HashMap k v) where
  readsPrec p s = map (\(v,r) -> (HashMap.fromList (Map.toList v), r)) (readsPrec p s)
