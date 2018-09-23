
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module CustomKey where
import Protolude
import Database.Esqueleto hiding (from)
newtype CustomKey a =
  Key Int
  deriving (Generic)

instance Generic (Key a) where
  type Rep (Key a) = Rep (CustomKey a)
  from = from
  to = to

