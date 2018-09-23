
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module UserSpec where
import Protolude
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import API
import           Test.Hspec
import           Servant.Swagger.Test
import Database.Esqueleto


instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Entity a) where
  arbitrary = Entity <$> (return (toSqlKey 1))<*> arbitrary

spec :: Spec
spec = describe "Swagger" $ do
  context "ToJSON matches ToSchema" $ validateEveryToJSON  appAPI
