module Common.Test.Select where

import Common.Test.Import

testSelect :: SpecDb
testSelect = do
    describe "select" $ do
        itDb "works for a single value" $ do
            ret <- select $ return $ val (3 :: Int)
            asserting $ ret `shouldBe` [ Value 3 ]

        itDb "works for a pair of a single value and ()" $ do
            ret <- select $ return (val (3 :: Int), ())
            asserting $ ret `shouldBe` [ (Value 3, ()) ]

        itDb "works for a single ()" $ do
            ret <- select $ return ()
            asserting $ ret `shouldBe` [ () ]

        itDb "works for a single NULL value" $ do
            ret <- select $ return nothing
            asserting $ ret `shouldBe` [ Value (Nothing :: Maybe Int) ]
