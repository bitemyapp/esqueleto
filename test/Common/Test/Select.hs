module Common.Test.Select where

import Common.Test.Import

testSelect :: Run -> Spec
testSelect run = do
    describe "select" $ do
        it "works for a single value" $
            run $ do
                ret <- select $ return $ val (3 :: Int)
                liftIO $ ret `shouldBe` [ Value 3 ]

        it "works for a pair of a single value and ()" $
            run $ do
                ret <- select $ return (val (3 :: Int), ())
                liftIO $ ret `shouldBe` [ (Value 3, ()) ]

        it "works for a single ()" $
            run $ do
                ret <- select $ return ()
                liftIO $ ret `shouldBe` [ () ]

        it "works for a single NULL value" $
            run $ do
                ret <- select $ return nothing
                liftIO $ ret `shouldBe` [ Value (Nothing :: Maybe Int) ]
