{-# language TypeApplications #-}

module Common.Test.CTE where

import Common.Test.Models
import Common.Test.Import
import Database.Persist.TH

testCTE :: SpecDb
testCTE = describe "CTE" $ do
    itDb "can refer to the same CTE twice" $ do
        let q :: SqlQuery (SqlExpr (Value Int), SqlExpr (Value Int))
            q = do
                bCte <- with $ do
                    b <- from $ table @B
                    pure b

                a :& b1 :& b2 <- from $
                    table @A
                        `innerJoin` bCte
                            `on` do
                                \(a :& b) ->
                                    a ^. AK ==. b ^. BK
                        `innerJoin` bCte
                            `on` do
                                \(a :& _ :& b2) ->
                                    a ^. AK ==. b2 ^. BK
                pure (a ^. AK, a ^. AV +. b1 ^. BV +. b2 ^. BV)
        insert_ $ A { aK = 1, aV = 2 }
        insert_ $ B { bK = 1, bV = 3 }
        ret <- select q
        asserting $ do
            ret `shouldMatchList`
                [ (Value 1, Value (2 + 3 + 3))
                ]
