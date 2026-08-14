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

    itDb "aliases new columns after a union with a CTE reference" $ do
        -- A union whose right branch selects already-aliased CTE references
        -- allocates fewer idents than its left branch. The enclosing query
        -- must not reuse the left branch's idents for later aliases, or the
        -- select list ends up with duplicate column names and references to
        -- them are ambiguous (a variant of issue #299).
        let q :: SqlQuery (SqlExpr (Value Int), SqlExpr (Value Int), SqlExpr (Value Int))
            q = do
                bCte <- with $ do
                    b <- from $ table @B
                    pure (b ^. BK, b ^. BV)
                (k, v, extra) <- from $ do
                    (k, v) <- from $
                        (do
                            b <- from $ table @B
                            pure (b ^. BK, b ^. BV))
                        `union_` from bCte
                    pure (k, v, val (42 :: Int))
                pure (k, v, extra)
        insert_ $ B { bK = 1, bV = 3 }
        ret <- select q
        asserting $ do
            ret `shouldMatchList`
                [ (Value 1, Value 3, Value 42)
                ]
