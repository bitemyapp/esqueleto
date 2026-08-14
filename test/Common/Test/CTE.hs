{-# language TypeApplications #-}

module Common.Test.CTE where

import Common.Test.Models
import Common.Test.Import
import Database.Persist.TH

testCTE :: SpecDb
testCTE = describe "CTE" $ do
    itDb "aliases a repeated reference in a subquery select list" $ do
        -- A reference into an inner scope can appear twice in one select
        -- list; each occurrence must get its own output alias or outer
        -- references to the duplicated column name are ambiguous.
        let q :: SqlQuery (SqlExpr (Value Int), SqlExpr (Value Int))
            q = do
                (a, b) <- from $ do
                    (x, _) <- from $ do
                        b <- from $ table @B
                        pure (b ^. BK, b ^. BV)
                    pure (x, x)
                pure (a, b)
        insert_ $ B { bK = 1, bV = 3 }
        ret <- select q
        asserting $ do
            ret `shouldMatchList`
                [ (Value 1, Value 1)
                ]

    itDb "layered CTEs over subqueries with repeated references" $ do
        let recentTransactions :: SqlQuery (SqlExpr (Value Int), SqlExpr (Value (Maybe Int)), SqlExpr (Value Int))
            recentTransactions = do
                b <- from $ table @B
                limit 10
                pure (b ^. BK, just (b ^. BV), b ^. BK)
            getPrev tr = with $ do
                (k, mr, _) <- tr
                pure (k, mr, just (k +. val 1), just (k +. val 2))
            filterRuns tr = do
                prev <- getPrev tr
                (k, mr, p1, _) <- from prev
                where_ $ isNothing_ p1 ||. p1 !=. just (val 0)
                pure (k, mr, k)
            inferTime tr = do
                let removed = from $ filterRuns tr
                prev <- getPrev removed
                (k, mr, _, p2) <- from prev
                let newT = coalesceDefault [p2] k
                pure (k, mr, newT)
            q :: SqlQuery (SqlExpr (Value Int), SqlExpr (Value (Maybe Int)), SqlExpr (Value Int))
            q = do
                recent <- with $ inferTime (from recentTransactions)
                sent <- with $ do
                    (k, mr, t) <- from recent
                    where_ $ k >=. val 0
                    pure (k, just (coalesceDefault [mr] (val 0)), t)
                recRecs <- with $ distinct $ do
                    (_, mr, _) <- from sent
                    pure mr
                recRecTx <- with $ do
                    (mr :& b) <- from $ recRecs
                        `innerJoin` table @B
                            `on` do
                                \(mr :& b) -> mr ==. just (b ^. BV)
                    pure (b ^. BK, mr, b ^. BV)
                deduped <- with $ do
                    r@(k, _, _) <- from recRecTx
                    where_ $ not_ $ exists $ do
                        (k2, _, _) <- from sent
                        where_ $ k2 ==. k
                    pure r
                (k, mr, t) <- from $ from sent `union_` from deduped
                pure (k, mr, t)
        insert_ $ B { bK = 1, bV = 3 }
        ret <- select q
        asserting $ do
            ret `shouldMatchList`
                [ (Value 1, Value (Just 3), Value 3)
                ]

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
