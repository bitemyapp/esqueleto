{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains functions specific to the [pgcrypto](https://www.postgresql.org/docs/current/pgcrypto.html) module
--
-- @since: 3.5.1

module Database.Esqueleto.PostgreSQL.Pgcrypto
    (HashAlgorithm(..),
     toCrypt,
     fromCrypt,
    ) where

import qualified Data.Text.Internal.Builder as TLB
import Database.Esqueleto.Experimental (toPersistValue)
import Database.Esqueleto.Internal.Internal

{- pgcrypto hashing algorithms
see: https://www.postgresql.org/docs/current/pgcrypto.html

`bf` and `xdes` algorithms have an optional iterations count parameter. All limitations and considerations
mentioned in the `pgcrypto` module documentation regarding iteration count apply. It is possible to supply
an invalid iteration count, which will lead to an sql error.

/Requires/ the pgcrypto module.

@since 3.5.1
-}
data HashAlgorithm
    = BF (Maybe Word)
    | MD5
    | XDES (Maybe Word)
    | DES
  deriving (Eq, Show)

{- | (@crypt()@) Calculate a crypt-like hash from the provided password

/Requires/ the pgcrypto module.

/WARNING/: Using `toCrypt` may leak sensitive data via logging. Filtering logs in production environments
when using `toCrypt`, such as using `filterLogger` on `monad-logger` based stacks is highly advised.

example:
@
share
    [mkPersist sqlSettings]
    [persistLowerCase|
    UserAccount json
        name T.Text
        UniqueName name
        passwordHash T.Text
        deriving Show Read Eq

insertSelect $ do
    pure $
        UserAccount
            <# val "username"
            <&> toCrypt (BF Nothing) "1234password"
@

-- @since 3.5.1
-}
toCrypt :: SqlString s => HashAlgorithm -> s -> SqlExpr (Value s)
toCrypt algorithm pass =
    let alg = case algorithm of
            BF mIterCount ->
                "'bf'" <> case mIterCount of
                             Nothing -> mempty
                             Just iterCount ->
                                "," <> TLB.fromString (show iterCount)
            MD5 -> "'md5'"
            XDES mIterCount ->
                "'xdes'" <> case mIterCount of
                             Nothing -> mempty
                             Just iterCount ->
                                "," <> TLB.fromString (show iterCount)
            DES -> "'des'"
    in ERaw noMeta $ \_ _ -> ("crypt (?, gen_salt(" <> alg <> "))", [toPersistValue pass])

{- | (@crypt()@) Retrieve a hashed password

/Requires/ the pgcrypto module.

example:
@
share
    [mkPersist sqlSettings]
    [persistLowerCase|
    UserAccount json
        name T.Text
        UniqueName name
        passwordHash T.Text
        deriving Show Read Eq


login name pwd = select $ do
    user <- from $ Table UserAccount
    where_ $ user ^. UserAccountName ==. val name
        &&. fromCrypt (user ^. UserAccountPasswordHash) pwd
    pure user
@

-- @since 3.5.1
-}
fromCrypt :: SqlString s => SqlExpr (Value s) -> s -> SqlExpr (Value Bool)
fromCrypt expr pass =
    expr
        ==. ERaw
            noMeta
            ( \_ info ->
                let name = columnName expr info
                in ("crypt (?, " <> name <> ")", [toPersistValue pass])
            )
  where
    columnName (ERaw _ f) info =
            fst $ f Never info
