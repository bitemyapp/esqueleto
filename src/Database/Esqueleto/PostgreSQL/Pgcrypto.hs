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

import Database.Esqueleto.Internal.Internal 
import Database.Esqueleto.Experimental (toPersistValue)

{- pgcrypto hashing algorithms
see: https://www.postgresql.org/docs/current/pgcrypto.html

/Requires/ the pgcrypto module.

@since 3.5.1
-}
data HashAlgorithm = BF | MD5 | XDES | DES
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
            <&> toCrypt BF "1234password"
@

-- @since 3.5.1
-}
toCrypt :: SqlString s => HashAlgorithm -> s -> SqlExpr (Value s)
toCrypt algorithm pass =
    let alg = case algorithm of
            BF -> "bf"
            MD5 -> "md5"
            XDES -> "xdes"
            DES -> "des"
    in ERaw noMeta $ \_ _ -> ("crypt (?, gen_salt('" <> alg <> "'))", [toPersistValue pass])

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
