--
-- Haskell port of ost.assets (financial system core).
--

--                         code   name   descr  precision
data AssetType = AssetType String String String Integer
    deriving Show

--                                   type      balance
data HoldingAccount = HoldingAccount AssetType Integer
    deriving Show

--                             name   descr  accounts
data AssetHolder = AssetHolder String String [HoldingAccount]
    deriving Show


class Named a where
    name :: a -> String
    descr :: a -> String

instance Named AssetType where
    name (AssetType _ n _ _) = n
    descr (AssetType _ _ d _) = d

instance Named AssetHolder where
    name (AssetHolder n _ _) = n
    descr (AssetHolder _ d _) = d

