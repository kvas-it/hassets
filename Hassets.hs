--
-- Haskell port of ost.assets (financial system core).
--

--                             name   descr  source      destination amount
data Transaction = Transaction String String AssetHolder AssetHolder AssetAmount
    deriving (Show, Eq)

--             name   descr  transactions
data Lot = Lot String String [Transaction]
    deriving (Show, Eq)

class Named a where
    name :: a -> String
    descr :: a -> String

instance Named Transaction where
    name (Transaction n _ _ _ _) = n
    descr (Transaction _ d _ _ _) = d

instance Named Lot where
    name (Lot n _ _) = n
    descr (Lot _ d _) = d
