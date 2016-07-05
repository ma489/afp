-- Generic programming (WIP)

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics (Data, gshows)

class ShowG a where
    showG :: a -> String

instance ShowG Bool where
    showG True = "True"
    showG False = "False"

-- ...

--Example usage: showG True

class ReadG a where
    readG :: String -> a

instance ReadG Bool where
    readG "True" = True
    readG "False" = False
    readG _ = error "Not a Bool"

-- ...

--Example usage: readG "True" :: Bool

showG' :: Data d => d -> String
showG' thingToShow = gshows thingToShow ""