module Type where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

data Type = Function [Type]
          | Const String
          | Var String

type Str2Type = M.Map String Type

convert :: String -> Type
convert s = parser $ filter (/= ' ') s

parser :: String -> Type
parser []             = Function []
parser ('(':rest)     = do
    let exp = parser rest
    let Function next = parser $ nxtString rest 1
    Function (exp:next)
parser (')':rest)     = Function []
parser ('-':'>':rest) = parser rest
parser rest           =
    if C.isUpper $ head name then
        Function ((Const name):next)
    else
        Function ((Var name):next)
    where
        name = takeWhile C.isAlpha rest
        Function next = parser $ dropWhile C.isAlpha rest

nxtString :: String -> Int -> String
nxtString s 0          = s
nxtString [] _         = error "PLEASE GIVE A VALID TYPE"
nxtString (')':rest) n = nxtString rest (n-1)
nxtString ('(':rest) n = nxtString rest (n+1)
nxtString (_:rest)   n = nxtString rest n

instance Show Type where
    show (Function ts) = "(" ++ (L.intercalate "->" (map show ts)) ++ ")"
    show (Const name)  = name
    show (Var name)    = name