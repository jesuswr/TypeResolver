module Type where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M

data Type = Function Type Type
          | Const String Type
          | Var String Type
          | Void

type Str2Type = M.Map String Type

convert :: String -> Type
convert s = parser $ filter (/= ' ') s

parser :: String -> Type
parser []             = Void
parser ('(':rest)     = do
    let exp = parser rest
    let next = parser $ nxtString rest 1
    Function exp next
parser (')':rest)     = Void
parser ('-':'>':rest) = parser rest
parser rest           =
    if C.isUpper $ head name then
        Const name next
    else
        Var name next
    where
        (name, newRest) = span C.isAlphaNum rest
        next = parser newRest

nxtString :: String -> Int -> String
nxtString s 0          = s
nxtString [] _         = error "PLEASE GIVE A VALID TYPE"
nxtString (')':rest) n = nxtString rest (n-1)
nxtString ('(':rest) n = nxtString rest (n+1)
nxtString (_:rest)   n = nxtString rest n

prevString :: Type -> String
prevString Void = ""
prevString _    = " -> "

instance Show Type where
    show (Function t nxtT) = "(" ++ show t ++ ")" ++ (prevString nxtT) ++ show nxtT
    show (Const name nxtT) = name ++ (prevString nxtT) ++ show nxtT 
    show (Var name nxtT)   = name ++ (prevString nxtT) ++ show nxtT
    show Void              = ""



data Exp = Chain [Exp]
         | Atomic Type
         deriving (Show)

transform :: String -> Str2Type -> Exp
transform [] typeMap         = Chain []
transform ('(':rest) typeMap = do
    let exp = transform rest typeMap
    let Chain nxt = transform (nxtString rest 1) typeMap
    Chain (exp:nxt)
transform (')':rest) typeMap = Chain []
transform (' ':rest) typeMap = transform rest typeMap
transform s typeMap          = do
    let (name, rest) = span C.isAlphaNum s
    let typ = typeMap M.! name
    let Chain nxt = transform rest typeMap
    Chain ((Atomic typ):nxt)


containsIds :: Str2Type -> String -> Bool
containsIds typeMap s =
    foldl (&&) True isContained
    where
        isContained = map (\k -> M.member k typeMap) names
        names = filter (/= "") names'
        names' = LS.splitOneOf "() " s


solve :: Exp -> Exp
solve (Chain es) = solve' $ Chain (map helper es)
    
helper :: Exp -> Exp
helper (Chain es) = solve (Chain es)
helper other      = other

solve' :: Exp -> Exp
solve' (Chain [e]) = e
solve' (Chain (e:e2:rest)) = solve' (Chain ((merge e e2):rest))

merge :: Exp -> Exp -> Exp
merge (Atomic t1) (Atomic t2) = Atomic (merge' t1 t2)

merge' :: Type -> Type -> Type
merge' Void Void = error "LA EVALUACION DADA NO ES VALIDA"
merge' anyt Void = anyt
merge' ()