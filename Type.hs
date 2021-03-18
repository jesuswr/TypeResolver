module Type where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M


-- data type to manage functions, constants and variables
data Type = Function Type Type
          | Const String Type
          | Var String Type
          | Void
          deriving (Eq)


-- alias to have a map of Types
type Str2Type = M.Map String Type


-- functions to parse the given string to a Type
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


-- function to get the next string to parse, it looks for the string
-- after the corresponding closing parenthesis
nxtString :: String -> Int -> String
nxtString s 0          = s
nxtString [] _         = error "PLEASE GIVE A VALID TYPE"
nxtString (')':rest) n = nxtString rest (n-1)
nxtString ('(':rest) n = nxtString rest (n+1)
nxtString (_:rest)   n = nxtString rest n


-- function to help the Show instane
prevString :: Type -> String
prevString Void = ""
prevString _    = " -> "


-- show instance of Type to print in the correct format
instance Show Type where
    show (Function t nxtT) = "(" ++ show t ++ ")" ++ (prevString nxtT) ++ show nxtT
    show (Const name nxtT) = name ++ (prevString nxtT) ++ show nxtT 
    show (Var name nxtT)   = name ++ (prevString nxtT) ++ show nxtT
    show Void              = ""


-- data to manage expressions to evaluate
data Exp = Chain [Exp]
         | Atomic Type
         deriving (Show)


-- Exp parser
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


-- function to check if all names in the expression exist
containsIds :: Str2Type -> String -> Bool
containsIds typeMap s =
    foldl (&&) True isContained
    where
        isContained = map (\k -> M.member k typeMap) names
        names = filter (/= "") names'
        names' = LS.splitOneOf "() " s


-- function to evaluate expressions, first you solve expressions
-- inside parenthesis and then you solve the resulting expression
solve :: Exp -> Exp
solve (Chain es) = solve' $ Chain (map helper es)


-- function to solve expressions inside parenthesis
helper :: Exp -> Exp
helper (Chain es) = solve (Chain es)
helper other      = other


-- function to evaluate the expression, basically it takes the
-- first two, evaluates them and them puts the resulting exp 
-- at the start of the list and calls itself again
solve' :: Exp -> Exp
solve' (Chain [e]) = e
solve' (Chain (e:e2:rest)) = solve' (Chain ((merge e e2):rest))


-- function to get Type out of Exp and call the apply function with them
merge :: Exp -> Exp -> Exp
merge (Atomic t1) (Atomic t2) = Atomic (fst $ apply t1 t2 M.empty)


-- function to apply the second type to the first one (has some bugs)
apply :: Type -> Type -> Str2Type -> (Type, Str2Type)
apply (Function p1 p2) (Function q1 q2) typeMap = do
    let (e, newMap) = apply p1 q1 typeMap
    apply p2 q2 newMap

apply (Function p1 p2) (Const name2 q) typeMap  = error "TIPO NO VALIDO"

apply (Function p1 p2) (Var name2 q) typeMap    = error "TIPO NO VALIDO"

apply (Function p1 p2) Void typeMap             = 
    (subst (Function p1 p2) typeMap, typeMap)

apply (Const name p) (Function q1 q2) typeMap   = error "TIPO NO VALIDO"

apply (Const name p) (Const name2 q) typeMap    = do
    if name == name2 then 
        apply p q typeMap
    else
        error "TIPO NO VALIDO"

apply (Const name p) (Var name2 q) typeMap      = apply p q typeMap

apply (Const name p) Void typeMap               = 
    (subst (Const name p) typeMap, typeMap)

apply (Var name p) (Function q1 q2) typeMap     = 
    if not $ M.member name typeMap then
        apply p q2 (M.insert name (Function q1 Void) typeMap)
    else if (typeMap M.! name == (Function q1 Void)) then
        apply p q2 typeMap
    else 
        error "TIPO NO VALIDO"  

apply (Var name p) (Const name2 q) typeMap      = 
    if not $ M.member name typeMap then
        apply p q (M.insert name (Const name2 Void) typeMap)
    else if (typeMap M.! name == (Const name2 Void)) then
        apply p q typeMap
    else 
        error "TIPO NO VALIDO" 

apply (Var name p) (Var name2 q) typeMap        = 
    if not $ M.member name typeMap then
        apply p q (M.insert name (Var name2 Void) typeMap)
    else if (typeMap M.! name == (Var name2 Void)) then
        apply p q typeMap
    else 
        error "TIPO NO VALIDO" 

apply (Var name p) Void typeMap                 = 
    (subst (Var name p) typeMap, typeMap)

apply Void (Function q1 q2) typeMap             = error "TIPO NO VALIDO"

apply Void (Const name2 q) typeMap              = error "TIPO NO VALIDO"

apply Void (Var name2 q) typeMap                = error "TIPO NO VALIDO"

apply Void Void typeMap                         = (Void, typeMap)


-- function to make the needed substitutions in the resulting type after
-- evaluating
subst :: Type -> Str2Type -> Type
subst Void typeMap           = Void
subst (Const name p) typeMap = Const name (subst p typeMap)
subst (Var name p) typeMap   = 
    if not $ M.member name typeMap then
        Var name (subst p typeMap)
    else do
        case typeMap M.! name of
            Function q  _ -> Function (subst q typeMap) (subst p typeMap)
            Const name2 _ -> Const name2 (subst p typeMap)
            Var name2   _ -> Var name2 (subst p typeMap)
subst (Function p q) typeMap = Function (subst p typeMap) (subst q typeMap)