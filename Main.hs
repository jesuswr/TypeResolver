module Main where

import Type
import qualified Data.Map as M
import qualified Data.Char as C

main :: IO()
main = do
    putStrLn "BIENVENIDO"
    runMainLoop M.empty


runMainLoop :: Str2Type -> IO()
runMainLoop typeMap = do
    putStrLn "\nQUE DESEA HACER?"
    tmp <- getLine
    let (command, rest) = span (/= ' ') tmp
    case command of
        "DEF" -> do
            let (name, exp) = span (/= ' ') (tail rest)
            let newTypeMap = M.insert name (convert exp) typeMap
            putStrLn $ "SE DEFINIO '" ++ name ++ "' CON TIPO: " ++ show (convert exp)
            runMainLoop newTypeMap
        "TIPO" -> do
            typeQuery  (tail rest) typeMap
            runMainLoop typeMap
        "SALIR" -> return()
        _ -> do
            putStrLn "COMANDO DESCONOCIDO"
            runMainLoop typeMap


typeQuery :: String -> Str2Type -> IO()
typeQuery exp typeMap = do
    if not $ containsIds typeMap exp then
        putStrLn "ERROR, UN NOMBRE DADO NO HA SIDO DEFINIDO"
    else do
        let Atomic e = solve (transform exp typeMap)
        putStrLn $ show e