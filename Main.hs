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
        "SALIR" -> return()
        _ -> do
            putStrLn "COMANDO DESCONOCIDO"
            runMainLoop typeMap