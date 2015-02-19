module Main where

import Main.Model
import Main.View
import Main.Controller

program :: ProgramData -> IO ProgramData
program pd = do
    putStrLn "Command: "
    command <- getLine
    pd' <- runCommand command pd
    print pd'
    program pd'

initial :: ProgramData
initial = ProgramData { 
    saved = True 
    , list = []
    , filepath = ""
    , errorMsg = Nothing
}

main = program initial
