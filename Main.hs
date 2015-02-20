module Main where

import Main.Model
import Main.Controller

program :: ProgramState -> IO ProgramState
program ps = do
    command <- getCommand
    ps' <- runCommand command ps
    print ps'
    program ps'

initial :: ProgramState
initial = ProgramState { 
    saved = True 
    , list = []
    , filepath = ""
}

main = program initial
