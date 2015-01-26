module Main where

import Main.Model
import Main.View
import Main.Controller

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe

setup :: MaybeT IO TodoList
setup = return . TodoList $ []

program :: TodoList -> MaybeT IO TodoList
program tdl = (lift requestChoice) 
	>>= (\ x -> parseChoice x tdl) 
	>>= printList
	>>= program

runProgram :: IO (Maybe TodoList)
runProgram = runMaybeT $ setup >>= program

main = runProgram
