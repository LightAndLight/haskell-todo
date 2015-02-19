module Main.View(printList) where

import Main.Model
import Control.Monad.State.Lazy
import System.IO
import Data.Text(unpack)

instance Show ProgramData where
    show (ProgramData s l fp) = 
        "\nFile: " ++ fp ++ (if s then "" else "*") ++ "\n\n"
        ++ (foldl (\acc x -> acc ++ show x) "" l)

instance Show TodoItem where
	show (TodoItem d m) = "Date: " ++ unpack d 
		++ "\t\t\tMessage: " ++ unpack m ++ "\n"

printList :: ProgramState ()
printList = StateT $ \ps -> do
    print ps
    return ((),ps) 
