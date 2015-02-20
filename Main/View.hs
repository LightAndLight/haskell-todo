module Main.View where

import Main.Model
import System.IO
import Data.Text(unpack)

instance Show ProgramState where
    show ps = "\nFile: " ++ (filepath ps) 
        ++ (if saved ps then "" else "*") ++ "\n\n"
        ++ printTodos ps
        where printTodos ps = foldl (\acc x -> acc ++ show x) "" $ list ps

instance Show TodoItem where
	show (TodoItem d m) = "Date: " ++ unpack d 
		++ "\t\t\tMessage: " ++ unpack m ++ "\n"
