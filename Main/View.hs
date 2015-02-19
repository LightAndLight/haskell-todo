module Main.View where

import Main.Model
import System.IO
import Data.Text(unpack)

instance Show ProgramData where
    show pd =   
        case pd of
            ProgramData s l fp Nothing -> "\nFile: " ++ fp 
                ++ (if s then "" else "*") ++ "\n\n"
                ++ (foldl (\acc x -> acc ++ show x) "" l)

            ProgramData _ _ _ (Just error) -> "ERROR: " ++ error

instance Show TodoItem where
	show (TodoItem d m) = "Date: " ++ unpack d 
		++ "\t\t\tMessage: " ++ unpack m ++ "\n"
