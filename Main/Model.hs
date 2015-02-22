module Main.Model(
    ProgramState (..)
	, TodoList (..)
	, TodoItem (..)
    , Date (..)
	, modifyList
    , removeAt
	) where

import Util.Date (Date)

import Data.Text (Text,unpack)

data TodoItem = TodoItem { 
	date :: Date
	, message :: Text
}

instance Show TodoItem where
	show (TodoItem d m) = "Date: " ++ show d
		++ "\t\t\tMessage: " ++ unpack m ++ "\n"

type TodoList = [TodoItem]

data ProgramState = ProgramState {
    saved :: Bool 
    , list :: TodoList 
    , filepath :: String
}

instance Show ProgramState where
    show ps = "\nFile: " ++ filepath ps
        ++ (if saved ps then "" else "*") ++ "\n\n" 
        ++ printTodos ps
        where printTodos ps = foldl (\acc x -> acc ++ show x) "" $ list ps

modifyList :: (TodoList -> TodoList) -> ProgramState -> ProgramState
modifyList f ps = ps { list = f (list ps) }

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = take n xs ++ drop (n + 1) xs
