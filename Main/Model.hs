module Main.Model(
    ProgramState(..)
	, TodoList(..)
	, TodoItem(..)
	, modifyList
    , removeAt
	) where

import Data.Text (Text)

data TodoItem = TodoItem { 
	date :: Text
	, message :: Text
	}

type TodoList = [TodoItem]

data ProgramState = ProgramState {
    saved :: Bool 
    , list :: TodoList 
    , filepath :: String
}

modifyList :: (TodoList -> TodoList) -> ProgramState -> ProgramState
modifyList f ps = ps { list = f (list ps) }

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = (take n xs) ++ (drop (n + 1) xs)
