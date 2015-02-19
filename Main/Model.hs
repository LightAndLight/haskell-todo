module Main.Model(
    ProgramData(..)
	, TodoList(..)
	, TodoItem(..)
	, addItem
	, removeItem
	) where

import Data.Text (Text)

data TodoItem = TodoItem { 
	date :: Text
	, message :: Text
	}

type TodoList = [TodoItem]

data ProgramData = ProgramData {
    saved :: Bool 
    , list :: TodoList 
    , filepath :: String
    , errorMsg :: Maybe String
}

addItem :: TodoItem -> ProgramData -> ProgramData
addItem tdi pd = pd { 
    saved = False
    , list = tdi:(list pd)
    , errorMsg = Nothing
}

removeItem :: Int -> ProgramData -> ProgramData
removeItem n pd 
    | n < 0 = pd { errorMsg = Just "Invalid index" }
    | otherwise = pd { 
        saved = False 
        , list = removeAt n (list pd)
        , errorMsg = Nothing
    }

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = (take n xs) ++ (drop (n + 1) xs)
