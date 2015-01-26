{-# LANGUAGE OverloadedStrings #-}

module Main.Model(
	TodoList(..)
	, TodoItem(..)
	, loadList
	, saveList
	, addItem
	, removeItem
	) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text,unpack)
import qualified System.IO as S

type TodoList = [TodoItem]

data TodoItem = TodoItem { 
	date :: Text
	, message :: Text
	}

instance FromJSON TodoItem where
	parseJSON (Object v) = TodoItem <$>
		v .: "date" <*>
		v .: "message"
	parseJSON _ = mzero

instance ToJSON TodoItem where
	toJSON (TodoItem date message) = object ["date" .= date,
						"message" .= message]

instance Show TodoItem where
	show (TodoItem d m) = "Date: " ++ unpack d 
		++ "\t\t\tMessage: " ++ unpack m

loadList :: Text -> StateT TodoList IO ()
loadList fp = StateT load
	where load = \xs -> do
		contents <- B.readFile . unpack $ fp
		case (decode contents :: Maybe TodoList) of
			Just list -> return ((),list)
			Nothing -> S.putStrLn "Syntax Error in JSON file" >> return ((),[])

saveList :: Text -> StateT TodoList IO ()
saveList fp = StateT save
	where save = \xs -> do
		B.writeFile (unpack fp) (encode xs)
		return ((),xs)

addItem :: TodoItem -> StateT TodoList IO ()
addItem x = modify (x:)

removeItem :: Maybe Int -> StateT TodoList IO ()
removeItem Nothing  = lift $ putStrLn "Invalid index entered"
removeItem (Just n) = modify (removeAt n)

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = (take n xs) ++ (drop (n + 1) xs)

