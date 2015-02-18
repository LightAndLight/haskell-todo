{-# LANGUAGE OverloadedStrings #-}

module Main.Model(
    ProgramData(..)
    , ProgramState(..)
	, TodoList(..)
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

data ProgramData b l = ProgramData {
    saved :: Bool
    , list :: TodoList
    }

type ProgramState = StateT (ProgramData Bool TodoList) IO ()

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

loadList :: Text -> ProgramState
loadList fp = StateT $ \ps -> do
		contents <- B.readFile . unpack $ fp
		case (decode contents :: Maybe TodoList) of
			Just list -> return ((),ProgramData True list)
			Nothing -> S.putStrLn "Syntax Error in JSON file" >> return ((),ps)

saveList :: Text -> ProgramState
saveList fp = StateT $ \(ProgramData _ l) -> do
		B.writeFile (unpack fp) . encode $ l
		return ((),ProgramData True l)

addItem :: TodoItem -> ProgramState
addItem x = StateT $ \(ProgramData _ l) -> return ((),ProgramData False (x:l))

removeItem :: Maybe Int -> ProgramState
removeItem Nothing  = lift $ putStrLn "Invalid index entered"
removeItem (Just n) = StateT $ \(ProgramData _ l) -> return ((),ProgramData False (removeAt n l))

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = (take n xs) ++ (drop (n + 1) xs)

