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

data ProgramData = ProgramData Bool TodoList String

type ProgramState a = StateT ProgramData IO a 

instance FromJSON TodoItem where
	parseJSON (Object v) = TodoItem <$>
		v .: "date" <*>
		v .: "message"
	parseJSON _ = mzero

instance ToJSON TodoItem where
	toJSON (TodoItem date message) = object ["date" .= date,
						"message" .= message]

loadList :: Text -> ProgramState ()
loadList fp = StateT $ \ps -> do
		contents <- B.readFile . unpack $ fp
		case (decode contents :: Maybe TodoList) of
			Just list -> return ((),ProgramData True list (unpack fp))
			Nothing -> S.putStrLn "Syntax Error in JSON file" >> return ((),ps)

saveList :: Text -> ProgramState ()
saveList "" = StateT $ \(ProgramData _ l fp) -> do
		B.writeFile fp . encode $ l
		return ((),ProgramData True l fp)

saveList fp = StateT $ \(ProgramData _ l _) -> do
		B.writeFile fps . encode $ l
		return ((),ProgramData True l fps)
        where fps = unpack fp 

addItem :: TodoItem -> ProgramState ()
addItem x = StateT $ \(ProgramData _ l fp) -> return ((),ProgramData False (x:l) fp)

removeItem :: Maybe Int -> ProgramState ()
removeItem Nothing  = lift $ putStrLn "Invalid index entered"
removeItem (Just n) = StateT $ \(ProgramData _ l fp) -> return ((),ProgramData False (removeAt n l) fp)

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = (take n xs) ++ (drop (n + 1) xs)

