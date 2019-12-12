module Main where

import Prelude ((<>),(==))
import Data.Function
import Data.List
import Data.Maybe

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
    { street :: String
    , city   :: String
    , state  :: String
    }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address


insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry books = Cons entry books

findEntry :: String -> AddressBook -> Maybe Entry
findEntry firstName books  = head $ filter filterEntry books
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == firstName

