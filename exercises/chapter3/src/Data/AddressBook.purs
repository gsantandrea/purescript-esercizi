module Main where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Control.Monad.Eff.Console (logShow,CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
printEntry ::     String
                  -> String
                     -> List
                          { firstName :: String
                          , lastName :: String
                          , address :: { street :: String
                                       , city :: String
                                       , state :: String
                                       }
                          }
                        -> Maybe String
printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)



findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry2 = entry2.address.street == street

entry1::      { firstName :: String
              , lastName :: String
              , address :: { street :: String
                           , city :: String
                           , state :: String
                           }
              }
entry1 = { firstName: "John", lastName: "Smith", address: address }


address :: { street :: String
      , city :: String
      , state :: String
      }
address = { street: "123 Fake St.", city: "Faketown", state: "CA" }


book1 :: List { firstName :: String
                 , lastName :: String
                 , address :: { street :: String
                              , city :: String
                              , state :: String
                              }
                 }
book1 = insertEntry entry1 emptyBook




printEntryByStreet :: String -> AddressBook ->  Maybe String
printEntryByStreet street book = map showEntry (findEntryByStreet  street  book)

s:: Maybe String
s = printEntryByStreet "123 Fake St." book1


main :: forall t.  Eff ( console :: CONSOLE | t ) Unit
main = logShow s
