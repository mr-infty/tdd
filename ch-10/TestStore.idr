module TestStore

import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner",  1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty

listItems : DataStore schema -> List (SchemaType schema)
listItems store with (storeView store)
  listItems store | SNil = []
  listItems (addToStore value store) | (SAdd rec) = value :: listItems store | rec

filterKeys : (test : SchemaType val_schema -> Bool) ->
             DataStore (SString .+. val_schema) -> List String
filterKeys test store with (storeView store)
  filterKeys test store | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec) = if test value then key :: (filterKeys test store | rec)
                                                                        else filterKeys test store | rec
