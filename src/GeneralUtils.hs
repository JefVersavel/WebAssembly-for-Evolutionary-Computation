module GeneralUtils where

-- | Helperfunction to print out lists in a nicer way
fancyShowList :: Show a => [[a]] -> String
fancyShowList [] = ""
fancyShowList (x : xs) = show x ++ "\n" ++ fancyShowList xs

-- | Delete the element at the given index, if the index is out of bounds the original list is returned.
deleteIndex :: [a] -> Int -> [a]
deleteIndex list n | n < 0 = list
deleteIndex [] _ = []
deleteIndex (_ : xs) 0 = xs
deleteIndex (_ : xs) n = deleteIndex xs $ n -1
