module GeneralUtils where

fancyShowList :: Show a => [[a]] -> String
fancyShowList [] = ""
fancyShowList (x : xs) = show x ++ "\n" ++ fancyShowList xs


