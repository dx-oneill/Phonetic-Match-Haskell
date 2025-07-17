module Submission where
import Data.Char (toUpper)


-- Filters out any non letter characters
discardNonLetters :: String -> String
discardNonLetters = filter (\c -> c `elem` ['a'..'z'] ++ ['A'..'Z'])


-- Changes all letters to uppercase so that the case doesn't affect it
toUpperCase :: String -> String
toUpperCase = map toUpper



-- Gets rid of all the selected letters 
discardLetters :: String -> String
discardLetters [] = []
discardLetters (x:providedNames) = 
  if x `elem` "AEIHOUWY" 
    then discardLetters providedNames
    else  x : discardLetters providedNames


-- shifts the string so that the first letter is unaffected by discardLetters
shiftString :: String -> String
shiftString = shiftString'
  where
    shiftString' [] = []
    shiftString' (x:providedNames) = x : discardLetters providedNames


-- Groups the letters together so they are seen as equivalent
equivalentLetters :: String -> String
equivalentLetters [] = []
equivalentLetters (x:providedNames)
  | x `elem` "AEIOU" = 'A' : equivalentLetters providedNames
  | x `elem` "CGJKQSXYZ" = 'C' : equivalentLetters providedNames
  | x `elem` "BFVPW" = 'B' : equivalentLetters providedNames
  | x `elem` "DT" = 'D' : equivalentLetters providedNames
  | x `elem` "MN" = 'M' : equivalentLetters providedNames
  | otherwise = x : equivalentLetters providedNames


-- gets rid of any consecutive equivalent letters
discardConsecutive :: String -> String
discardConsecutive [] = []
discardConsecutive [x] = [x]
discardConsecutive (x:y:providedNames) =
  if x == y 
    then discardConsecutive (y:providedNames)
    else x : discardConsecutive (y:providedNames)

-- function to implement all the rules, on a name, in the correct order 
filterName :: String -> String
filterName = discardConsecutive . equivalentLetters .
  shiftString . toUpperCase . discardNonLetters


-- creates the list of names with their phonetic matches
matchNames :: [String] -> [String] -> [(String, [String])]
matchNames providedNames allNames =
  let filteredProvidedNames = map (\name -> (name, filterName name)) providedNames
      filteredAllNames = map (\name -> (name, filterName name)) allNames 
      -- maps the names paired with its filtered version
      grouped = [(provided, [n | (n, pn) <- filteredAllNames, pn == pp]) 
       | (provided, pp) <- filteredProvidedNames] 
       -- extracts the filtered names and checks their equivalence and creates pairs   
  in filter (\(provided, matches) -> not (null matches)) grouped 
  -- filters out all pairs without at least one match  


-- test
providedNames :: [String]
providedNames = ["Jones", "Smith", "Oscar"]

allNames :: [String]
allNames = ["$Oscar", "Oscar", "scar", "Smith", "Smyth", "Smythe", "Smid", "Schmidt", "Smithers",
 "Jonas", "Johns", "Johnson", "Macdonald", "Nest O'Malett", "Ericsson", "Erikson", "Saunas", "Van Damme"]



