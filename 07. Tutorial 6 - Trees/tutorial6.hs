-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 6/7 November

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen detailsOfItems = maximum [length (fst (snd x)) | x <- detailsOfItems]
--longestProductLen x = maximum [length y | (_,(y,_)) <- x]

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (barcode, (productName, unit)) = barcode ++ "..." ++ productName ++ (replicate (n - (length productName) + 3) '.') ++ unit
-- Защо 2 = . (една точка). Сега го напиши.
-- Принципно това е заради факта, че по условие максималната дължина на името (първия параметър) е винаги
-- правилен, т.е. Prodict никога няма да има повече символи от зададените в първия параметър
-- В случая с примера:  formatLine 2 ("9780201342758", ("Name", "Book"))
-- length Prodict = 4
-- 2(максимална дължана на името(подадена като първи параметър)) - 4(реална дължина на името)
-- + 3(точките, които имаме по подразбиране) = 1(реалния брой точки, които се залепят за стринга) => получаваме една точка
showCatalogue :: Catalogue -> String
showCatalogue cataloge = unlines [formatLine catalogeMaxProductLength x | x <- (toList cataloge)]
					where catalogeMaxProductLength = longestProductLen (toList cataloge)
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x 

getJustValue :: Maybe a -> a
getJustValue Nothing = error "Cant get nothing"
getJustValue (Just a) = a

catMaybes :: [Maybe a] -> [a]
catMaybes [Nothing] = []
catMaybes input = [getJustValue x | x <- input]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems itemsBarcodes cataloge = catMaybes [(get x cataloge) | x <- itemsBarcodes]






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)