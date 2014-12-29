-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (23/24 Oct)

import Data.List
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString firstString secondString
						| length firstString == length secondString = and [x == y | (x,y) <- zip firstString secondString]
						| otherwise = False


-- 2.
prefix :: String -> String -> Bool
prefix firstString secondString = and [toLower x == toLower y | (x,y) <- zip firstString secondString, isAscii x && isAscii y]

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str subStr = isPrefixOf (toLowerStr subStr) (toLowerStr str) || contains (tail (toLowerStr str)) (toLowerStr subStr)
						where toLowerStr text = map toLower text

prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined


-- 4.
takeUntil :: String -> String -> String
takeUntil "" text = text
takeUntil pivotEnd [] = []
takeUntil _ [x] = [x]
takeUntil stringBarrier (head:tail)
				| not (prefix stringBarrier (head:tail)) = head : takeUntil stringBarrier tail 
				| otherwise = []

dropUntil :: String -> String -> String
dropUntil stringBarrier [] = []
dropUntil stringBarrier (x:xs)
				| prefix stringBarrier (x:xs) = drop (length stringBarrier) (x:xs)
				| otherwise = dropUntil stringBarrier xs 

-- 5.
split :: String -> String -> [String]
split [] _ = error "The separator is empty"
split _ [] = []
split separator text = (takeUntil separator text) : (split separator (dropUntil separator text))



reconstruct :: String -> [String] -> String
reconstruct [] _ = error "The separator is empty"
reconstruct _ [] = []
reconstruct separator listOfStrings = ((head listOfStrings) ++ separator) ++ (reconstruct separator (tail listOfStrings))

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML [] = []
linksFromHTML htmlString
				| contains htmlString "<a href=\"" = takeUntil "<a href=\"" (dropUntil "<a href=\"" htmlString) : linksFromHTML (dropUntil "<a href=\"" htmlString)
				| otherwise = []

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks

-- 7. -- Taка, prefix, какво е това prefix "Bc" "bCDE" => True; "mailto:", така започва е-mail, mhm, v HTML link-ове
-- Добре, намери ми
takeEmails :: [Link] -> [Link]
takeEmails listOfLinks = [x | x <- listOfLinks, prefix "mailto:" x]


-- 8.
-- <a href="ourLink">nameOfOurLink</a>
link2pair :: Link -> (Name, Email)
link2pair link = undefined


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML  = undefined

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail  = undefined


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = undefined


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]