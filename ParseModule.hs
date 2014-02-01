module ParseModule where

parseShows :: String  -> [[String]]
parseShows [] = []
parseShows ('p':'o':'p':'u':'l':'a':'r':'_':'s':xs) = parseList (dropTillUl xs)
parseShows (_:xs) = parseShows xs 

dropTillUl :: String -> String
dropTillUl [] = []
dropTillUl ('<':'u':'l':xs) = tail(dropWhile notRight xs)
dropTillUl (_:xs) = dropTillUl xs

dropTillLi :: String -> String
dropTillLi [] = []
dropTillLi ('<':'/':'l':'i':'>':xs) = xs
dropTillLi (_:xs) = dropTillLi xs


notRight :: Char -> Bool
notRight c = not(c == '>')
notLeft c = not(c=='<')

removespaces :: String -> String 
removespaces [] = [] 
removespaces ('\t':xs) = removespaces xs
removespaces ('\n':xs) = removespaces xs
removespaces (x:xs) = x:removespaces xs

parseList :: String  -> [[String]]
parseList [] = []
parseList ('<':'l':'i':xs) = [parseCounter xs,parseTitle xs,parseChannel xs]:(parseList (dropTillLi xs))
parseList ('<':'/':'u':'l':xs) = []
parseList (_:xs) = parseList xs

parseCounter :: String -> String
parseCounter [] = []
parseCounter ('c':'o':'u':'n':'t':'e':'r':xs) = takeWhile notLeft (tail(dropWhile notRight xs))
parseCounter (_:xs) = parseCounter xs

parseChannel :: String -> String 
parseChannel [] = []
parseChannel ('r':'i':'g':'h':'t':'_':'i':xs) = removespaces (takeWhile notLeft (tail(dropWhile notRight xs)))
parseChannel (_:xs) = parseChannel xs

parseTitle :: String -> String
parseTitle [] = []
parseTitle ('t':'i':'t':'l':'e':xs) = takeWhile notLeft (tail(dropWhile notRight (tail(dropWhile notRight xs))))
parseTitle (_:xs) = parseTitle xs

parseTwitter :: String -> [[String]]
parseTwitter [] = []
parseTwitter ('t':'w':'i':'t':'t':'e':'r':'_':'p':xs) = parseTShow xs
parseTwitter (_:xs) = parseTwitter xs 

parseTShow :: String -> [[String]]
parseTShow [] = []
parseTShow ('c':'l':'a':'s':'s':'=':'"':'i':'t':'e':'m':xs) = [parseScore xs, removespaces(parseTitle xs)]:(parseTShow (dropTillDiv xs))
parseTShow ('c':'l':'a':'s':'s':'=':'_':'c':'l':'e':'a':'r':'f':'i':'x':xs) = []
parseTShow (_:xs) = parseTShow xs

dropTillDiv :: String -> String
dropTillDiv [] = []
dropTillDiv ('<':'/':'d':'i':'v':'>':xs) = xs
dropTillDiv (_:xs) = dropTillDiv xs

parseScore :: String -> String
parseScore [] = []
parseScore ('s':'c':'o':'r':'e':xs) = removespaces(takeWhile notLeft (tail(dropWhile notRight xs)))
parseScore (_:xs) = parseScore xs
