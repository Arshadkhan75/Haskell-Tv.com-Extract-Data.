module DBModule where
import ParseModule
import HTTPModule
import Database.HDBC
import Database.HDBC.Sqlite3

createDB :: IO ()
createDB = do conn <- connectSqlite3 "shows.db"
              run conn "CREATE TABLE shows (rank TEXT, title TEXT, channel TEXT)" []
	      run conn "CREATE TABLE twitter (score TEXT, title TEXT)" []
              commit conn

--storeShows :: [String] -> [String] -> IO ()
--storeShows [] = return ()
--storeShows :: [String] -> [String] -> IO
storeShows xs ys =
     do conn <- connectSqlite3 "shows.db"
        stmt <- prepare conn "INSERT INTO shows (rank, title, channel) VALUES (?,?,?)"
        executeMany stmt (map myToSql xs)
	stmt <- prepare conn "INSERT INTO twitter (score, title) VALUES (?,?)"
        executeMany stmt (map myToSql ys)
        commit conn  

myToSql :: [String] -> [SqlValue] 
myToSql xs = map toSql xs      

printAll :: IO ()
printAll = do content <- getData
              mapM_ print content

getData :: IO [String]
getData = do conn <- connectSqlite3 "shows.db"
             res <- quickQuery' conn "SELECT * FROM shows" []
             return $ map fromSql (concat res)

getTwitter :: IO [String]
getTwitter = do conn <- connectSqlite3 "shows.db"
                res <- quickQuery' conn "SELECT * FROM twitter" []
                return $ map fromSql (concat res)

dispChannel :: String -> IO [String]
dispChannel name = do conn <- connectSqlite3 "shows.db"
		      res <- quickQuery' conn "SELECT title from shows where channel=?" [toSql name]
		      return $ map fromSql (concat res)

dispRank :: String -> IO [String]
dispRank rank = do conn <- connectSqlite3 "shows.db"
		   res <- quickQuery' conn "SELECT title, channel from shows where rank=?" [toSql rank]
		   return $ map fromSql (concat res)

dispShow :: String -> IO [String]
dispShow title = do conn <- connectSqlite3 "shows.db"
		    res <- quickQuery' conn "SELECT rank, channel from shows where title=?" [toSql title]
		    return $ map fromSql (concat res)

             

