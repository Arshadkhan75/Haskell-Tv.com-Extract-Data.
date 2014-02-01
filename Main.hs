import DBModule
import HTTPModule
import ParseModule
import System.Environment

main = do args <- getArgs
          case args of
             ["create"] -> createDB	
             ["saved"] -> printAll
	     ["twitter"] ->
		do content <- getTwitter
	           mapM_ print content
             ["show"] ->
             	do urlText <- downloadURL "http://www.tv.com"
		   let shows = parseShows urlText
             	   print shows
		   let twitter = parseTwitter urlText
		   print twitter
             ["store"] ->
             	do urlText <- downloadURL "http://www.tv.com"
             	   let shows = parseShows urlText
		   let twitter = parseTwitter urlText
             	   storeShows shows twitter
	     ["channel", name] ->
		do urls <- dispChannel name
                   mapM_ print urls
	     ["rank", rank] ->
		do urls <- dispRank rank
                   mapM_ print urls
	     ["display", title] ->
		do urls <- dispShow title
                   mapM_ print urls
             _ -> syntaxError

syntaxError = putStrLn 
  "Usage: Crawler command [args]\n\
  \\n\
  \ create           Create database shows.db\n\
  \ show             Shows contents of tv.com\n\
  \ saved            List content of shows table\n\
  \ twitter          List content of twitter score table\n\
  \ store            Gather Shows details and store in database\n\
  \ channel name     Search TV shows by giving channel name\n\
  \ display title    Display TV show details using title\n\
  \ rank value       Display TV show details using ranking\n"
