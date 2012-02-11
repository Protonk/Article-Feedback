# Mediawiki requires an informative user agent. Yours should be different

options(HTTPUserAgent="Category Enumeration by User:Protonk on en.wp, R 2.14.1")

# This is NOT rjson 0.2.6. I have changed it so the internal call to readLines doesn't toss warngings
# Also RJSONIO will mangle special characters like —, don't use it

library(rjson)

# Needlessly complicated. I'll pare it down over time. 

fetchCompleteCat <- function(category, namespace = 0, limit = 500, continue = NULL) {
	fetchCategoryURL <- function(category, namespace, limit, continue) {
	  url.start <-paste(
				  "http://en.wikipedia.org/w/api.php?", 
				  "action=query",
				  "list=categorymembers",
				  paste("cmtitle", "=", category, sep = ""),
				  paste("cmnamespace", "=", namespace, sep = ""),
				  paste("cmlimit", "=", limit, sep = ""),
				  "cmtype=page",
				  "cmprop=title",
				  "format=json",
				  sep = "&")
	  if (is.null(continue)) url.gen <- url.start
	  else url.gen <- paste(url.start, paste("cmcontinue", "=", continue, sep = ""), sep = "&")
	  url.gen
	}
	cat.list.init <- fromJSON(file = fetchCategoryURL(category, namespace, limit, continue))
	cat.unform <- unlist(cat.list.init$query$categorymembers)[names(unlist(cat.list.init$query$categorymembers)) == "title"]
	while (!is.null(cat.list.init$`query-continue`)) {
		cat.list.init <- fromJSON(file = fetchCategoryURL(category, namespace, limit, continue = unlist(cat.list.init$`query-continue`)[[1]]))
		cat.unform <- append(cat.unform, unlist(cat.list.init$query$categorymembers)[names(unlist(cat.list.init$query$categorymembers)) == "title"])
		}
	cat.final <- unname(cat.unform)
	cat.list.init
	return(cat.final)
}