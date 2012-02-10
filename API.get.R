options(HTTPUserAgent="R (2.14.1 Category Info Script Contact-User:Protonk)")

library(RJSONIO)

short.art <- as.character(read.delim("/Users/protonk/r/shortarticles.txt", header = FALSE, sep = "\n", quote = "")[,1])

fetchCategoryURL <- function(category, continue = NULL, namespace = 0) {
  url.start <-paste(
  "http://en.wikipedia.org/w/api.php?", 
  "action=query",
  "list=categorymembers",
  paste("cmtitle", "=", category, sep = ""),
  paste("cmnamespace", "=", namespace, sep = ""),
  "cmtype=page",
  "cmprop=title",
  "cmlimit=500",
  "format=json",
  sep = "&")
  if (is.null(continue)) url.gen <- url.start
  else url.gen <- paste(url.start,
                          "cmcontinue",
                          "=",
                          cmcontinue.string,
                          sep = "")
}


  

readLines(con = url(cat.url), warn = FALSE)
    