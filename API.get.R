
library(RJSONIO)
library(RCurl)

options(RCurlOptions = list(verbose = FALSE, useragent = "Category Enumeration by User:Protonk on en.wp, R 2.14.1 using RCurl"))

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


    