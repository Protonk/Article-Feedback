# load with:
#            source('/PATH_TO_FILE/feed.import.R')
# Adding the exact file path, of course. 


## Function to grab local data or remote. Defaults to local
## Your local path will be different

buildFeedDf <- function(remote = FALSE) {
  grabWikiFeed <- function() {
    aap.url <- "http://dumps.wikimedia.org/other/articlefeedback/aa_combined-20110321.csv.gz"
    temp <- tempfile()
    download.file(aap.url, temp)
    aap.file <- read.csv(gzfile(temp), as.is = TRUE)
    unlink(temp)
    return(aap.file)
  }
  if (remote) grabWikiFeed()
  else read.csv("/Users/protonk/R/Dropbox/Article-Feedback/Articles/afdump.csv", as.is= TRUE)
}

# This is NOT rjson 0.2.6. I have changed it so the internal call to readLines doesn't toss warngings
# Also RJSONIO will mangle special characters like —, don't use it

library(rjson)

# Enumerate categories from the Mediawiki API

fetchCompleteCat <- function(category, namespace = 0) {
  # Mediawiki requires an informative user agent. Yours should be different
  options(HTTPUserAgent="Category Enumeration by User:Protonk on en.wp, R 2.14.1")
  fetchCategoryURL <- function(category, namespace, continue = NULL) {
    url.start <-paste(
      "http://en.wikipedia.org/w/api.php?", 
      "action=query",
      "list=categorymembers",
      paste("cmtitle", "=", category, sep = ""),
      paste("cmnamespace", "=", namespace, sep = ""),
      "cmlimit=500",
      "cmtype=page",
      "cmprop=title",
      "format=json",
      sep = "&")
    if (is.null(continue)) url.gen <- url.start
    else url.gen <- paste(url.start, paste("cmcontinue", "=", continue, sep = ""), sep = "&")
    url.gen
  }
  cat.list.init <- fromJSON(file = fetchCategoryURL(category, namespace))
  cat.unform <- unlist(cat.list.init$query$categorymembers)[names(unlist(cat.list.init$query$categorymembers)) == "title"]
  while (!is.null(cat.list.init$`query-continue`)) {
    cat.list.init <- fromJSON(file = fetchCategoryURL(category, namespace, continue = unlist(cat.list.init$`query-continue`)[[1]]))
    cat.unform <- append(cat.unform, unlist(cat.list.init$query$categorymembers)[names(unlist(cat.list.init$query$categorymembers)) == "title"])
  }
  cat.final <- unname(cat.unform)
  closeAllConnections()
  return(cat.final)
}


# Generate character vectors for "rated" articles and use them to create
# factors in the feedback dataframe. Local or remote. 

applyFactors <- function(remote = FALSE) {
  local.articles <- c("/Users/protonk/r/Dropbox/Article-Feedback/Articles/formerGA.txt", 
                      "/Users/protonk/r/Dropbox/Article-Feedback/Articles/formerFA.txt",
                      "/Users/protonk/r/Dropbox/Article-Feedback/Articles/fmrGAnom.txt", 
                      "/Users/protonk/r/Dropbox/Article-Feedback/Articles/GA.txt",
                      "/Users/protonk/r/Dropbox/Article-Feedback/Articles/FL.txt", 
                      "/Users/protonk/r/Dropbox/Article-Feedback/Articles/FA.txt")
  remote.articles <- c("Category:Delisted_good_articles",
                       "Category:Wikipedia_former_featured_articles",
                       "Category:Former_good_article_nominees",
                       "Category:Good_articles",
                       "Category:Featured_lists",
                       "Category:Featured_articles")
  charvec.names <- c("formerGA.list",
                     "formerFA.list",
                     "nomGA.list",
                     "GA.list",
                     "FL.list",
                     "FA.list")

# This is the function that grabs all the categories (calling fetchCompleteCat)
  
  remoteFactors <- function() {
    namespace <- numeric()
    list.out <- vector("list", 6)
    names(list.out) <- charvec.names
    for (i in seq_along(remote.articles)) {
      # First three are actually talk page categories
      # in any case, restricting by namespace removes some erroneously tagged pages
      if (i <= 3) namespace <- 1 
      else { 
        namespace <- 0
      }
      list.out[[i]] <- fetchCompleteCat(category = remote.articles[i], namespace = namespace)
      if (i <= 3) {
        #Some have the category transcluded on an archive page
        #The rest need "Talk:" stripped out 
        list.out[[i]] <- gsub("(/Archive)(.*)", replacement = "", list.out[[i]])
        list.out[[i]] <- gsub(pattern = "Talk:", replacement = "", list.out[[i]])
      }
    }
    return(list.out)
  }
  if (remote) {
    comb.list <- remoteFactors()
    # Builds 6 individual character vectors. Easiest for assignment to the DF
    list2env(comb.list, envir = parent.frame())
  }
  else {
    for (x in seq_along(local.articles)) {
      assign(charvec.names[x], as.character(read.delim(local.articles[x], sep = "\n", header = FALSE, as.is= TRUE)[,1]))
    }
  }
  feed.df[feed.df[,"title"] %in% formerGA.list, "Assessment"] <<- "Former Good Article"
  feed.df[feed.df[,"title"] %in% formerFA.list, "Assessment"] <<- "Former Featured Article"
  feed.df[feed.df[,"title"] %in% nomGA.list, "Assessment"] <<- "Good Article Nominee"
  feed.df[feed.df[,"title"] %in% GA.list, "Assessment"] <<- "Good Article"
  feed.df[feed.df[,"title"] %in% FL.list, "Assessment"] <<- "Featured List"
  feed.df[feed.df[,"title"] %in% FA.list, "Assessment"] <<- "Featured Article"
}

# Drops namespace and page id
# in the future, page id will be returned to the df but for now lists of pertinent articles are generated by 
# article title. 

feed.df <- buildFeedDf()[, c(3:12)]
names(feed.df) <- c("title", "length", "total_wellsourced", "count_wellsourced", "total_neutral", "count_neutral", "total_complete", "count_complete", "total_readable", "count_readable")

# Compute rating/count sums

feed.df$sum_ratings <- feed.df[, "total_wellsourced"] + feed.df[, "total_neutral"] + feed.df[, "total_complete"] + feed.df[, "total_readable"]
feed.df$sum_count <- feed.df[, "count_wellsourced"] + feed.df[, "count_neutral"] + feed.df[, "count_complete"] + feed.df[, "count_readable"]
feed.df$rating_avg <- feed.df[, "sum_ratings"] / feed.df[, "sum_count"]

# Create factor variable for Project Assessment and assign it with applyFactors()

feed.df$Assessment<- factor(x = c("Unassessed"), levels=c("Unassessed", "Former Good Article", "Former Featured Article", "Good Article Nominee",  "Good Article", "Featured List", "Featured Article"))
# Titles grabbed from the mediawiki API will not have the underscore. 
feed.df[, "title"] <- gsub(pattern = "_",replacement = " ", feed.df[, "title"])
applyFactors()


# Drop rows with:
##  50 length or less. < 0 length are erroneous values, 0-50 tend to be redirects
###  In the future I may match these to their redirect targets, but it isn't worth the effort now
##  Extreme erroneous values
##  Nonsensical averages (> 5)
##  0 ratings

unclean.rows <- function() {
	extreme.out<- numeric()
  # Append is used here because the for loop is walking columns (which may have variable #'s of extreme values)
	for (i in 3:11) {
		extreme.out <- append(extreme.out, which(feed.df[,i] == 4294967295), after = length(extreme.out))
		}
	feed.oor <- c(which(feed.df[,"length"] <= 50), which(feed.df[,"sum_count"] == 0), which(feed.df[,"sum_ratings"]/feed.df[,"sum_count"] > 5))
	extreme.out <- c(extreme.out, feed.oor)
	return(unique(extreme.out))
	}
feed.df <- feed.df[-unclean.rows(), ]

# Correlation matrix for the specific categories. Note that complete.cases() is used to drop 
# any row w/ NA values which we will have due to div by zero. 

precorMatrix <- function() {
  pre.cor.mat <- matrix(0, nrow(feed.df), 5)
  colnames(pre.cor.mat) <- c("Well Sourced", "Neutral", "Complete", "Readable", "Overall")
  pre.cor.mat <- as.matrix(feed.df[, c("total_wellsourced", "total_neutral", "total_complete", "total_readable", "sum_ratings")] / feed.df[, c("count_wellsourced", "count_neutral", "count_complete", "count_readable", "sum_count")])
  return(pre.cor.mat[complete.cases(pre.cor.mat),])
}

# generate low count ratings with only rating numbers to show different correlations at multiples of 4

feed.small <- feed.df[feed.df[, "sum_count"] %in% 2:40, ]
feed.small[, c("Well Sourced", "Neutral", "Complete", "Readable", "Overall")] <- feed.small[, c("total_wellsourced", "total_neutral", "total_complete", "total_readable", "sum_ratings")] / feed.small[, c("count_wellsourced", "count_neutral", "count_complete", "count_readable", "sum_count")]
feed.small <- feed.small[complete.cases(feed.small), c("Well Sourced", "Neutral", "Complete", "Readable", "Overall", "sum_count")]

# Load Individual rating categories into a numerical matrix (along w/ col's 6 & 7 for length/sum_count

ratings.corr <- cor(precorMatrix())

# move computation to import script. This duplicates efforts in precoMatrix a bit.

plotcormat <- matrix(0, 37, 4)
colnames(plotcormat) <- c("Well Sourced", "Neutral", "Complete", "Readable")
for (i in c("Well Sourced", "Neutral", "Complete", "Readable")) {
  plotcormat[,i] <- mapply(function(x) cor(feed.small[feed.small[, "sum_count"] %in% x, c(i, "Overall")])[1,2], seq(4,40,1))
}

# Removes individual ratings and sum_ratings from the dataframe. Cleans up calls to summary() and doesn't remove 
# any information I really need for plotting or analysis.

feed.df <- feed.df[, c("title", "length", "sum_count", "rating_avg", "Assessment")]

# Reset row names for the data frames so that regression diagnostics will be informative 

rownames(feed.df) <- 1:nrow(feed.df)

# Just GA/FA/former GA/Former FA/Failed GA noms/Featured Lists
# Be mindful, this is a miniscule fraction of overall sample
# No attempt is made to isolate those which held this status during the sample period. 

feed.rated <- feed.df[feed.df[, "Assessment"] != "Unassessed", ]
feed.rated[, "Assessment"] <- factor(feed.rated[, "Assessment"])
rownames(feed.rated) <- 1:nrow(feed.rated)

# Cleans up objects used for importing 
# Purely cosmetic, none of them are particularly large
# Close remote connections, if we opened any

# precorMatrix is removed because the rows it depends upon are removed

remove(unclean.rows, buildFeedDf, applyFactors, precorMatrix, fetchCompleteCat, remove feed.small, i)
closeAllConnections()