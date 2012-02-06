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
  else read.csv("~/R/Dropbox/afdump.csv", as.is= TRUE)
}


# Generate character vectors for "rated" articles and use them to create
# factors in the feedback dataframe. Local or remote. 

applyFactors <- function(remote = FALSE) {
  local.articles <- c("~/R/Dropbox/delGa.txt", 
                      "~/R/Dropbox/formerFA.txt",
                      "~/R/Dropbox/fmrGAnom.txt", 
                      "~/R/Dropbox/GA.txt", 
                      "~/R/Dropbox/FA.txt")
  remote.articles <- c("http://pastebin.com/raw.php?i=iFugze1d",
                       "http://pastebin.com/raw.php?i=zxibKJdz",
                       "http://pastebin.com/raw.php?i=TMb6hmSU",
                       "http://pastebin.com/raw.php?i=eedeTxwU",
                       "http://pastebin.com/raw.php?i=pq883wFp")
  charvec.names <- c("formerGA.list",
                     "formerFA.list",
                     "nomGA.list",
                     "GA.list",
                     "FA.list")
  if (remote) input <- remote.articles
  else input <- local.articles
  for (x in seq_along(charvec.names)) {
    assign(charvec.names[x], as.character(read.delim(file(input[x]), sep = "\n", header = FALSE, as.is= TRUE)[,1]))
  }
  feed.df[feed.df[,"title"] %in% formerGA.list, "Rating"] <<- "Former Good Article"
  feed.df[feed.df[,"title"] %in% formerFA.list, "Rating"] <<- "Former Featured Article"
  feed.df[feed.df[,"title"] %in% nomGA.list, "Rating"] <<- "Good Article Nominee"
  feed.df[feed.df[,"title"] %in% GA.list, "Rating"] <<- "Good Article"
  feed.df[feed.df[,"title"] %in% FA.list, "Rating"] <<- "Featured Article"
  closeAllConnections()
}

# Drops namespace and page id

feed.df <- buildFeedDf()[, c(3:12)]
names(feed.df) <- c("title", "length", "total_wellsourced", "count_wellsourced", "total_neutral", "count_neutral", "total_complete", "count_complete", "total_readable", "count_readable")

# Compute rating/count sums

feed.df$sum_ratings <- feed.df[,3] + feed.df[,5] + feed.df[,7] + feed.df[,9]
feed.df$sum_count <- feed.df[,4] + feed.df[,6] + feed.df[,8] + feed.df[,10]

# Create factor variable for Project Assessment and assign it with applyFactors()

feed.df$Rating<- factor(x = c("Unrated"), levels=c("Unrated", "Former Good Article", "Former Featured Article", "Good Article Nominee",  "Good Article", "Featured Article"))
applyFactors()


# Drop rows with:
##  0 length
##  Extreme erroneous values
##  Nonsensical averages (> 5)
##  0 ratings

unclean.rows <- function() {
	extreme.out<- numeric()
	for (i in 3:10) {
		extreme.out <- append(extreme.out, which(feed.df[,i] == 4294967295), after = length(extreme.out))
		}
	feed.oor <- c(which(feed.df[,"length"] <= 0), which(feed.df[,"sum_count"] == 0), which(feed.df[,"sum_ratings"]/feed.df[,"sum_count"] > 5))
	extreme.out <- c(extreme.out, feed.oor)
	return(unique(extreme.out))
	}
feed.df <- feed.df[-unclean.rows(), ]

# Drop very low counts for ratings. 
feed.red.5 <- feed.df[feed.df[,"sum_count"] >= 5, ]

# Just GA/FA/former GA/Former FA/Failed GA noms
# Be mindful, this is a miniscule fraction of overall sample
feed.rated <- feed.df[feed.df[, "Rating"] != "Unrated", ]
feed.rated[,"Rating"] <- factor(feed.rated[,"Rating"])

# Cleans up objects used for importing 
# Purely cosmetic, none of them are particularly large
remove(unclean.rows, buildFeedDf, applyFactors)
