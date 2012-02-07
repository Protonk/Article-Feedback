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
  feed.df[feed.df[,"title"] %in% formerGA.list, "Assessment"] <<- "Former Good Article"
  feed.df[feed.df[,"title"] %in% formerFA.list, "Assessment"] <<- "Former Featured Article"
  feed.df[feed.df[,"title"] %in% nomGA.list, "Assessment"] <<- "Good Article Nominee"
  feed.df[feed.df[,"title"] %in% GA.list, "Assessment"] <<- "Good Article"
  feed.df[feed.df[,"title"] %in% FA.list, "Assessment"] <<- "Featured Article"
  closeAllConnections()
}

# Drops namespace and page id

feed.df <- buildFeedDf()[, c(3:12)]
names(feed.df) <- c("title", "length", "total_wellsourced", "count_wellsourced", "total_neutral", "count_neutral", "total_complete", "count_complete", "total_readable", "count_readable")

# Compute rating/count sums

feed.df$sum_ratings <- feed.df[, "total_wellsourced"] + feed.df[, "total_neutral"] + feed.df[, "total_complete"] + feed.df[, "total_readable"]
feed.df$sum_count <- feed.df[, "count_wellsourced"] + feed.df[, "count_neutral"] + feed.df[, "count_complete"] + feed.df[, "count_readable"]
feed.df$rating_avg <- feed.df[, "sum_ratings"] / feed.df[, "sum_count"]

# Create factor variable for Project Assessment and assign it with applyFactors()

feed.df$Assessment<- factor(x = c("Unassessed"), levels=c("Unassessed", "Former Good Article", "Former Featured Article", "Good Article Nominee",  "Good Article", "Featured Article"))
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
feed.rated <- feed.df[feed.df[, "Assessment"] != "Unassessed", ]
feed.rated[, "Assessment"] <- factor(feed.rated[, "Assessment"])

# Cleans up objects used for importing 
# Purely cosmetic, none of them are particularly large
remove(unclean.rows, buildFeedDf, applyFactors)
