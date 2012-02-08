### Some simple regression models 

## Build an aux. regression for length (4th order polynomial) and fit categorical ratings to those residuals
## 

length.lm.4 <- lm(rating_avg ~ log(length) + I(log(length)^2) + I(log(length)^3) + I(log(length)^4) , data = feed.red.5)
simple.aux.lm <- lm(resid(length.lm.4) ~ Assessment, data = feed.red.5)

# Shapiro-Wilk test for rated and unrated articles. Unsurprisingly, average ratings for both are not 
# normally distributed. 
sw.ratings <- by(feed.red.5[feed.red.5$Assessment != "Unassessed",], feed.red.5[feed.red.5$Assessment != "Unassessed", "Assessment"] , function(x) shapiro.test(x[,"sum_ratings"] / x[,"sum_count"]))

# Basic linear (non-robust) regression for explanatory variables of interest
simple.lm <- lm(rating_avg ~ log(length) + log(sum_count) + Assessment, data = feed.red.5)

# Correlation matrix for the specific categories. Note that complete.cases() is used to drop 
# any row w/ NA values which we will have due to div by zero. 

avgMatrix <- function() {
  avgmat <- matrix(0, nrow(feed.df), 7)
  colnames(avgmat) <- c("Well Sourced", "Neutral", "Complete", "Readable", "Overall", "log_length", "log_total_ratings")
  for (i in c(3,5,7,9,11)) {
    avgmat[,(i - 1)/2] <- (feed.df[, i] / feed.df[, i + 1])
  }
  avgmat[, 6] <- log(feed.df[, "length"])
  avgmat[, 7] <- log(feed.df[, "sum_count"])
  return(avgmat[complete.cases(avgmat),])
}

# Mostly set up for printing 
full.cor <- cor(avgMatrix())
ratings.only.cor <- cor(avgMatrix()[, c(1:5)])

ratingMap <- function () {
  try(require(RColorBrewer))
  heatmap(ratings.only.cor, symm = TRUE, Rowv = NA, col = brewer.pal(9, "Blues"), keep.dendro = FALSE, labCol = c("Well\nSourced", "Neutral", "Complete", "Readable", "Overall"), labRow = c("Well\nSourced", "Neutral", "Complete", "Readable", "Overall"), margins = c(8.2, 4))
}

##





