### Animations ###

## All will require the "animation" library. Most should require feed.import.R to be sourced in. 


# Exact Distribution of Averages by Number of ratings

exact.dist.plot <- function(max.count=30) {
  try(require(animation))
  ani.options(outdir = getwd())
  unique.dist <- function(max.count) {
    tabcount <- tabulate(feed.df[feed.df[,"sum_count"] <= max.count, "sum_count"])
    # We want counts and ratings. Eventually this will be rewritten as
    # feed.df will just include average ratings
    spread.df <- feed.df[, c("sum_ratings", "sum_count", "rating_avg")]
    spread.df <- spread.df[spread.df[, "sum_count"] <= max.count, -1]
    # possible averages 
    short.options <- mapply(seq, 1, 5, by = (5 / seq(5, 5*max.count, 5)))
    unique.list <- list()
    # Generate matches in sample for possible averages. Accurate to > 30 ratings per article
    # Stored as FP so perfect accuracy for >> 30 is NOT expected
    for (i in 1:max.count) {
      unique.list[[i]] <- mapply(function(x) sum(round(signif(spread.df[spread.df[, "sum_count"] == i, "rating_avg"], 8) %in% signif(short.options[[i]][x], 8))), 1:length(short.options[[i]]))
    }
    return(unique.list)
  }
  avg.dist <- unique.dist(max.count)
  exact.movie <- function(max.count) {
    # Laziest hack ever to avoid writing a function to distinguish "Assessment" from "Ratings"
    barplot(avg.dist[[1]], yaxt = "n", main = "Exact Distribution of Rating Averages per Number of Ratings", xlab = "1 Rating per Article", col = "blue", ylab = "Scale is not Maintained between Frames")
    for (i in 2:max.count) {
      xaxis.text <- paste(i, "Ratings per Article", sep = " ")
      barplot(avg.dist[[i]], yaxt = "n", main = "Exact Distribution of Rating Averages per Number of Ratings", xlab = xaxis.text, col = "blue", ylab = "Scale is not Maintained between Frames")
    }
  }
  saveGIF(exact.movie(max.count))
}

#Estimated Distribution of Rating Averages for Number of ratings >= n

# Slightly different approach here than the above animation

density.rate.plot <- function(max.thresh=20) {
  try(require(animation))
  ani.options(outdir = getwd())
  density.meas <- array(0, c(2,512,max.thresh))
  density.size <- numeric(max.thresh)
  ratings.subset <- matrix(c(feed.df[,"sum_ratings"] / feed.df[,"sum_count"] , feed.df[, "sum_count"]), c(nrow(feed.df), 2))
  # Mostly done using a loop because accessing multiple elements from a list simultaneously 
  # is clumsy. For ~20 frames this isn't that slow
  for (i in 1:max.thresh) {
    # density() produces an estimated pdf for an input
    # plot() has a method for density but we can't change as much
    # as we can w/ just grabbing the data
    density.meas[1, ,i] <- density(ratings.subset[ratings.subset[,2] >= i,1])$x
    density.meas[2, ,i] <- density(ratings.subset[ratings.subset[,2] >= i,1])$y
    density.size[i] <- density(ratings.subset[ratings.subset[,2] >= i,1])$n
  }
  den.movie <- function(max.thresh) {
    for (n in 1:max.thresh) {
      plot(density.meas[1, ,n], density.meas[2, ,n], type = "l", col = "blue", ylim = c(0,1), xlab = paste("N", "=",  as.character(density.size[n]), sep = " "), main = "Distribution of Rating Averages\nas Minimum Number of Ratings Increases", ylab = "Estimated Kernel Density", frame.plot = FALSE)
      text(x = 2, y = 0.8, labels = paste("Minimum Number of Ratings", as.character(n), sep = "\n"))
    }
  }
  saveGIF(den.movie(max.thresh))
}

