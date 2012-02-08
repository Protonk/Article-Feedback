### Only non-animated plots using base graphics
### Plots with ggplot2 are in feed.ggplot.R and animations are in feed.Animations.R


# Plot functions

# Illustrates one half of the long tail
# Also shows the strong periodicity due to framing of responses in feedback tool

count.low.plot <- function(max.sum=60) {
  # tabulate() is awesome
	count.rows <- cbind(1:max.sum, tabulate(feed.df[feed.df[, "sum_count"] <= max.sum, "sum_count"]))
	period <- seq(from = 4,to = max.sum, by = 4)
	plot(count.rows[,1], log(count.rows[,2]), type= "l", frame.plot = FALSE, xlab= "Number of Ratings", ylab= "log Articles Rated", main= "Most articles have few ratings with\njumps at multiples of four")
	segments(y0 = c(log(count.rows[period,2])), x0 = period, y1 = 0, lty = 2, col = "green")
	# Spline meant only to illustrate the deviation
    count.spline <- smooth.spline(count.rows[,1], log(count.rows[,2]), nknots=5)
	lines(x = count.spline$x, y = count.spline$y, col = "blue", lty = 3)
}

# The upper half of the long tail, dominated by Justin Bieber

count.high.plot <- function(min.rank=60) {
	# Sorted so I can keep the same rtl orientation for both high and low plots
	count.sum <- cbind(min.rank:1,sort(feed.df[order(feed.df[, "sum_count"], decreasing = TRUE)[1:min.rank], "sum_count"]))
	plot(log(count.sum[,1]), log(count.sum[,2]), type = "l", xlim = rev(range(log(count.sum[,1]))) , axes = FALSE, ylim = c(7.5, 10.5), xlab = '', ylab = '')
	title(ylab = "log Article ratings received", main = "Log/Log plot of Rank and Number of Ratings Received")
	# Custom axis because we are looking at log rank, not actual rank
  axis(1, at = c(4,0.2), labels = c("Lower Rank (60th)", "Higher Rank"))
	axis(4)
}

# Area under the distribution
# Gets noisy near the end because of many ratings per article and many zeros

pareto.plot <- function(max.plot = 1200) {
	tabcount <- tabulate(feed.df[, "sum_count"])
	tabsum <- cumsum(1:length(tabcount)*tabcount)
  # There is probably a better way to do this than tail()
	count.8020 <- tail(which(tabsum <= 0.8*max(tabsum)), 1)
  # R completes polygons by drawing from the last point to the first. 
  # Hence the fixed start and end points
	leftpoly <- rbind(c(1,0) ,matrix(c(1:count.8020, log(tabcount[1:count.8020])), count.8020, 2) , c(count.8020, 0))
	rightpoly <- rbind(c((count.8020 + 1),0) , matrix( c( (count.8020 + 1):max.plot, log(pmax(tabcount[(count.8020 + 1):max.plot], 1) ) ), (max.plot - count.8020), 2), c(max.plot, 0))
	plot(1:max.plot, log(tabcount[1:max.plot]), type = "l", frame.plot = FALSE, xlab = "Number of Ratings", ylab = "log Articles Rated", main = "Overall Distribution roughly follows Pareto Principle" )
	polygon(leftpoly, col = "sky blue", density = 35)
	polygon(rightpoly, col = "light grey" , density = 35)
	segments(x0 = count.8020, y0 = 0, y1 = 7, col = "grey", lty = 2, lwd = 2)
	points(count.8020, 7, col = "grey", pch = 20)
	text((count.8020 + 10), 7, labels = paste("80% of all article ratings\naccounted for by articles rated\n", count.8020, " times or fewer.", sep = ""), pos = 4, adj = 0, cex = 0.8)
	text(0.8*max.plot, 4, labels = paste("Remaining ", (length(tabcount) - max.plot), " rows\nommitted for clarity", sep = ""), adj = 0, cex = 0.6)
}

# Visual map of correlation between rating categories and overall ratings. 

ratingMap <- function () {
  try(require(RColorBrewer))
  heatmap(cor(ratings.corr), symm = TRUE, Rowv = NA, col = brewer.pal(9, "Blues"), keep.dendro = FALSE, labCol = c("Well\nSourced", "Neutral", "Complete", "Readable", "Overall"), labRow = c("Well\nSourced", "Neutral", "Complete", "Readable", "Overall"), margins = c(8.2, 4))
}


### Not quite ready for prime-time

# Basic idea is that freedom for averages are necessarily constrained at low
# rating per article numbers. For 1 rating per article the only possible 
# averages are 1, 2, 3, 4 and 5. 

# Because these coincide with enormous numbers of articles rated, the probability
# that all possible averages are included in the sample is very high until 
# roughly 20 ratings per article

var.plot <- function(max.count = 400, reps = 100) {
  tabcount <- tabulate(feed.df[,"sum_count"])
  spread.df <- feed.df[, c("sum_ratings", "sum_count", "rating_avg")]
  spread.df <- spread.df[spread.df[,"sum_count"] <= max.count, -1]
  spread.df[, "sum_count"] <- factor(spread.df[, "sum_count"])
  dist.un <- by(spread.df, spread.df[, "sum_count"], function(x) length(unique(x[,2])))
  plot(1:length(dist.un), dist.un , type = "l", ylim = c(0, 400))
  lines(1:max.count, tabcount[1:max.count], col = "blue")
  abline(a = 0, b =5, col = "green", lty = 2)
}

# Not fully there yet, but a good show of how measures of key variables become unstable 
# as ratings/article rise and # articles fall (faster)

instabilityPlot <- function(max.count = 400) {
	computePerRating <- function(max.count) {
	  tabcount <- tabulate(feed.df[feed.df[,"sum_count"] <= max.count, "sum_count"])
	  spread.df <- feed.df[, c("sum_ratings", "sum_count", "rating_avg", "length", "Assessment")]
	  spread.df <- spread.df[spread.df[, "sum_count"] <= max.count, ]
	  summary.mat <- matrix(0, max.count, 5)
	  summary.mat[, 3] <- tabcount
	  for (i in 1:max.count) {
		summary.mat[i,1] <- mean(spread.df[spread.df[, "sum_count"] == i, "rating_avg"], na.rm = TRUE)
		summary.mat[i, 2] <- mean(log(spread.df[spread.df[, "sum_count"] == i, "length"]))
		summary.mat[i,4] <- nrow(spread.df[spread.df[, "sum_count"] == i & spread.df[, "Assessment"] != "Unassessed",])
	  }
	  summary.mat[,5] <- summary.mat[,4] / summary.mat[,3]
	  summary.df <- data.frame(summary.mat)
	  names(summary.df) <- c("rating_avg", "avg_log_length", "number_art", "number_rated_art", "prop_rated")
	  return(summary.df)
	}
	unstable.df <- computePerRating(max.count)
	x.count.lab <- "Ratings per Article"
	log.lab <- "Average log Article Length"
	prop.lab <- "Project Quality Assessed Articles\nas a Fraction of all Articles"
	avg.lab <- "Rating Average"
	plot.title <- "Variables of Interest Become Unstable as Fewer Articles are Rated"
	par(mfrow = c(3,1))
	plot(1:max.count, unstable.df[, "rating_avg"], type = "l", ylab = '', xlab = '', main = plot.title, frame.plot = FALSE)
	points(25, 2.8, pch = 15, cex = 2)
	text(30, 2.8, labels = avg.lab, pos = 4, cex = 2)
	plot(1:max.count, unstable.df[, "avg_log_length"], type = "l", col = "blue", frame.plot = FALSE, ylab = '', xlab = '')
	points(25, 10.5, pch = 15, cex = 2, col = "blue")
	text(30, 10.5, labels = log.lab, pos = 4, cex = 2)
	plot(1:max.count, unstable.df[, "prop_rated"], type = "l", col = "green", frame.plot = FALSE, xlab = x.count.lab, ylab = '')
	points(25, 0.45, pch = 15, cex = 2, col = "green")
	text(30, 0.45, labels = prop.lab, pos = 4, cex = 2)
	par(mfrow = c(1,1))
}