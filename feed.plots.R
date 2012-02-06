### Only non-animated plots using base graphics
### Plots with ggplot2 are in feed.ggplot.R and animations are in feed.Animations.R


# Plot functions

# Illustrates one half of the long tail
# Also shows the strong periodicity due to framing of responses in feedback tool

count.low.plot <- function(max.sum=60) {
  # tabulate() is awesome
	count.rows <- cbind(1:max.sum,tabulate(feed.df[feed.df[,"sum_count"] <= max.sum,"sum_count"]))
	period <- seq(from = 4,to = max.sum, by = 4)
	plot(count.rows[,1], log(count.rows[,2]), type= "l", frame.plot = FALSE, xlab= "Number of Ratings", ylab= "log Articles Rated", main= "Most articles have few ratings with\njumps at multiples of four")
	segments(y0=c(log(count.rows[period,2])), x0=period, y1=0, lty=2, col="green")
	# Spline meant only to illustrate the deviation
  count.spline <- smooth.spline(count.rows[,1], log(count.rows[,2]), nknots=5)
	lines(x = count.spline$x, y = count.spline$y, col = "blue", lty = 3)
	}

# The upper half of the long tail, dominated by Justin Bieber

count.high.plot <- function(min.rank=60) {
	# Sorted so I can keep the same rtl orientation for both high and low plots
	count.sum <- cbind(min.rank:1,sort(feed.df[order(feed.df[,"sum_count"], decreasing = TRUE)[1:min.rank],"sum_count"]))
	plot(log(count.sum[,1]), log(count.sum[,2]), type = "l", xlim = rev(range(log(count.sum[,1]))) , axes = FALSE, ylim = c(7.5, 10.5), xlab = '', ylab = '')
	title(ylab = "log Article ratings received", main = "Log/Log plot of Rank and Number of Ratings Received")
	# Custom axis because we are looking at log rank, not actual rank
  axis(1, at = c(4,0.2), labels = c("Lower Rank (60th)", "Higher Rank"))
	axis(4)
	}

# Area under the distribution
# Gets noisy near the end because of many ratings per article and many zeros

pareto.plot <- function(max.plot = 1200) {
	tabcount <- tabulate(feed.df[,"sum_count"])
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

### Not quite ready for prime-time

# Basic idea is that freedom for averages are necessarily constrained at low
# rating per article numbers. For 1 rating per article the only possible 
# averages are 1, 2, 3, 4 and 5. 

# Because these coincide with enormous numbers of articles rated, the probability
# that all possible averages are included in the sample is very high until 
# roughly 20 ratings per article

var.plot <- function(max.count = 400, reps = 100) {
  tabcount <- tabulate(feed.df[,"sum_count"])
  spread.df <- feed.df[, c("sum_ratings", "sum_count")]
  spread.df$Average <- spread.df[, "sum_ratings"] / spread.df[, "sum_count"]
  spread.df <- spread.df[spread.df[,"sum_count"] <= max.count, -1]
  spread.df[, "sum_count"] <- factor(spread.df[, "sum_count"])
  dist.un <- by(spread.df, spread.df[, "sum_count"], function(x) length(unique(x[,2])))
  plot(1:length(dist.un), dist.un , type = "l", ylim = c(0, 400))
  lines(1:max.count, tabcount[1:max.count], col = "blue")
  abline(a = 0, b =5, col = "green", lty = 2)
}
