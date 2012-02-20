### Scratchpad for some thoughts about distribution of ratings on the lower ends


array.test <- function(max.count = 400, reps = 100) {
	arts.t.rate <- max.count * reps
	sim.ratings <- round(runif(max.count*max.count*reps, 1, 5))
	init.tri <- lower.tri(matrix(0, max.count, max.count))
	tri.index <- rep(init.tri, reps)
	sim.ratings[tri.index] <- NA
	indv.avg <- rowMeans(matrix(sim.ratings, max.count*reps, max.count, byrow = TRUE), na.rm = TRUE)
	indv.avg <- indv.avg[unlist(split(1:arts.t.rate, 1:max.count))]
	rating.avg <- matrix(indv.avg, max.count, reps, byrow = TRUE)
	n.uniques <- apply(rating.avg, 1, function(x) length(unique(x)))
	return(n.uniques)
	}
  

var.plot <- function(max.count = 400, reps = 100) {
  tabcount <- tabulate(feed.df[,"sum_count"])
  spread.df <- feed.df[feed.df[,"sum_count"] <= max.count, ]
  dist.un <- mapply(function(x) length(unique(spread.df[spread.df[,"sum_count"] == x,"rating_avg"])), 1:400)
  plot(1:length(dist.un), dist.un , type = "l", ylim = c(0, 400))
  lines(1:max.count, tabcount[1:max.count], col = "blue")
  abline(a = 0, b =5, col = "green", lty = 2)
}


nuniques <- function(inp) {
  length(unique(inp))
}                                
df.prob <- mean(feed.df[, "rating_avg"])/5

bn.prob <- mean(feed.df[, "rating_avg"])/4
binomavg <- function(counts, rate) {
  replicate(counts, mean(rbinom(rate,4,df.prob) + 1))
}
binom.uni <- unlist(lapply(mapply(binomavg, counts = tabcount[1:400], rate = 1:400), nuniques))

unifavg <- function(counts, rate) {
  replicate(counts, mean(round(runif(rate, min = 1, max = 5))))
}
unif.uni <- unlist(lapply(mapply(unifavg, counts = tabcount[1:400], rate = 1:400), nuniques))
head(unif.uni, 20)

unif.list <- mapply(unifavg, counts = tabcount[20:40], rate = 20:40)

stmean <- mean(feed.df[, "rating_avg"])
stsd <- sd(feed.df[, "rating_avg"])

