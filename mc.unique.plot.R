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
  

feed.sim.mapply <- function(max.count = 400, reps = 100) {
	sim.uniq.out <- matrix(c(1:max.count, rep(0,max.count)), max.count, 2)
	vector.uniq <-	Vectorize(function(vec.max.count, vec.reps) length(unique(replicate(vec.reps, mean(round(runif(vec.max.count, 1, 5)))))))
	sim.uniq.out[,2] <- vector.uniq(1:max.count, reps)
	return(sim.uniq.out)
	}

#loop is just as fast as mapply, of course. 

feed.sim.loop <- function() {
	sim.uniq.out <- matrix(c(1:10, rep(0,10)), 10, 2)
	for (i in i:10) {
		sim.uniq.out[i,2] <- length(unique(replicate(50, mean(round(runif(i, 1, 5))))))
		}
	return(sim.uniq.out)
	}

	
	
	
	
	
	
	
	
	
	


