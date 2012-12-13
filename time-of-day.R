# original data is timestamped to the second.

timestamp.fmt <- c("%Y%m%d%H%M%S")


# Assign 0 values to ratings as NA
# This potentially creates an imputation issue as cats 
# left blank may be intended 0's but I'm not worried
rating.times[, c("Sourced", "Neutral", "Complete", "Readable")][rating.times[, c("Sourced", "Neutral", "Complete", "Readable")] == 0] <- NA

# ratings w/ no cats rated are NA rows in meanrat 

rating.times[, "Mean"] <- rowMeans(rating.times[, c("Sourced", "Neutral", "Complete", "Readable")], na.rm = TRUE)
rating.times <- rating.times[!is.na(rating.times[, "Mean"]), ]


# POSIXlt is what we want eventually. But we'll get there. If we convert it in the data frame it gets wonky
rating.times[, "Time"] <- as.POSIXct(rating.times[, "Time"], tz = "UTC", format = timestamp.fmt)



# start with some small samples

quickSubset <- function(data, n = 1000, names = c("Time", "Mean")) {
  qs <- data[sample(nrow(data), n), names]
  qs[, "HMS"] <- format(qs[, "Time"], format = "%H%M%S")
  qs[, "Time"] <- as.POSIXct(paste0("20110906", qs[, "HMS"]), tz = "UTC", format = timestamp.fmt)
  qs[, "Hour"] <- factor(substr(qs[, "HMS"], 1, 2))
  return(qs)
}

meanGen <- function(data, n = 1000, names = c("Time", "Mean")) {
  require(plyr)
  qs <- data[sample(nrow(data), n), names]
  qs[, "Hour"] <- factor(format(qs[, "Time"], format = "%H"))
  ddply(qs, "Hour", summarise, Mean = mean(Mean, na.rm = TRUE))
}

bl.part <- rgb(r = 0, g = 0, b = 0, alpha = 0.08)
plot(1:24, seq(3.2, 4.2, length.out = 24), type = "n", frame.plot = FALSE)
for (i in 1:1000) {
  by.hr <- meanGen(rating.times, n = 2500)
  with(by.hr, lines(as.numeric(Hour), Mean, col = bl.part))
}

rowGen <- function(data, n = 1000) {
  qs <- data[sample(nrow(data), n), "Time", drop = FALSE]
  qs[, "HMS"] <- format(qs[, "Time"], format = "%H%M%S")
  qs[, "Time"] <- as.POSIXct(paste0("20110906", qs[, "HMS"]), tz = "UTC", format = timestamp.fmt)
  qs[, "Minutes"] <- cut.POSIXt(qs[, "Time"], breaks = "15 min",
                                right = TRUE)
  ddply(qs, "Minutes", "nrow")
}

xseq <- seq(1, 97, length.out = 7)
xseqlab <- c("0000", "0400", "0800", "1200", "1600", "2000", "2400")
bl.part <- rgb(r = 70, g = 130, b = 180, alpha = 25, max = 255)
plot(1:97, seq(0, 80, length.out = 97), type = "n", frame.plot = FALSE,
     xaxt = 'n', xlab = '', ylab = "Number of ratings",
     main = "Ratings Throughout the Day", sub = "15 minute intervals")
axis(1, at = xseq, labels = xseqlab)
for (i in 1:1000) {
  by.hr <- rowGen(rating.times, n = 2500)
  with(by.hr, lines(as.numeric(Minutes), nrow, col = bl.part, lwd = 1.5))
}



  
  


# we want number of rows as well.

# and we shoudl just set the day to some random day for all of them

