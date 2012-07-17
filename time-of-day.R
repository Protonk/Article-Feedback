# original data is timestamped to the second.

timestamp.fmt <- c("%Y%m%d%H%M%S")

# This will change eventually.
ratingcats <- names(indrat)[grep("(aa_rating_).*", names(indrat))]
keep.ind <- c("aa_page_id", "page_title", "aa_user_id", "aa_timestamp", ratingcats)
final.names <- c("Page_ID", "Title", "Registered", "Time", "Sourced", "Neutral", "Complete", "Readable")
indrat <- indrat[, keep.ind]
names(indrat) <- final.names


# Assign 0 values to ratings as NA
# This potentially creates an imputation issue as cats 
# left blank may be intended 0's but I'm not worried
indrat[, c("Sourced", "Neutral", "Complete", "Readable")][indrat[, c("Sourced", "Neutral", "Complete", "Readable")] == 0] <- NA

# ratings w/ no cats rated are NA rows in meanrat 

indrat[, "Mean"] <- rowMeans(indrat[, c("Sourced", "Neutral", "Complete", "Readable")], na.rm = TRUE)

indrat <- indrat[!is.na(indrat[, "Mean"]), ]

indrat[, "Time"] <- as.character(indrat[, "Time"])
# POSIXlt is what we want eventually. But we'll get there. If we convert it in the data frame it gets wonky
indrat[, "Time"] <- as.POSIXct(indrat[, "Time"], tz = "UTC", format = timestamp.fmt)



# start with some small samples

quickSubset <- function(data, n = 1000, names = c("Time", "Mean")) {
  qs <- data[sample(nrow(data), n), names]
  qs[, "HMS"] <- format(qs[, "Time"], format = "%H%M%S")
  qs[, "Time"] <- as.POSIXct(paste0("20110906", qs[, "HMS"]), tz = "UTC", format = timestamp.fmt)
  qs[, "Hour"] <- factor(substr(qs[, "HMS"], 1, 2))
  return(qs)
}

# we want number of rows as well.

# and we shoudl just set the day to some random day for all of them

