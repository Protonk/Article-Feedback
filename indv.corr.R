# aft.web <- readLines("http://dumps.wikimedia.org/other/articlefeedback/")
# dump.dates <- regmatches(aft.web[grep("aa_", aft.web)], regexpr("\\d{8}\\.csv\\.gz", aft.web[grep("aa_", aft.web)]))
# dump.urls <- paste("http://dumps.wikimedia.org/other/articlefeedback/aa_combined", dump.dates, sep = "-")
# 
# dir.create(file.path(getwd(), "AFT Dump"))
# outfiles <- file.path(getwd(), "AFT Dump", dump.dates)
# 
# for (i in 1:length(dump.urls)) {
# 	download.file(dump.urls[i], outfiles[i])
# 	}


# Above script should only be run once. 
# After downloading: 
#      sed "1q" 20110523.csv > names.txt
#  This batch has some undocumented fields which aren't worth figuring out atm
#      rm 20110620.csv
#      cat *.csv > temp
#      grep -v "aa_page" temp > out.csv
#      rm temp

in.classes <- c(rep("numeric", 2), "character", rep("numeric", 2),
                "character", rep("numeric", 13))

indrat <- read.csv("/Users/protonk/R/AFT Dump/out.csv",
                   header = FALSE, colClasses = in.classes,
                   nrows = 2508605)
names(indrat) <- scan("/Users/protonk/R/AFT Dump/names.txt", what = "character", sep = ",")

indrat[, "aa_timestamp"] <- as.Date(substr(indrat[, "aa_timestamp"], 1, 8), format = "%Y%m%d", tz = "UTC")

ratingcats <- names(indrat)[grep("(aa_rating_).*", names(indrat))]
keep.ind <- c("aa_page_id", "page_title", "aa_user_id", "aa_timestamp", ratingcats)
final.names <- c("Page_ID", "Title", "Registered", "Date", "Sourced", "Neutral", "Complete", "Readable")
indrat <- indrat[, keep.ind]
names(indrat) <- final.names

# Assign 0 values to ratings as NA
# This potentially creates an imputation issue as cats 
# left blank may be intended 0's but I'm not worried
indrat[, c("Sourced", "Neutral", "Complete", "Readable")][indrat[, c("Sourced", "Neutral", "Complete", "Readable")] == 0] <- NA

# mark ratings w/ 4 cats rated and 
# ratings w/ no cats rated are NA rows in meanrat (latter to be removed)

indrat[, "Rated All"] <- complete.cases(indrat[, c("Sourced", "Neutral", "Complete", "Readable")])

indrat[, "Mean"] <- rowMeans(indrat[, c("Sourced", "Neutral", "Complete", "Readable")], na.rm = TRUE)

indrat <- indrat[!is.na(indrat[, "Mean"]), ]