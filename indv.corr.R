library(plyr)


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
# You can scan in the names from there or just include them below
#      sed "1q" 20110523.csv > names.txt
#  This batch has some undocumented fields which aren't worth figuring out atm
#      rm 20110620.csv
#      cat *.csv > temp
#      grep -v "aa_page" temp > out.csv
#      rm temp

# Preset classes speeds importation a bit
in.classes <- c(rep("numeric", 2), "character", rep("numeric", 2),
                "character", "numeric", "character", rep("numeric", 11))

derived.names <- c("aa_page_id", "page_namespace", "page_title", "rev_len", "aa_user_id", 
                   "aa_user_anon_token", "aa_revision", "aa_timestamp", "aa_rating_wellsourced", 
                   "aa_rating_neutral", "aa_rating_complete", "aa_rating_readable", 
                   "aa_design_bucket", "expertise_general", "expertise_studies", 
                   "expertise_profession", "expertise_hobby", "expertise_helpimprove_email", 
                   "expertise_other")

indrat <- read.csv("/Users/protonk/R/AFT Dump/out.csv",
                   header = FALSE, colClasses = in.classes,
                   nrows = 2508605, comment.char = "")
names(indrat) <- derived.names




ratingcats <- names(indrat)[grep("(aa_rating_).*", names(indrat))]
keep.ind <- c("aa_page_id", "page_title", "aa_user_id", "aa_timestamp", ratingcats)
final.names <- c("Page_ID", "Title", "Registered", "Date", "Sourced", "Neutral", "Complete", "Readable")
indrat <- indrat[, keep.ind]
names(indrat) <- final.names

# pluck out a subset for timing

rating.times <- indrat[, c("Title", "Registered", "Date", "Sourced", "Neutral", "Complete", "Readable")]
names(rating.times)[3] <- "Time"

# for most work we want to deal with just dates

indrat[, "Date"] <- as.Date(substr(indrat[, "Date"], 1, 8), format = "%Y%m%d", tz = "UTC")


# Assign 0 values to ratings as NA
# This potentially creates an imputation issue as cats 
# left blank may be intended 0's but I'm not worried
indrat[, c("Sourced", "Neutral", "Complete", "Readable")][indrat[, c("Sourced", "Neutral", "Complete", "Readable")] == 0] <- NA

# mark ratings w/ 4 cats rated and 
# ratings w/ no cats rated are NA rows in meanrat (latter to be removed)

indrat[, "Rated All"] <- complete.cases(indrat[, c("Sourced", "Neutral", "Complete", "Readable")])

indrat[, "Mean"] <- rowMeans(indrat[, c("Sourced", "Neutral", "Complete", "Readable")], na.rm = TRUE)

indrat <- indrat[!is.na(indrat[, "Mean"]), ]