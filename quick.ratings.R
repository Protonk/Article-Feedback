library(ggplot2)

# sort of a waste to download all of them from within R. 
# much easier to download and then run something like:
#      sed "1q" 20110523.csv > names.txt
#  This batch has some undocumented fields which aren't worth figuring out atm
#      rm 20110620.csv
#      cat *.csv > temp
#      grep -v "aa_page" temp > out.csv
#      rm temp

header <- scan("/Users/protonk/R/AFT Dump/names.txt", what = "character", sep = ",")

in.classes <- c(rep("numeric", 2), "character", rep("numeric", 2), "character", rep("numeric", 13))

indrat <- read.csv("/Users/protonk/R/AFT Dump/out.csv",
                   header = FALSE, colClasses = in.classes,
                   nrows = 2508605)
names(indrat) <- header

# we only want the ratings
reduced <- indrat[, c("aa_rating_wellsourced", "aa_rating_neutral", "aa_rating_complete", 
                        "aa_rating_readable")]

# We are only interested in rows where the user rated all 4 categories
reduced[reduced == 0] <- NA
rating.avgs <- rowMeans(reduced[complete.cases(reduced), ])

count.table <- table(rating.avgs)

# Builds factors of the rating avgs (ordered by count)
# and another factor for just the integers (this is a ggplot2 thing)
count.out <- factor(rating.avgs, levels = names(sort(count.table, decreasing = TRUE)), ordered = TRUE)
integers <- factor(count.out, levels = as.character(1:5))
# placed into a data frame for easier plotting
preplot <- data.frame(count.out, integers)

# Plot frequency of averages
qplot(count.out, fill = integers, geom = "bar", data = preplot) + 
  opts(legend.position = "none", title = expression("Averages of ratings where users rated all four categories")) + 
  scale_y_continuous(name = "") +  scale_x_discrete(name = "")