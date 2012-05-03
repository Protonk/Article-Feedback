library(ggplot2)


infile <- function() {
  temp <- tempfile()
  download.file("http://dumps.wikimedia.org/other/articlefeedback/aa_combined-20110919.csv.gz", temp)
  out <- read.csv(gzfile(temp), header = TRUE, as.is = TRUE, nrows = 283285)
  unlink(temp)
  return(out)
}
# we only want the ratings
reduced <- infile()[, c("aa_rating_wellsourced", "aa_rating_neutral", "aa_rating_complete", 
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
qplot(count.out, fill = test, geom = "bar", data = preplot) + 
  opts(legend.position = "none", title = expression("Averages of ratings where users rated all four categories")) + 
  scale_y_continuous(name = "") +  scale_x_discrete(name = "")