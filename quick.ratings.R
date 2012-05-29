library(ggplot2)

# build dfs for ranking and for pages in those rankings
# merge to preserve order
# top 1000 chosen
toplist <- data.frame(Page_ID = head(order(tabulate(indrat[, "Page_ID"]), decreasing = TRUE), 1000),
                      Rank = 1:1000)
toptitles <- unique(indrat[indrat[, "Page_ID"] %in% toplist[, "Page_ID"], c("Page_ID", "Title")])
topfinal <- merge(toplist, toptitles, by = "Page_ID")
# Sort for easy viewing w/ head()
topfinal <- topfinal[order(topfinal[, "Rank"]), ]


aveByRating <- function(data = indrat, all = TRUE) {
  # We are only interested in rows where the user rated all 4 categories
  rating.avgs <- data[data[, "Rated All"] == all, "Mean"] 
  count.table <- table(rating.avgs)
  # Builds factors of the rating avgs (ordered by count)
  # and another factor for just the integers (this is a ggplot2 thing)
  count.out <- factor(rating.avgs, levels = names(count.table), ordered = TRUE)
  integers <- factor(count.out, levels = as.character(1:5))
  # placed into a data frame for easier plotting
  data.frame(count.out, integers)
}
fourcat.table <- aveByRating(data = indrat, all = TRUE)
# Plot frequency of averages
qplot(count.out, fill = integers, geom = "bar", data = fourcat.table) + 
  opts(legend.position = "none", title = expression("Averages of ratings where users rated all four categories")) + 
  scale_y_continuous(name = "") +  scale_x_discrete(name = "")





