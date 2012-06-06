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


aveByRating <- function(data = indrat) {
  # Building tables to feed to ggplot2 requires a slight trick
  # See http://stackoverflow.com/a/3153333/1188479
  count.table.four <- table(data[data[, "Rated All"] == TRUE, "Mean"])
  count.table.else <- table(data[, "Mean"])
  four.df <- data.frame(Categories = "4",
                        Rating = names(count.table.four),
                        Count = as.numeric(count.table.four)
                        )
  else.df <- data.frame(Categories = "< 4",
                        Rating = names(count.table.else),
                        Count = as.numeric(count.table.else)
                        )
  full.df <- rbind(four.df, else.df)
  full.df[, "Rating"] <- factor(x = testme[, "Rating"], levels = sort(unique(as.numeric(testme[, "Rating"]))))
  full.df[, "Categories"] <- as.factor(full.df[, "Categories"])
  # Color coding for integers
  full.df[, "Integer"] <- as.factor(ifelse(full.df[, "Rating"] %in% 1:5, "Yes", "No"))
  full.df
}
testme <- aveByRating(data = indrat)


# Plot frequency of averages
qplot(count.out, fill = integers, geom = "bar", data = aveByRating(data = indrat, all = FALSE)) + 
  opts(legend.position = "none", title = expression("Averages of ratings where users rated all four categories")) + 
  scale_y_continuous(name = "") +  scale_x_discrete(name = "")



barplot(scale(table(indrat[, "Mean"]), center = FALSE)[, 1])

# for ggplot2
# http://stackoverflow.com/questions/3153025/grouped-bar-chart-with-ggplot2-and-already-tabulated-data