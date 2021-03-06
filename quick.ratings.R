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
  count.table.else <- table(data[data[, "Rated All"] == FALSE, "Mean"])
  four.df <- data.frame(Categories = "All Four Rated",
                        Rating = names(count.table.four),
                        Count = as.numeric(count.table.four)
                        )
  else.df <- data.frame(Categories = "Fewer Than Four Rated",
                        Rating = names(count.table.else),
                        Count = as.numeric(count.table.else)
                        )
  full.df <- rbind(four.df, else.df)
  full.df[, "Rating"] <- factor(x = full.df[, "Rating"],
                                levels = sort(unique(c(names(count.table.else), names(count.table.four)))))
  full.df[, "Categories"] <- as.factor(full.df[, "Categories"])
  # Color coding for integers
  full.df[, "Integer"] <- as.factor(ifelse(full.df[, "Rating"] %in% 1:5, "Yes", "No"))
  full.df
}

# Faceted
ggplot(data =  aveByRating(data = indrat), aes(x = Rating, y = Count, fill = Integer)) +
  geom_bar(position="dodge", stat="identity") + scale_x_discrete(breaks = 1:5, labels = as.character(1:5)) +
  facet_wrap(~ Categories) + guides(fill=FALSE) + xlab("Average Rating For Individual Rating Event") +
  scale_y_continuous(labels = c("0", "10,000", "20,000", "30,000", "40,000")) + ylab("Occurrences")
# combined
ggplot(data =  aveByRating(data = indrat), aes(x = Rating, y = Count, fill = Integer)) +
  geom_bar(position="dodge", stat="identity") + scale_x_discrete(breaks = 1:5, labels = as.character(1:5)) + 
  guides(fill=FALSE) + xlab("Average Rating For Individual Rating Event") +
  scale_y_continuous(labels = c("0", "10,000", "20,000", "30,000", "40,000")) + ylab("Occurrences")


# for ggplot2
# http://stackoverflow.com/questions/3153025/grouped-bar-chart-with-ggplot2-and-already-tabulated-data