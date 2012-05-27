library(ggplot2)

toplist <- head(order(tabulate(indrat[, "Page_ID"]), decreasing = TRUE), 1000)
# Lazy way to get a top list of rated articles
toptitles <- unique(indrat[indrat[, "Page_ID"] %in% toplist, c("Page_ID", "Title")])

#  ddply(subset(indrat, Page_ID == 23680998), .(Date), "nrow")

with(ddply(subset(indrat, Page_ID == 23680998), .(Date), "nrow"), plot(Date, seq(1, 7, length.out = 156), type = "n", frame.plot=FALSE))

for (i in 1:6) {
  with(ddply(subset(indrat, Page_ID == toplist[i]), .(Date), "nrow"), lines(Date, log(pmax(nrow, 1)), type = "l", col = i))
}


brevik <- ddply(subset(indrat, Page_ID == 32501324), .(Date), summarise, mean = mean(Mean))
brevik[, "Ratings"] <-  ddply(subset(indrat, Page_ID == 32501324), .(Date), "nrow")[, 2]

with(brevik, plot(Date, mean, col = "blue", pch = 20, ylim = c(0,6), frame.plot = FALSE, type = "b", cex = log(brevik[, "Ratings"]) + 1))

bieber <- ddply(subset(indrat, Page_ID == 23680998), .(Date), summarise, mean = mean(Mean))
bieber[, "Ratings"] <-  ddply(subset(indrat, Page_ID == 23680998), .(Date), "nrow")[, 2]

with(bieber, plot(Date, mean, col = "blue", pch = 20, ylim = c(0,6), frame.plot = FALSE, type = "b", cex = log(bieber[, "Ratings"]) + 1))


# We are only interested in rows where the user rated all 4 categories
rating.avgs <- rowMeans(reduced[complete.cases(reduced), ])

count.table <- table(rating.avgs)

# Builds factors of the rating avgs (ordered by count)
# and another factor for just the integers (this is a ggplot2 thing)
count.out <- factor(rating.avgs, levels = names(count.table), ordered = TRUE)
integers <- factor(count.out, levels = as.character(1:5))
# placed into a data frame for easier plotting
preplot <- data.frame(count.out, integers)

# Plot frequency of averages
qplot(count.out, fill = integers, geom = "bar", data = preplot) + 
  opts(legend.position = "none", title = expression("Averages of ratings where users rated all four categories")) + 
  scale_y_continuous(name = "") +  scale_x_discrete(name = "")






