library(plyr)
library(ggplot2)
library(scales)

byday <- ddply(indrat, "Date", "nrow")
byday[, "Week"] <- cut(byday[, "Date"], "week")

byweek <- ddply(byday, "Week", function(x) sum(x[, "nrow"]))[-27, ]
names(byweek) <- c("Date", "Feedback Recorded")
byweek[, "Labels"] <- as.character(as.Date(byweek[, "Date"]), format = "%b %d")
byweek[, "Deployment"] <- factor(c(rep("1%", 8), rep("5%", 9), rep("100%", 9)))

weekplot <- ggplot(data= byweek, aes(Date, `Feedback Recorded`)) +
    geom_bar(aes(fill = Deployment), alpha = 0.6) + guides(fill=FALSE) + scale_x_discrete(labels = byweek[, "Labels"]) + 
    scale_y_continuous(labels = c("0", "10,000", "20,000", "30,000")) + xlab('') +
    geom_text(data = byweek[4, ], label = "1% Rollout", vjust = -4, size = 9) +
    geom_text(data = byweek[13, ], label = "5% Rollout", vjust = -6, size = 9) + 
    geom_text(data = byweek[23, ], label = "100% Rollout", vjust = -18, size = 9) +
    opts(axis.title.y = theme_text(face="bold", size=15, angle=90))