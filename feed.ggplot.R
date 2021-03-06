#GGplot2 plots

## Unfortunately these are less straightforward than base graphics, as ggplot has 
## its own syntax and frame for thinking

## All of these plots require "ggplot2" to be installed and loaded

#
# Eventually the three below will be combined into one function for ease of presentation
#

# Density estimate of various project ratings. Not dispositive but a nice low clutter view

density.rating.plot <- function() {
  ggplot(data = feed.df[feed.df[,"sum_count"] > 5, ], aes(rating_avg)) + geom_density(aes(colour = Assessment)) + scale_y_continuous(name = "Estimated Density") +
  opts(axis.title.x = theme_blank(), title = expression("Distribution of Rating Averages by Project Quality Assessment"))
}

# Number of ratings by project quality, as a density estimate.
# Only "rated" articles are included.
# Very similar to density.ql.plot()

density.count.plot <- function() {
  ggplot(data = feed.rated, aes(log(sum_count))) + geom_density(aes(colour = Assessment)) + 
  scale_y_continuous(name = "Estimated Density") + scale_x_continuous(name = "log Rating Counts") +
  opts(title = expression("Distribution of log Rating Counts for Project Quality Assessed Articles"))
}

# Article length by project quality, as a density estimate.
# Very similar to density.ql.plot()

density.length.plot <- function() {
  ggplot(data = feed.df, aes(log(length))) + geom_density(aes(colour = Assessment)) + 
    scale_y_continuous(name = "Estimated Density") + scale_x_continuous(name = "log Article Length") +
    opts(title = expression("Distribution of log Article Length for Project Quality Assessed Articles"))
}

# Binning for simpler graphs

prebin <- as.character(feed.df[, "Assessment"])
basic.bin <- factor(gsub("Former\\s+|\\s+Nominee", "", prebin), levels = c("Unassessed", "Good Article", "Featured List", "Featured Article"), ordered = TRUE)

assess.bin <- prebin
assess.bin[assess.bin != "Unassessed"] <- "Assessed"
assess.bin <- factor(assess.bin)

feed.df[, "Simplified Assessment"] <- basic.bin
feed.df[, "Binary Assessment"] <- assess.bin

ggplot(data = feed.df, aes(log(length))) + geom_density(aes(fill = `Simplified Assessment`), alpha = 0.4) + 
  scale_y_continuous(name = "Estimated Density") + scale_x_continuous(name = "log Article Length")  +
  opts(axis.title.y = theme_text(face="bold", size=15, angle=90)) +
  opts(axis.title.x = theme_text(face="bold", size=15))


# Boxplot for different project quality assessments. Using a trimmed sample
# in order to make the "Unassessed" IQR reasonable

rate.box.plot <- function() {
  ggplot(data = feed.df, aes(Assessment, rating_avg)) + geom_boxplot(aes(fill = Assessment)) + scale_y_continuous(name = "Average of Feedback Categories") +
  opts(axis.title.x = theme_blank(), axis.text.x = theme_blank(), title = expression("Box Plot Comparing Feedback Rating by Project Quality Assessment"))
}

ggplot(data = feed.df, aes(Assessment, rating_avg)) + geom_boxplot(aes(fill = Assessment)) + scale_y_continuous(name = "Average of Feedback Within Categories") +
  opts(axis.text.x = theme_blank()) + xlab('') + opts(axis.title.y = theme_text(face="bold", size=13, angle=90))

# Takes a while to plot.
# Scatterplot, colored by project quality assessment.

length.scatter <- function() {
  # Dumb hack for fixed legend size and alpha, see http://stackoverflow.com/questions/5290003/how-to-set-legend-alpha-with-ggplot2
  dummy.df <- feed.df[1,]
  dummy.df[, "length"] <- NaN
  ggplot() + geom_point(data = feed.df, aes(log(length), rating_avg, colour = Assessment), alpha = I(0.9), size = I(1), position= position_jitter(h = 1), legend = FALSE) +
  geom_point(data = dummy.df, aes(log(length), rating_avg, colour = Assessment), alpha = 1.0, size = I(3), position= position_jitter(h = 1), na.rm=TRUE) +
  scale_y_continuous(name = "Approximate Feedback Average") + scale_x_continuous(name = "log Article Length") + opts(title = expression("Article Ratings by Length and Project Quality Measure"), axis.text.y = theme_blank()) 
}


ggplot() + geom_point(data = feed.df, aes(log(length), rating_avg, colour = `Simplified Assessment`), alpha = I(0.9), size = I(1), position= position_jitter(h = 1)) +
  scale_y_continuous(name = "Approximate Feedback Average", breaks = NULL) + 
  scale_x_continuous(name = "log Article Length") + 
  opts(axis.text.y = theme_blank())





# Uncolored scatterplot w/ a (robust) linear regression line drawn through
# Eventually I'd like to add a few more

# Actual value for the robust regression
# rlm(I(rating_avg) ~ log(sum_count), data = feed.df[log(feed.df[, "length"]) > 4 & log(feed.df[, "sum_count"]) > 2.5, ])
 
count.scatter <- function() {
  try(require(MASS))
  ggplot(data = feed.df[log(feed.df[, "length"]) > 4 & log(feed.df[, "sum_count"]) > 2.5, ], aes(log(sum_count), rating_avg)) + 
    geom_point(position= position_jitter(w = 0.2, h = 0.05), alpha = I(0.5), size = I(1)) + stat_smooth(method = MASS::rlm) +
    geom_text(aes(9, 3.5, label = "slope = 0.0698\n             (0.0023)"), size = I(4)) +
    scale_y_continuous(name = "Average of Feedback Categories") + scale_x_continuous(name = "log Ratings Received per Article") +
    opts(title = expression("Relationship between Number of Ratings Received and Average Rating"))
}

