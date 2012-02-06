#GGplot2 plots

## Unfortunately these are less straightforward than base graphics, as ggplot has 
## its own syntax and frame for thinking

## All of these plots require "ggplot2" to be installed and loaded


# Density estimate of various project ratings. Not dispositive but a nice low clutter view

density.ql.plot <- function() {
  den.ggplot <- ggplot(data = feed.red.5, aes(sum_ratings/sum_count)) + geom_density(aes(colour = Rating)) + scale_y_continuous(name = "Estimated Density") +
    opts(axis.title.x = theme_blank(), title = expression("Distribution of Rating Averages by Project Quality Measure"))
  den.ggplot
}

# Number of ratings by project quality, as a density estimate.
# Only "rated" articles are included.
# Very similar to density.ql.plot()

density.count.plot <- function() {
  count.ggplot <- ggplot(data = feed.rated, aes(log(sum_count))) + geom_density(aes(colour = Rating)) + 
  scale_y_continuous(name = "Estimated Density") + scale_x_continuous(name = "log Rating Counts") +
  opts(title = expression("Distribution of log Rating Counts for Project Quality Rated Articles"))
  count.ggplot
}

# Boxplot for different project quality assessments. Using a trimmed sample
# in order to make the "Unrated" IQR reasonable

rate.box.plot <- function() {
  box.ggplot <- ggplot(data = feed.red.5, aes(Rating, sum_ratings/sum_count)) + geom_boxplot(aes(fill = Rating)) + scale_y_continuous(name = "Average of Feedback Categories") +
  opts(axis.title.x = theme_blank(), axis.text.x = theme_blank(), title = expression("Box and Whiskers Plot Feedback Rating by Project Quality Measure"))
  box.ggplot
}

# Takes a while to plot.
# Scatterplot, colored by project quality assessment.

length.scatter <- function() {
  # Dumb hack for fixed legend size and alpha, see http://stackoverflow.com/questions/5290003/how-to-set-legend-alpha-with-ggplot2
  dummy.df <- feed.red.5[1,]
  dummy.df[, "length"] <- NaN
  length.ggplot <- ggplot() + 
    geom_point(data = feed.red.5, aes(log(length), sum_ratings/sum_count, colour = Rating), alpha = I(0.9), size = I(1), position= position_jitter(h = 1), legend = FALSE) +
    geom_point(data = dummy.df, aes(log(length), sum_ratings/sum_count, colour = Rating), alpha = 1.0, size = I(3), position= position_jitter(h = 1), na.rm=TRUE) +
    scale_y_continuous(name = "Approximate Feedback Average") + scale_x_continuous(name = "log Article Length") + opts(title = expression("Article Ratings by Length and Project Quality Measure"), axis.text.y = theme_blank()) 
  length.ggplot
}

# Uncolored scatterplot w/ a (robust) linear regression line drawn through
# Eventually I'd like to add a few more

# Actual value for the robust regression
# rlm(I(sum_ratings/sum_count) ~ log(sum_count), data = feed.red.5[log(feed.red.5[, "length"]) > 4 & log(feed.red.5[, "sum_count"]) > 2.5, ])
 
count.scatter <- function() {
  try(require(MASS))
  ggplot(data = feed.red.5[log(feed.red.5[, "length"]) > 4 & log(feed.red.5[, "sum_count"]) > 2.5, ], aes(log(sum_count), sum_ratings/sum_count)) + 
    geom_point(position= position_jitter(w = 0.2, h = 0.05), alpha = I(0.5), size = I(1)) + stat_smooth(method = MASS::rlm) +
    geom_text(aes(9, 3.5, label = "slope = 0.0698\n             (0.0023)"), size = I(4)) +
    scale_y_continuous(name = "Average of Feedback Categories") + scale_x_continuous(name = "log Ratings Received per Article") +
    opts(title = expression("Relationship between Number of Ratings Received and Average Rating"))
}

