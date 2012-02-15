library(boot)

### It will take a while to run simply because the 3 linear models aren't tucked away in
### functions on demand. Have patience, a < 2.0 GHz C2D should run this inside of 30 seconds.

## Summary Stats

#It shouldn't be this hard to get a fixed formatted table of summary stats
simpleSummary <- function() {
  # Eventually this will go into a table. So we just throw a character vector in there for subheadings
  Variable <- character(9)
  Variable[seq(2, 8, 3)] <- c("length",  "sum_count", "rating_avg")
  fin.mat <- matrix(0,7,9)
  rownames(fin.mat) <- levels(feed.df[, "Assessment"])
  colnames(fin.mat) <- rep(c("mean", "median", "sd"), 3)
  # Only seems to make sense for length, count and average rating. 
  for (i in levels(feed.df[, "Assessment"])) {
    fin.mat[i, seq(1, 7, 3)] <- unname(apply(feed.df[feed.df[, "Assessment"] == i, c("length",  "sum_count", "rating_avg")], 2, mean))
    fin.mat[i, seq(2, 8, 3)] <- unname(apply(feed.df[feed.df[, "Assessment"] == i, c("length",  "sum_count", "rating_avg")], 2, median))
    fin.mat[i, seq(3, 9, 3)] <- unname(apply(feed.df[feed.df[, "Assessment"] == i, c("length",  "sum_count", "rating_avg")], 2, sd))
  }
  #this looks ugly as sin (because it is now a character matrix) but it will print fine w/ xtable
  rbind(Variable, signif(fin.mat, 4))
}

### Some simple regression models 

## Build an aux. regression for length (4th order polynomial) and fit categorical ratings to those residuals
## 

length.lm.4 <- lm(rating_avg ~ log(length) + I(log(length)^2) + I(log(length)^3) + I(log(length)^4) , data = feed.df)
simple.aux.lm <- lm(resid(length.lm.4) ~ log(sum_count) + Assessment, data = feed.df)

# Shapiro-Wilk test for rated and unrated articles. Unsurprisingly, average ratings for both are not 
# normally distributed. 

# Multiple re-sampling for SW statistic. log(log(length)) appears to be normal. Soo...log(length) is log-normal!
# I think

# Reject null hypothesis that ratings are normally distributed (even after censoring at > 5)
# Not a surprise that ratings were non-normal

lgn.len <- log(log(feed.df[, "length"]))
rating.sm <- feed.df[feed.df[, "sum_count"] > 5, "rating_avg"]
shap.len.out <- replicate(2000, unlist(shapiro.test(sample(lgn.len, 100, replace = TRUE))[c(1,2)]))
shap.rating.out <- replicate(2000, unlist(shapiro.test(sample(rating.sm, 100, replace = TRUE))[c(1,2)]))
remove(lgn.len, rating.sm)


# Basic linear (non-robust) regression for explanatory variables of interest

simple.lm <- lm(rating_avg ~ log(length) + log(sum_count) + Assessment, data = feed.df)

# Check the linear regression against non-parametric boostrap. 200 resamples is
# not generally enough for a production grade result but it does let us know the basics 
# of the lm aren't too ifluenced by h-sked.
bootSimpleLm <- function(R = 200) {
  # provides a function to iteratively refit model
  refitSimple <- function(data, i) {
    coef(lm(rating_avg ~ log(length) + log(sum_count) + Assessment, data = data[i, ]))
  }
  # so the results can be reproduced if needed.
  set.seed(123)
  boot(feed.df, refitSimple, R = R)
}

# Tukey HSD for comparison of Assessment group means. 

tukey.Assess <- TukeyHSD(aov(rating_avg ~ Assessment, data = feed.df), ordered= TRUE)

## Proportional odds model for ratings


# Build for a reasonable binary choice (assessed vs. not)

# Chose a binomial regression over a prop. odds logit because the proportion is miniscule regardless
# So all assessment factors are rolled into "Assessed"
# this will take a while to run
coefBinPlot <- function(replications = 500) {
  coefBin <- function(replications) {
    feed.bin <- feed.df
    assessed <- feed.df[, "Assessment"] %in% levels(feed.df[, "Assessment"])[2:7]
    assessment.char <- rep("Unassessed", nrow(feed.bin))
    assessment.char[assessed] <- "Assessed"
    feed.bin[, "Assessment"] <- factor(assessment.char)
    # relevel purely to change sign of coefficients. :)
    feed.bin[, "Assessment"] <- relevel(feed.bin[, "Assessment"], "Unassessed")
    # not truly a bootstrap but we can get some non-parametric spread on coefficients
    assess.bin.coef <- replicate(replications, coef(glm(Assessment ~ rating_avg + log(length) + log(sum_count), family = "binomial",data = feed.bin[sample(nrow(feed.bin), size = 30000), ])))
    return(assess.bin.coef)
  }
  # nearly identical to the plotting function from the Assessed sample
  bin.coef <- bin.return.coef[-1,]
  coef.full.df <- data.frame(cbind(bin.coef[1,], bin.coef[2,], bin.coef[3,]))
  names(coef.full.df) <- c("Rating", "Length", "Count")
  # Plots coefficients against ~ 0 to 2.0, adds some jitter to allow overplotting to indicate distribution
  # Colors chosen from color brewer
  plot(seq(-0.2, 2.1, length.out = 10), seq(0, 3.5, length.out = 10), type = "n", yaxt = "n", 
       xlab = "Bootstrapped Coefficient Estimates", ylab = "", frame.plot = FALSE,
       main = "Relationship of Variables and log Odds of Project Quality Assessment")
  plotcol <- rbind(c(252, 141, 98), c(141, 160, 203), c(166, 216, 84))
  for ( p in 1:3) {
    points( coef.full.df[, p], jitter(rep(p, 500)), pch = 20, col = rgb(red = plotcol[p,1], green = plotcol[p,2], blue = plotcol[p,3], alpha = 51, max = 255))
    segments(x0 = median(coef.full.df[, p]), y0 = 0, y1 = p, lty = 2, col = rgb(red = plotcol[p,1], green = plotcol[p,2], blue = plotcol[p,3], max = 255))
    text( x = quantile(coef.full.df[, p], 0.4), y = p + 0.15, labels = paste(names(coef.full.df[p]), "=", signif(median(coef.full.df[, p]), 3), sep = " "), pos = 3)
  }
}

# we can handle multiple levels meaningfully when we restrict to rated articles.
# base factor is a little more arbitrary here. 
plotCoefRated <- function(replications = 1000) {
  coefMatRated <- function(replications) {
    rated.coef <- replicate(replications, coef(polr(Assessment ~ rating_avg + log(length) + log(sum_count), data = feed.rated[sample(nrow(feed.rated), size = 1000), ])))
    return(rated.coef)
  }
  # Not actually necessary as I'm working with base graphics, but handy
  coef.df <- data.frame(cbind(rated.coef[1,], rated.coef[2,], rated.coef[3,]))
  names(coef.df) <- c("Rating", "Length", "Count")
  # Plots coefficients against ~ -0.5 to 0.5 adds some jitter to allow overplotting to indicate distribution
  # Colors chosen from color brewer
  plot(seq(-0.8, 0.8, length.out = 10), seq(0, 3.3, length.out = 10), type = "n", yaxt = "n", 
       xlab = "Bootstrapped Coefficient Estimates", ylab = "", frame.plot = FALSE,
       main = "Relationship of Variables and log Odds of Increasing Project Quality Assessment")
  plotcol <- rbind(c(252, 141, 98), c(141, 160, 203), c(166, 216, 84))
  for ( p in 1:3) {
    points( coef.df[, p], jitter(rep(p, 1000)), pch = 20, col = rgb(red = plotcol[p,1], green = plotcol[p,2], blue = plotcol[p,3], alpha = 51, max = 255))
    segments(x0 = median(coef.df[, p]), y0 = 0, y1 = p, lty = 2, col = rgb(red = plotcol[p,1], green = plotcol[p,2], blue = plotcol[p,3], max = 255))
    text( x = quantile(coef.df[, p], 0.4), y = p + 0.15, labels = paste(names(coef.df[p]), "=", signif(median(coef.df[, p]), 3), sep = " "), pos = 2)
  }
}