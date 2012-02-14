

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

# Tukey HSD for comparison of Assessment group means. 

tukey.Assess <- TukeyHSD(aov(rating_avg ~ Assessment, data = feed.df), ordered= TRUE)

## Proportional odds model for ratings

# 20000 as a rough minimum to see a reasonable number of rated articles, may show more later

assess.init <- polr(Assessment ~ rating_avg + log(length) + log(sum_count), data = feed.df[sample(nrow(feed.df), size = 20000), ])

# Will take a while to run. here for some color more than anything else. 

assess.coefs <- replicate(500, coef(polr(Assessment ~ rating_avg + log(length) + log(sum_count), data = feed.df[sample(nrow(feed.df), size = 20000), ])))