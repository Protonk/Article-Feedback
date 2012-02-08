### Some simple regression models 

## Build an aux. regression for length (4th order polynomial) and fit categorical ratings to those residuals
## 

length.lm.4 <- lm(rating_avg ~ log(length) + I(log(length)^2) + I(log(length)^3) + I(log(length)^4) , data = feed.df)
simple.aux.lm <- lm(resid(length.lm.4) ~ log(sum_count) + Assessment, data = feed.df)

# Shapiro-Wilk test for rated and unrated articles. Unsurprisingly, average ratings for both are not 
# normally distributed. 

# Commented out for now. 

# sw.ratings <- by(feed.df[feed.df$Assessment != "Unassessed",], feed.df[feed.df$Assessment != "Unassessed", "Assessment"] , function(x) shapiro.test(x[,"rating_avg"]))

# Basic linear (non-robust) regression for explanatory variables of interest
simple.lm <- lm(rating_avg ~ log(length) + log(sum_count) + Assessment, data = feed.df)

# Tukey HSD for comparison of Assessment group means. 

tukey.Assess <- TukeyHSD(aov(rating_avg ~ Assessment, data = feed.df), ordered= TRUE)






