
avgBoot <- function() {
  rating.avgs <- rowMeans(red.bootstrap[complete.cases(red.bootstrap), ])
  count.table <- table(rating.avgs)
  trimmed.count <- data.frame(Rating = seq(1,5,0.25), Count = as.numeric(count.table))[2:16, ]
  pred.count <- predict(smooth.spline(trimmed.count), x = seq(1,5,0.25))
  fake.prob <- pred.count$y/sum(pred.count$y)
  whole.num <- sum(as.numeric(count.table)[c(1,17)])
  sample(seq(1,5,0.25),
         size = whole.num,
         replace = TRUE,
         prob = fake.prob)
}

red.bootstrap <- indrat[, c("Sourced", "Neutral", "Complete", "Readable")]

sim.avg <- avgBoot()

full.avg <- rowMeans(red.bootstrap, na.rm = TRUE)
# Removes only rows where no rating was offered (~20000)
full.avg <- full.avg[complete.cases(full.avg)]

