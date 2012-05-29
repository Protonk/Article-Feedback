
# Separate by page_ID
# can accept multiple IDs
convertID <- function(id, data = indrat) {
  ind <- ddply(data[data[, "Page_ID"] %in% id,], c("Title", "Date"),
               function(df) {
                             c(mean(df[, "Mean"]), sd(df[, "Mean"]), nrow(df))
                        })
  names(ind) <- c("Title", "Date", "Mean", "SD", "Ratings")
  ind[, "Title"] <- as.factor(ind[, "Title"])
  return(ind)
}

# input data frame should have
# Date, Ratings, Mean and SD columns
# named as such
rollingPrePlot <- function(data) {
  # 3 period moving average (of sorts)
  # Derived from http://stackoverflow.com/a/4862334/1188479
  maReplace <- function(x, n = 3){
    pre <- as.numeric(filter(x,rep(1/n,n), sides=1))
    # fills in first few values w/ originals
    c(x[1:n-1], pre[n:length(pre)])
  }
  data[, c("Mean", "SD")] <- c(maReplace(data[, "Mean"]),
                               maReplace(data[, "SD"]))
  # Here to make plotting easier for
  # different date ranges 
  date.range <- seq.Date(from = min(data[, "Date"]),
                    to = max(data[, "Date"]),
                    by = 1)
  return(list(data = data[, c("Title", "Date", "Mean", "SD", "Ratings")],
              range = list(x = date.range, 
                           y = seq(from = 0, to = 5.5, length.out = length(date.range)))))
}

preplot <- rollingPrePlot(convertID(c(32501324, 23680998)))

actualPlot <- function(input = preplot, title = "Comparison of Articles") {
  plot(input$range$x, input$range$y, frame.plot = FALSE, type = "n",
       ylab = "Rating Averages", xlab = "Dates", main = title)
  articles  <- dlply(input$data, .(Title))
  lapply(articles, function(x) lines(x$Date, x$Mean))
}
# lol base plotting is dumb
qplot(Date, Mean, data = preplot$data, colour = Title, geom = "line")
actualPlot()


with(rollingPrePlot(convertID(23680998)), {
    plot(Date, Mean, type = "l", col = "blue", frame.plot = FALSE, ylim = c(0, 5.5))
    lines(Date, Upper, col = "grey")
    lines(Date, Lower, col = "grey")
  }
)


