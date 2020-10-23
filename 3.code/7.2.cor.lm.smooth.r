# create time series with trends ==============================================================

time.series.length <- 50

# trends
trend.1 <- 10 + seq(1:time.series.length) / time.series.length
trend.2 <- 8 + seq(1:time.series.length) / (time.series.length / 3)
plot(trend.1, trend.2)

# noisy data
time.series.1 <- trend.1 + rnorm(n = time.series.length, mean = 0, sd = .2)
time.series.2 <- trend.2 + rnorm(n = time.series.length, mean = 0, sd = .35)
plot(time.series.1, time.series.2)


# run somes statistical tests / analyses ===========================================================

# calculate correlation
cor.test(time.series.1, time.series.2)

# store lm outputs in object and use summary
lm.results <- lm(time.series.2 ~ time.series.1)
summary(lm.results)

plot(time.series.1, time.series.2) 

# use lm parameters to draw regression line on plot
curve(lm.results$coefficients[1] + lm.results$coefficients[2] * x,
      lwd = 2, col = 'blue', add = T)