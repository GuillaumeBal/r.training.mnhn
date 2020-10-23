### lowess(example) ====================================================================

# creating noisy time series with a sinus signal
n.time.steps <- 100 # number of data
time.seq <- 1:n.time.steps # time index
n.waves <- .7 # number waves during time period
length.wave <- n.time.steps / n.waves # number of time steps for complete wave
wave <- 5 + 2 * sin(2 * pi * time.seq / length.wave) # create weave trend
wave.noisy <- rnorm(n = n.time.steps, mean = wave, sd = 1.5) # add noise

# plot the data
plot(time.seq, wave.noisy, pch = 16,
     xlab = 'Time step',
     ylab = 'My time series')
# add lowess curves with different degrees
lines(lowess(wave.noisy, f = 1), col = 'red', lwd = 2)
lines(lowess(wave.noisy, f = 1 / 3), col = 'blue', lwd = 2)
lines(lowess(wave.noisy, f = 1 / 10), col = 'green', lwd = 2)
# add a legend
legend('bottomleft', 
       c('f = 1', 'f = .3', 'f = .1'),
       lty = c(1, 1, 1),
       col = c('red', 'blue', 'green'),
       bty = 'n')