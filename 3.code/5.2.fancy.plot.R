##################
### a bit of a fancy plot

par(mfrow = c(1,1))

# generate trend data to plot
n.points <- 100
data.to.plot <- rnorm(n = n.points, 
                      mean = seq(0, by = 1, length.out = n.points),
                      sd = 5)

# plot without axes details
plot(data.to.plot, 
     pch = 16, col = 'green',
     axes = FALSE,
     xlab ='', ylab = '')

# create axis
axis(4, at = seq(0, 100, 10), cex.axis = 1.5)
axis(2, at = seq(0, 100, 20), cex.axis = .5)

# create axes labels
mtext(side = 1, line = 3, 'X', cex = 3)
mtext(side = 2, line = 1.5, 'Y', cex = .5)

# add a curve
curve(1 * x, add = TRUE, col = 'blue', lwd = 2)

# add legend
legend( x = 'topleft',
        legend = 'Regression line',
        lty = 1,
        col = 'blue',
        bty = 'n',
        cex = 2, lwd = 3)

# add tenxt in the middle of the lot
text(x = 50, y = 60, label = 'FAKE', cex = 5, col = 'red') 