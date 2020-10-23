dim1 <- 1000000
dim2 <- 100

# create matrix with dim1 and dim"
data.speed.test <- matrix(rnorm(n = dim1 * dim2), nrow = dim1)

# with a loop compute mean per column and retur time needed
system.time({
  dim1.means.1 <- rep(NA, dim1)
  for(a in 1:dim1){
    dim1.means.1[a] <- mean(data.speed.test[a , ])
  }
})

# with apply compute mean per column and return time needed
system.time({
    dim1.means.2 <- apply(data.speed.test, 1, mean)
})
