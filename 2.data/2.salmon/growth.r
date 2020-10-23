l0 <- 15 # length going at sea
l.inf <- c(100, 110) # theoritical max length, different by country
k <- .5 # yearly growth rate start
k.year <- k * (1 - .01 * 0:(year.end - year.start))# growth rate decreasing with year
sd.length <- 5 # var around mean
t0 <- -.91 # parameter shifting von bertalanffy growth curve

# mean length faben version von bertqlanffy growth model
mean.length <- l0 +
  (l.inf[as.numeric(as.factor(salmons$country))] - l0) *
  (1 - 
     exp(- k.year[salmons$year - min(salmons$year) + 1] * 
           (salmons$sea.age - t0))) 

hist(mean.length)

# add gamma noise
length <- rgamma(n = dim(salmons)[1],
                 shape = (mean.length / sd.length) ^ 2,
                 scale = (sd.length ^ 2) / mean.length) 
hist(length)
min(length)
salmons$length <- round(length, 1)
# add noise with normal and coeffcient of variation
#length <- rnorm(n = dim(salmons)[1], mean = mean.length, sd = mean.length * .1) 
#length <- round(length, 1)
#hist(length)