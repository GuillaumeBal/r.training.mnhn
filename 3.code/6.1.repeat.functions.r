####################################################################
###         use apply, lapply, sapply, tapply and by             ###
####################################################################


# create some data of particular class ==========================================================

### a data frame
my.data.frame <- as.data.frame(matrix(sample(size = 500, x = 1:10, replace = T),
                                      ncol = 5)
)
colnames(my.data.frame) <- letters[1:dim(my.data.frame)[2]]

### a vector
my.vector <- my.data.frame[, 1]

### a list
my.list <- as.list(c(my.data.frame, my.data.frame, my.data.frame))

# use the different function to repeat an action over dimensions ======================================================

# mean per column for dataframe
mean.colums.df <- apply(my.data.frame, MARGIN = 2,
                        FUN = mean)

# print quantiles per columns
apply(my.data.frame, MARGIN = 2,
      FUN = quantile, probs = c(0.20, 0.45, 0.65))

### lapply and sapply for mean over elements of list
lapply(my.list, FUN = mean)
sapply(my.list, FUN = sd)    

# by and tapply to repeat accross levels =================================================================

# add variable of class factor to my.data.frame
my.data.frame$fac1 <- sample(size = dim(my.data.frame)[1], 
                             c('level1', 'level2'), replace = TRUE)
my.data.frame$fac1 <- as.factor(my.data.frame$fac1)

# add second variable of class factor to my.data.frame
my.data.frame$fac2 <- sample(size = dim(my.data.frame)[1], 
                             c('factor1', 'factor2'), replace = TRUE)
my.data.frame$fac2 <- as.factor(my.data.frame$fac2)

# use by to compute some mean
by(data = my.data.frame$a, 
   INDICES = list(my.data.frame$fac1, my.data.frame$fac2),
   FUN = mean)

# use tapply
#tapply(X = my.data.frame$a, 
#       INDEX = list(my.data.frame$fac1, my.data.frame$fac2),
#       FUN = mean)