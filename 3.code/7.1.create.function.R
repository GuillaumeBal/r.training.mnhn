# Examples about creating functions ============================


rm(list = ls())

# right and try function summing 2 integers

sum.2.integers <- function(a, b){
  to.return <- a + b
  return(to.return)
}

sum.2.integers(a = 1, b = 1)

sum.2.integers(a = 1, b = '1')

### same function with stop ----------------------------------

sum.2.integers <- function(a, b){
  # check arguments are integers, stop if one is not
  if(!is.integer(a) | !is.integer(b)){
    stop('at least one argument is not an integer')
  }
  to.return <- a + b
  return(to.return)
}

sum.2.integers(a = 1, b = 1)

sum.2.integers(a = 1, b = '1')

# warnings number not integer -------------------------------

sum.2.integers <- function(a, b){
  # check arguments are numbers, stop if one is not
  if(!is.numeric(a) | !is.numeric(b)){
    stop('at least one argument is not a number')
  }
  # check arguments are integers, return warning rounding if not
  if(!is.integer(a) | !is.integer(b)){
    warning('at least one value is not an integer, result rounded')
  }
  to.return <- round(a + b, 0)
  return(to.return)
}

sum.2.integers(a = 1, b = 1)

sum.2.integers(a = 1, b = '1')

sum.2.integers(a = 1, b = 1.1)

# add default value to arguments -------------------------------------------

linear.equation <- function(x, a = NULL, b = NULL){
  if(is.null(a)) a <- 1
  if(is.null(b)) b <- 0
  y <- a * x + b
  return(y)
}

linear.equation(x = 2, a = 4, b = 5)
linear.equation(3)

# if you wanna play with debug -----------------------------------------------

# base function
sum.2.integers <- function(a, b){
  to.return <- a + b
  return(to.return)
}

# this is alright 
sum.2.integers(a = 1, b = 2)

#this is not
sum.2.integers(a = 1, b = '1')

# use debug to find eroor
debug(sum.2.integers)
sum.2.integers(a = 1, b = '1')
undebug(sum.2.integers)

# check a few thing about a function =============================================================

# get list of arguments
formals(mean.vector)

# get core of the function
body(mean.vector)

# know where its is stored
environment(mean.vector)


# function computing the mean of vector ====================================

mean.vector <- function(vec, na.rm = FALSE){
  # if vec is a vector, returns its mean
  if(is.vector(vec)){
    sum.vec <- sum(vec, na.rm = na.rm)
    ## length.vec <- length(vec)
    length.vec <- sum(! is.na(vec))
    return(sum.vec / length.vec)
  }else{ # if vec is not a vector print error message
    print('Vec is not a vector')
 }
}

my.matrix <- rbind(1:4, 1:4)
my.vector <- c(1:4, NA)

mean.vector(my.vector)
mean.vector(my.vector, na.rm = TRUE)
mean.vector(my.matrix)