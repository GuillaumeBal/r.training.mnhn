###############
## 2.1 Vectors:
###############


## =======================
## Exercises 1.2-3


## 2) Create variables 
##    a = 2.3, b = 3.5, c = -2, d = 2, e= .011

a <- 2.3
b <-  3.5
c <- -2
d <- 2
e <- 0.011

## 3) Compute following equation Y = a*(b + c) + d / e

a * (b + c) + d / e

## 4) Store result

Y <- a * (b + c) + 
  d / e

# Y <- a * (b + c) 
#   + d / e   ## !!! two different valid instructions

Y  # just checking the result
Y + 2


## =======================
## Exercises 1.2-4

## 1) Create a vector using runif()

random.values <- runif(n = 5, min = 2, max =6)


# Note the default value (1) for min and max if omitted:
runif(n = 10)

## 2) Compute its mean

mean.rd <- mean(x = random.values)
# mean.rd <- mean(x = runif(n = 5, min = 2, max =6)) # could be nested.
mean.rd

## 3) Compute its standard deviation

??"standard deviation"

sd.rd <- sd(x = random.values)

## 4) Look for function generating samples from a normal distribution. (topic search)

help.search("normal distribution")
# Look at the results for package stats::

## 5) Repeat steps 1 - 3 with the Normal distribution.


norm.rd <- rnorm(n = 30)
norm.rd

mean(norm.rd)  # close to 0
sd(norm.rd)    # close to 1
