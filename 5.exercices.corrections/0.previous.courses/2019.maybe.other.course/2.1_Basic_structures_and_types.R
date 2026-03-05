###############
## 2.1 Vectors:
###############

## =======================
## Exercises 2.1-1

## 1) Create a vector 'a' containing the values 
##    10, 5, 4.1 and 100. (function c)

a <- c(10, 5, 4.1, 100)
a

## 2) Which is its class? Its mode?

class(a)
mode(a)

## 3) Add 2 to each element of a.

a + c(2, 2, 2, 2)
a + 2

## 4) Calculate its log in base 10 (use the help if needed).

?log
log(x = a, base = 10)
log10(x = a)

## 5) Create a vector of two elements with today and 
##    yesterday's date (use Sys.Date)

Sys.Date()
as.numeric(Sys.Date())

Sys.Date() - 1

c(Sys.Date(), Sys.Date() - 1)

Sys.Date() - c(0, 1)

## =======================
## Exercises 2.1-2

## 1) Create a random vector "x" of length 10, with values
##    comprised between 0 and 10 (function runif(.), use the help).
x <- runif(n = 10, min = 0, max = 10)
x

## 2) Which elements of this vector are greater than 5?

## 2.1) Write and store the test
x > 5

test5 <- x > 5

## 2.2) Use which
which(test5)

which(x > 5)


## 3) Test if sqrt(x)^2 equals x in all cases.
sqrt(x)^2 == x

all.equal(sqrt(x)^2, x)


## !!! if one = omitted
# sqrt(x)^2 = x
# 
# x = sqrt(x)^2

## 4) Same for sqrt(x^2)
sqrt(x^2) == x

## 5) (optional) Can you show that sqrt(x)^2 equals
##    x ± the machine precision 
##    (use sqrt(.Machine$double.eps) as precision)?

## =======================
## Exercises 2.1-3

## 1) Test if sqrt(x)^2 equals x in all cases (alternative method).
sqrt(x)^2 == x
all(sqrt(x)^2 == x)
all(sqrt(x^2) == x)

## 2) Which elements of x are greater than 8 or less than 4?
x > 8
x < 4

x > 8 | x < 4

which(x > 8 | x < 4)

test <- x > 8 | x < 4
which(test)

## 3) Which elements have values between 4 and 8 ?
! test
which(! test)

which(x > 4 & x < 8)

## 4) Extract elements of the vector 
##    setLet <- c("A", "b", "Z", "g", "J") 
##    which are small caps (reference: predefined vector letters).

setLet <- c("A", "b", "Z", "g", "J")
letters

is.element(setLet, letters)
setLet %in% letters

which(setLet %in% letters)

## =======================
## Exercises 2.1-4

## 1) Create a sequence (function seq) from 2 to 11, with an increment of 3.
 seq(from = 2, to = 11, by = 3)

## 2) Create a sequence  starting at 2, 
##      * with the length of the vector x (from exercises section 2.2.7 - assume you don't know its actual length) 
##      * with an increment of 1
##      * in two different ways . of which one using ":"!
x <- runif(n = 10, min = 0, max = 10)
x

?seq

seq(from = 2, by = 1, along.with = x)
seq(from = 2, by = 1, length.out  = length(x))

(1:length(x)) + 1

  
## 3) Create the same sequence using seq_along().

seq_along(x) + 1

## 4) Reverse x and store it in x2.

x2 <- rev(x)
x2

## 5) Create a sequence from 100 to 0 by an increment of 5
seq(from = 100, to = 0, by = -5)

## =======================
## Exercises 2.1-5

## 1) Create a vector "seq5" as a sequence from 2 to 6

2:6

seq(from = 2, to = 6, by = 1)

seq5 <- 2:6
seq5

## 2) Name the elements as the first five letters (from the vector letters).
letters
letters[1:5]

names(seq5)

names(seq5) <- letters[1:5]

names(seq5)
seq5

## 3) Select the elements 2 and 4 using two different ways (position / name).
idx1 <- c(2, 4)
seq5[idx1]

idx2 <- c("b", "d")
seq5[idx2]

## 4) Which elements are greater than 3? (use a test)
seq5 > 3

test1 <- seq5 > 3

seq5[test1]

seq5[seq5 > 3]

## 5) How to extract the last element
 (idxLast <- length(seq5))
seq5[idxLast]

## 6) Run size <- rnorm(n = 50, mean = 30, sd = 3)
##    * Which "individuals" have a size of at least 33?
##    * How many are they?

size <- rnorm(n = 50, mean = 30, sd = 3)

test33 <- size >= 33
posIndiv33 <- which(test33)

length(posIndiv33)

indiv33 <- size[test33]
length(indiv33)

sum(test33)  ## implicit conversion of logical to numeric (TRUE = 1/FALSE = 0)


## 7) . and replace their value by NA.
## a <- size[test33] ## Store, does not modify.

size[test33] <- NA
size
