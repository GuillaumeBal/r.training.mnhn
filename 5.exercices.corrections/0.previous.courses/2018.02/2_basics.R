## --------------------------------------------------
## 2.1.5

## 1) Create a vector "a" containing the values 10, 5, 4.1 and 100:

a <- c(10, 5, 4.1, 100)
a

## 2) Which is its class? Its mode?

mode(a)
class(a)

## 3) Add 2 to each element of a:
a + 2

## 4) Calculate its log in base 10:
?log
log10(a)
log(x = a, base = 10)

## 5) Create a vector of two elements with today and yesterday date:
td <- Sys.Date()
as.integer(Sys.Date())

# Yesterday:
Sys.Date() - 1
yd <- td - 1

# Vector (different ways):
c(Sys.Date(), yd)
c(td, yd)

Sys.Date() - c(0, 1)

as.numeric(Sys.Date() - c(0, 1))

## --------------------------------------------------
## 2.1.6 - tests

## 1) Create a random vector "x" of length 10, with values comprised between 0 and 10:

?runif
set.seed(12345)  # for reproducibility!
x <- runif(n = 10, min = 0, max = 10)
print(x)

## 2) Which elements of this vector are greater than 5?

## 2.1) Write and store the test:
testx <- x > 5
print(testx)

## 2.2) Use which:
which(testx)

## 3) Test if sqrt(x)^2 equals x in all cases:
sqrt(x)^2 == x
sqrt(x)^2 - x                           # ! machine precisions.

## x = sqrt(x)^2 ##! double ==

## 4) Same for sqrt(x^2):
sqrt(x^2) == x

## 5) Can you show that sqrt(x)^2 equals x ± the machine precision?

abs(sqrt(x)^2 - x) < sqrt(.Machine$double.eps) # Absolute difference to have it unsigned.

## --------------------------------------------------
## 2.1.7:

## 1) Test if sqrt(x)^2 equals x in *all* cases (alternative method):
testsqrt <- sqrt(x)^2 == x
all(testsqrt)

x2 <- runif(n = 10000, min = 0, max = 10)
testsqrt2 <- sqrt(x2^2) == x2
all(testsqrt2)

## 2) Which elements of x are greater than 8 *or* less than 4?
test1 <- x > 8
test2 <- x < 4

testComb <- test1 | test2
(x > 8) | (x < 4)

which(testComb)

## 3) Which elements have values *between* 4 and 8 ?

which((x <= 8) & (x >= 4))
which( ! testComb)                      # or all others compared to 2)!

## 4) Extract elements of the vector setLet <- c("A", "b", "Z", "g", "J")... which are small caps?
setLet <- c("A", "b", "Z", "g", "J")
letters

setLet %in% letters
lower <- is.element(setLet, letters)

which(lower)
which(setLet %in% letters)

## --------------------------------------------------
## 2.1.9 - sequences/rep:

## 1) Create a sequence (function seq)
##    * starting at 2
##    * with the length of the vector x
##    * with an increment of 1

x <- runif(n = 10)

lenx <- length(x)

?seq

seq(from = 2, by = 1, length.out = lenx)
seq(from = 2, by = 1, along.with = x)

1:lenx
(1:lenx) + 1

## 2) Create the same sequence using seq_along():
?seq_along
seq_along(x) # not starting at two.

seq_along(x) + 1

## 3) Reverse x and store it in x2:
x
rev(x)

x2 <- rev(x)
x2

## 4) Create a sequence from 100 to 0 by an increment of 5:
seq(from = 100, to = 0, by = 5) ## error sign of by
seq(from = 100, to = 0, by = -5)

rev(seq(from = 0, to = 100, by = 5))

(20:0) * 5

20:0 * 5


## --------------------------------------------------
## 2.1.11 - indexation:

## 1) Create a vector “seq5” as a sequence from 2 to 6:
2:6
seq5 <- 2:6
seq5


## 2) Name the elements as the first five letters:
names(seq5)

letters
letters[1:5]
letters[seq5]

names(seq5) <- letters[1:5]
names(seq5)
seq5


## 3) Select the elements 2 and 4 using two different ways (position / name):

seq5[c(2, 4)]
seq5[c("b", "d")]

## 4) Which elements are greater than 3? (use a test):
idx <- seq5 > 3
idx2 <- which(seq5 > 3)

seq5[idx]
seq5[idx2]

## 5) How to extract the last element?
seq5
lastElement <- length(seq5)

seq5[lastElement]
seq5[lastElement - 1]


## 6) Run size <- r	norm(n = 50, mean = 30, sd = 3)
size <- rnorm(n = 50, mean = 30, sd = 3)

##    * Which “individuals” have a size of at least 33?
size >= 33
ixd33 <- which(size >= 33)

##    * How many are they?
length(ixd33)
sum(size >= 33)

## 7) … and replace their value by NA:
size[ixd33]

size[ixd33] <- NA
size

# alternative:
size <- replace(size, ixd33, NA)


## --------------------------------------------------
## 2.2.3 - Other structures:

## 1) Create a variable (data.frame) people based on the table in .\2.data\1.people\

people <- data.frame(name = c("guillaume", "yves", "jack", "maximo",
                              "hugo", "agnieszka", "anthony", "niamh",
                              "dave", "michael", "amy", "paul"),
                     surname = c("bal", "reecht", "darcy", "jove",
                                 "maxwell", "rupnik", "rafferty",
                                 "slattery", "clarke", "sheridan",
                                 "fitzpatrick", "coleman"),
                     section = c("feas", "feas", "feas", "osis", "feas",
                                 "mefss", "osis", "feas", "mefss", "feas",
                                 "mefss", NA),
                     irish = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                               TRUE, TRUE, TRUE, TRUE, TRUE),
                     age = c(28, 22, 28, 29, 23, 26, 24,
                             27, 21, 28, 25, 29))

## 2) Print it on the console:
print(people)
people
## head(people)

## 3) Extract the number of columns:
ncol(people)
dim(people)[2]

nDims <- dim(people)
nDims[2]

## 4) …and the number of rows:
nrow(people)

nDims[1]

## 5) What are the column names?
colnames(people)

dimnames(people)[[2]]

