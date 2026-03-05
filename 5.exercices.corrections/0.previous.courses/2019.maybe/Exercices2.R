## Exercices 2.1:

## 2
setwd("C:/Users/gbal/Desktop/r.training/5.exo")
getwd()

## 3
rep53 <- rep(x = 5, times = 3)
## 4
print(rep53)

## 5
nRep <- length(x = rep53)
print(nRep)

## 2 main concepts:
# Variable
(var1 <- 2)
# function calling = task
rep(5, 4)


## Exercices 2.2.5:

# 1
a <- c(10, 5, 4.1, 100)
print(a)

# 2
class(a)
mode(a)
typeof(a)

# 3
?log
log(x = a, base = 10)
log10(a)


# 6
today <- Sys.Date()
yesterday <- Sys.Date() - 1

toyes <- c(today, yesterday)

print(toyes)

Sys.Date() - c(0, 1)
Sys.Date() + c(0, -1)

as.numeric(Sys.Date())

## Exo 2.2.7
# 1
?runif

x <- runif(n = 10, min = 0, max = 10)
print(x)

# 2
x > 5
which(x > 5)

# 3
sqrt(x)^2 == x
all(sqrt(x)^2 == x)

# 4
all(abs(sqrt(x)^2 - x) < sqrt(.Machine$double.eps))


## Exo 2.2.9:

# 1
nseq <- length(x)
seq(from = 2, by = 1, length.out = nseq)

seq(from = 2, by = 1, length.out = length(x))

seq(from = 2, by = 1, along.with = x)

2:(length(x) + 1)

(1:length(x)) + 1

# 2
seq_along(x) + 1

# 3
rev(x)
x2 <- rev(x)
x2

# 4
seq(from = 100, to = 5, by = -5)


## Exo 2.2.11

# 1
(seq5 <- 1:5)

# 2
names(seq5)

letters[1:5]

names(seq5) <- letters[1:5]
seq5

# 3
seq5[c(2, 4)]
seq5[c("b", "d")]
seq5[seq5 %in% c(2, 4)]

seq5 > 3
seq5[seq5 > 3]
which(seq5 > 3)

length(seq5[seq5 > 3])

# 4
size <- rnorm(n = 10, mean = 30, sd = 3)
print(size)

size[size >= 33]
which(size >= 33)

length(size[size >= 33])  # OR
length(which(size >= 33))
