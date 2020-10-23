## 2.1.5:

## 1)
a <- c(10, 5, 4.1, 100)
a

## 2)
class(a)
mode(a)

## 3)
a + 2

## 4)
log10(a)
log(x = a, base = 10)

## 5)
?Sys.Date
c(Sys.Date(), Sys.Date() - 1)
Sys.Date() - c(0, 1)


## 2.1.6:

## 1)
x <- runif(n = 10, min = 0, max = 10)
x

## 2)
x > 5

## 3)
sqrt(x)^2 == x
x == sqrt(x)^2

## 4) 
sqrt(x^2) == x


## 5)
abs(sqrt(x)^2 - x) < sqrt(.Machine$double.eps)

## 2.1.7

## 1)
sqrt(x)^2 == x
all(sqrt(x)^2 == x)
any(sqrt(x)^2 == x)

all(sqrt(x^2) == x)

## 2)
x > 8
x < 4
x > 8 | x < 4
(x > 8) | (x < 4)
which((x > 8) | (x < 4))

## 3)
x >= 4
x <= 8
x >= 4 & x <= 8

which(x >= 4 & x <= 8)
which( ! ((x > 8) | (x < 4)))


## 4)
setLet <- c("A", "b", "Z", "g", "J")
letters

setLet %in% letters
which(setLet %in% letters)
length(which(setLet %in% letters))


## 2.1.9:

## 1)
x
seq(from = 2, by = 1, length.out = 10)
length(x)
seq(from = 2, by = 1, length.out = length(x))
seq(from = 2, by = 1, along.with = x)

2:11
2:(length(x) + 1)
(1:length(x)) + 1

## 2)
seq_along(x) + 1

## 3)
x2 <- rev(x)
x2

## 4)
seq(from = 100, to = 0, by = -5)
rev(seq(from = 0, to = 100, by = 5))

## 2.1.11:

## 1)
seq(from = 1, to = 5, by = 1)
seq_len(5)
1:5

seq5 <- 1:5
seq5

## 2)
letters[1:5]
seq5names <- letters[seq5]
seq5names

names(seq5) <- seq5names
seq5

## 3)
seq5[c(2, 4)]
seq5[(1:2) * 2]
seq5[c("b", "d")]
seq5[letters[c(2, 4)]]

seq5 > 3
seq5[seq5 > 3]
seq5 > 3 & seq5 < 5
seq5[seq5 > 3 & seq5 < 5]

## 4:
length(seq5)
seq5[length(seq5)]
# tail(x = seq5, n = 4)

## 5)
size <- rnorm(n = 10, mean = 30, sd = 3)
size

idx33 <- size >= 33
size[idx33]
which(idx33)

## 6)
size[idx33] <- NA
size
size[]

