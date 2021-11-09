rm(list = ls())


# exercices n°xxx ----

# this is some calculation
a <- 2.3; b <- 3.5; c <- -2; d <- 2; e <- .011
Y <- a*(b + c) +  d / e
Y


# exercices 1.2-4 ----

vec.unif <- rnorm(n = 1000,
                  mean = 4,
                  sd = 10)
mean(x = vec.unif)
sd(x = vec.unif)

# Exercices 2.1-1 ----

my.vector <- c(10, 5, 4.1, 100)
class(my.vector)
mode(my.vector)

my.vector + 2
my.vector ^ (1/3)

c(Sys.Date(),
  Sys.Date() - 1)

# Exercices 2.1-2 ----

vector.0.10 <- runif(n = 10, min = 0, 
                     max = 10)

more.than.5 <- which(vector.0.10 > 5)

(sqrt(vector.0.10) ^ 2) == vector.0.10

# Exercice 2.1-3 ----

x <- runif(n = 15, min = 0 , max = 10)
which(x > 4 & x < 8)

setLet <- c("A", "b", "Z", "g", "J")

which(setLet %in% letters)

# Exercice 2.1-4 ----

seq(from = 2, to = 11, by = 3)
seq(from = 100, to = 0, by = -5)

seq(from = 2 , by = 1, along.with = x)
seq(from = 2 , by = 1, length.out = length(x))
seq(from = 2 , by = 1, to = length(x) + 1)
2:(length(x) + 1)

# Exercices 2.1-5 ----

seq5 <- 2:6
names.seq5 <- letters[seq5]
names(seq5) <- names.seq5
seq5

seq5[seq5 %in% c(2, 4)]
seq5[c('b', 'd')]

seq5[seq5 > 3]

seq5[length(seq5)]
tail(seq5, n = 1)

size <- rnorm(n = 50, mean = 30, sd = 3)
head(size)

length(size[size >= 33])

size[size >= 33] <- NA

# Exercice 2.2-1 ----

# creation table people
people <- data.frame(
  name = c('santiago', 'salomee' , 'olivier' , 'brian', 'nina', 'aurelie'),
  surname = c('forero', 'gelot', 'delzon', 'padilla', 'king-gillies', 'lacoeuilhe'),
  age = c('27', '24', '34', '28', '25', '30'),
  size = c(175, 152.5, 174, 183, 159, 175),
  team = c('corporate', 'public', 'corporate', 'public', 'public', 'both')
)

dim(people)[1]
c(nrow(people), ncol(people))

names(people)
colnames(people)
rownames(people) <- letters[1:dim(people)[1]]

# Exercices 3.2-1 ----

people[ , 3] <- as.numeric(people[ , 3])
people[ , 3]

people[4 , ]

people[2, 'age']

test.team <- people$team == 'public'
people[test.team , 'size'] 

people[people$team %in% c('public', 'corporate') , 'size']
people[people$team != 'both' , 'size']
people[-which(people$team == 'both'), 'size']

# exercices 3.2-2 ----

class(people$team)
#sapply(people, 'class')

unique(people$team)
levels(as.factor(people$team))
str(people$team)

new <- people[people$team != 'public' , ]
dim(new)[1]

people[people$team == 'corporate' & people$age < 29 , ]

new.2 <- people[people$team == 'public' , ]

rbind(new, new.2)

# Exercice 3.4-1 ----

people.sub.3 <- people[c(1, 3, 5), ]
people.sub.3$name <- NULL
people.sub.3[people.sub.3$age >= 26, ]

people.sub.3[ , - which(colnames(people.sub.3) == 'surname')]

# 3.6-1 ----

class(people$team)
people$team <- as.factor(people$team)
sort(people$age)

order.index <- order(people$team, people$age)
people[order.index, ]


