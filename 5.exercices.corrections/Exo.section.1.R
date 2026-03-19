rm(list  <-  ls())

#install.packages('magrittr')
library("magrittr")
#test <- require(magrittr)

# une premiere series d'exercices 
a <- 2.3 ; b <- 3.5
c <- -2
d <- 2
e <- 0.011

# calcul equation 
Y <-   a * (b + c) + d / e

sum(a,b)

# series exo 2  <- <- <- <- =====================================

monVector <- 
  runif(n = 10, min = 2, max = 32)

monVector %T>% 
  print() %>% mean() 
#mean(x = .)
#{c(sd(x = .), mean(x = .))}

??'normal distribution'

monVectorNorm <- 
  rnorm(n = 10, mean = 2, sd = 32)

monVectorNorm %>% mean() # sd(x = .)

# exo 2.1.1 ============================

myVector <- c(10, 5, 4, 100)
myVector %>% class()
myVector %>% str()
class(myVector)

sqrt(myVector)
myVector ^ (1/3)

dates <- 
  c(today = Sys.Date(), 
    yesterday = Sys.Date() - 1)
dates

# exo 2.1.2 ======================================

newVec <- runif(n = 10, min = 0, max = 1)

newVec %>% `>`(5) %>% which()

all((sqrt(newVec) ^ 2) == newVec)
all((sqrt(newVec ^ 2)) == newVec)

newVec > 6 & newVec < 3

newVec
newVec > 5 & newVec < 8
#newVec %>% {.> 5 & . == 2}
#newVec %>% `>`(5) %>% `&`(newVec == 2)

# exo 2.1.2 ======================================

newVec <- runif(n = 10, min = 0, max = 10)

test.1 <- 
  newVec > 8 | newVec < 4

test.2 <- 
  newVec > 4 & newVec < 8

# meme resultat que test 2, logique diferente
!test.1

# test sur lettres 
setLet <- c("A", "b", "Z", 'g', "J")
letters

setLet %in% letters
which(setLet %in% letters)

match(setLet, letters)

intersect(setLet, letters)

# 2.1.4 ======================================================

# sequence from 2 to 11 by 3
seq(from = 2, to = 11, by = 3)
# sequence from to 11 with 3 elements
seq(from = 2, to = 11, length.out = 3)

# create vector x
x <- runif(10)

# sequence starting at 2 by 1, with same length as x
seq(from = 2, by = 1, along.with = x)
seq(from = 2, by = 1, length.out = length(x))
2:(2 + length(x) - 1)
4:(4 + length(x) - 1)
2:(x %>% length %>% `+`(2))

# exo 2.1.5 =========================================

# create vector and add names basedon letters
seq5 <- 2:6
names(seq5)
names(seq5) <- letters[1:length(seq5)]
names(seq5)
seq5
names(seq5) <- letters[seq5]
names(seq5)

# select some bits
seq5[c(2, 4)]
seq5[c("b", "d")]
seq5[(1:length(seq5)) %in% c(2, 4)]
seq5 %>% `[`(c(2,4))

size <- rnorm(n = 50, mean = 30, sd = 3)

size >= 33
which(size >= 33)
length(which(size >= 33))
sum(size >= 33)

size[size >= 33] <- NA
max(size, na.rm = TRUE)

length(size)
size[length(size)]

tail(size, 4)
head(size, 4)

# exo 2.2.1 =============================================

n.people <- 9

people <- 
  data.frame(
    taille = runif(n.people, 160, 180),
    poids = runif(n.people, 50, 80),
    pole = c('SI', 'ASA', 'ISSP')
  ) %>% `rownames<-`(., c("Julien", "Federica", "Jordan", "Emma", 
                          "Charlotte", "Elina", "Lucile",
                          "Octave", "Guillaume"))

rownames(people) <- c("Julien", "Federica", "Jordan", "Emma", 
                      "Charlotte", "Elina", "Lucile",
                      "Octave", "Guillaume")

`rownames<-`(c("Julien", "Federica", "Jordan", "Emma", 
               "Charlotte", "Elina", "Lucile",
               "Octave", "Guillaume"))

prenom <- c(people, "Julien", "Federica", "Jordan", "Emma", 
            "Charlotte", "Elina", "Lucile",
            "Octave", "Guillaume")

pole <- c('SI', 'ASA', 'ISSP')
taille <- runif(n.people, 1.60, 1.80) %>%  round()
poids <- runif(n.people, 50, 80) %>% round

rbind(prenom, pole, taille, poids,
      bmi = poids / (taille^2)) %>% t() %>% as.data.frame()

dim(people)
nrow(people)

colnames(people)
row.names(people)

# nom <- c(1, 2, 3) %>% `names<-`(c('a', 'b', 'c'))
# nom2 <- c(1, 2, 3) %>% `names<-`(c('a', 'b', 'd'))
# cbind(nom, nom2)

people

people[ , c('poids'), drop = FALSE]

people$pole == 'SI'
people[
  people$pole == 'SI' &
    people$poids < 70,
  'taille', drop = FALSE]

people$poids[2]
people[2, 'poids']

people$bmi <- people$poids /people$taille
people[, 'imb'] <- people$poids / people$taille

people
people$imb <- NULL
people[ , -which(colnames(people) == 'imb')]
people[ , colnames(people) != 'imb']

attributes(people)
attributes(people)$info <- 'une info sur mes donnees'
attributes(people)$info2 <- matrix(NA, ncol = 2, nrow = 2)

# exo 3.2.2 =================================================

# ways of changing class
people$pole <- as.factor(people$pole)
people$pole %<>% as.factor()
class(people$pole)

# check possible values
levels(people$pole)
unique(people$poids)

people <- 
  data.frame(
    taille = runif(n.people, 160, 180),
    poids = runif(n.people, 50, 80),
    pole = c('SI', 'ASA', 'ISSP'), 
    prenom = c("Julien", "Federica", "Jordan", "Emma", 
               "Charlotte", "Elina", "Lucile",
               "Octave", "Guillaume"),
    age = runif(n.people, 20, 30) %>% round)

# find my team
my.pole <- people$pole[people$prenom == 'Guillaume']
my.pole <- people[people$prenom == 'Guillaume', 'pole'] # same thing

people[people$pole == my.pole &
         people$age == 24, ] %>%  class()





