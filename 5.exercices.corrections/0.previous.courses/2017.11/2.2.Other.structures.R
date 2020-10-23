## 2.2.3:

## 1)

people <- 
  data.frame(name = c("guillaume", "yves", "jack", "maximo", "hugo", 
                      "agnieszka", "anthony", "niamh", "dave", 
                      "michael", "amy", "paul"),
             surname = c("bal", "reecht", "darcy", "jove", "maxwell", 
                         "rupnik", "rafferty", "slattery", "clarke", 
                         "sheridan", "fitzpatrick", "coleman"),
             section = c("feas", "feas", "feas", "osis", "feas", 
                         "mefss", "osis", "feas", "mefss", "feas", "mefss", NA),
             irish = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, 
                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
             age = c(28, 22, 28, 29, 23, 26, 24, 27, 21, 28, 25, 29))

## 2)
people
print(people)

## 3)
ncol(people)

dim(people)[2]

ndim <- dim(people)
ndim[2]

## 4)
nrow(people)

dim(people)[1]

## 5)
colnames(people)

names(people)

dimnames(people)[[2]]

