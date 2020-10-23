## 3.2.1 - indexation:

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

## 1.1) Extract the third column:
people[ , 3]
dim(people[ , 3])
length(people[ , 3])

## 1.2) Extract the fourth row:
people[4 , ]
dim(people[4 , ])

## 1.3) How to extract the age of the second row?
people[2 , 5]
people[2 , "age"]


## 3.2.3 - Indexing data.frames, continued...

## 1) What is the class of the column "section"?
sections <- people$section
class(sections)

## 2) List the different "sections" in the dataset:
levels(sections)

## 3) Create a new data.frame (named peopleSub) with only persons from "osis" and "mefss"
## 3.1:

test1 <- sections == "osis" | sections == "mefss"
test2 <- sections %in% c("osis", "mefss")
test3 <- is.element(sections, c("osis", "mefss"))
test4 <- (sections == "osis" | sections == "mefss") & ! is.na(sections)


## 3.2:
people[test1 , ]
people[test2 , ]
people[test3 , ]
people[test4 , ]

## 3.3:
peopleSub <- people[test2 , ]

## 4) How many rows does it contain?

nrows(peopleSub)


## 5) Extract the data for people from feas who are less the 25.

testFeas <- sections %in% c("feas")
test25 <- people$age < 25

testFeas25 <- testFeas & test25

people[testFeas25 , ]

## Or:
people[sections %in% c("feas") &
         people$age < 25, ]

## !!! NAs:
people[sections == c("feas") &
         people$age > 25, ]             # Issue with NA!

people[(sections == c("feas") | is.na(sections))&
         people$age > 25, ]

## 3.4 subset and data modification 2D:

## 1) Create a new data.frame "peopleSub6" with the 6 first rows of people.

## 1.1 sequence:
seq(from = 1, to  = 6, by = 1)
1:6

## 1.2 subset:
people[1:6 , , drop = FALSE]
people[1:6 , ]


## 1.3 save it in a new data.frame:
peopleSub6 <- people[1:6 , ]


## 2) Remove the column "Irish" from peopleSub6:
colnames(peopleSub6)

peopleSub6[ , -4]
peopleSub6 <- peopleSub6[ , -4]
peopleSub6

## Alternative (better):
peopleSub6$irish
peopleSub6$irish <- NULL  ## Change it directly in the original object.
peopleSub6


## Alternative (fine but more complicated; and works for matrices as well):
colnames(peopleSub6) %in% c("irish")
-which(colnames(peopleSub6) %in% c("irish"))
# name is not irish:
! colnames(peopleSub6) %in% c("irish")
peopleSub6 <- peopleSub6[ , ! colnames(peopleSub6) %in% c("irish")]

cols2sup <- c("irish", "section")
peopleSub6[ , ! colnames(peopleSub6) %in% cols2sup]

## 3) Select the individuals in peopleSub6 with a "age" greater or equal to 26:
## test:
peopleSub6$age >= 26
idx <- which(peopleSub6$age >= 26)

## subset:
peopleSub6[idx , ]

## alternative using subset:
subset(x = peopleSub6,
       age >= 26)

# Subset supresses NAs in test:
subset(x = people, section == "feas")

## 3.6, Conversion and order (Using the people data):

## 1)
## 1.1) What is the class of the column "name"?
people$name
class(people$name)

## 1.2) Extract it as a character string (extract and convert).
peopleNames <- people$name

as.character(peopleNames)

class(as.character(peopleNames))


## 1.2) Transform it as a character string (make it permanent)
peopleNamesCh <- as.character(peopleNames)
people$name <- as.character(peopleNames)
people$name

## 2) Extract the sorted ages.
people$age

sort(people$age)
sort(people$age, decreasing = TRUE)

?sort

## 3) Order people by "section" and "age" and
##    store it in a variable peopleOrdered

## 3.1)
people[order(people$section, people$age) , ]

people[order(people$section, people$age, na.last = FALSE) , ]

## 3.2)
peopleOrdered <- people[order(people$section, people$age, na.last = FALSE) , ]
peopleOrdered

?order ## e.g method = "radix" for column-wise options.
