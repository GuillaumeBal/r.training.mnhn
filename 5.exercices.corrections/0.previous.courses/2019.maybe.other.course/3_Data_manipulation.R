#######################
## 3 Data manipulation:
#######################


people <- data.frame(
  firstname = c("guillaume", "yves", "jack", "maximo",
                "hugo", "agnieszka", "anthony", "niamh", 
                "dave", "michael", "amy", "paul"),
  name = c("bal", "reecht", "darcy", "jove", "maxwell", 
           "rupnik", "rafferty", "slattery", "clarke", "sheridan",
           "fitzpatrick", "coleman"),
  section = c("feas", "feas", "feas", "osis", "feas", 
              "mefss", "osis", "feas", "mefss", "feas", "mefss", NA),
  irish = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, 
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  age = c(28, 22, 28, 29, 23, 26, 24, 27, 21, 28, 25, 29)
)

## =======================
## Exercises 3.2-1

## 1) With the table people :

## 1.1) Extract the third column
people[ , 3]

## 1.2) Extract the fourth row
people[4 , ]

## 1.3) How to extract the age of the second row?
people[2 , 5]
people[2 , "age"]
people[2 , 'age']

## =======================
## Exercises 3.2-2

## 1) What is the class of the column "section"?

people
class(people)

people[ , "section"]

people$section

class(people$section)

## 2) List the different "sections" in the dataset (clue in 2.1.4, 2.2.3).
levels(people$section)

## 3) Create a "new" data.frame (named peopleSub) with only 
##    persons from osis and mefss ( %in% ) .

## 3.1) extract the col section:
people$section

## 3.2) test:
idx <- people$section %in% c("osis", "mefss")

is.element(people$section, c("osis", "mefss"))

(people$section == "osis" | people$section == "mefss") & ! is.na(people$section)

## 3.3) subset:
people[idx , ]

## 3.4) storing:
peopleSub <- people[idx , ]
peopleSub

## 4) How many rows does it contain?
nrow(peopleSub)


## 5) Extract the data for people from feas who are less the 25.
people$section %in% c("feas")

people$section == c("feas") & ! is.na(people$section)

typeof(people$age)
people$age < 25 & ! is.na(people$age)

idx2 <- people$section %in% c("feas") & (people$age < 25 & ! is.na(people$age))

idx2 <- people$section %in% "feas" & (people$age < 25 & ! is.na(people$age))

people[idx2 , ]

## only < 25?
people[people$age < 25 , ]


## =======================
## Exercises 3.4-1

## 1) Create a new data.frame "peopleSub6" with 
##    the 6 first rows of people.
people[1:6 , ]

peopleSub6 <- people[1:6 , ]
print(peopleSub6)

## 2) Remove the column "irish" from peopleSub6.
peopleSub6$irish
peopleSub6[ , "irish"]

peopleSub6$irish <- NULL
print(peopleSub6)

## 3) Select the individuals in peopleSub6 with
##    an "age" greater or equal to 26.
idx26 <- peopleSub6$age >= 26

peopleSub6[idx26 , ]

idx26notNA <- peopleSub6$age >= 26 & ! is.na(peopleSub6$age)
peopleSub6[idx26notNA , ]

which(idx26)
peopleSub6[which(idx26) , ]

subset(x = peopleSub6,
       subset = age >= 26)

## select columns as well:
subset(x = peopleSub6,
       subset = age >= 26,
       select = c("section", "age"))


## =======================
## Exercises 3.6-1

## using "people" data

## 1) What is the class of the column "name"? 

class(people$name)

##    1.1) Extract the column as a character string (extract and convert).
as.character(people$name)

##    1.2) Transform it as a character string (make it permanent).
people$name <- as.character(people$name)
people$name
class(people$name)

## 2) Extract the sorted ages.
people$age

sort(people$age)

sort(people$age)[1:5]
sort(people$age, decreasing = TRUE)[1:5]


## 3) Order people by "section" and "age" and
##    store it in a variable peopleOrdered.
#

people[order(people$section, people$age) , ]

people[order(people$section, people$age, na.last = FALSE) , ]

peopleOrdered <- people[order(people$section, people$age) , ]
print(peopleOrdered)

# class(dataf$treatment) <- "numeric"
# class(dataf$Indiv) <- "character"


