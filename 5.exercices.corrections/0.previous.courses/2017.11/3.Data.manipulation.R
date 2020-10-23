## 3.3 basic selection:

## 1.1)
dim(people)
people[ , 3]

## 1.2)
people[4 , ]

## 1.3)
colnames(people)
people[2 , 5]

## 3.3 advanced selection:
## 1.1)
people[ , "section"]
people$section

class(people$section)

## 1.2)
levels(people$section)


## 1.3)
people$section %in% c("osis", "mefss")
is.element(people$section, c("osis", "mefss")) ## strictly equivalent!

people$section == "osis" | people$section == "mefss"

people[people$section %in% c("osis", "mefss") , ]
## people[(people$section == "osis" | people$section == "mefss") & ! is.na(people$section) , ]

peopleSub <- people[people$section %in% c("osis", "mefss") , ]
peopleSub

##  1.4)
nrow(peopleSub)

## 1.5)
idx.feas25 <- people$section %in% c("feas") &
  people$age < 25

# idx.feas25 <- people$section == c("feas") & ! is.na(people$section) &
#   people$age < 25

people[idx.feas25 , ]


## 3.4

## 1)
people[1:6 , ]

peopleSub6 <- people[1:6 , ]
peopleSub6

## 2)
peopleSub6$irish <- NULL

peopleSub6[ , "irish"] <- NULL

## 3)
peopleSub6$age >= 26
peopleSub6[peopleSub6$age >= 26 , ] 

subset(x = peopleSub6,
       age >= 26)


## 3.6

## 1)
people$name
class(people$name)

as.character(people$name)
class(as.character(people$name))

people$name <- as.character(people$name)
people$name

## 2)
people$age
sort(people$age)
sort(people$age, decreasing = TRUE)

## 3):
people[order(people$section, people$age, na.last = FALSE) , ]

peopleOrdered <- people[order(people$section, people$age) , ]
peopleOrdered

?order

## different values of the decreasing param for different columns?
peopleOrdered <- people[order(as.character(people$section), people$age, method = 'radix',
                              decreasing = c(TRUE, FALSE), na.last = FALSE) , ]

