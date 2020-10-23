##############################
## 2.2 Further data structure:
##############################

## =======================
## Exercises 2.2-1

## 1) Create a variable people based on the table in 
##    .\2.data\1.people\Use the function data.frame(...)

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

## 2) Print it on the console.
people
print(people)

## 3) Extract the number of columns.
ncol(people)
dim(people)[2]

## 4) ...and the number of rows.
nrow(people)
dim(people)[1]

## 5) What are the column names?
colnames(people)
dimnames(people)
dimnames(people)[[2]]
