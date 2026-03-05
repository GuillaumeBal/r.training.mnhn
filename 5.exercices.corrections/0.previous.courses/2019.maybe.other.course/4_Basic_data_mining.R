#################
## 4 Data mining:
#################

## =======================
## Exercises 4.1-1

my.wd <-'C:/Users/gbal/Desktop/r.training/2.data/2.salmon/'
setwd(my.wd)


## 1) Import the salmon.raw.data.txt file in R

salmon.raw <- read.table("salmon.data.raw.txt", sep = ',', header = TRUE)

## 2) Display a few rows to check everything is alright
head(salmon.raw)

## 3) Look at the dimension of the dataframe
dim(salmon.raw)

## 4) Save the data in your folder as 'salmonRawYourName.txt'
write.table(salmon.raw,
            file = 'salmon.cooked.csv',
            sep = ',',
            row.names =  FALSE)

## =======================
## Exercises 4.2-1

## 1) Store salmonRaw (or whichever name you stored it in)
##    in a new "salmonTidy" data.frame.
salmonTidy <- salmon.raw

## 2) Detect incorrect weights and correct them.
##    Check the mean/min/max weight before/after.
summary(salmonTidy$weight)

## Test thresholds:
sum(salmonTidy$weight > 50, na.rm = TRUE)
sum(salmonTidy$weight > 100, na.rm = TRUE)
sum(salmonTidy$weight > 10, na.rm = TRUE)

## selection of wrong records:
salmonTidy[which(salmonTidy$weight > 50) , "weight"]
salmonTidy$weight[which(salmonTidy$weight > 50)]

# salmonTidy[which(salmonTidy$weight > 50) ,
#            "weight"] <- salmonTidy[which(salmonTidy$weight > 50) ,
#                                    "weight"] / 1000

## Index can be saved and used on both sides:
idxWD <- which(salmonTidy$weight > 50)
salmonTidy$weight[idxWD] <- salmonTidy[idxWD , "weight"] / 1000

summary(salmonTidy$weight)


## 3) Recode the column "wild" (1 -> TRUE, 0 -> FALSE).
##    Display the number of TRUE/FALSE/NAs
head(salmonTidy$wild) == 1

as.logical(head(salmonTidy$wild))

salmonTidy$wild <- salmonTidy$wild == 1

head(salmonTidy)

## 4) Save it in a salmonTidy.csv file
write.table(salmonTidy,
            file = "salmonTidy.csv",
            sep = ",",
            row.names = FALSE)

## 5) (optional)
##    Remove entries with impossible dates (ignore leap years).
