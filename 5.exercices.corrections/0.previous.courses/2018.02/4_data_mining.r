rm(list = ls())  ## Clear the objects in memory.

getwd() ## the current working directory.

## ...that can be changed:
wd <-  'C:/Users/gbal/Desktop/r.training/5.exercices.corrections'
setwd(wd)

## example relative path for data: "../2.data/2.salmon/salmon.data.raw.txt"
## "../" jumps to the parent directory, "../../", second parent, etc.

## loading files =========================================================================

# from folder
salmon.data <- read.table(file = '../2.data/2.salmon/salmon.data.raw.txt',
                          header = TRUE,
                          sep = ',',
                          dec = '.')

## from the web:
# web.address <- 'https://github.com/GuillaumeBal/r.training/blob/master/2.data/2.salmon/salmon.data.raw.txt'
# salmon.data <- read.table(file = web.address,
#                           header = TRUE,
#                           sep = ',',
#                           dec = '.')

## load excel from folder
# install.packages('openxlsx')
require(openxlsx)
# library(openxlsx)
# salmon <- read.xlsx('../2.data/2.salmon/salmon.data.raw.xlsx')

head(salmon.data)
dim(salmon.data)

write.table(salmon.data,
            file = 'salmon.g.txt',
            sep = ',',
            dec = '.',
            row.names = FALSE)


## 4.2.4) Data exploration and correction:

## 1) Store salmonRaw (salmon.data) in a new "salmonTidy" data.frame
salmonTidy <- salmon.data

head(salmonTidy)


## 2) Detect incorrect weights and correct them.
summary(salmonTidy$weight)

# salmonTidy$weight > 50

## Number of individuals with weight over threshold:
table(salmonTidy$weight > 50, useNA = "ifany")
sum(salmonTidy$weight > 50, na.rm = TRUE)

## Extract/access weights over threshold:

# salmonTidy$weight[which(salmonTidy$weight > 50)]
idx <- which(salmonTidy$weight > 50)
salmonTidy$weight[idx]

## Correct them:

# salmonTidy$weight[which(salmonTidy$weight > 50)] / 1000
salmonTidy$weight[idx] / 1000

## Correct them in the object (modify as you access):

# salmonTidy$weight[which(salmonTidy$weight > 50)] <-
#   salmonTidy$weight[which(salmonTidy$weight > 50)] / 1000

salmonTidy$weight[idx]  <- salmonTidy$weight[idx] / 1000
# salmonTidy[idx, "weight"]  <- salmonTidy[idx, "weight"] / 1000

## Check:
summary(salmonTidy$weight)

## 3) Recode the column "wild" (1 -> TRUE, 0 -> FALSE).
##    Display the number of TRUE/FALSE/NAs:

summary(salmonTidy$wild)
table(salmonTidy$wild)

head(salmonTidy$wild)
head(as.logical(salmonTidy$wild))

salmonTidy$wild2 <- salmonTidy$wild == 1
salmonTidy$wild <- as.logical(salmonTidy$wild)

summary(salmonTidy$wild)
table(salmonTidy$wild)

summary(salmonTidy$wild2)
table(salmonTidy$wild2)

## 4) Save it in a salmonTidy.csv file
write.csv(x = salmonTidy, file = "salmonTidy.csv", row.names = FALSE)


## 5) Remove entries with impossible dates (ignore leap years)
summary(salmonTidy$julian.day)  # Some values over 365!

## Where are they?
unrelevantIdx <- salmonTidy$julian.day > 365

table(unrelevantIdx)

## -> positions of values out of range (drop the NAs):
posOver365 <- which(unrelevantIdx)

salmonTidy[posOver365, "julian.day"]

## Replace by NAs:
salmonTidy[posOver365, "julian.day"] <- NA

summary(salmonTidy$julian.day)

## Override the saved file:
write.csv(x = salmonTidy, file = "salmonTidy.csv", row.names = FALSE)

