## Loading/saving Data:

rm(list = ls())

# data minin part ############################################################################

# loading /saving data ================

web.address <-'https://github.com/GuillaumeBal/r.training/blob/master/2.data/2.salmon/salmon.data.raw.txt'

local.address <- 'Y:/MI/Workshops - Trainnings/R-Beginners/r.training/2.data/2.salmon'

setwd(local.address)

salmonWeb <- read.table(file = web.address, header = TRUE, sep = ",", dec = ".")
head(salmonWeb)


salmonLocal <- read.table('salmon.data.raw.2.txt', header = TRUE, sep = ";",dec = ",")
head(salmonLocal)

dim(salmonLocal)


write.table(x = salmonLocal, 'salmonGuillaume.csv',sep =',' , row.names = FALSE)

salmonRaw <- salmonLocal

## Basic data mining and cleaning:

## 1)

salmonTidy <- salmonLocal


## 2)

summary(salmonTidy$weight)
min(salmonTidy$weight, na.rm = TRUE)

sum(salmonTidy$weight > 50)

idxModif <- salmonTidy$weight > 50
salmonTidy[idxModif , ]

salmonTidy[salmonTidy$weight > 50 , ]

salmonTidy[salmonTidy$weight > 50 , "weight"]

salmonTidy[salmonTidy$weight > 50 , 
           "weight"] <- salmonTidy[salmonTidy$weight > 50 , 
                                   "weight"] / 1000

salmonTidy[idxModif , ]

summary(salmonTidy$weight)

## 3)
head(salmonTidy$wild, n = 10)

wild <- c(1, 0, NA, 1, 2)
wild == 1
as.logical(wild)

head(salmonTidy$wild, n = 10) == 1
as.logical(head(salmonTidy$wild, n = 10))

salmonTidy$wild <- as.logical(salmonTidy$wild)

head(salmonTidy)

## 4)
getwd()
write.table(salmonTidy, "salmonTidy.csv", sep = ",", dec = ".", row.names = FALSE)

## 5)
summary(salmonTidy$julian.day)
dim(salmonTidy)

salmonTidy <- subset(salmonTidy,
                     julian.day <= 365)

dim(salmonTidy)












