rm(list = ls()) # suppress everything in memory

wd <- 'C:/Users/gbal/Desktop/r.training/2.data'
setwd(wd)

salmon.raw <- read.table(file = 'salmon.data.raw.2.txt', sep = ';', dec = ',',  header = TRUE)

head(salmon.raw)

dim(salmon.raw)
nrow(salmon.raw)
ncol(salmon.raw)

write.table(salmon.raw, file = 'salmon.data.raw.2.gb.txt', sep = ',', dec = '.')


require(openxlsx)
library(openxlsx)

## Exo 3.4

# 1
salmon.raw[1:10 , ]

head(salmon.raw, n = 10)
?head

salmonSub10 <- salmon.raw[1:10 , ]
print(salmonSub10)

# 2
salmonSub10 <- salmonSub10[ , -3]

salmonSub10$wild <- NULL
print(salmonSub10)

# 3
salmonSub10[salmonSub10$length >= 80 , ]

subset(salmonSub10, length >= 80)


## Exo 3.6

# 1
class(salmon.raw$river)

# 2
levels(salmon.raw$river)
unique(salmon.raw$river)

dim(salmon.raw)

# 3
head(salmon.raw[salmon.raw$river == "corrib" | salmon.raw$river == "scorff" , ])

salmonSub <- salmon.raw[salmon.raw$river %in% c("corrib" , "scorff") , ]

salmonSub <- subset(salmon.raw, river %in% c("corrib" , "scorff"))
head(salmonSub)

# 4
salmonSub <- salmonSub[order(salmonSub$river, salmonSub$year), ]
head(salmonSub)

# 5
nrow(salmonSub)

# 6
write.table(salmonSub, file = "salmonSub.csv", sep = ",", dec = ".", 
            col.names = TRUE, row.names = FALSE)
