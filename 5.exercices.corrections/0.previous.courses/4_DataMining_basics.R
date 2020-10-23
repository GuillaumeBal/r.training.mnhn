## Exo 4.1
rm(list = ls())

salmonRaw <- read.table(file = "./salmon.data.raw.txt", sep = ",", dec = ".", header = TRUE)

# 1
salmonTidy <- salmonRaw
head(salmonTidy)

# 2
summary(salmonTidy$weight)

sum(salmonTidy$weight > 30)

salmonTidy[salmonTidy$weight > 30 , "weight"]

salmonTidy$weight[salmonTidy$weight > 30]

salmonTidy$weight[salmonTidy$weight > 30] / 1000

salmonTidy$weight[salmonTidy$weight > 30] <- salmonTidy$weight[salmonTidy$weight > 30] / 1000

sum(salmonTidy$weight > 30)

# 3

# 4
c(1,2,2,1,1,2, 3) == 1

table(salmonTidy$wild)

salmonTidy$wild == 1

head(salmonTidy$wild)
head(salmonTidy$wild == 1)

salmonTidy$wild <- salmonTidy$wild == 1

head(salmonTidy)

# 5
write.csv( salmonTidy, file = "salmonTidy.csv", row.names = FALSE)
