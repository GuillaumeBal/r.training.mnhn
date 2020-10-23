# quick examples ==============================================
x <- matrix(rnorm(100), ncol = 10)

quantile(x[, 1], probs = c(.2, .4, .6))

apply(x, MARGIN = 1, FUN = quantile, probs = c(.2, .4, .6), na.rm = TRUE)

tapply(X = salmon$weight,
       INDEX = list(salmon$river, salmon$year),
       FUN = mean, na.rm = TRUE)

# 6.1.1 ===========================================================

#Subset salmon data for columns year, weight, length & julian.day and store as salmon.num
salmon.num <- salmon[ , c('year', 'julian.day', 'length', "weight")]
head(salmon.num)

#Use apply() to calculate the mean of the columns 
apply(salmon.num, MARGIN = 2, FUN = mean, na.rm = TRUE)

#Use apply() to compute quantiles 0.2, 0.4, 0.6, 0.8 
apply(salmon.num, MARGIN = 2, FUN = quantile,
      probs = seq(0.2, 0.8, .2), na.rm = TRUE)

#Calculate mean size of fish by (use tapply())
#year
#year and sea age
tapply(salmon$weight,
       INDEX = list(salmon$year, salmon$sea.age),
       #INDEX = list(salmon$year, salmon$sea.age, salmon$river.age),
       FUN = mean, na.rm = TRUE)

seq(0.1, 0.8, .2)

# for loops ==========================================================

x <- rep(NA, 9)
for(a in 2:9){
  #print(a + 3)
  #x[a] <- a + 3
  #print(paste('result =', a + 3, sep = ' '))
  print(paste('plot', a + 3, '.tiff', sep = '@'))
}
x

#==================================================================

list.rivers <- levels(salmon$river)

for(river in list.rivers){
  #salmon.subset <- salmon[salmon$river == river, ]
  #file.name <- paste('salmon', river, 'txt', sep = '.')
  #write.table(salmon.subset, file = file.name, row.names = FALSE, sep = ',')
  write.table(salmon[salmon$river == river, ], 
              file = paste('salmon', river, 'txt', sep = '.'), 
              row.names = FALSE, sep = ',')
}



list.years <- unique(salmon$year)
#list.years <- list.years[1:(length(list.years) - 1)]
list.years <- list.years[! is.na(list.years)]
#list.years <- head(list.years, n = - 1)

for(year in list.years){
  #salmon.subset <- salmon[salmon$year == year, ]
  #file.name <- paste('salmon', year, 'txt', sep = '.')
  #write.table(salmon.subset, file = file.name, row.names = FALSE, sep = ',')
  write.table(salmon[salmon$year == year, ], 
              file = paste('salmon', year, 'txt', sep = '.'), 
              row.names = FALSE, sep = ',')
}
