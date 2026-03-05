#rm(list = ls())

require('magrittr')

dir('2.data/2.salmon')

salmon.data <- read.table('2.data/2.salmon/salmonTidy.csv', header = T, sep = ',', dec = '.')
salmon.data %>% head(3)

pdf('mon.pdf.pdf')

par(mfcol = c(3, 1), oma = c(1, 5 , 1, 5))

plot(salmon.data$weight[salmon.data$river == 'scorff'], salmon.data$length[salmon.data$river == 'scorff'])
hist(salmon.data$weight, col = salmon.data$weight)
boxplot(salmon.data$weight ~ salmon.data$year)

dev.off()

# exercices 6.1.1 =====

salmon.data %>% head
salmon.num <- salmon.data[ , sapply(salmon.data, FUN = is.numeric)]

apply(salmon.num, MARGIN = 2, FUN = median, na.rm = TRUE)
sapply(salmon.num, FUN = median, na.rm = TRUE)

tapply(X = salmon.data$length, INDEX = list(salmon.data$year, salmon.data$river),
   FUN = mean) %>% t %>% head(3) %>% `*`(3) %>% sum

head(t(tapply(X = salmon.data$length, INDEX = list(salmon.data$year, salmon.data$river),
       FUN = mean)), 3) 

# exercices 6.2.1 ==================

#a counter 'a' from 2 to 9 
max.a <- 9
storage.vec <- rep(NA, max.a - 1)

for(a in 2:max.a){
  addition <- a + 3
  storage.vec[a - 1] <- print(paste('addition =', addition, sep = ' '))
  print(paste('a value = ', a))
}

storage.vec










