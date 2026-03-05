rm(list = ls()) 

require(magrittr)

# set the wd
setwd("C:/Users/gbal/Desktop/r.training.mnhn")

salmon.data <- 
  read.table(
    file = '2.data/2.salmon/salmon.data.raw.txt',
    header = TRUE,
    sep = ',',
    dec = '.'
  )

write.table(x = salmon.data,
            file = 'saumon.2.txt',
            sep = ',', dec = '.',
            row.names = FALSE)

salmon.data$wild <- salmon.data$wild == 1
salmon.data[ , c("country", "sex", 'river')] <-
  sapply(salmon.data[ , c("country", "sex", 'river')], FUN = 
           function(x) as.factor(x), simplify = FALSE)
head(salmon.data)
summary(salmon.data)
salmon.data$river


salmon.data$weight[salmon.data$weight > 50 & !is.na(salmon.data$weight)] <- 
  salmon.data$weight[salmon.data$weight > 50 & !is.na(salmon.data$weight)] / 1000


salmon.data$length[salmon.data$length < 20 & !is.na(salmon.data$lenght)] <- 
  salmon.data$length[salmon.data$length < 20 & !is.na(salmon.data$length)] * 2.54


which(salmon.data$length < 20 & !is.na(salmon.data$lenght))

# exercice graph 1 ----

plot(x = salmon.data$length[salmon.data$river == 'moy' & 
                              salmon.data$sea.age == 1],
     y = salmon.data$weight[salmon.data$river == 'moy' & 
                              salmon.data$sea.age == 1],
     ylab = 'Weight', xlab = 'Length', pch = '.')

hist(salmon.data$weight[salmon.data$country == 'ie'], main = 'Weight')

pdf('mes.saumons.pdf', useDingbats = FALSE)
boxplot(salmon.data$weight ~ salmon.data$year, outline = FALSE)
boxplot(weight ~ year, data = salmon.data, outline = FALSE)
dev.off()
dir()

barplot(table(salmon.data$country, salmon.data$year), ylab = 'Effectif')

matplot(table(salmon.data$year, salmon.data$river),
        type = 'b', lwd = 2)

#######################################

plot(x = salmon.data$weight,
     y = salmon.data$length,
     pch = '.', ylim = c(60, 100),
     col = salmon.data$river)

legend(x = 'right', 
       fill = salmon.data$river,
       legend = levels(salmon.data$river))

mtext(side = 3, line = 3, 'Titre', col = "red")







