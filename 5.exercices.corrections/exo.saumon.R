rm(list = ls())

#install.packages('magrittr')
library("magrittr")
require(ggplot2)
require(patchwork)

getwd()
wd <- 'C:/Users/gbal/Desktop/r.training.mnhn/5.exercices.corrections'
setwd(wd)

dir('../2.data/2.salmon')


# load from txt
salmon.data.raw <- 
  read.table('../2.data/2.salmon/salmon.data.raw.txt',
             header = TRUE, sep = ',', dec = '.')
tail(salmon.data.raw, 6)

# load from excel file
#install.packages('openxlsx')
#require(openxlsx)

# the raw data 
salmon.data.raw <- 
  openxlsx::read.xlsx('../2.data/2.salmon/salmon.data.raw.xlsx')
?openxlsx::read.xlsx

# the one I work on
salmon.GB <- salmon.data.raw
salmon.GB %>% summary

write.table(
  salmon.GB,
  'salmonGB.txt',
  row.names = FALSE,
  sep = ',',
  dec = '.'
)

# exo 4.2.1 ===================================

salmonTidy <- 
  salmon.data.raw

attributes(salmonTidy)

salmonTidy %>% dim
salmonTidy %>% summary()

table(salmonTidy$weight > 30, useNA = 'always')

salmonTidy$weight[which(salmonTidy$weight > 30)] <- 
  salmonTidy$weight[which(salmonTidy$weight > 30)] / 1000

salmonTidy$weight[salmonTidy$weight > 30 & !is.na(salmonTidy$weight)] <- 
  salmonTidy$weight[which(salmonTidy$weight > 30)] / 1000

# recode wild as TRUE/FALSE
salmonTidy$wild <- salmonTidy$wild == 1
summary(salmonTidy$wild)

# save as csv
write.csv2(x = salmonTidy, 'salmonTidy.csv')
write.table(x = salmonTidy, file = 'salmonTidy.csv',
            sep = ';', dec =',', row.names = FALSE)


leap.year <- seq(from = 2000, to = 2030 , by = 4)
date.issue <-
  (salmonTidy$julian.day > 365 &
     !(salmonTidy$year %in% leap.year)) | 
  (salmonTidy$julian.day > 366 &
     salmonTidy$year %in% leap.year)| 
  is.na(salmonTidy$julian.day)

salmonTidy <-
  salmonTidy[-which(date.issue), ]

salmonTidy %>% dim
salmon.data.raw %>% dim

# 5.1.1 =============================================

salmonTidy <- 
  salmonTidy[salmonTidy$length >=50, ]

#Plot length Vs weight of sea.age = 1 fish from Moy river

ggplot(data = salmonTidy[salmonTidy$sea.age == 1 &
                           salmonTidy$river == 'moy', ]) +
  geom_point(aes(x = length, y = weight)) +
  geom_smooth(method = 'lm',
              aes(x = length, y = weight),
              formula = y~x) 

#Histogram weight of fish from Ireland
ggplot(data = salmonTidy[salmonTidy$country == 'ie', ],
       aes(y = weight)) +
  geom_histogram(binwidth = 0.1) +
  ggtitle('Weight by 0.1 kg')

# Boxplot of weight with a box per year
my.boxplot <- 
  ggplot(data = salmonTidy,
         aes(y = weight, x = year %>% as.factor(), 
             fill = sea.age %>% as.factor)) +
  geom_boxplot()

# Save some of your graphics
pdf('my.graph.pdf', width = 10, height = 4)
print(my.boxplot)
dev.off()

#Barplot number of fish per year
ggplot(data = salmonTidy,
       aes(x = year %>% as.factor(), 
           fill = sea.age %>% as.factor)) +
  geom_bar()

# Plot number fish per river per year, all on one panel
# see above change age by river

# Barplot number of fish per year from compute stats
n.fish.per.year <-
  table(salmonTidy$year) %>% 
  reshape2::melt() %>% 
  `colnames<-`(c('year', 'number'))

ggplot(data = n.fish.per.year,
       aes(x = year, 
           y = number)) +
  geom_bar(stat = 'identity') 


# exo 5.2.1 ================================================

# fish from moy, 1 year and wild, playing with options
size.moy.1.wild <- 
  ggplot(
    data = salmonTidy[
      salmonTidy$river == 'moy' &
        salmonTidy$sea.age == 1 &
        salmonTidy$wild, ],
    aes(x = length, y = weight)) +
  geom_point(col = 'red', pch = '.') +
  geom_smooth() +
  theme_classic()

# all fish with different colors
size.all <- 
  ggplot(
    data = salmonTidy,
    aes(x = length, y = weight, col = river), alpha = .1) +
  geom_point(pch = '.') +
  geom_smooth() +
  theme(legend.position = 'bottom')

pdf('my.dev.pdf')
(size.moy.1.wild + size.all) / size.all
dev.off()

salmonTidy[ , c("length", "weight")]
my.column <- c("length", "weight", 'julian.day')
salmonTidy[ , colnames(salmonTidy) %in% my.column]

# apply on some columns
apply(salmon.data.raw[,  c("length", "weight", 'julian.day')],
      MARGIN = 2, 
      FUN = mean,
      na.rm = TRUE)
sapply(salmon.data.raw[,  c("length", "weight", 'julian.day')],
       FUN = mean,
       na.rm = TRUE)
lapply(salmon.data.raw[,  c("length", "weight", 'julian.day')],
       FUN = mean,
       na.rm = TRUE)

# tapply weight per year and sea.Age
tapply(salmonTidy$weight,
       INDEX = 
         list(salmonTidy$year, salmonTidy$river),
       #salmonTidy[ , c("river", "sea.age")], 
       FUN = mean, 
       na.rm = TRUE)

table(salmonTidy$year, salmonTidy$river)

# exo 6.2.2 ========================================

indic.loop <- 1:9
my.vector <- rep(NA, length(indic.loop))
for(a in indic.loop){
  my.vector[a + 1] <- paste("result", a, sep = ' = ' )
  print(my.vector[a])
}
my.vector

# exo 6.2.3 ==============================================

nom.de.riviere <-
  #unique(salmonTidy$river[!is.na(salmonTidy$river)])
  levels(salmonTidy$river %>% as.factor())

for(river.index in nom.de.riviere){
  
  saumon.riviere <-
    salmonTidy[salmonTidy$river == river.index, ]
  
  saumon.riviere %>% head(., 4) %>% print(.)
  
  name <- paste('subset.river', river.index, 'txt', sep = '.')
  
  write.table(x = saumon.riviere, file = name, sep = ',', dec = '.')
  
  name.var <- name %>% gsub(., pattern = '.txt', replacement = '')
  
  assign(x = name.var, value = saumon.riviere)
  
}

# ordonner donnes
salmonTidy <- salmonTidy[
  order(salmonTidy$river, salmonTidy$year, decreasing = FALSE), ]

# exo 6.2.3 ==============================================

nom.de.riviere <-
  #unique(salmonTidy$river[!is.na(salmonTidy$river)])
  levels(salmonTidy$river %>% as.factor())

riviere.fr <- 
  levels(salmonTidy[salmonTidy$country == 'fr', 'river'] %>% as.factor)

#save only if river is french
for(river.index in nom.de.riviere){
  
  saumon.riviere <- salmonTidy[!is.na(salmonTidy$country) &
                                 !is.na(salmonTidy$river), ]
  
  saumon.riviere <-
    saumon.riviere[saumon.riviere$river == river.index, ]
  
  if(all(saumon.riviere$country == 'fr')){
    
    saumon.riviere %>% head(., 4) %>% print(.)
    
    name <- paste('subset.river', river.index, 'txt', sep = '.')
    
    write.table(x = saumon.riviere, file = name, sep = ',', dec = '.')
    
    name.var <- name %>% gsub(., pattern = '.txt', replacement = '')
    
    assign(x = name.var, value = saumon.riviere)
    
    print('french')
    
  }else{
    print('not french')
  }
  
}

saumon.riviere$country %>% table(useNA = 'always')

# exo 7.1.1 ==============================================================

power.it <- 
  function(a, b){
    if(!is.numeric(a) & !is.numeric(b)){
      stop('numbers please')
    }
    if(!is.integer(a) | !is.integer(b)){
      warning('not all integers')
    }
    return(a ^ b)
  }

power.it(3, 2.1)

body(power.it)
args(power.it)
