# section 5 ==================================================================================

# make plot basic =====================

#plot(salmonRaw$length, salmonRaw$weight,
#     xlim = c(0, 150),
#     ylim = c(0, 20))

### base plot

plot(salmonTidy$length, salmonTidy$weight)

### inches to cm correction

salmon.inches <- which(salmonTidy$length < 46)

salmonTidy$length[salmonTidy$length < 46] <- 
  salmonTidy$length[salmonTidy$length < 46] * 2.54

plot(salmonTidy$length, salmonTidy$weight)

points(salmonTidy$length[salmon.inches], salmonTidy$weight[salmon.inches],
       col = 'blue')

### question 1, plot for Moy and  sea.age. 1

lines.moy.seaage.x <- which(salmonTidy$river == 'moy' & salmonTidy$sea.age == 4)
plot(salmonTidy$length[lines.moy.seaage.x], salmonTidy$weight[lines.moy.seaage.x])


plot(weight ~ length, 
     data = subset(salmonTidy,
                   river == "moy" & sea.age == 4))

## question 2, weight fish from ireland

ie <- which(salmonTidy$country == 'ie')
irish.salmon <- salmonTidy[ie, ]
hist(irish.salmon$weight, breaks = seq(0, 14, .1))

## question 3 boxplot weight per year

boxplot(salmonTidy$weight ~ salmonTidy$year * salmonTidy$country * 
          salmonTidy$sea.age)

boxplot(weight ~ year * country, data = salmonTidy)

plot(weight ~ length, data = salmonTidy)

## question 4, save graphics

pdf('mypdf.pdf',
    height = 4, width =  7)

boxplot(weight ~ year * country, data = salmonTidy)

plot(weight ~ length, data = salmonTidy)

dev.off()

## question 5 barplot n fish per year

barplot(table(salmonTidy$year))

barplot(table( salmonTidy$country, salmonTidy$year), beside = TRUE)

## question 6 matplot river year

matplot(table(salmonTidy$year, salmonTidy$river), type = 'l')


#### customizing graphics

plot(salmonTidy$length[salmonTidy$river == 'moy'],
     salmonTidy$weight[salmonTidy$river == 'moy'],
     pch = '.', col = 'pink',
     ylim = c(0, 10),
     ylab = 'weight'
)



plot(salmonTidy$length[salmonTidy$river == 'moy'],
     salmonTidy$weight[salmonTidy$river == 'moy'],
     pch = '.', col = 'red',
     ylim = c(0, 10),
     ylab = 'weight')


points(salmonTidy$length[salmonTidy$river == 'corrib'],
       salmonTidy$weight[salmonTidy$river == 'corrib'],
       pch = '.', col = 'blue'
)


plot(salmonTidy$length,
     salmonTidy$weight,
     pch = '.', col = salmonTidy$river,
     ylim = c(0, 10),
     ylab = 'weight')

#colors()

### global parameters graphics

#x11()

pdf(file = '3.graph.pdf',
    width = 7, height = 4)

par(mfrow = c(1, 3),
    oma = c(.1, .1, .1, .1))

plot(salmonTidy$length,
     salmonTidy$weight)
points(80, 6, pch = 19, col = 'red', cex = 3)


hist(salmonTidy$length)

boxplot(salmonTidy$weight ~ salmonTidy$river)

dev.off()

## graphics.off() ## if nothing in the file after you used dev.off()... 
##        may be opened several time => closes all graphics.