#####################################################
## r training graphics exercises ###
#####################################################



rm(list = ls()) # erase everything in memory

# set working directory
wd <- 'C:/Users/gbal/Desktop/r.training/2.data'
setwd(wd)
getwd()

# loading data
salmon <- read.table('salmon.data.raw.txt', sep = ',', dec = '.', h = TRUE)
head(salmon)

# data correction, length anbd weight back to kg and cm
salmon$weight[!is.na(salmon$weight) & salmon$weight > 50] <- salmon$weight[!is.na(salmon$weight) & salmon$weight > 50] / 1000
salmon$length[!is.na(salmon$length) & salmon$length < 44] <- salmon$length[!is.na(salmon$length) & salmon$length < 44] * 2.54


pdf(file = 'all.graph.pdf') # create pdf storiung a few figures

# plot length weight fish moy river age 1 =====================================================================

# create logical vector to subset data
select.moy.age.1 <- salmon$river == 'moy' & salmon$sea.age == 1
plot(x = salmon$length[select.moy.age.1], y = salmon$weight[select.moy.age.1])

salmon.moy.1 <- salmon[select.moy.age.1, ]
plot(salmon.moy.1$length, salmon.moy.1$weight)

# Histogram weight of fish from Ireland ============================================================

hist(salmon$weight[salmon$country == 'ie'],
     main = 'Weight all fish',
     ylab = 'number of fish',
     xlab = 'Weight (Kg)')

# Boxplot of weight with a box per year ===========================================

boxplot(salmon$weight ~ salmon$year,
        outline = FALSE)

dev.off() # close pdf

# Barplot number of fish per year ====================================

number.fish.per.year <- table(salmon$year) # table(salmon$year, salmon$river)
barplot(number.fish.per.year, #t(number.fish.per.year)
        ylab = 'Abundance',
        xlab = 'Year')

# Matplot number fish per river per year ==============================

n.fish.river.year <- table( salmon$year, salmon$river)
matplot(n.fish.river.year, type = 'l')

legend('top',
       legend = colnames(n.fish.river.year),
       lty = 1:ncol(n.fish.river.year),
       col = 1:ncol(n.fish.river.year),
       ncol = 3,
       bty = 'n')

# play with colors and legends ================================================

levels(salmon$river)

my.colors <- c('darkgreen', 'lightgreen', 'red', 'orange', 'yellow')

plot(salmon$length, salmon$weight, pch = '.',
     col = my.colors[salmon$river])

head(salmon$river, 10)
head(my.colors[salmon$river], 10)

par(xpd=TRUE)
legend('topleft',
       legend = levels(salmon$river),
       col = my.colors,
       #pch = 16,
       lty = 1,
       bty = 'n', 
       ncol = 2, inset = -1)

