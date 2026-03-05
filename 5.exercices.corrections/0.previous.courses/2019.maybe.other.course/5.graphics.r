rm(list = ls())

wd.main <- 'C:/Users/gbal/Desktop/r.training'
setwd(wd.main)

salmon <- read.table('2.data/2.salmon/salmon.data.raw.2.txt',
                     header = TRUE,
                     sep = ';',
                     dec = ',')

head(salmon)

salmon$weight[which(salmon$weight > 50)] <- 
  salmon$weight[which(salmon$weight > 50)] / 1000

salmon$length[which(salmon$length < 44)] <- 
  salmon$length[which(salmon$length < 44)] * 2.54

# 5.1.1 ============================================================

# Plot length Vs weight of sea.age = 1 fish from Moy river

#salmon$sea.age == 1 & salmon$river == 'moy'
plot(x = salmon$weight[salmon$sea.age == 1 & salmon$river == 'moy'], 
     y = salmon$length[salmon$sea.age == 1 & salmon$river == 'moy'])

# option 2
idx.a1.moy <- salmon$sea.age == 1 & salmon$river == 'moy'
plot(x = salmon$weight[idx.a1.moy], 
     y = salmon$length[idx.a1.moy])

#option 3
salmon.subset <- salmon[idx.a1.moy, ]
plot(salmon.subset$weight,
     salmon.subset$length)

#Histogram weight of fish from Ireland
hist(salmon$weight[salmon$country == 'ie'])
hist(salmon$weight[salmon$river %in% c('moy', 'corrib')],
     xlab = 'Weight (kg)',
     main = 'Weight Ireland',
     col = 'pink',
     breaks = seq(from = 0, to = 20, by = .1))

#Boxplot of weight with a box per year
boxplot(salmon$weight ~ salmon$river.age * salmon$year, outline = FALSE)

#Save some of your graphics
pdf(file = 'my.first.pdf.pdf',
    useDingbats = FALSE)
#jpeg(file = 'graph.2.jpg') # here save only last one
boxplot(salmon$weight ~ salmon$river.age * salmon$year, outline = FALSE)
plot(salmon$weight)
dev.off()

#Barplot number of fish per year (contengency table)
barplot(table(salmon$year))

#Matplot number fish per river per year (if time)

# 5.2.1 =======================================================

ixd <- salmon$river == 'corrib' #&
plot(salmon$weight[ixd],
     salmon$length[ixd],
    # ylim = c(60, 90),
    # xlim = c(2, 5),
     pch = 16,
     cex = .5,
    col = salmon$sea.age[ixd])

legend(
  'topleft',
  legend = sort(unique(salmon$sea.age[!is.na(salmon$sea.age)])),
  col = sort(unique(salmon$sea.age[!is.na(salmon$sea.age)])),
  pch = 16
  )

# 5.3.1 ===========================================================

#use x11() to create new graph windows
#x11()

pdf('multi.plot.pdf', useDingbats = FALSE)

#Use argument mfrow of par() to allow for 3 graphics
par(mfrow = c(1, 3))

#Use argument oma of par() to make really small outer margins
#Add 3 graphics on the figure panel (your choice)
par(oma = c(1, 1, 1, 18), mar = c(1, 1, 1, 1))
barplot(table(salmon$year))
barplot(table(salmon$river))
par(mar = c(4, 4, 4, 4))
barplot(table(salmon$sea.age))

#Save that panel as a .pdf file
dev.off()
