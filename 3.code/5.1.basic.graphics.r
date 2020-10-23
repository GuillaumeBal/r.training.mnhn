#clear environment
rm(list = ls())

# set working directory
wd <- 'C:/Users/guillaume/Desktop/r.training/2.data'
setwd(wd)


# load salmon .data
salmon <- read.table('salmon.data.raw.txt', sep = ',', dec = '.', h = TRUE)


# multiple plot on panel
#par(mfcol = c(3, 2))

# regular plot =============================================

salmon$weight[!is.na(salmon$weight) & salmon$weight > 50] <- salmon$weight[!is.na(salmon$weight) & salmon$weight > 50] / 1000

salmon$length[!is.na(salmon$length) & salmon$length < 44] <- salmon$length[!is.na(salmon$length) & salmon$length < 44] * 2.54


#plot length vs weight
plot(x = salmon$length, y = salmon$weight,
     pch = '.', col = 'blue',
     xlab = 'Length (cm)', ylab = 'Weight (g)',
     main ='Lenght weight relationship')

# weight histogram
hist(x = salmon$weight)

# boxplot weight per river
boxplot(salmon$weight ~ salmon$river,
        outline = FALSE,
        ylab = 'Weight (Kg)',
        main = 'Salmon weight per River')

# pairs function
pairs(salmon[ , c(2, 6, 7)])


# barplot contengency table
barplot(table(salmon$sea.age, salmon$river),
        main = 'Number fish per sea age',
        ylab = 'Abundance')

# compute weight by river per year for 1 sw fish
weight.river.year.1sw <- by(data = salmon$weight[salmon$sea.age == 1], 
                        INDICES = list(salmon$river[salmon$sea.age == 1], salmon$year[salmon$sea.age == 1]), 
                        FUN = mean, na.rm = TRUE)
# reorganize as matrix
weight.river.year.1sw <- matrix(unlist(weight.river.year.1sw),
                            ncol = length(unique(salmon$river)),
                            nrow = length(unique(salmon$year)),
                            byrow = TRUE)

# plot multiple time series
matplot(x = weight.river.year.1sw, type = 'l',
        xlab = 'Year index', ylab = 'Mean weight (Kg)')

# save a graph =================================================================

pdf(file = 'matplot.pdf',
    useDingbats = FALSE)

matplot(x = weight.river.year.1sw, type = 'l',
        xlab = 'Year index', ylab = 'Mean weight (Kg)')

dev.off()