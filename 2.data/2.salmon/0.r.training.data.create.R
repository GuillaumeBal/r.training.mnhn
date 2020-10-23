rm(list = ls())

wd <- "C:/Users/gbal/Desktop/r.training/2.data/2.salmon"
setwd(wd)

#library for xlsx file
require(openxlsx)
# library for dirichlet distribution
require(gtools)

# number of  years ==============================================

year.start <- 2000
year.end <- 2017
n.years <- year.end - year.start + 1

# decresing abundance wild fish ====================================

wild.abundances.start <- 1000
wild.abundances.cv <- .1
wild.abundances <- seq(from = wild.abundances.start , to = wild.abundances.start / 3, length.out = n.years)
wild.abundances <- round(rnorm(n = n.years, mean = wild.abundances, sd = wild.abundances * wild.abundances.cv), 0) # noise
plot(wild.abundances)

# ratio abundance ranched fish abundance ======================================== 

ratio.wild.farmed <- 1.33
ranched.abundances <- ceiling(wild.abundances * rnorm(n = n.years, mean = ratio.wild.farmed, sd = .15))
plot(wild.abundances, ranched.abundances)
curve(ratio.wild.farmed * x, col = 'grey', add = T)

# main structure of the data table ==================================================

salmons.var <- c('year',
                 'julian.day',
                 'wild',
                 'river.age',
                 'sea.age',
                 'length', 
                 'weight',
                 'sea.lice')

salmons <- matrix(nrow = sum(c(wild.abundances, ranched.abundances)), ncol = length(salmons.var),
                  dimnames = list(NULL,
                                  salmons.var))
salmons <- as.data.frame(salmons)
#dim(salmons)
#head(salmons)

# wild or ranched =================================================

salmons$wild <- c(rep(1, sum(wild.abundances)), rep(2, sum(ranched.abundances))) 
#table(salmons$wild)

# year capture ============================================================

salmons$year <- c(rep(year.start:year.end, times = wild.abundances),
                  rep(year.start:year.end, times = ranched.abundances))
hist(salmons$year)

# ages of fish =================================================================

# dirchlet parameters for increasing proportion 1sw fish
prob.ages.year <- matrix(nrow = n.years, ncol = 4)
for (y in 1:n.years){
  prob.ages.year[y , ] <- c(64 * (1 + .15 * (y - 1)), 
                            128, 
                            32, 
                            16)
}

# sapply loop to sample fish age
salmons$sea.age <- sapply(1:dim(salmons)[1], 
                          function(i){
                            sample(size = 1, x = 1:4, 
                                   prob = rdirichlet(1, alpha = prob.ages.year[salmons$year[i] - min(salmons$year) + 1, ]),
                                   replace = T)
                          } )
#hist(salmons$sea.age)

# salmon river age ==================================================================

wild.row.max <- max(which(salmons$wild == 1))

salmons$river.age[1:wild.row.max] <- sample(size = wild.row.max, x = 1:3, prob = c(0.4, 0.5, 0.10), replace = T)
salmons$river.age[(wild.row.max + 1):dim(salmons)[1]] <- sample(size = dim(salmons)[1] - wild.row.max, x = 1:3, prob = c(0.8, .19, 0.01), replace = T)

barplot(table(salmons$river.age, salmons$wild))  

# salmon river ========================================================

rivers.names <- c('corrib', 'moy', 'oir', 'scorff', 'nivelle')
salmons$river <- rivers.names[n = sample(dim(salmons)[1], x = 1:5, replace = T, 
                                         prob = c(.29, .26, .1, .2, .15))]
summary(salmons)

# salmon country ======================================================

salmons$country <- 'fr'
salmons$country[salmons$river %in% c('corrib', 'moy')] <- 'ie'


# day of capture with age + year + wild effect ==================================

salmons$julian.day <- 90 + 
  (salmons$sea.age == 1) * 50 +
  (salmons$year - min(1 * salmons$year)) +
  (salmons$wild == 2) * 15 +
  rnorm(dim(salmons)[1], 0, 10)
hist(salmons$julian.day)

# sex, more males in 1 SW =========================================================================

sw1 <- which(salmons$sea.age == 1)
swm <- which(salmons$sea.age != 1)
salmons$sex[sw1] <- sample(size = length(sw1), x = 1:2, prob = c(65, 35), replace = T)
salmons$sex[swm] <- sample(size = length(swm), x = 1:2, prob = c(20, 80), replace = T)

# length, Von model with age, country effect and time =============================================  

source('growth.r')

# weight through length weight relationship ======================================================

salmons$weight <- exp(- 5 + .2 * (salmons$wild == 1)  +  3 * log(salmons$length) + 
                        rnorm(dim(salmons)[1], 0, .1)) / 1000 # for grams to kg
salmons$weight <- round(salmons$weight, 2)
hist(salmons$weight)
plot(salmons$length, salmons$weight, col = salmons$wild)

# sea lice infection logit age ==================================================================

salmons$sea.lice <- inv.logit(-2 + 0.075 * (salmons$length - mean(salmons$length)) + rnorm(dim(salmons)[1], 0, .2))
salmons$sea.lice <- rbinom(n = dim(salmons)[1], size = 1 ,  prob = salmons$sea.lice)
plot(salmons$length, salmons$sea.lice)

# weight outliers, grams instead kg  ===============================================

weight.issue.index <- sample(78, x = 1:dim(salmons)[1], replace = F)
salmons$weight[weight.issue.index] <- salmons$weight[weight.issue.index] * 1000

# length outliers, conversion issue inches ==========================================
length.issue.index <- salmons$year == 2007 & salmons$country == 'ie'
salmons$length[length.issue.index] <- salmons$length[length.issue.index] / 2.54

# infected as true or false =======================================================
salmons$sea.lice <- as.logical(salmons$sea.lice)

# break lines order ==================================================================
salmons <- salmons[sample(size = dim(salmons)[1] , x = 1:dim(salmons)[1], replace = F), ]


# add NA in data ===================================================

n.na <- 300

# pick random location for na
na.rows <- sample(size = n.na, x = 1:dim(salmons)[1], replace = F)
na.cols <- sample(size = n.na, x = 1:dim(salmons)[2], replace = T)

# modify to na
for (i in 1: n.na) salmons[na.rows[i], na.cols[i]] <- NA

# round some values ================================================================

salmons$length <- round(salmons$length, 1)
salmons$julian.day <- round(salmons$julian.day, 0)
salmons$wild <- (salmons$wild == 1) * 1
salmons$sex[salmons$sex == 1] <- 'M'
salmons$sex[salmons$sex == 2] <- 'F'

n.dates <- 27
salmons$julian.day[sample(1:nrow(salmons), size = n.dates, replace = FALSE)] <- sample(366:500, size = n.dates, replace = FALSE)

# write data in files =================================================================

summary(salmons)

write.table(salmons, file = 'salmon.data.raw.txt', sep =',', dec = '.', row.names = F)
write.table(salmons, file = 'salmon.data.raw.2.txt', sep =';', dec = ',', row.names = F)
write.xlsx(salmons, file = 'salmon.data.raw.xlsx')