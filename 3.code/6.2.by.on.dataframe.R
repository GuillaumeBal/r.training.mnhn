rm(list = ls()) # clean environment

# modify working directory
wd <- "C:/Users/gbal/Desktop/r.training/2.data"
setwd(wd)

# n values to generate
n.values <- 50

# create two random series of 1 and 2
random.1.2.a <- sample.int(n = 2, size = n.values, replace = TRUE)
random.1.2.b <- sample.int(n = 2, size = n.values, replace = TRUE)

# create data frame with 2 quantitative and 2 qualitative columns
my.data.frame <- data.frame(a = runif(n.values),
                            b = rnorm(n.values, mean = 10, sd = 1),
                            age = c('1sw', '2sw')[random.1.2.a],
                            sex = c('female', 'male')[random.1.2.b])

# 2 by 2 plot area
par(mfrow = c(2, 2))

# plot a and b by combination of age and sex
by(data = my.data.frame,
   INDICES = list(my.data.frame[, 3],  my.data.frame[ , 4]),
   FUN = function(x) plot(x[ , 1], x[ , 2],
                          main = paste(x[1, 3],
                                       x[1, 4],
                                       sep = ' / ')))


###################################################################################
### other example using salmon data

# read salmon data
#salmon.data <- read.table('salmon.data.raw.txt', sep = ';', dec = ',', header = TRUE)
salmon.data <- read.table('salmon.data.raw.txt', sep = ',', dec = '.', header = TRUE)
dim(salmon.data)
head(salmon.data)

# change weight in g into kg
salmon.data$weight[salmon.data$weight > 50] <-
  salmon.data$weight[salmon.data$weight > 50] / 1000

# open new graphic windows
X11(10, 10)

# set graphical parameters
par(mfrow = c(4, 6), mar = c(3, 3, 2, 0.5)+0.1,
    mgp = c(2, 0.8, 0))

# plot lenght weight relationship per year
by(data = salmon.data,
   INDICES = list(salmon.data$year),
   FUN = function(dataf){
     plot(x = dataf$length,
          y = dataf$weight,
          col = "blue", pch = ".",
          main = dataf$year[1])
   })

# open new graph windows
X11(10, 10)

# set graphical parameters
par(mfrow = c(4, 6), mar = c(3, 3, 2, 0.5)+0.1,
    mgp = c(2, 0.8, 0))

# plot lenght weight relationship per year with custom titles
by(data = salmon.data,
   INDICES = list(salmon.data$year, salmon.data$sex),
   FUN = function(subSalmon){
     plot(x = subSalmon$length,
          y = subSalmon$weight,
          col = "blue", pch = ".",
          main = paste(subSalmon$year[1], c("F", "M")[subSalmon$sex[1]]))
   })

