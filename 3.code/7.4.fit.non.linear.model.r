#clear environment
rm(list = ls())

# set working directory
wd <- 'C:/Users/gbal/Desktop/r.training/2.data'
setwd(wd)


# load and clan data ==========================================================

# load salmon .data
salmon <- read.table('salmon.data.raw.txt', sep = ',', dec = '.', h = TRUE)

# correct weight length data for converson issues
salmon$weight[!is.na(salmon$weight) & salmon$weight > 50] <- salmon$weight[!is.na(salmon$weight) & salmon$weight > 50] / 1000
salmon$length[!is.na(salmon$length) & salmon$length < 44] <- salmon$length[!is.na(salmon$length) & salmon$length < 44] * 2.54


# fit length weight relationship ==================================================

#install.packages(optimx)

require(optimx)


# define model to fit, need to create function
length.weight <- function(param, length, weight){
  a <- exp(param[1])
  b <- exp(param[2])
  sd.l <- exp(param[3])
  weight.pred <- a * length ^ b
  nll <- dlnorm(weight, meanlog = log(weight.pred) - (sd.l ^ 2) / 2, sdlog = sd.l, log = TRUE)
  return(- sum(nll))
}


# fit method selection if wished
optimx.methods.selected <- c('Nelder-Mead', 
                             #'BFGS', 
                             #'CG', 
                             #'L-BFGS-B', 
                             #'nlm', 
                             'nlminb'#, 
                             #'spg', 
                             #'ucminf', 
                             #'newuoa',
                             #'bobyqa',
                             #'nmkb'#, 
                             #'hjkb',
                             #'Rcgmin',
                             #'Rvmmin'
)


# run fit with optimix function
optimx.fit <- optimx(par = c(1, 1, 1),
                     fn = length.weight, 
                     length = salmon$length, 
                     weight = salmon$weight * 1000,
                     #method = optimx.methods.selected 
                     #control = list(save.failures = TRUE)
                     control = list(all.methods = TRUE, save.failures = TRUE)
)

# check fit ====================================================================

plot(salmon$length, salmon$weight * 1000)
# add fitted curve
curve(exp(optimx.fit$p1[1]) * (x ^ exp(optimx.fit$p2[1])),
      col = 'red',
      lwd = 3,
      add = TRUE)

# lm fit fit based on log tranformation
lm(log(salmon$weight * 1000) ~ log(salmon$length))