########################################################################
###           graphics
#######################################################################

################
### basic graph

plot(x = my.data.frame$a,
     y = my.data.frame$b, 
     col = 'blue', pch = 16, type = 'b',
     xlab = 'A', ylab = 'B', main = 'A Vs B')

######################################
### draw multiple graphics in one panel

par(mfrow = c(2, 3),
    oma = c(6, .5, .5, .5),
    mar = c(.5, .5, .5, 4))

plot(rnorm(n = 10))
plot(rnorm(n = 10))
plot(rnorm(n = 10))
plot(rnorm(n = 10))
plot(rnorm(n = 10))
plot(rnorm(n = 10))

####################
### save a graph

par(mfrow = c(1,1))

pdf(file = 'figure1.pdf',
    width = 4, height = 3,
    useDingbats = F)
# sizes are specific to function, pixels, inches...
# useDingbats = F, argument specif to pdf,
# it avoid display issues

plot(rnorm(n = 10))

dev.off()





########################################################################
###     functions
########################################################################

###########################
### simple function example 

my.function <- function(){
  print(Sys.time())
}

my.function()

###########################
### 'complicated' function drawing a triangle made of stars

stars.triangle <- function (max.stars){
  result <- 0
  for(i in 1:max.stars){
    cat("\n", rep("*", i))
    result <- result + i
  }
  return(result)
}

stars.triangle(10)