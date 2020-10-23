##########################################################################
###                         loops
##########################################################################

# basic for loop ======================================================================

for(i in 1:10){
  print(paste('hello' , i,  sep = ' '))
}
print(i)


# basic while loop ====================================================================

start.time <- Sys.time() # get sys.time
comp.time <- 0 # initialize computing time allowed
n <- 0 # create variable n
while(comp.time < 10){
  n <- n + 1 # increment n
  comp.time <- as.numeric(difftime(Sys.time(),  start.time, units = 's')) # update time spent computing
}
print(n)

# nested for loops: multiplication table ===============================================================

mymat <- matrix(nrow = 20, ncol = 20) # create a 20 x 20 matrix

for(i in 1:dim(mymat)[1]){  # for each row
  for(j in 1:dim(mymat)[2]){ # for each column
    mymat[i, j] = i * j # assign values based on position: product of two indexes
  }
}

print(mymat)

# nested for and if loops : multiplication table ======================================================

mymat <- matrix(nrow = 20, ncol = 20) # create a 20 x 20 matrix

for(i in 1:dim(mymat)[1]){  # for each row
  for(j in 1:dim(mymat)[2]){ # for each column
    if(i * j < 20){
      mymat[i, j] = i * j # assign values based on position: product of two indexes
    }else{
      mymat[i, j] <- 'BIG'  
      #break()
      #next()  
    }  
  }
}

print(mymat)
