### next
mymat <- matrix(nrow = 5, ncol = 5) # create a 20 x 20 matrix

for(i in 1:dim(mymat)[1]){  # for each row
  for(j in 1:dim(mymat)[2]){ # for each column
    
    if((i * j > 7) && (i * j < 14)) {
      next()  ## jumps to next iteration
    }
    mymat[i, j] = i * j # assign values based on position: product of two indexes
  }  
}

print(mymat)

### break
mymat <- matrix(nrow = 5, ncol = 5) # create a 20 x 20 matrix

for(i in 1:dim(mymat)[1]){  # for each row
  for(j in 1:dim(mymat)[2]){ # for each column
    
    if((i * j > 7) && (i * j < 14)) {
      break()  ## Stops the current loop (over values of j)!
    }
    mymat[i, j] = i * j # assign values based on position: product of two indexes
  }  
}

print(mymat)