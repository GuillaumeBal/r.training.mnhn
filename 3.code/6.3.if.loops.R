# create sequence
seq.numbers <- runif(6, -2, 2)

# if number positive, print number position and yes
for(n in 1:length(seq.numbers)){
  if(seq.numbers[n] > 0){ 
    print(paste(n, 'yes'))
  }
}

# if number positive, print number position and yes, 
# otherwise position and no
for(n in 1:length(seq.numbers)){
  if(seq.numbers[n] > 0){ 
    print(paste(n, 'yes'))
  }else{
    print(paste(n, 'no'))
  }
}

# more complicated examples =================================================================

### Next

mymat <- matrix(nrow = 5, ncol = 5) # create a 5 x 5 matrix

for(i in 1:nrow(mymat)){  # for each row
  
  for(j in 1:ncol(mymat)){ # for each column
    
    Sys.sleep(2)
    cat("\014")
    message("\ni = ", i, "\nj = ", j, "\ni * j = ", i * j, "\n")
    
    if((i * j > 7) && (i * j < 14)) {
      print(mymat)
      next()  ## jumps to next iteration
    }
    
    mymat[i, j] <-  i * j # assign values based on position: product of two indexes
    print(mymat)
    
  }
  
}

### break, stop innermost loop
mymat <- matrix(nrow = 5, ncol = 5) # create a 5 x 5 matrix

for(i in 1:nrow(mymat)){  # for each row
  for(j in 1:ncol(mymat)){ # for each column
    
    Sys.sleep(2)
    cat("\014")
    message("\ni = ", i, "\nj = ", j, "\ni * j = ", i * j, "\n")
    
    if((i * j > 7) && (i * j < 14)) {
      message("Breaks loop over j (jumps to next iteration of i)!!!")
      print(mymat)
      break()  ## Stops the current loop (over values of j)! 
               ## => jumps directly to the next row.
    }
    
    mymat[i, j] <-  i * j # assign values based on position: product of two indexes
    print(mymat)
    
  }  
}

