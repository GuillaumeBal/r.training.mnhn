# simple loops examples -----------------------------------------------

#check counter
for(i in 1:5){
  print(i)
}

# mathematical series
x <- rep(NA, 5)
x[1] <- 2
for(i in 2:length(x)) x[i] <- x[i - 1] ^ 2
x

# fill matrix with normal samples
matrix.norm <- matrix(NA, nrow = 10, ncol = 5) 
for(r in 1:dim(matrix.norm)[1]){
  matrix.norm[r, ] <- rnorm(n = dim(matrix.norm)[2], mean = r, sd = .3)
}
matrix.norm

# nested loops for --------------------------------------------------------------

mymat <- matrix(nrow = 5, ncol = 5) # create a 5 x 5 matrix

for(i in 1:nrow(mymat)){  # for each row
  
  for(j in 1:ncol(mymat)){ # for each column
    
    ## Just printing some information:
    Sys.sleep(2)
    cat("\014")
    message("\ni = ", i, "\nj = ", j, "\ni * j = ", i * j, "\n")
    
    mymat[i, j] <-  i * j # assign values based on position: product of two indexes
    
    ## Printing the result:
    print(mymat)
    
  }# end j loop
  
}# end i loop 