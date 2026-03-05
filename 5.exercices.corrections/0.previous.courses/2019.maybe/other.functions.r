wd <-'C:/Users/gbal/Desktop/r.training/'
setwd(wd)


n.matrices <- 40
dim.1 <- 100
dim.2 <- 10

for(m in 1:n.matrices){
  sample <- rnorm(n = dim.1 * dim.2, mean = 10, sd = 1)
  my.matrix <- matrix(data = sample, nrow = dim.1, ncol = dim.2)
  colnames(my.matrix) <- letters[1:dim.2]
  my.file.name <- paste('my.matrix', m, 'txt', sep = '.')
  write.table(x = my.matrix, file = my.file.name)  
}





