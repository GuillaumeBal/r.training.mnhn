# clean environment
rm(list = ls())

# set directory
wd <- "C:/Users/gbal/Desktop/r.training/3.code"
setwd(wd)

# create data for example ==================================================

# new folder to store things
new.folder <- 'excel.vs.r'
dir.create(new.folder)

# create random matrices and save them
n.matrices <- 100
for(m in 1:n.matrices){
  n.cols <- sample(size = 1, 10:100)
  n.rows <- sample(size = 1, 10:100)
  new.matrix <- matrix(round(rnorm(n = n.rows * n.cols, mean = 5, sd = 2), 2),
                       nrow = n.rows,
                       ncol = n.cols)
  write.table(new.matrix, file = paste0(new.folder, '/matrix.', m, '.txt'))
}

# calculate mean per column for file ! =======================================

# file location
file.loc <- "C:/Users/gbal/Desktop/r.training/3.code/excel.vs.r/matrix.1.txt"

# load file
data.1 <- read.table(file = file.loc)

#compute mean per columns
apply(X = data.1, MARGIN = 1, FUN = mean)


# mean per colums for each file and store ====================================

# time start part of script
time.start <- Sys.time()

# location files 
wd.files <-  "C:/Users/gbal/Desktop/r.training/3.code/excel.vs.r"

# get list of files 
list.files <- dir(wd.files)

# create list to store mean
means.list <- list()

# fill list
for(i in 1:length(list.files)){
#i <- 1
  file.name <- paste0(wd.files, '/',list.files[i])
  data <- read.table(file.name)
  dim(data)
  mean.c <- apply(data, 
                 2, 
                 mean)
  means.list[[list.files[i]]] <- mean.c 
  
}

# from list to matrix,  complicated because vector of different lengthes
means.matrix <- sapply(means.list, '[', seq(max(lengths(means.list))))

# store matrix
write.table(means.matrix, file = 'means.matrix.txt')

# time end 
time.end <- Sys.time()

# duration script
difftime(time.end, time.start)

# suppress things
file.remove('means.matrix.txt')

# suppress data directory
unlink(wd.files, recursive = TRUE)