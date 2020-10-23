rm(list = ls())

# reloading data using paste function ======================================================

# create root directory address
root.wd <- 'Y:/MI/Workshops - Trainnings/R-Beginners/r.training'
setwd(root.wd)

# create data directory address
data.folder <- paste(root.wd, '/2.data/2.salmon/', sep ='' )

# use data directory to load data 
salmon <- read.table(paste(data.folder, 'salmonTidy.csv', sep = ''),
                     header = TRUE, sep = ',')

# use apply and tapply =============================================================

#subset for numerical columns
salmon.num <- salmon[ , c('year', 'julian.day', 'length', 'weight')]
salmon.num <- subset(salmon, select = c('year', 'julian.day', 'length', 'weight'))

# create 10 * 10 matrix
r.matrix<- matrix(rnorm(100),
                  nrow = 10,
                  ncol = 10)

# use apply accross columns
apply(r.matrix, MARGIN = 2, FUN = mean)
#apply(FUN = mean, X = r.matrix, MARGIN = 2)

# use apply, quantiles and special arguments
apply(r.matrix, MARGIN = 2, 
      FUN = quantile, 
      probs = seq(from = 0.2, to = 0.8, by = 0.2))

# introduce NA values
salmon$length[seq(from = 1, to = length(salmon$length), by = 100)] <- NA 

# use tapply on salmon data to compute mean lengthes by combination of factors
tapply(X = salmon$length, INDEX = list(salmon$year, salmon$river), 
       FUN = mean, na.rm = TRUE)

# LOOPS ===============================================================================================

for(a in c('may', 'april', 'june')){
  print(a)
}

for(a in seq(3, 25, 2.5)){
  result <- paste('result =', a + 3, sep = ' ')
  print(result)
}

##
rivers <- levels(salmon$river) ## unique river names

r <- rivers[1]
salmon.river <- salmon[salmon$river == r, ]
#unique(salmon.river$river)
file.name <- paste0('salmon.', r, '.txt')
write.table(x = salmon.river, file = file.name, 
            sep = ',', dec = '.', row.names = FALSE)


for(r in rivers){
  salmon.river <- salmon[salmon$river == r, ]
  #unique(salmon.river$river)
  file.name <- paste0('salmon.', r, '.txt')
  write.table(x = salmon.river, file = file.name,
              sep = ',', dec = '.', row.names = FALSE)
}

#### plot per column

cc <- 6

class(salmon[ , cc]) == 'numeric'

boxplot(salmon[ , cc],
        main = colnames(salmon)[cc])


for(cc in 1:dim(salmon)[2]){
  
  if(class(salmon[ , cc]) == 'numeric'){
    
    pdf(paste0(colnames(salmon)[cc], '.pdf'))
    
    boxplot(salmon[ , cc],
            main = colnames(salmon)[cc])
    
    dev.off()
    
  }else{
    
    pdf(paste0(colnames(salmon)[cc], '.pdf'))
    
    barplot(table(salmon[ , cc]))
    
    dev.off()
    
  }
  
}


