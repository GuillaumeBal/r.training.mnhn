rm(list = ls())

wd.main <- 'C:/Users/gbal/Desktop/r.training'
setwd(wd.main)

salmon <- read.table('2.data/2.salmon/salmon.data.raw.2.txt',
                     header = TRUE,
                     sep = ';',
                     dec = ',')

head(salmon)

salmon$weight[which(salmon$weight > 50)] <- 
  salmon$weight[which(salmon$weight > 50)] / 1000

salmon$length[which(salmon$length < 44)] <- 
  salmon$length[which(salmon$length < 44)] * 2.54

#============================================================================

for(c in 1:dim(salmon)[2]){
  
  if(is.numeric(salmon[ , c])){
    boxplot(salmon[ , c], 
            main = colnames(salmon)[c]) 
  }else{
    barplot(table(salmon[ , c]),
            main = colnames(salmon)[c])
  }
  
}


