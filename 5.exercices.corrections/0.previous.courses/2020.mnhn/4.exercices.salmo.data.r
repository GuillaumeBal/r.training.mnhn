#Save data linked to each river from the salmon dataset into a different '.txt' 

river.names <- unique(salmon.data$river)
for(r in river.names){
  my.file.name <- paste(r, 'txt', sep = '.')
  my.data.to.save <- salmon.data[salmon.data$river == r , ]
  write.table(x = my.data.to.save, file = my.file.name, row.names = FALSE, sep = ',')
}

#####################################################

year.list <- sort(unique(salmon.data$year))

for(y in year.list){
  plot(salmon.data$length[salmon.data$year == y], 
       salmon.data$weight[salmon.data$year == y],
       xlab = 'taille', ylab = 'poids', main = y)
}

# 6.2.3 ==========

col.seq <- 1:dim(salmon.data)[2]


#is.numeric(salmon.data[ , 1])

for(c in col.seq){
  test.numeric <- class(salmon.data[ , c]) %in% c('numeric', 'integer') 
  if(test.numeric == TRUE){
    boxplot(salmon.data[ , c], main = colnames(salmon.data)[c])
  }else{
    #print(paste(colnames(salmon.data)[c], 'not numeric'))
    barplot(table(salmon.data[ , c]), main = colnames(salmon.data)[c])
  }
}


# save in txt if river is french
table(salmon.data$river, salmon.data$country, useNA = 'always')

list.rivers <- unique(salmon.data$river)
list.rivers <- list.rivers[!is.na(list.rivers)]

for(r in list.rivers){
  salmon.sub <- salmon.data[salmon.data$river == r, ]
  if(all(salmon.sub$country %in% c('fr', NA))){
    write.table(salmon.sub, file =  paste(r,'.txt', sep = ''))
  }else{
    print(paste(r,'pas francaise', sep = ' '))
  }
}











