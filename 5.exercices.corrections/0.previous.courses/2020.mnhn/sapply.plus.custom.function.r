sapply(salmon.data, FUN = mean, na.rm = TRUE)


sapply(salmon.data,
       FUN = function(a){
         test <- is.numeric(a)
         if(test == TRUE){
           mean.x <- mean(a, na.rm = TRUE)
         } else{
           mean.x <- 'cannot compute'
         }
         return(mean.x)
       }, simplify = FALSE)
