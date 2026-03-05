# vector attributes
x <- structure(1:3, names = c("premier", "deuxième", "troisième"))
attributes(x) <- c(attributes(x), comment = 'je ne sais pas quoi')
attributes(x)

# matrix attributes
x <- cbind(a = 1:3, pi = pi) # simple matrix with dimnames
attributes(x) <- c(attributes(x), comment = 'je ne sais pas quoi')
attributes(x)

# dataframe.attributes
x <- structure(data.frame(c1 = 1:3, c2 = letters[1:3]), rownames = c('x', 'y', 'z'))
attributes(x)
