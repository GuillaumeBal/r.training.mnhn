rm(list = ls())

require(magrittr)

iris %>% {c(class(.), mode(.), typeof(.))}
iris$Sepal.Length %>% {c(class(.), mode(.), typeof(.))}
iris$Species %>% {c(class(.), mode(.), typeof(.))}
c('a', 'b', 'c') %>% {c(class(.), mode(.), typeof(.))}
c(TRUE, FALSE) %>% {c(class(.), mode(.), typeof(.))}
iris$Species %>% str

iris$Species %>% {c(class(.), mode(.), typeof(.), str(.))}