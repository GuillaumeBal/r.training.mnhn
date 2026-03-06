rm(list = ls())

require(magrittr)
require(ggplot2)
require(ggmap)

wd <- 'C:/Users/gbal/Desktop/git/r.training.mnhn/' %T>% setwd()

wd %>% paste(., '/2.data/2.salmon', sep = '') %>% dir()
salmon.data <- '2.data/2.salmon/salmon.data.raw.txt' %>% read.table(file = ., sep = ',', h = T)

# custom function stolen from R help
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# some plots ===================================================================

my.plot <-
  ggplot() +
  geom_point(data = salmon.data, aes(x = length, y = weight))

x11()
my.plot
gglocator(my.plot, n = 2)             


ggplot() +
  geom_bar(data = salmon.data, aes(x = river %>% .simpleCap, fill = sea.age %>% as.factor())) +
  scale_fill_discrete(name = 'Sea age') +
  ylab('River') + xlab('Number') +
  labs(title = "Sea age of trapped salmon per river",
       caption = "In case you want a caption explaning some stuff about your graph but right now I can't bother", cex = 2)

trees
iris2
mtcars
longley
USArrests
