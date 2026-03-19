rm(list = ls())

require(magrittr)
require(ggplot2)
require(ggmap)

wd <- 'C:/Users/gbal/Desktop/r.training.mnhn/' %T>% setwd()

wd %>% paste(., '/2.data/2.salmon', sep = '') %>% dir()
salmon.data <- '2.data/2.salmon/salmon.data.raw.txt' %>% read.table(file = ., sep = ',', h = T)

# custom function stolen from R help
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#https://ggplot2-book.org/getting-started

# summary of data ==============================================================

salmon.data %<>% 
  lapply(., FUN = function(x){
    if(x %>% is.character) x %<>% as.factor()
    return(x)
  }
  ) %>% as.data.frame()

salmon.data %>% summary

# remove weight outliers
salmon.data <- salmon.data[salmon.data$weight < 50 & salmon.data$length > 50, ]

# regular plot =================================================================

ggplot(data = salmon.data[salmon.data$sea.age > 2, ],
       aes(x= length, y = weight)) +
  geom_point() +
  geom_smooth() +
  xlab('River') + ylab('Number')

# scale_x_continuous(limits = c(0, 120),
#                    expand = expansion(mult = 0, add = 0))

# geom_point vs geom path ======================================================

random.coord <- data.frame(X = runif(10, 0, 1),
                           Y = runif(10,0, 1))

# geom_line
ggplot(random.coord, aes(x = X, y = Y)) +
  geom_line() +
  ggtitle('geom_line')

# geom_path
ggplot(random.coord, aes(x = X, y = Y)) +
  geom_path() +
  ggtitle('geom_path')

# some plots ===================================================================

# histogram
ggplot() +
  geom_histogram(data = salmon.data, 
                 aes(x = weight),
                 binwidth = 1) +
  xlab('Weight in kg') + ylab('Number')



# barplots ===============================================

ggplot() +
  geom_bar(data = salmon.data, 
           aes(x = river, factor = river, 
               fill = sea.age)) +
  scale_fill_discrete(name = 'Sea age') +
  xlab('River') + ylab('Number') +
  labs(title = "Sea age of trapped salmon per river",
       caption = "A caption")

# some stats computed
n.sea.age.per.river <- 
  table(salmon.data$river, salmon.data$sea.age) %>%
  reshape2::melt(.) %>% 
  `colnames<-`(c('river', 'sea.age', 'number'))

# barplot based on stat
ggplot() +
  geom_bar(data = n.sea.age.per.river,
           aes(y = number, x = river, 
               fill = sea.age %>% as.factor),
           stat = "identity")

# barplot based on stat 2
ggplot() +
  geom_col(data = n.sea.age.per.river, 
           aes(y = number, x = river, 
               fill = sea.age %>% as.factor()))

# boxplot ==============================================

# easy way
ggplot() +
  geom_boxplot(data = salmon.data,
               aes(y = weight, x = river, 
                   fill = sea.age %>% as.factor),
               outlier.shape = NA) +
  scale_fill_discrete(name = 'Sea age') +
  theme(legend.position = "bottom")

# some stats
weight.river.sea.age.1 <-
  tapply(salmon.data$weight[salmon.data$sea.age == 1], 
         INDEX = salmon.data$river[salmon.data$sea.age == 1],
         FUN = quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>% 
  do.call(., what = rbind) %>%
  `colnames<-`(paste('Q', c(0.025, 0.25, 0.5, 0.75, 0.975), sep = '')) %>%
  as.data.frame() %>%  dplyr::mutate(river = rownames(.)) %>% `rownames<-`(NULL)
# reshape2::melt()
# `colnames<-`(c('river'), quantile, value)
# reshape2::melt()

# from stats
ggplot(
  data = weight.river.sea.age.1, 
  aes(x = river, fill = river)) +
  geom_boxplot(
    aes(ymin = Q0.025 , lower = Q0.25, middle = Q0.5 ,
        upper = Q0.75  , ymax =  Q0.975),
    stat = "identity")

# facet_wrap and facet grid ==============================

salmon.data$sea.age %<>% as.factor() 
salmon.data$river %<>% as.factor() 

ggplot(data = salmon.data) +
  geom_point(
    aes(x = length, y = weight)) +
  facet_grid(sea.age ~ river)

# trying locator in ggplot =====================================================

my.plot <-
  ggplot() +
  geom_point(data = salmon.data, aes(x = length, y = weight)) +
  facet_grid(~ river * sea.age) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

x11()
my.plot
gglocator(my.plot, n = 2)   


