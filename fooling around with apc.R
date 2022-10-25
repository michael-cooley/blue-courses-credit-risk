

library(data.table)
library(ggplot2)
library(gam)
library(plotly)
library(plot3D)
library(magrittr)

# create pretend overall set

my_guys_x <- data.table(ex_x = rexp(100000, rate = 0.10))

my_guys_x[, ex:=ceiling(ex_x)]

my_guys <- my_guys_x[, .(ex=.N), by=.(time=ceiling(ex_x))][order(time)]

my_guys[, rn := .I]

my_guys[, rn2:=rn-1]

merge.data.table(
  x = my_guys,
  y = my_guys,
  by.x = 'rn',
  by.y = 'rn2'
) %>%
  .[, .(diff = ex.x - ex.y)]

my_guys[, .N, by=ex][order(ex)][1:10,]

ggplot(my_guys, aes(x=ex)) + geom_histogram()