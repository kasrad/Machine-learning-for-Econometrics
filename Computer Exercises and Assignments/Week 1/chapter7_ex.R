#7.3.4
str(diamonds)
summary(diamonds)
ggplot(diamonds) + geom_histogram(aes(diamonds$x), fill='indianred')
ggplot(diamonds) + geom_histogram(aes(diamonds$y), fill='indianred')
ggplot(diamonds) + geom_histogram(aes(diamonds$z), fill='indianred')

ggplot(diamonds) + geom_histogram(aes(diamonds$price), fill='indianred', binwidth = 100)
ggplot(diamonds) + geom_histogram(aes(diamonds$carat), fill='indianred', binwidth = 0.01)


dim(diamonds %>% filter(carat == 0.99))
dim(diamonds %>% filter(carat == 1))

#coord cartesiana  xlim ylim se mi nechce delat


