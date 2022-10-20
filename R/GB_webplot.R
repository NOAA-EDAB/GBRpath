#Balanced Rpath model
library(Rpath); library(data.table); library(here)

#Load balanced model
load(here('data', 'GB.params.bal.rda'))
load(here('data', 'GB.bal.rda'))

png(file = here('outputs', 'Georges_Bank_EMAX_Foodweb.png'),
    height = 1700, width = 2200, res = 200)
my.groups <- c(c(61, 59, 60), 
               c(48, 50, 51, 52, 53, 54, 56, 57, 58),
               c(11, 12, 26, 33, 34, 37, 47, 49, 55, 62, 63, 64),
               c(7, 8, 9, 10, 15, 21, 23, 24, 27, 28, 31, 32, 35, 36, 42, 43, 
                 44, 45, 46),
               c(1, 3, 5, 14, 19, 22, 29, 38, 39, 40, 41, 67),
               c(2, 4, 6, 13, 16, 17, 18, 25, 30, 66, 69),
               c(20, 65, 68, 70, 71))
webplot(GB.bal, labels = T, box.order = my.groups, label.cex = 0.8)
dev.off()


