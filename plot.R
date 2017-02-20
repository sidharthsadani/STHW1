library(ggplot2)
library(gridExtra)
library(grid)
library("reshape2")

# sometimes R preview doesn't work properly, export the plots as pdf in that case

# d1,d2,d3,d4 are data-frames for seperate lines
# y1 - x-axis; y2 - y-axis
d1 <- data.frame(y1=c(1,2,3,4),y2=c(4,5,6,7))
d2 <- data.frame(y1=c(1,2,3),y2=c(6,5,4))
d3 <- data.frame(y1=c(1,2,3),y2=c(5,2,1))
d4 <- data.frame(y1=c(1,2,3),y2=c(6,4,2))
my <- list(d1, d2,d3,d4)

# bring the data into long (molten) format
# makes it easier to plot multiple lines in the same plot
mdf <- melt(my, id = c("y1","y2"))

# ggplot - line plotting
p1 <- ggplot(data=mdf, aes(x=y1, y=y2, group = L1)) +
  geom_line() +
  xlab("Time of day") + ylab("Total bill")+
  ggtitle("Curve 1")


d5 <- data.frame(y1=c(1,2,3,4),y2=c(4,5,6,17))
d6 <- data.frame(y1=c(1,2,3),y2=c(6,5,14))
d7 <- data.frame(y1=c(1,2,3),y2=c(5,2,11))
d8 <- data.frame(y1=c(1,2,3),y2=c(6,4,12))
my2 <- list(d5, d6,d7,d8)

mdf2 <- melt(my2, id = c("y1","y2"))


d9 <- data.frame(y1=c(1,2,3,4),y2=c(4,5,6,17))
d10 <- data.frame(y1=c(1,2,3),y2=c(6,5,14))
d11 <- data.frame(y1=c(1,2,3),y2=c(5,2,11))
d12 <- data.frame(y1=c(1,2,3),y2=c(6,4,12))
my3 <- list(d9, d10,d11,d12)

mdf3 <- melt(my3, id = c("y1","y2"))

p2 <- ggplot(data=mdf2, aes(x=y1, y=y2, group = L1)) +
  geom_line() +
  xlab("Time of day") + ylab("Total bill")+
  ggtitle("Curve 2")

p3 <- ggplot(data=mdf2, aes(x=y1, y=y2, group = L1)) +
  geom_line() +
  xlab("Time of day") + ylab("Total bill")+
  ggtitle("Curve 3")

p4 <- ggplot(data=mdf3, aes(x=y1, y=y2, group = L1)) +
  geom_line() +
  xlab("Time of day") + ylab("Total bill")+
  ggtitle("Curve 4")


# create this list for plotting in the same page
pltList <- list()
pltList[[1]] = p1
pltList[[2]] = p2
pltList[[3]] = p3
pltList[[4]] = p4

# plot the plots in two columns
do.call("grid.arrange", c(pltList, ncol=2))


