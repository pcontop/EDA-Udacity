library(ggplot2)
data("diamonds")

str(diamonds)
?diamonds

qplot(x=price, data=diamonds, binwidth=500, color=I('black'), fill=I('#099DD9'))+
  scale_x_continuous(breaks = seq(0,2000,1000))
?
sum(diamonds$price<500)

sum(diamonds$price<250)

sum(diamonds$price>=15000)


qplot(x=price, data=diamonds, binwidth=25, color=I('black'), fill=I('#099DD9'))+
  scale_x_continuous(breaks = seq(250,1450,50), limits=c(250,1450))

ggsave('priceHistogram.png')
  

qplot(x=price, data=diamonds, binwidth=500, color=I('black'), fill=I('#099DD9'))+
  scale_x_continuous(breaks = seq(0,19000,2000))+
  facet_wrap(~cut)

by(diamonds$price, diamonds$cut, summary, digits=max(getOption('digits')))

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales="free_y")

library("gridExtra")

qplot(x = price, data = diamonds, binwidth=0.01) + facet_wrap(~cut, scales="free_y", ncol = 1) + scale_x_log10()

qplot(x = cut, y = price, data=diamonds, geom="boxplot") + coord_cartesian(ylim=c(0,7000))
ggsave('priceCutBoxPlot.png')

by(diamonds$price, diamonds$cut, summary, digits=max(getOption('digits')))

qplot(x = color, y = price, data=diamonds, geom="boxplot") + coord_cartesian(ylim=c(0,8000))
ggsave('priceColorBoxPlot.png')

by(diamonds$price, diamonds$color, summary, digits=max(getOption('digits')))

help(diamonds)

IQR(subset(diamonds, color=="D")$price)

IQR(subset(diamonds, color=="J")$price)


qplot(x = color, y = price/carat, data=diamonds, geom="boxplot") + coord_cartesian(ylim=c(0,6000))
ggsave('pricePerCaratColorBoxPlot.png')
qplot(x = color, y = price/carat, data=diamonds, geom="boxplot") + facet_wrap(~cut, ncol=1)+ coord_cartesian(ylim=c(0,6000))
ggsave('pricePerCaratColorBoxPlotWrapCut.png')

qplot(x = carat, 
      y = ..count.., 
      data = diamonds,
      binwidth = 0.1,
      geom="freqpoly"#,
      #color=carat
) + scale_y_continuous(breaks = seq(0, 10000, 1000)) 
+ coord_cartesian(ylim=c(0,10000), xlim=c(0,2)) 
+ scale_x_continuous(breaks=seq(0,6,.1))

sum(diamonds$carat==1.01)

sum(diamonds$carat==1.6)

