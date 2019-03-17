
library(data.table)
library(lubridate)


mpg <- fread("mpg.csv", stringsAsFactors = F)

mpg$date <- as.POSIXct(strptime(mpg$date, '%m/%d/%Y'))
mpg$mpg <- mpg$miles / mpg$gallons
mpg$cost <- mpg$gallons * mpg$price


mpg$year <- year(mpg$date)
mpg$month <- month(mpg$date)
mpg$day <- day(mpg$date)

mpg$yyyymm <- floor_date(mpg$date, 'month')

mpg[order(date), dt := as.numeric(date - shift(date))]
mpg[1,'dt'] <- 0

mean(mpg$dt)

plot(mpg$dt, mpg$miles)

plot(mpg$price, mpg$gallons, col=mpg$miles)

ggplot(mpg, aes(price, gallons)) +
  geom_point(aes(color=dt))

# plot(mpg)

mpg[, lapply(.SD, sum), by=year, .SDcols=c('gallons', 'miles', 'cost')]

mpg[, lapply(.SD, sum), by=year, .SDcols=c('gallons', 'miles', 'cost')][, lapply(.SD, mean), .SDcols=c('gallons', 'miles', 'cost')]

# gallons   miles     cost
# 1: 2819.746 53586.9 9336.479
total <- mpg[, lapply(.SD, sum, na.rm=T), .SDcols=c('gallons', 'miles', 'cost')]

total[, `:=`(hours = as.numeric(difftime(max(mpg$date), min(mpg$date), units = 'hours')) )]

#range(mpg$date)

#difftime(max(mpg$date), min(mpg$date), units = 'hours')
# Time difference of 4845.958 days


mpg <- mpg[complete.cases(mpg),]


driving.hours <- total$miles / 45

driving <- driving.hours / total$hours
not.driving <- 1 - driving

total
driving
not.driving

total$cost / driving.hours


#####

driving <- data.frame(mph = seq(20,80,by=5))
driving$hours <- total$miles / driving$mph
driving$driving <- driving$hours / total$hours
driving$not.driving <- 1 - driving$driving
driving$cost <- total$cost / driving$hours

driving

ggplot(driving, aes(hours, cost)) + geom_line() + geom_point(aes(color=mph))

#####

library(ggplot2)
library(GGally)

plot(mpg[,2:6])
ggpairs(mpg[,2:6])

ggplot(mpg, aes(x=date, y=price)) + geom_point() + geom_smooth(method = 'lm')
ggplot(mpg, aes(x=date, y=miles)) + geom_point() + geom_smooth(method = 'lm')
ggplot(mpg, aes(x=date, y=gallons)) + geom_point() + geom_smooth(method = 'lm')
ggplot(mpg, aes(x=date, y=mpg)) + geom_point() + geom_smooth(method = 'lm')

plot(mpg$date, cumsum(mpg$price), type='l')
plot(mpg$date, cumsum(mpg$gallons), type='l')
plot(mpg$date, cumsum(mpg$miles), type='l')

ggplot(mpg, aes(price)) +
  geom_density(color='black', fill='red', alpha=0.25) 

ggplot(mpg, aes(mpg)) +
  geom_density(color='black', fill='red', alpha=0.25) 

ggplot(mpg, aes(miles)) +
  geom_density(color='black', fill='red', alpha=0.25) 

ggplot(mpg, aes(gallons)) +
  geom_density(color='black', fill='red', alpha=0.25) 


ggplot(mpg, aes(year, mpg)) + geom_boxplot(aes(group=year))
ggplot(mpg, aes(year, cost)) + geom_boxplot(aes(group=year))
ggplot(mpg, aes(year, miles)) + geom_boxplot(aes(group=year))
ggplot(mpg, aes(year, gallons)) + geom_boxplot(aes(group=year))







ggplot(mpg[, .(k=mean(price)), by=year], aes(year, k)) + 
  geom_bar(stat = 'identity')

ggplot(mpg[, .(k=mean(gallons)), by=year], aes(year, k)) + 
  geom_bar(stat = 'identity')

ggplot(mpg[, .(k=mean(miles)), by=year], aes(year, k)) + 
  geom_bar(stat = 'identity')

ggplot(mpg[, .(k=mean(cost)), by=year], aes(year, k)) + 
  geom_bar(stat = 'identity')

ggplot(mpg[, .(k=mean(mpg)), by=year], aes(year, k)) + 
  geom_bar(stat = 'identity')

ggplot(mpg[, .(k=.N), by=year], aes(year, k)) + 
  geom_bar(stat = 'identity')
           
ggplot(mpg[, .(g=sum(gallons), fills=.N), by=year], aes(year, g, fill=fills)) + 
  geom_bar(stat = 'identity')
                      
                      



library(hexbin)

ggplot(mpg) +
  geom_hex(aes(yyyymm, mpg), color='white', bins=13*4) 

library(dplyr)
library(Rmisc)

mpg %>% 
  group_by(year) %>% 
  dplyr::summarise(n=n(), m=mean(mpg), l=CI(mpg, ci=0.95)[[3]], h=CI(mpg, ci=0.95)[[1]]) %>% 
  ggplot(aes(year, m)) + 
  geom_line(size=2) + 
  geom_errorbar(aes(x=year, ymin=l, ymax=h))


ggplot(mpg, aes(year, mpg, group=year)) + 
  geom_boxplot()


mpg %>% 
  group_by(year) %>% 
  summarise(m=sum(miles)) %>% 
  ggplot(aes(year, m)) + 
  geom_bar(stat='identity') +
  geom_smooth(method='lm')


ggplot(mpg, aes(x=date, y=cumsum(miles))) + geom_line() 
ggplot(mpg, aes(x=date, y=cumsum(gallons))) + geom_line() 
ggplot(mpg, aes(x=date, y=cumsum(cost))) + geom_line() 

ggplot(mpg, aes(x=date, y=miles/price)) + geom_line() 

ggplot(mpg, aes(x=date, y=price/miles)) + geom_line() 

ggplot(mpg, aes(x=date, y=miles/price)) + geom_boxplot() 
ggplot(mpg, aes(x=date, y=price/miles)) + geom_boxplot() 


w <- mpg
w$price <- scale(w$price)
w$gallons <- scale(w$gallons)
w$miles <- scale(w$miles)

ggplot(w) +
  geom_density(aes(price), color='black', fill='red', alpha=0.25) +
  geom_density(aes(gallons), color='black', fill='green', alpha=0.25) +
  geom_density(aes(miles), color='black', fill='blue', alpha=0.25)
  



library(lubridate)

library(dplyr)


q <- mpg %>% 
  group_by(yyyy=year(date)) %>% 
  dplyr::summarise(
    n=n(), 
    g=sum(gallons), 
    t=sum(gallons*price), 
    m=mean(gallons*price), 
    p=mean(price))
  
ggplot(q, aes(x=yyyy, y=t)) + 
  geom_bar(stat='identity') + 
  geom_smooth(method='lm')

ggplot(q, aes(x=yyyy, y=t)) + 
  geom_bar(stat='identity') + 
  geom_smooth(method='loess') +
  xlab("Year") +
  ylab("Cost") +
  ggtitle("Annual Gas Expenditure")


mpg %>% 
  group_by(yyyy=year(date), mm=month(date)) %>% 
  dplyr::summarise(n=n(), 
    g=sum(gallons), 
    t=sum(gallons*price), 
    p=mean(price)) %>%
  ggplot(aes(x=factor(paste(yyyy, mm)), y=t)) +
  geom_bar(aes(fill=t),
           color='white', stat='identity') 


ggplot(mpg, aes(gallons)) + geom_histogram(bins = 100)
ggplot(mpg, aes(price)) + geom_histogram(bins = 100)
ggplot(mpg, aes(miles)) + geom_histogram(bins = 100)


mpg %>% group_by(d=floor_date(date, 'months')) %>% dplyr::summarise(n=n(), mpg=mean(mpg)) %>% ggplot(aes(x=d, y=mpg)) + geom_bar(aes(fill=n),stat='identity')

mpg %>% group_by(d=floor_date(date, 'quarters')) %>% dplyr::summarise(n=n(), mpg=mean(mpg)) %>% ggplot(aes(x=d, y=mpg)) + geom_bar(aes(fill=n),stat='identity')

mpg %>% group_by(d=floor_date(date, 'years')) %>% dplyr::summarise(n=n(), mpg=mean(mpg)) %>% ggplot(aes(x=d, y=mpg)) + geom_bar(aes(fill=n),stat='identity') + geom_hline(aes(yintercept=mean(mpg)), color='red') +
  geom_hline(aes(yintercept=mean(n)), color='black')

library(forecast)

q <- mpg %>% group_by(d=floor_date(date, 'months')) %>% dplyr::summarise(n=n(), mpg=mean(mpg))
m <- ts(q$mpg, frequency = 12, start=c(2005,3), end=c(2017,9))
fit <- auto.arima(m)
plot(forecast(fit, h=12))

seasonplot(m)
ts.plot(m)

f <- ets(m)
plot(f)

plot(stl(m, s.window = 12))



qq <- mpg %>% mutate(yyyy=year(date), g=cumsum(gallons), t=cumsum(gallons*price), p=cummean(price))

ggplot(qq, aes(x=yyyy, y=miles, group=yyyy)) + geom_boxplot(aes(fill=miles)) + scale_fill_continuous()


x <- lapply(2005:2018, function(y) { 
    p <- seq(1, 100, length.out = 100) / 100
    data.frame(y=y, p=p, q=quantile(qq[qq$yyyy==y,]$price, p)) 
  })
z <- do.call(rbind, x)
  
ggplot(z) + geom_line(aes(x=p, y=q, group=y, color=as.factor(y)))  


v <- mpg %>% mutate(yyyy=year(date), mpg=miles/gallons, m=cumsum(miles), cmpg=cumsum(miles)/cumsum(gallons),  g=cumsum(gallons), t=cumsum(gallons*price), p=cummean(price))

ggplot(v) + geom_line(aes(x=date, y=cmpg))


library(cluster)

plot(mpg$miles, mpg$price)

d <- mpg[,c('price', 'gallons', 'miles')]
#d$mpg <- d$miles / d$gallons

ggpairs(d)

d <- as.data.frame(scale(d))
fit <- kmeans(d, centers=5)

fit
fit$cluster

par(mfcol=c(2,2))
plot(d$price, d$gallons, col=fit$cluster, pch=20)
plot(d$price, d$miles, col=fit$cluster, pch=20)
plot(d$miles, d$gallons, col=fit$cluster, pch=20)
par(mfcol=c(1,1))


# library(rgl)
# plot3d(d)
# plot3d(d, col=fit$cluster)

d$cluster <- factor(fit$cluster)

par(mfcol=c(2,2))
plot(d$cluster, d$gallons)
plot(d$cluster, d$price)
plot(d$cluster, d$miles)
par(mfcol=c(1,1))


library(reshape)

dm <- melt(d)
ggplot(dm, aes(cluster, value, fill=cluster)) + 
  geom_boxplot() +
  facet_wrap( ~ variable, nrow=2, scales = 'free_y') 

ggplot(dm, aes(cluster, value, fill=cluster)) + 
  geom_point() +
  facet_wrap( ~ variable, nrow=2, scales = 'free_y') 



comp <- princomp( ~ miles + gallons + price, data=d)
comp
summary(comp)
plot(comp)
biplot(comp)

plot(comp$scores[,1], comp$scores[,2], pch=20, col=factor(comp$scores[,3]))

d <- as.data.frame(comp$scores)


fit <- kmeans(d[, 1:3], centers=6)

#fit
#fit$cluster

d$cluster <- factor(fit$cluster)

#plot(d$Comp.1, d$Comp.2, pch=20, col=factor(fit$cluster))

ggplot(d) +
  geom_point(aes(Comp.1, Comp.2, color=cluster))





library(gridExtra)


g1 <- ggplot(d, aes(cluster, miles)) + geom_boxplot()
g2 <- ggplot(d, aes(cluster, gallons)) + geom_boxplot()
g3 <- ggplot(d, aes(cluster, mpg)) + geom_boxplot()
g4 <- ggplot(d, aes(cluster, price)) + geom_boxplot()
grid.arrange(g1, g2, g3, g4, nrow=2)



g1 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=miles, color=cluster))

g2 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=price, color=cluster))

g3 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=gallons, color=cluster))

grid.arrange(g1, g2, g3, nrow=3)



g1 <- ggplot(d) + 
  geom_jitter(aes(x=cluster, y=mpg, color=cluster))

g2 <- ggplot(d) + 
  geom_jitter(aes(x=cluster, y=miles, color=cluster))

g3 <- ggplot(d) + 
  geom_jitter(aes(x=cluster, y=price, color=cluster))

g4 <- ggplot(d) + 
  geom_jitter(aes(x=cluster, y=gallons, color=cluster))

grid.arrange(g1, g2, g3, g4, nrow=2)





g1 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=miles, color=cluster))

g2 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=price, color=cluster))

g3 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=gallons, color=cluster))

g4 <- ggplot(d) + 
  geom_point(aes(x=miles, y=price, color=cluster))

g5 <- ggplot(d) + 
  geom_point(aes(x=price, y=gallons, color=cluster))

g6 <- ggplot(d) + 
  geom_point(aes(x=miles, y=gallons, color=cluster))


grid.arrange(g1, g2, g3, g4, g5, g6, nrow=3)


v <- mpg %>% group_by(yyyy=year(date), mm=month(date)) %>% dplyr::summarise( mpg=sum(miles)/sum(gallons), tp=sum(price), n=n(), ap=mean(price), ag=mean(gallons), tg=sum(gallons), tm=sum(miles) , am=mean(miles) ) %>% arrange(yyyy, mm) %>% mutate( cm=cumsum(tm), ampg=cummean(mpg) )



ggplot(v) + geom_line(aes(group=yyyy, x=mm, y=mpg)) + facet_wrap( ~ yyyy, ncol=3 )

ggplot(v) + geom_line(aes(x=1:147, y=ampg))

k <- ts(v[,c('mpg')], frequency = 12)

fit <- HoltWinters(k)
plot(forecast(fit,12*5))


fit <- auto.arima(k)
plot(forecast(fit, 12*5))


k <- ts(v[,c('ampg')], frequency = 12)

fit <- HoltWinters(k)
plot(forecast(fit,12*5))


fit <- auto.arima(k)
plot(forecast(fit, 12*5))



ggplot(as.data.frame(v)) + geom_boxplot(aes(x=yyyy, y=mpg, group=yyyy), outlier.color = NA) + geom_jitter(width=0.1, height=0.1, alpha=0.3, aes(x=yyyy, y=mpg, group=yyyy))

ggplot(as.data.frame(v)) + geom_violin(aes(x=yyyy, y=mpg, group=yyyy)) 

ggplot(as.data.frame(v)) + geom_jitter(width=0.1, height=0.1, alpha=0.3, aes(x=yyyy, y=mpg, group=yyyy))

ggplot(as.data.frame(v)) + geom_boxplot(aes(x=mm, y=mpg, group=mm), outlier.color = NA) + geom_jitter(width=0.1, height=0.1, alpha=0.3, aes(x=mm, y=mpg, group=mm))

ggplot(as.data.frame(v)) + geom_jitter(width=0.1, height=0.1, alpha=0.3, aes(x=mm, y=mpg, group=mm))

ggplot(as.data.frame(v)) + geom_violin(aes(x=mm, y=mpg, group=mm)) 

