
mpg <- read.csv("mpg.csv", stringsAsFactors = F)

mpg$date <- as.POSIXct(strptime(mpg$date, '%m/%d/%Y'))
mpg$mpg = mpg$miles / mpg$gallons

mpg <- mpg[complete.cases(mpg),]

plot(mpg)


library(ggplot2)
library(GGally)

ggpairs(mpg[,-1])

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


q <- mpg %>% group_by(yyyy=year(date)) %>% summarise(n=n(), g=sum(gallons), t=sum(gallons*price), p=mean(price))
  
ggplot(q, aes(x=yyyy, y=t)) + geom_bar(stat='identity') + geom_smooth(method='lm')

ggplot(q, aes(x=yyyy, y=t)) + geom_bar(aes(fill=n),stat='identity')


mpg %>% group_by(d=floor_date(date, 'months')) %>% summarise(n=n(), mpg=mean(mpg)) %>% ggplot(aes(x=d, y=mpg)) + geom_bar(aes(fill=n),stat='identity')

mpg %>% group_by(d=floor_date(date, 'quarters')) %>% summarise(n=n(), mpg=mean(mpg)) %>% ggplot(aes(x=d, y=mpg)) + geom_bar(aes(fill=n),stat='identity')

mpg %>% group_by(d=floor_date(date, 'years')) %>% summarise(n=n(), mpg=mean(mpg)) %>% ggplot(aes(x=d, y=mpg)) + geom_bar(aes(fill=n),stat='identity') + geom_hline(aes(yintercept=mean(mpg)), color='red') +
  geom_hline(aes(yintercept=mean(n)), color='black')

library(forecast)

q <- mpg %>% group_by(d=floor_date(date, 'months')) %>% summarise(n=n(), mpg=mean(mpg))
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


x <- lapply(2005:2017, function(y) { 
    p <- seq(1, 100, length.out = 100) / 100
    data.frame(y=y, p=p, q=quantile(qq[qq$yyyy==y,]$price, p)) 
  })
z <- do.call(rbind, x)
  
ggplot(z) + geom_line(aes(x=p, y=q, group=y, color=as.factor(y)))  


v <- mpg %>% mutate(yyyy=year(date), mpg=miles/gallons, m=cumsum(miles), cmpg=cumsum(miles)/cumsum(gallons),  g=cumsum(gallons), t=cumsum(gallons*price), p=cummean(price))

ggplot(v) + geom_line(aes(x=date, y=cmpg))


library(cluster)

plot(mpg$miles, mpg$price)

d <- v
d <- mpg[,c('price', 'gallons', 'miles')]
d$mpg <- d$miles / d$gallons

fit <- kmeans(d[,c('miles', 'gallons', 'mpg', 'price')], centers=5)

fit
fit$cluster
# plot(d$price, d$mpg, col=fit$cluster)
# plot(d$gallons, d$miles, col=fit$cluster)
# plot(d$gallons, d$price, col=fit$cluster)
# library(rgl)
# plot3d(d)
# plot3d(d, col=fit$cluster)

d$cluster <- factor(fit$cluster)

library(gridExtra)

g1 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=miles, color=cluster))

g2 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=price, color=cluster))

g3 <- ggplot(d) + 
  geom_point(aes(x=mpg, y=gallons, color=cluster))

grid.arrange(g1, g2, g3, nrow=3)




v <- mpg %>% group_by(yyyy=year(date), mm=month(date)) %>% summarise( mpg=sum(miles)/sum(gallons), tp=sum(price), n=n(), ap=mean(price), ag=mean(gallons), tg=sum(gallons), tm=sum(miles) , am=mean(miles) ) %>% arrange(yyyy, mm) %>% mutate( cm=cumsum(tm), ampg=cummean(mpg) )



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
