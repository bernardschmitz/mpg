
library(dplyr)



ggplot(mpg, aes(as.Date(date), miles)) +
  #geom_col() +
  geom_line() +
  # geom_point() +
  geom_smooth() + 
  scale_x_date() +
  geom_vline(xintercept = as.numeric(as.Date('20080915', format='%Y%m%d')), color='steelblue') +
  geom_vline(xintercept = as.numeric(as.Date('20101110', format='%Y%m%d')), color='steelblue') +
  geom_vline(xintercept = as.numeric(as.Date('20160525', format='%Y%m%d')), color='steelblue')





m <- mpg %>% 
  group_by(yyyymm) %>%
  mutate(mean.miles = mean(miles)) %>%
  ungroup() %>% 
  distinct(yyyymm, mean.miles)

ggplot(m, aes(as.Date(yyyymm), mean.miles)) +
  geom_line() +
  # geom_point() +
  geom_smooth() + 
  scale_x_date() +
  geom_vline(xintercept = as.numeric(as.Date('20080915', format='%Y%m%d')), color='steelblue') +
  geom_vline(xintercept = as.numeric(as.Date('20101110', format='%Y%m%d')), color='steelblue') +
  geom_vline(xintercept = as.numeric(as.Date('20160525', format='%Y%m%d')), color='steelblue')


summary(m)   

m.t <- ts(m$mean.miles, start=c(2005,3), end=c(2018,12), frequency = 12)
    
m.t

plot(m.t, type='h')


plot(stl(m.t, s.window = 'periodic'))


