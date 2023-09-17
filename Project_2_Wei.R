library(tidyverse)
library(dplyr)
install.packages("features")
install.packages("lokern")
library(lokern)
library(features)
library(ggplot2)

#In a script named “your_module2.R”, combine the code above so that you can establish the pseed.wide data tibble.

pseed<- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#custom Functions
exp1 <- pseed2 %>%
  filter(date=="2019-06-17-151149", fin=="L") %>% 
  
  f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
fget(f1)

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)

f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>%
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter
ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  print() 

#Question 2: Create a custom function that computes the standard error of the mean (SE)
standard.error <- function(x){
  se<- (sd(x)/(sqrt(length(x))))
  return(se)
}
standard.error(c(1:16))

#Compute the mean maximum* of all the amp.sums across 
#all fin-beat cycles for each specific swimming speed 
#for each fish just like we did for mean maximum amplitude of each fin
find.max<-function(x,y,mult=100){
  f<-fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% #
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}
# make a new tiddle pseed.filter to filter out the rows with maximum amp.sum for each speed
pseed.filter<-pseed.wide %>% 
  group_by(fish,bl.s) %>% 
  mutate(peak=frame%in%find.max(frame,amp.sum)) %>% 
  filter(peak==T)

#plot the mean and make a new tiddle with mean max for each date and speed for each fish
pseed.filter %>%
  group_by(fish,bl.s) %>%
  summarize(mean.max=mean(amp.sum)) %>%
  ggplot(aes(bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

pseed.filter<-pseed.filter %>% 
  group_by(fish,bl.s) %>% 
  mutate(n=length(unique(amp.sum))) %>% 
  mutate(SD=sd(unique(amp.sum)))

pseed.sum.max<-pseed.filter %>%
  group_by(fish,bl.s,n,SD) %>%
  summarize(mean.max=mean(amp.sum)) %>% 
  as_tibble()
pseed.sum.max<-pseed.sum.max %>% 
  mutate(SE=find.SE(pseed.sum.max))

#Question 5 - plot the mean amp.sum vs. specific swimming speed
pseed.sum.max%>%
  ggplot(aes(y=amp.sum.mean,x=bl.s,col=fish))+geom_point() + 
  labs(y = "Specific Speeds", x = "Amp.Sum.Mean") +
  geom_errorbar(aes(ymax = amp.sum.mean - amp.sum.se, ymin = amp.sum.mean + amp.sum.se, width = 0.2))

#question 6
met_rate <- read_csv("pseed.met.rate.csv")
met_rate
pseed.sum.max <- pseed.sum.max %>% 
  left_join(met_rate,by="fish") %>% 
  print()

#question 7
pseed.sum.max %>% 
  ggplot(aes(x=amp.sum.mean, y=met.rate))+geom_point()


