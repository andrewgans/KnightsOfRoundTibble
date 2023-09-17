#Load tidyverse + features
library(tidyverse)
library(lokern)
library(features)
#Read in data files
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
## Question 1: Make pseed.wide tibble##

#Join "pseed" table with water tunnel speed data table, "speeds"
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
#Join pseed body lengths to thew new table 
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
#Computer specific speed by speed/ body length for each fish 
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
#Plot fin amplitudes over specific speeds
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
#Left fin amplitude for a specific date
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()
#Custom functions
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
f1
fget(f1)
#plot vertical lines at
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)
##multiply fget curvature values to avoid rounding to zero
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)
#critical points and peaks in a table
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()
#plot critial points/peaks
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()
pseed2%>%
  summarize(n=length(unique(date)))
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter
pseed.max
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")
#ANOVA 
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")
pseed2
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
pseed2 %>%
  filter(fin=="R")
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 
##Question 2 - Make a custom function to compute the standard error of the mean##
standard.error <- function(x) {
  (sd(x)/(sqrt(length(x))))
  
}
##Question 3 and Question 4
pseed.sum.max <- pseed.wide %>%
  group_by(fish,bl.s) %>% 
  summarise(amp.sum.mean = max(amp.sum),
            amp.sum.se = standard.error(amp.sum))
pseed.sum.max
#Question 5 - plot the mean amp.sum vs. specific swimming speed
pseed.sum.max%>%
  ggplot(aes(y=amp.sum.mean,x=bl.s,col=fish))+geom_point() + 
  labs(y = "Specific Speeds", x = "Amp.Sum.Mean") +
  geom_errorbar(aes(ymax = amp.sum.mean - amp.sum.se, ymin = amp.sum.mean + amp.sum.se, width = 0.2))
#Question 6 - merge metabolism table with pseed.sum.max
pseed.metab <- read_csv("pseed.met.rate.csv")
pseed.metab
pseed.sum.max
pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.metab, by = c("fish", "bl.s")) %>% 
  print()
#Question 7
pseed.sum.max %>% 
  ggplot(aes(x=amp.sum.mean, y=met.rate))+
  geom_point() +
  labs(x = "Mean Maximum Amplitude", y = "Metabolic Rate")
###
