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
  summarize(n=length(unique(date)))
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))

pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

#ANOVA 
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl))

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
