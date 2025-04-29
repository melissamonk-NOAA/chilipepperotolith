#######################################################################################################
# Script to look at the relationship between chilipepper otolith weight, traditional age, and 
# fish length
#
# 2025 SWFSC
#######################################################################################################

#load packages
#Chilipepper rockfish otolith weight analysis
library(ggplot2)
#library(Rmisc)
library(dplyr)
#library(readr)
library(RColorBrewer)
#library(fishmethods)
#library(DLMtool)
library(FLR)
library(tidyr)
library(tidyverse)
#library(EnvStats)
#library(ggpmisc)
library(pak)
#library(nwfscSurvey)
#library(ggridges)
#library(rstatix)
library(FSA)
library(nlstools)
library(plotrix)
#library(car)
#library(boot)
#library(FSAdata)
library(here)
library(tidyr)

#setwd if not already
setwd(here())

#read in data - changed this to a shortened name to reduce errors
dat <- read.csv(here("data-raw", "cleanedmergedchili.csv"))
#3502 ages
#turn region and sex into factors 
#two columns for sex
#The unsexed fish are duplicated here - that's fine for now
dat$Region <- as.factor(dat$Region)
dat$Sex <- as.factor(dat$Sex) #original column with unsexed fish
dat$sex <- as.factor(dat$sex) #splits the unsexed fish 50:50 - or are they already duplicated??


#look to see how many from each year and age
summary(dat)
with(dat, table(Capture_Date, age))
#There are a number of year classes to look at and track 
#This includes the 2010 to 2014 data

#how many samples per region
summary(dat$Region)
#only 4 in washington
#Collapse to just north and south of Conception
dat <- dat %>%
mutate(region = case_when(Latitude <34.45 ~ "south", Latitude >= 34.45 ~ "north"))

with(dat, table(region, age))

#Otolith weight - age distribution
ggplot(dat, aes(x=Wgt_g, y = age, colour = sex)) + 
    geom_point(alpha = .5) 

#Switched these - we want to know if we can predict age from otolith weight so 
#age is on the left hand size and otolith weight is the 
#linear model of otolith weight to TMA ages without accounting for region or sex
reg <- lm(age ~ Wgt_g, data = dat)
reg
#coeff = coefficients(reg) #you can call coef to make this a bit cleaner

#look at region - I don't think growth matters by region from 
reg1 <- lm(age ~ Wgt_g + region, data = dat)

#look at sex additive
reg2 <- lm(age ~ Wgt_g + sex, data = dat)

#look at sex interaction
reg2a <- lm(age ~ Wgt_g*sex, data = dat)

#look at sex and region
reg3 <- lm(age ~ Wgt_g + sex + region, data = dat)
reg3
anova(reg3)

reg4 <- lm(age ~ Wgt_g + sex*region, data = dat)
reg4
anova(reg4)

AIC(reg, reg1, reg2, reg2a, reg3, reg4)

#Looking at the plots, region changes aren't that meaningful so just look at sex

summary(reg2a)

#model females and males separately
reg_m <- lm(age ~ Wgt_g, data = dat %>% filter(sex=="M"))
reg_f <- lm(age ~ Wgt_g, data = dat %>% filter(sex=="F"))

summary(reg_m)
summary(reg_f)



# Equation of the line : 
eq = paste0("y = ", round(coef(reg)[1]), "*x + ", round(coef(reg)[2]))

# Plot the data and the line for males and
sp + geom_abline(intercept = coef(reg), slope = 0.02086)+
  ggtitle(eq)
# Change line type, color and size
sp + geom_abline(intercept = 0.02292, slope = 0.02086, color="red", 
                 linetype="dashed", size=1.5)+
  ggtitle(eq)









ageotowgt_region<- ggplot(dat ,aes(x= age, y= Wgt_g, colour= as.factor(Region)))+
  geom_point(alpha = 0.1)+
  geom_smooth(method= "lm", formula = y ~ poly(x, 2, raw = TRUE),se = FALSE)+
  labs(x="Age (years)", y = "Otolith weight (g)", title= "Age-otolith weight relationship")+
  scale_x_continuous(name="Age (years)", limits=c(0,25)) +
  scale_y_continuous(name="Otolith weight (g)", limits=c(0,0.5))+
  stat_poly_eq(
    formula = y ~ x + I(x^2),
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE
  )
ageotowgt_region

agelength<- ggplot(dat,aes(x= age, y= Length_cm, colour= as.factor(sex)))+
  geom_point(alpha = 0.1)+
  geom_smooth(method= "lm", formula = y ~ poly(x, 2, raw = TRUE),se = FALSE)+
  labs(x="Age (years)", y = "Length (cm)", title= "Age-length relationship")+
  scale_x_continuous(name="Age (years)", limits=c(0,25)) +
  scale_y_continuous(name="Length (cm)", limits=c(0,55))+
  stat_poly_eq(
    formula = y ~ x + I(x^2),
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE
  )
agelength




















#vonbert curve
#Length - age model VBGF
  vbmodel <- Length_cm ~ Linf * (1 - exp(-K * (age - t0)))
  starts <- vbStarts(formula = Length_cm~age, data = dat)
  starts
  vbo <- vbFuns("typical")
  ggplot(data=dat,aes(x=age,y=Length_cm,color=sex)) +
    geom_point(size=2,alpha=0.3) +
    scale_y_continuous(name="Length (cm)",limits=c(0,60)) +
    scale_x_continuous(name="Age (years)",breaks=0:24) +
    scale_color_manual(values=c("M"="cyan3","F"="indianred1")) +
    geom_smooth(method="nls",se=FALSE,
                method.args=list(formula=y~vbo(x,Linf,K,t0),start=starts),
                linewidth=1) +
    theme(panel.grid.minor.x=element_blank(),
          legend.position=c(0.8,0.2),
          legend.title=element_blank())+
    theme_bw() +
    theme(panel.grid=element_blank()) 


#vonbert curve
  #otolith weight - age relationship
  vbmodel <- Wgt_g ~ Linf * (1 - exp(-K * (age - t0)))
  starts <- vbStarts(formula = Wgt_g~age, data = dat)
  starts
  vbo <- vbFuns("typical")
  ggplot(data=dat,aes(x=age,y=Wgt_g,color=sex)) +
    geom_point(size=2,alpha=0.3) +
    scale_y_continuous(name="Otolith weight (g)",limits=c(0,0.45)) +
    scale_x_continuous(name="Age (years)",breaks=0:24) +
    scale_color_manual(values=c("M"="cyan3","F"="indianred1")) +
    geom_smooth(method="nls",se=FALSE,
                method.args=list(formula=y~vbo(x,Linf,K,t0),start=starts),
                linewidth=1) +
    theme(panel.grid.minor.x=element_blank(),
          legend.position=c(0.8,0.2),
          legend.title=element_blank())+
    theme_bw() +
    theme(panel.grid=element_blank())

    #Length age model
  vbmodel <- Length_cm ~ Linf * (1 - exp(-K * (age - t0)))
  #starting values for each parameters using the `vbStarts` function
  starts <- vbStarts(formula = Length_cm~age, data = dat)
  starts
  
  ##Notes from EJ
  #check na.omit just for length and age
  #look at sex differences
  #constant cv effect needs to be applied
  
  
  mymod<- nls(vbmodel,data=dat, start = starts)
  summary(mymod)
  mymod_na <- (mymod$na.action) 
  pred<- predict(mymod)
  head(pred,50)
  vbo <- vbFuns("typical")
  vb_fit<- nls(Length_cm~vbo(age,Linf,K,t0), data = (dat), start=starts) #na.omit needs to stay for compat.
  boot_fit<-nlsBoot(vb_fit)
  
  # bootstrapped confidence intervals
  boot_preds <- data.frame(
    predict(boot_fit, vbo, t = sort(unique(dat$age))))
  names(boot_preds) <- c("age", "fit", "lwr", "upr")
  my.formula<-  y ~ x
  chilidata_preds<- merge(dat, boot_preds, by = "age")
  ggplot(chilidata_preds, aes(x = age, y = Length_cm, colour= as.factor(sex))) +
    geom_jitter(width = 0.1, alpha = 0.15, size = 2)+
    geom_line(aes(y = fit))+
    geom_smooth(aes(col=sex))+
    geom_ribbon(
      aes(x = age, ymin = lwr, ymax = upr, color = NULL), alpha = 0.3)+
    stat_poly_eq(
      formula = my.formula,
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
      parse = TRUE
    )+
    xlab("Age (years)") +
    ylab("Length (cm)") 
  
  #Using stat_smooth but need to use a different method
  vbo <- vbFuns(param="Typical") 
  
  ggplot(data=chilidata_preds,aes(x=age,y=Wgt_g,color=sex)) +
    geom_point(size=2,alpha=0.3) +
    scale_y_continuous(name="Otolith Weight (g)",limits=c(0,0.45)) +
    scale_x_continuous(name="Age (years)",breaks=0:24) +
    scale_color_manual(values=c("M"="blue","F"="red")) +
    stat_smooth()+
    geom_smooth(method="nls",se=FALSE,
                method.args=list(formula=y~vb(x,Linf,K,t0),start=starts),
                linewidth=1) +
    theme(panel.grid.minor.x=element_blank(),
          legend.position=c(0.8,0.2),
          legend.title=element_blank())+
    theme_bw() +
    theme(panel.grid=element_blank())
  
