##### Marie Small Mammals ##########
##### Julian NC  ##########

library(psych)
library(plyr)
library(lattice)
library(Rmisc)
library(grid)
library(ggplot2)
library(nlme)
library(lme4)
library(MASS)
library(ecodist)
library(vegan)
library(car)
library(cluster)
library(PMCMR)
library(remotes)
library(patternplot)
library(readxl)
library(tidyverse)
library(dplyr)
library(multcomp)
library(ggforce)
library(RColorBrewer)
library(pscl)
library(patternplot)
library(readr)
library(ggpubr)
library(FSA)

data <- read.csv("master.csv")

ggplot(data, aes(x=Restored, y=total)) +
  geom_boxplot() +
  xlab("Post-mitigation")+
  ylab("Captures")+
  #ylim(60,75)+   ### sets the y axis limit
  geom_jitter(position = position_jitter(0.1)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    axis.title.x = element_text(colour="grey20",size=16,face="bold"),
    axis.title.y = element_text(colour="grey20",size=16,face="bold"), 
    axis.text.x = element_text(colour="grey20",size=12,face="bold"),
    axis.text.y = element_text(colour="grey20",size=12,face="bold")
  )

data$Site <- factor(data$Site, levels = c("A" , "B" ,"C", "D", "E", "CF1", "CF2", "CF3" ))

ggplot(data, aes(x=Site, y=total)) +
  geom_boxplot() +
  xlab("Site")+
  ylab("Captures")+
  geom_jitter(position = position_jitter(0.1)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    axis.title.x = element_text(colour="grey20",size=16,face="bold"),
    axis.title.y = element_text(colour="grey20",size=16,face="bold"), 
    axis.text.x = element_text(colour="grey20",size=12,face="bold"),
    axis.text.y = element_text(colour="grey20",size=12,face="bold")
  )

##### capture data is not normally distributed
shapiro.test(data$total)

tab<-aggregate(total~Restored ,data=data, FUN = "summary")
tab
sum = summarySE(data, 
                measurevar="total", 
                groupvars=c("Restored"))
sum

tab<-aggregate(total~Site ,data=data, FUN = "summary")
tab
sum = summarySE(data, 
                measurevar="total", 
                groupvars=c("Site"))
sum

fit <- glm(total ~ Restored, family = "poisson", data=data)
summary(fit)

fit <- glm(total ~ Site, family = "poisson", data=data)
summary(fit)

kruskal.test(total ~ Site, data=data)
dunnTest(total ~ Site, data=data,method="bonferroni")

fit <- glm(pele ~ Site, family = "poisson", data=data)
summary(fit)
kruskal.test(pele ~ Site, data=data)
dunnTest(pele ~ Site, data=data,method="bonferroni")

fit <- glm(pele ~ Restored, family = "poisson", data=data)
summary(fit)

####### Use zero inflated for species data - not PELE
ZeroInfModel <- zeroinfl(rehu ~ Restored, data = data, dist = 'poisson')
summary(ZeroInfModel)

ZeroInfModel <- zeroinfl(rehu ~ Site, data = data, dist = 'poisson')
summary(ZeroInfModel)

##### ALL SPECIES TOGETHER #############################################################
ggscatter(data, x = "MaxHerb", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "MaxShrub")

ggscatter(data, x = "DensityS", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "DensityS")

ggscatter(data, x = "DensityW", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "DensityW")

ggscatter(data, x = "AvgDensity", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Captures", ylab = "GroundCover")

##### PELE #############################################################

ggscatter(data, x = "MaxHerb", y = "pele", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White-footed Mouse Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "pele", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White-footed Mouse Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "pele", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White-footed Mouse Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "pele", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White-footed Mouse Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "pele", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White-footed Mouse Captures", ylab = "GroundCover")

######SIHI

ggscatter(data, x = "MaxHerb", y = "sihi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hispid Cotton Rat Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "sihi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hispid Cotton Rat Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "sihi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hispid Cotton Rat Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "sihi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hispid Cotton Rat Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "sihi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hispid Cotton Rat Captures", ylab = "GroundCover")

#####REHU

ggscatter(data, x = "MaxHerb", y = "rehu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Harvest Mouse Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "rehu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Harvest Mouse Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "rehu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Harvest Mouse Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "rehu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Harvest Mouse Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "rehu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Harvest Mouse Captures", ylab = "GroundCover")

#####ORPA

ggscatter(data, x = "MaxHerb", y = "orpa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Marsh Rice Rat Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "orpa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Marsh Rice Rat Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "orpa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Marsh Rice Rat Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "orpa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Marsh Rice Rat Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "orpa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Marsh Rice Rat Captures", ylab = "GroundCover")

#####MUMU

ggscatter(data, x = "MaxHerb", y = "mumu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "House Mouse Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "mumu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "House Mouse Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "mumu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "House Mouse Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "mumu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "House Mouse Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "mumu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "House Mouse Captures", ylab = "GroundCover")

#####MIPI

ggscatter(data, x = "MaxHerb", y = "mipi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Woodland Vole Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "mipi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Woodland Vole Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "mipi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Woodland Vole Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "mipi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Woodland Vole Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "mipi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Woodland Vole Captures", ylab = "GroundCover")

#####MIPE

ggscatter(data, x = "MaxHerb", y = "mipe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Meadow Vole Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "mipe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Meadow Vole Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "mipe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Meadow Vole Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "mipe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Meadow Vole Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "mipe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eastern Meadow Vole Captures", ylab = "GroundCover")

#####RARA

ggscatter(data, x = "MaxHerb", y = "rara", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Black Rat Captures", ylab = "Max Herb")

ggscatter(data, x = "MaxShrub", y = "rara", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Black Rat Captures", ylab = "MaxShrub")

ggscatter(data, x = "AvgDensity", y = "rara", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Black Rat Captures", ylab = "AvgDensity")

ggscatter(data, x = "WoodyDebri", y = "rara", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Black Rat Captures", ylab = "WoodyDebri")

ggscatter(data, x = "GroundCover", y = "rara", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Black Rat Captures", ylab = "GroundCover")

