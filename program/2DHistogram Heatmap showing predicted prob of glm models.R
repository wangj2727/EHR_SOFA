### Project: EHR SOFA
### Purpose: create 2D histogram or heatmap to visualize predicted prob of 3 different logistic regression models
###          Models: ~ SOFA, ~ Age, ~ Sofa + Age
### Date: 20JUL2021
### Programmer: Jing Wang

library(gridExtra)
library(ggplot2)
library(dplyr)
library(pROC)
library(ROCR)

# setwd("J:\\SOFA\\Tree\\TenPct\\")
# 
# load("FirstMV_20JUL2021_final.RData")
load(here::here("FirstMV_20JUL2021_final.RData"))

### SOFA alone
glm0 <- glm(Outcome~ .,data=FirstMV.select%>%select(Pre1day_total_score_num_imp, Outcome)%>%filter(!is.na(Pre1day_total_score_num_imp)),family=binomial())
broom::tidy(glm0, exponentiate = TRUE, conf.int=TRUE )%>%
  mutate_if(is.numeric, round, 3)

prob.discharge.train.sofa <- predict(glm0,  newdata=FirstMVdata.train%>%
                                       filter(!is.na(Pre1day_total_score_num_imp)), type="response",simplify=T)
prob.discharge.test.sofa <- predict(glm0, newdata=FirstMVdata.test%>%
                                      filter(!is.na(Pre1day_total_score_num_imp)), type="response",simplify=T)

### age alone
glm0 <- glm(Outcome~ .,data=FirstMV.select%>%filter(!is.na(Pre1day_total_score_num_imp))%>%select(AGE, Outcome),family=binomial())
broom::tidy(glm0, exponentiate = TRUE, conf.int=TRUE )%>%
  mutate_if(is.numeric, round, 3)

prob.discharge.train.age <- predict(glm0,  newdata=FirstMVdata.train%>%
                                      filter(!is.na(Pre1day_total_score_num_imp)), type="response",simplify=T)
prob.discharge.test.age <- predict(glm0, newdata=FirstMVdata.test%>%
                                     filter(!is.na(Pre1day_total_score_num_imp)), type="response",simplify=T)

### SOFA + age 
glm0 <- glm(Outcome~ .,data=FirstMV.select%>%
              filter(!is.na(Pre1day_total_score_num_imp))%>%
              select(Pre1day_total_score_num_imp,AGE, Outcome),family=binomial())
broom::tidy(glm0, exponentiate = TRUE, conf.int=TRUE )%>%
  mutate_if(is.numeric, round, 3)

prob.discharge.train.sofaAge <- predict(glm0,  newdata=FirstMVdata.train%>%
                                          filter(!is.na(Pre1day_total_score_num_imp)), type="response",simplify=T)
prob.discharge.test.sofaAge <-predict(glm0, newdata=FirstMVdata.test%>%
                                        filter(!is.na(Pre1day_total_score_num_imp)), type="response",simplify=T)


plotdta <- FirstMV.select%>%
  filter(!is.na(Pre1day_total_score_num_imp))%>%
  select(Pre1day_total_score_num_imp,AGE, Outcome)%>%
  bind_cols(prob.discharge.train.sofa)%>%
  bind_cols(prob.discharge.train.age)%>%
  bind_cols(prob.discharge.train.sofaAge)%>%
  rename(prob.sofa = `...4`, prob.age = `...5`, prob.sofaAge = `...6`)


### plot of predictions by SOFA alone, and by Age alone, separately
scatterP<-ggplot(plotdta, aes(x=prob.sofa, y=prob.age))+
  geom_point()+
  geom_density_2d()+
  ylab("Predicted prob by Age alone")+xlab("Predicted prob by SOFA alone")+
  theme_minimal()+
  xlim(c(0,1))+ylim(c(0,1))

heatP <- ggplot(plotdta, aes(x=prob.sofa, y=prob.age))+
  stat_bin2d()+
  ylab("Predicted prob by Age alone")+xlab("Predicted prob by SOFA alone")+
  theme_minimal()+
  xlim(c(0,1))+ylim(c(0,1))

xdensityP <-ggplot(plotdta, aes(x = prob.sofa)) +
  geom_histogram(aes(y = ..density..),alpha=.5)+
  geom_density(aes(y = ..density..),alpha=.5)+
xlab("Predicted prob by SOFA alone")+
  theme_minimal()+
  xlim(c(0,1))

ydensityP <-ggplot(plotdta, aes(x = prob.age)) +
  geom_histogram(aes(y = ..density..),alpha=.5)+
  geom_density(aes(y = ..density..),alpha=.5)+xlab("Predicted prob by Age alone")+
  coord_flip()+
  xlim(c(0,1))+
  theme_minimal()

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

pdf("ContourPlot_predictedPro_bySofaAlone_byAgeAlone.pdf", height=8, width = 8)

TwoDDensity <- grid.arrange(xdensityP, blankPlot, scatterP, ydensityP, 
                            ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
TwoDDensity
dev.off()


pdf("2DHistPlot_predictedPro_bySofaAlone_byAgeAlone.pdf", height=8, width = 8)

TwoDDensity <- grid.arrange(xdensityP, blankPlot, heatP, ydensityP, 
                            ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
TwoDDensity
dev.off()



### plot of predictions by SOFA alone, and by Age +sofa, separately
scatterP<-ggplot(plotdta, aes(x=prob.sofa, y=prob.sofaAge))+
  geom_point()+
  geom_density_2d()+
  ylab("Predicted prob by Age + Sofa")+xlab("Predicted prob by SOFA alone")+
  theme_minimal()+
  xlim(c(0,1))+ylim(c(0,1))

heatP <- ggplot(plotdta, aes(x=prob.sofa, y=prob.sofaAge))+
  stat_bin2d()+
  ylab("Predicted prob by Age + Sofa")+xlab("Predicted prob by SOFA alone")+
  theme_minimal()+
  xlim(c(0,1))+ylim(c(0,1))

xdensityP <-ggplot(plotdta, aes(x = prob.sofa)) +
  geom_histogram(aes(y = ..density..),alpha=.5)+
  geom_density(aes(y = ..density..),alpha=.5)+
xlab("Predicted prob by SOFA alone")+
  theme_minimal()+
  xlim(c(0,1))

ydensityP <-ggplot(plotdta, aes(x = prob.sofaAge)) +
  geom_histogram(aes(y = ..density..),alpha=.5)+
  geom_density(aes(y = ..density..),alpha=.5)+xlab("Predicted prob by Age + Sofa")+
  coord_flip()+
  xlim(c(0,1))+
  theme_minimal()

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

pdf("ContourPlot_predictedPro_bySofaAlone_byAge+Sofa.pdf", height=8, width = 8)

TwoDDensity <- grid.arrange(xdensityP, blankPlot, scatterP, ydensityP, 
                            ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
TwoDDensity
dev.off()


pdf("2DHistPlot_predictedPro_bySofaAlone_byAge+Sofa.pdf", height=8, width = 8)

TwoDDensity <- grid.arrange(xdensityP, blankPlot, heatP, ydensityP, 
                            ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
TwoDDensity
dev.off()


