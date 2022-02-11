### Project: EHR SOFA
### Purpose: Shaded plot of predicted prob of mortality based on glm and tree models, using age and total SOFA as the predictors
### Date: 20JUL2021
### Author: Martha Nason, changed slightly by Jing Wang
### Note: points represent the actual value of age and SOFA from training set (many dots were overlapped)
#         background shade represents the predicted values (predicted using a user-defined small set of data for visualization purpose)


# setwd("J:\\SOFA\\Tree\\TenPct\\")
# 
# load("FirstMV_20JUL2021_final.RData")
load(here::here("FirstMV_20JUL2021_final.RData"))

library(RColorBrewer)
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

cols <- addalpha(hcl.colors(10),.5)

pal<- addalpha(colorRampPalette(c("dark red","yellow","dark green" ))(30),.5)

#Make pretty plots of predictions

use.pch <- 17+as.numeric(as.factor(FirstMVdata.train$death))
use.col <- ifelse(FirstMVdata.train$death=="discharged","red","dark green")
use.col.test <- ifelse(FirstMVdata.test$death=="discharged","red","dark green")
newdata2=expand.grid(AGE=40:90,Pre1day_total_score_num_imp=0:18)


#Glm: Total SOFA  and age
temp_glm <- glm(as.factor(death)~Pre1day_total_score_num_imp+AGE,data=FirstMVdata.train,family=binomial())
grid.pred <- matrix(predict(temp_glm, newdata=newdata2),nrow=51,ncol=19,byrow=F)
predict.discharge.glm <- predict(temp_glm, newdata=FirstMVdata.test, type="response",simplify=T) 

pdf("glm_Age_TotalSofaImp.pdf", width=12, height=8)
image(x=40:90,y=0:18,z=as.matrix(grid.pred),xlab="Age", ylab="Total Sofa Score",col=pal)
points(FirstMVdata.train$AGE, jitter(FirstMVdata.train$Pre1day_total_score_num_imp), pch=use.pch, col=use.col,cex=1.5)
dev.off()

#Tree model: Total SOFA  and age
temp2a <- ctree(as.factor(death)~Pre1day_total_score_num_imp+AGE, data=FirstMVdata.train, minbucket=1000, mincriterion=.99)
grid.pred <- matrix(predict(temp2a, newdata=newdata2,type="prob")[,2],nrow=51,ncol=19,byrow=F)
predict.discharge.tree <- predict(temp2a, newdata=FirstMVdata.test, type="prob",simplify=T)

pdf("tree_AGE_TotalSofaImp.pdf",width=12, height=8)
image(x=40:90,y=0:18,z=as.matrix(grid.pred),xlab="Age", ylab="Total Sofa Score",col=pal)
with(FirstMVdata.train, points(AGE, jitter(Pre1day_total_score_num_imp), pch=use.pch, col=use.col,cex=1.5))
dev.off()




### same-day SOFA
newdata2=expand.grid(AGE=40:90,total_score_num_imp=0:18)

#Glm: Total SOFA  and age
temp_glm <- glm(as.factor(death)~total_score_num_imp+AGE,data=FirstMVdata.train,family=binomial())
grid.pred <- matrix(predict(temp_glm, newdata=newdata2),nrow=51,ncol=19,byrow=F)
predict.discharge.glm <- predict(temp_glm, newdata=FirstMVdata.test, type="response",simplify=T) 

pdf("glm_Age_TotalSofaImp_sameDaySOFA.pdf", width=12, height=8)
image(x=40:90,y=0:18,z=as.matrix(grid.pred),xlab="Age", ylab="Total Sofa Score",col=pal)
points(FirstMVdata.train$AGE, jitter(FirstMVdata.train$total_score_num_imp), pch=use.pch, col=use.col,cex=1.5)
dev.off()

#Tree model: Total SOFA  and age
temp2a <- ctree(as.factor(death)~total_score_num_imp+AGE, data=FirstMVdata.train, minbucket=1000, mincriterion=.99)
grid.pred <- matrix(predict(temp2a, newdata=newdata2,type="prob")[,2],nrow=51,ncol=19,byrow=F)
predict.discharge.tree <- predict(temp2a, newdata=FirstMVdata.test, type="prob",simplify=T)

pdf("tree_AGE_TotalSofaImp_sameDaySOFA.pdf",width=12, height=8)
image(x=40:90,y=0:18,z=as.matrix(grid.pred),xlab="Age", ylab="Total Sofa Score",col=pal)
with(FirstMVdata.train, points(AGE, jitter(total_score_num_imp), pch=use.pch, col=use.col,cex=1.5))
dev.off()


### pre 2 day SOFA
newdata2=expand.grid(AGE=40:90,Pre2day_total_score_num_imp=0:18)

#Glm: Total SOFA  and age
temp_glm <- glm(as.factor(death)~Pre2day_total_score_num_imp+AGE,data=FirstMVdata.train,family=binomial())
grid.pred <- matrix(predict(temp_glm, newdata=newdata2),nrow=51,ncol=19,byrow=F)
predict.discharge.glm <- predict(temp_glm, newdata=FirstMVdata.test, type="response",simplify=T) 

pdf("glm_Age_TotalSofaImp_with48hrPriortoVent.pdf", width=12, height=8)
image(x=40:90,y=0:18,z=as.matrix(grid.pred),xlab="Age", ylab="Total Sofa Score",col=pal)
points(FirstMVdata.train$AGE, jitter(FirstMVdata.train$Pre2day_total_score_num_imp), pch=use.pch, col=use.col,cex=1.5)
dev.off()

#Tree model: Total SOFA  and age
temp2a <- ctree(as.factor(death)~Pre2day_total_score_num_imp+AGE, data=FirstMVdata.train, minbucket=1000, mincriterion=.99)
grid.pred <- matrix(predict(temp2a, newdata=newdata2,type="prob")[,2],nrow=51,ncol=19,byrow=F)
predict.discharge.tree <- predict(temp2a, newdata=FirstMVdata.test, type="prob",simplify=T)

pdf("tree_AGE_TotalSofaImp_with48hrPriortoVent.pdf",width=12, height=8)
image(x=40:90,y=0:18,z=as.matrix(grid.pred),xlab="Age", ylab="Total Sofa Score",col=pal)
with(FirstMVdata.train, points(AGE, jitter(Pre2day_total_score_num_imp), pch=use.pch, col=use.col,cex=1.5))
dev.off()

