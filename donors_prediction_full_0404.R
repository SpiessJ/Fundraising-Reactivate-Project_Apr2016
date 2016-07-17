rm(list = ls()) # Reset code, only run when really need

## =======================================================
## Project definition
## =======================================================

## Goal of the predictive model is to predict whether a donor will respond
## to a future reactivation campaign with a gift of at least 30 EUR.

## =======================================================
## Data preparation
## =======================================================

## Load data
setwd("D:/Dropbox/IESEG/04. Semester II/09. Descriptive & Predictive Analytics_24Feb16/PRJ - Fundraising Reactivate Project/data")
donors = read.delim(file="donors.txt",sep="\t")
campaigns = read.delim(file="campaigns.txt",sep="\t")
selections = read.delim(file="selections.txt",sep="\t")
gifts = read.delim(file="gifts.txt",sep="\t")
score = read.delim(file="score.txt",sep="\t")

## Clean dataset: donors 
donors$zipcode_1digit = as.factor(substr(donors$zipcode,1,1))
donors$zipcode_2digit = as.factor(substr(donors$zipcode,1,2))
donors$zipcode = NULL
donors$region = NULL

## Clean dataset: campaign
# install.packages("lubridate")
library(lubridate)

campaigns$send_begin = as.Date(campaigns$date,"%d%b%Y")
campaigns$send_end = campaigns$send_begin + months(2)
campaigns$gap_begin = campaigns$send_begin - weeks(2)
campaigns$period_begin = campaigns$send_begin - weeks(2) - years(4)
campaigns$date = NULL
campaigns$sent = NULL # Consider later
campaigns$language = NULL # No need to keep
campaigns$description = NULL

## Clean dataset: gifts
gifts$date = as.Date(gifts$date,"%d/%m/%Y")

## =======================================================
## Build basetable for Campaign 1-7
## =======================================================

# install.packages("data.table")
library(data.table)

campList = unique(selections$campID) # campID increases by time

## This function has 02 purposes:
## [1] Create basetable for training and building predicting model
## [2] Create predictors 
buildBasetable = function(selections,campList,flag) {

basetable = c()

for (id in 1:length(campList)) { # Combine full campaign list
  
  basetable.tmp = selections[selections$campID==campList[id],]
  
  ## Merge with: donors
  basetable.tmp = merge(basetable.tmp,donors,by=c("donorID"),all.x=T)
  
  ## Merge with: campaigns
  basetable.tmp = merge(basetable.tmp,campaigns,by=c("campID","commID"),
                        all.x=T,suffixes=c("","_camp"))
  
  ## -------------------------------------------------------
  ## Merge with: gifts
  ## -------------------------------------------------------
  
  if (flag=="7campaigns") {
  
  ## Calculate the donated amount for one specific campagin
  gift = as.data.table(gifts[gifts$campID==campList[id],])
  
  ## Keep only donations in the 2 months date range [send_begin, send_end]
  gift = gift[(gift$date >= basetable.tmp$send_begin[1]) & 
                (gift$date <= basetable.tmp$send_end[1])]
  gift[,date:=NULL]
  
  ## Aggregate donate amount and merge
  gift = gift[,lapply(.SD,sum),by=c("campID","commID","donorID")]
  basetable.tmp = merge(basetable.tmp,gift,by=c("campID","commID","donorID"),
                        all.x=T,suffixes=c("","_gift"))
  basetable.tmp$amount[is.na(basetable.tmp$amount)] = 0
  
  } # Else flag=="lastcampaign"
  
  ## -------------------------------------------------------
  ## Calculate predictors
  ## -------------------------------------------------------
  
  ## RFM calculation
  g = gifts[gifts$donorID %in% unique(basetable.tmp$donorID),]
  g = g[which(g$date >= basetable.tmp$period_begin[1] &
                g$date < basetable.tmp$gap_begin[1]),]
  
  ## Recency: last donation date / donorID
  lastDonate = aggregate(date~donorID,data=g,FUN=max)
  names(lastDonate)[2] = c("last_donate")
  basetable.tmp = merge(basetable.tmp,lastDonate,by=c("donorID"),all.x=T)
  basetable.tmp$last_donate[is.na(basetable.tmp$last_donate)] = basetable.tmp$period_begin[1]
  basetable.tmp$recency = as.numeric(basetable.tmp$last_donate - basetable.tmp$period_begin[1])
  # basetable.tmp$last_donate = NULL
  
  ## Frequency: count donation times / donorID
  countDonate = aggregate(date~donorID,data=g,FUN=length)
  names(countDonate)[2] = c("frequency")
  basetable.tmp = merge(basetable.tmp,countDonate,by=c("donorID"),all.x=T)
  basetable.tmp$frequency[is.na(basetable.tmp$frequency)] = 0
  
  ## Monetary: Total donation / donorID
  totalDonate = aggregate(amount~donorID,data=g,FUN=sum)
  names(totalDonate)[2] = c("monetary")
  basetable.tmp = merge(basetable.tmp,totalDonate,by=c("donorID"),all.x=T)
  basetable.tmp$monetary[is.na(basetable.tmp$monetary)] = 0
  
  ## Min, Max, Avg donation / donorID
  minDonate = aggregate(amount~donorID,data=g,FUN=min)
  names(minDonate)[2] = c("min_donate")
  basetable.tmp = merge(basetable.tmp,minDonate,by=c("donorID"),all.x=T)
  basetable.tmp$min_donate[is.na(basetable.tmp$min_donate)] = 0
  
  maxDonate = aggregate(amount~donorID,data=g,FUN=max)
  names(maxDonate)[2] = c("max_donate")
  basetable.tmp = merge(basetable.tmp,maxDonate,by=c("donorID"),all.x=T)
  basetable.tmp$max_donate[is.na(basetable.tmp$max_donate)] = 0
  
  avgDonate = aggregate(amount~donorID,data=g,FUN=mean)
  names(avgDonate)[2] = c("avg_donate")
  basetable.tmp = merge(basetable.tmp,avgDonate,by=c("donorID"),all.x=T)
  basetable.tmp$avg_donate[is.na(basetable.tmp$avg_donate)] = 0
  
  ## Average donation / campaign / donorID
  avgDonateCamp = aggregate(amount~campID+donorID,data=g,FUN=sum)
  avgDonateCamp = aggregate(amount~donorID,data=avgDonateCamp,FUN=mean)
  names(avgDonateCamp)[2] = c("avg_camp_donate")
  basetable.tmp = merge(basetable.tmp,avgDonateCamp,by=("donorID"),all.x=T)
  basetable.tmp$avg_camp_donate[is.na(basetable.tmp$avg_camp_donate)] = 0
  
  ## Min, Max, Avg, Avg_Camp  >= 30 / donorID
  basetable.tmp$min_donate_over30 = factor(ifelse(basetable.tmp$min_donate>=30,1,0),
                                           labels=c("no","yes"))
  basetable.tmp$max_donate_over30 = factor(ifelse(basetable.tmp$max_donate>=30,1,0),
                                           labels=c("no","yes"))
  basetable.tmp$avg_donate_over30 = factor(ifelse(basetable.tmp$avg_donate>=30,1,0),
                                           labels=c("no","yes"))
  basetable.tmp$avg_camp_donate_over30 = factor(ifelse(basetable.tmp$avg_camp_donate>=30,1,0),
                                                labels=c("no","yes"))

  ## Count number of times donor maked donation >= 30 / campaign
  totalDonateCamp = aggregate(amount~campID+donorID,data=g,FUN=sum)
  totalDonateCamp$count_over30 = ifelse(totalDonateCamp$amount>=30,1,0)
  countOver30 = aggregate(count_over30~donorID,data=totalDonateCamp,FUN=sum)
  basetable.tmp = merge(basetable.tmp,countOver30,by=c("donorID"),all.x=T)
  basetable.tmp$count_over30[is.na(basetable.tmp$count_over30)] = 0
  
  ## Count number of campaigns donor makes donation
  temp = aggregate(amount~campID+donorID,data=g,FUN=sum)
  countDonateCamp = aggregate(campID~donorID,data=temp,FUN=length)
  names(countDonateCamp)[2] = c("count_donate_camp")
  basetable.tmp = merge(basetable.tmp,countDonateCamp,by=c("donorID"),all.x=T)
  basetable.tmp$count_donate_camp[is.na(basetable.tmp$count_donate_camp)] = 0
  
  ## Longtime donors: last_donate - first_donate
  firstDonate = aggregate(date~donorID,data=g,FUN=min)
  names(firstDonate)[2] = c("first_donate")
  basetable.tmp = merge(basetable.tmp,firstDonate,by=c("donorID"),all.x=T)
  basetable.tmp$first_donate[is.na(basetable.tmp$first_donate)] = basetable.tmp$period_begin[1]
  basetable.tmp$longtime_donor = as.numeric(basetable.tmp$last_donate - basetable.tmp$first_donate)
  basetable.tmp$first_donate = NULL
  basetable.tmp$last_donate = NULL
  
  ## Average gap days between donations / donor
  donorDate = g[order(g$donorID,g$date),c("donorID","date")]
  donorList = unique(g$donorID)

  gapDonation = c()
  for (i in donorList) {
    dd = donorDate[donorDate$donorID==i,]
    dd$gap_day_donation = 0
    dd$gap_day_donation[nrow(dd)] = as.numeric(basetable.tmp$gap_begin[1] - dd$date[nrow(dd)]) - 1
    for (j in (nrow(dd)-1):1) { # For loop backwards
      if (j<1) break # If nrow(dd) = 1, wrongly for loop [0;1], then exit
      dd$gap_day_donation[j] = dd$date[j+1] - dd$date[j]
    }
    gapDonation = rbind(gapDonation,dd)
  }
  avgGapDonation = aggregate(gap_day_donation~donorID,data=gapDonation,FUN=mean)
  names(avgGapDonation)[2] = c("avg_gap_donation")
  basetable.tmp = merge(basetable.tmp,avgGapDonation,by=c("donorID"),all.x=T)
  basetable.tmp$avg_gap_donation[is.na(basetable.tmp$avg_gap_donation)] = as.numeric(basetable.tmp$gap_begin[1] - basetable.tmp$period_begin[1]) - 1
  
  ## Repeat activated donors: How many times we "spammed" this donor?
  campSelect = c(campList[id])
  for (i in 1:length(campList)) {
    if (i==id) next
    c = campaigns[campaigns$campID==campList[i],]
    if (c$send_begin[1] >= basetable.tmp$period_begin[1] &
        c$send_begin[1] < basetable.tmp$gap_begin[1]) campSelect = c(campSelect,campList[i])
  }

  s = selections[selections$campID %in% campSelect,]
  s$activated = 1
  countActivated = aggregate(activated~donorID,data=s,FUN=sum)
  names(countActivated)[2] = c("count_activation")
  basetable.tmp = merge(basetable.tmp,countActivated,by=c("donorID"),all.x=T)
  
  ## Combine with the main basetable
  basetable = rbind(basetable,basetable.tmp)
  
} # End the great for loop, finish creating predictors

return(basetable)

} # End of buildBasetable() function

scoreTable = buildBasetable(score,c(7541),"lastcampaign")
basetable = buildBasetable(selections,campList,"7campaigns")

## Define predicting target
basetable$active = ifelse(basetable$amount>=30,1,0)
basetableRaw = basetable
basetable$amount = NULL

## Drop some unused columns
dropList = c("campID","commID","donorID") # ID vars, no predictive meaning
dropList = c(dropList,"send_begin","send_end","gap_begin","period_begin") # Date-time vars, no predictive meaning
basetable = basetable[,!(names(basetable) %in% dropList)]

## -------------------------------------------------------
## Create train/select/test sets
## -------------------------------------------------------

## Create train, select and test datasets: 50:20:30
## This method will shuffle the train/select/test sets everytime re-run script
n = dim(basetable)[1]
n.train = round(n*.5) # 50%
n.select = round(n*.2) # 20%

list.full = c(1:n)
list.train = sample(n,n.train)
train = basetable[list.train,] # Train set
rest = basetable[!(list.full %in% list.train),]

list.rest = c(1:(n-n.train))
list.select = sample(n-n.train,n.select)
select = rest[list.select,] # Select set
test = rest[!(list.rest %in% list.select),] # Test set

## Create RAW datasets to run Decision Tree model
trainRaw = train
selectRaw = select
testRaw = test

## -------------------------------------------------------
## Replace outliers and create dummy variables
## -------------------------------------------------------

## Replace outliers
numericList = names(which(sapply(basetable,is.numeric)))
for (v in numericList) {
  UL = mean(basetable[,v]) + 3*sd(basetable[,v])
  LL = mean(basetable[,v]) - 3*sd(basetable[,v])
  basetable[basetable[,v]<LL,v] = LL
  basetable[basetable[,v]>UL,v] = UL 
}

## Segmentation and combination
train$segment = c("train")
select$segment = c("select")
test$segment = c("test")
basetable = rbind(train,select,test)

# install.packages("dummies")
library(dummies)

factorVars = names(which(sapply(basetable,is.factor)))
for (v in factorVars) {
  d = as.data.frame(dummy(v,data=basetable,sep="_"))
  basetable = cbind(basetable,d)
  basetable[,v] = NULL # Drop column v
}

## Drop some more columns, blank or duplicated zipcode
basetable$zipcode_1digit_ = NULL # Blank
basetable$zipcode_2digit_ = NULL # Blank
# basetable$zipcode_2digit_0 = NULL # Duplicated with zipcode_1digit_0
# basetable$zipcode_1digit_S = NULL # Duplicated with zipcode_2digit_sw

## Divide into train/select/test datasets
train = basetable[basetable$segment=="train",]
select = basetable[basetable$segment=="select",]
test = basetable[basetable$segment=="test",]
train$segment = NULL
select$segment = NULL
test$segment = NULL

## =======================================================
## Model building - Selecting model (LG, DT, RF)
## =======================================================

# install.packages("pROC")
library(pROC)

## -------------------------------------------------------
## Model basic - Logistic Regression
## -------------------------------------------------------

## Feature selection, p.value < 0.05
## Note: some p.value == NA
varList = names(train)
selectList = c()
for (v in varList) {
  p = cor.test(train[,v],train$active)$p.value
  if ((!is.na(p)) & (p<.05)) selectList = c(selectList,v)
  if (is.na(p)) print(v) # Print out the error vars to check
}

## Drop some other dummy variables to prevent multi-collinearity
selectList = selectList[!(selectList %in% c("min_donate_over30_yes",
                                            "max_donate_over30_yes",
                                            "avg_donate_over30_yes",
                                            "avg_camp_donate_over30_yes"))]
selectList # Print out final selection list

trainSelected = train[,selectList]
selectSelected = select[,selectList]
testSelected = test[,selectList]

modelLG = glm(active~.,data=trainSelected,family=binomial)
summary(modelLG)

## -------------------------------------------------------
## Model basic - Decision Tree
## -------------------------------------------------------

# install.packages("rpart")
library(rpart)

set.seed(133)

## Decision Tree with less strictly controls
## Run on RAW dataset
modelDT = rpart(active~.,data=trainRaw,method="anova",
                control=rpart.control(minsplit=2,minbucket=1,cp=0.001))
summary(modelDT)

## -------------------------------------------------------
## Model advance - Random Forest
## -------------------------------------------------------

# install.packages('randomForest')
library(randomForest)

## Run on selected data set
modelRF = randomForest(active~.,data=trainSelected,importance=T,mtry=2,ntree=100)
summary(modelRF)

## Compare model with different number of trees (ntree)
# colors = c("red","blue","green","yellow")
# plot(roc(target~predict,data=evRF.train),col=colors[1])
# i = 2
# for(ntree in c(250,500,1000)) {
#   modelRF2 = randomForest(active~.,data=trainSelected,importance=T,mtry=2,ntree=ntree)
#   predictRF2.test = predict(modelRF2,newdata=testSelected)
#   evRF2.test = cbind(predictRF2.test,testSelected$active)
#   colnames(evRF2.test) = c("predict","target")	
#   
#   plot(roc(target~predict,data=evRF.test),add=T,col=colors[i])
#   i = i+1
# }

## -------------------------------------------------------
## Evaluation and selection models
## -------------------------------------------------------

## Evaluation for Logistic Regression
predictLG.train = predict(modelLG,newdata=trainSelected)
predictLG.select = predict(modelLG,newdata=selectSelected)

evLG.train = cbind(predictLG.train,train$active)
colnames(evLG.train) = c("predict","target")
evLG.select = cbind(predictLG.select,select$active)
colnames(evLG.select) = c("predict","target")

## Evaluation for Decision Tree
predictDT.train = predict(modelDT,newdata=trainRaw)
predictDT.select = predict(modelDT,newdata=selectRaw)

evDT.train = cbind(predictDT.train,trainRaw$active)
colnames(evDT.train) = c("predict","target")
evDT.select = cbind(predictDT.select,selectRaw$active)
colnames(evDT.select) = c("predict","target")

## Evaluation for Random Forest
predictRF.train = predict(modelRF)
predictRF.select = predict(modelRF,newdata=selectSelected)

evRF.train = cbind(predictRF.train,trainSelected$active)
colnames(evRF.train) = c("predict","target")
evRF.select = cbind(predictRF.select,selectSelected$active)
colnames(evRF.select) = c("predict","target")

## Plotting to select the best model

## Plot Logistic Regression training vs. selecting
plot(roc(target~predict,data=evLG.train),main="Logistic Regression ROC Curve",col=2)
plot(roc(target~predict,data=evLG.select),add=T,col=3)
legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Selecting"),bty='n')

## Plot Decision Tree training vs. selecting
plot(roc(target~predict,data=evDT.train),main="Decision Tree ROC Curve",col=2)
plot(roc(target~predict,data=evDT.select),add=T,col=3)
legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Selecting"),bty='n')

## Plot Random Forest training vs. selecting
plot(roc(target~predict,data=evRF.train),main="Random Forest ROC Curve",col=2)
plot(roc(target~predict,data=evRF.select),add=T,col=3)
legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Selecting"),bty='n')

## Plot 3 models together (selecting sets)
plot(roc(target~predict,data=evLG.select),main="LG, DT and RF ROC Curves",col=2)
plot(roc(target~predict,data=evDT.select),add=T,col=3)
plot(roc(target~predict,data=evRF.select),add=T,col=4)
legend("bottomright",col=c(2:4),lwd=2,legend=c("LR","DT","RF"),bty='n')

## Area Under the ROC (AUC)
aucMT = matrix(0,3,2)
rownames(aucMT) = c("LG","DT","RF")
colnames(aucMT) = c("train","select")
aucMT[1,1] = roc(target~predict,data=evLG.train)$auc
aucMT[1,2] = roc(target~predict,data=evLG.select)$auc
aucMT[2,1] = roc(target~predict,data=evDT.train)$auc
aucMT[2,2] = roc(target~predict,data=evDT.select)$auc
aucMT[3,1] = roc(target~predict,data=evRF.train)$auc
aucMT[3,2] = roc(target~predict,data=evRF.select)$auc
aucMT

## -------------------------------------------------------
## Logistic Regression with stepwise selection
## -------------------------------------------------------

## Run LG with stepwise for both directions
noVar = glm(active~1,data=trainSelected,family=binomial)
fullVar = glm(active~.,data=trainSelected,family=binomial)
modelSW = step(noVar,scope=formula(fullVar),data=trainSelected,direction="both")
summary(modelSW)

varSW = names(modelSW$coefficients)
varSW

## Create comparison table and plotting
aucTrain = rep(0,length(varSW)-1)
aucSelect = rep(0,length(varSW)-1)
aucTest = rep(0,length(varSW)-1)
for(i in c(1:(length(varSW)-1))) {
  vars = varSW[2:(i+1)]
  formula = paste("active","~",paste(vars,collapse="+"))
  model = glm(formula,data=trainSelected,family=binomial)
  
  predictSW.train = predict(model,newdata=trainSelected,type="response")
  predictSW.select = predict(model,newdata=selectSelected,type="response")
  predictSW.test = predict(model,newdata=testSelected,type="response")
  
  evSW.train = cbind(predictSW.train,trainSelected$active)
  colnames(evSW.train) = c("predict","target")
  evSW.select = cbind(predictSW.select, selectSelected$active)
  colnames(evSW.select) = c("predict","target")
  evSW.test = cbind(predictSW.test,testSelected$active)
  colnames(evSW.test) = c("predict","target")
  
  aucTrain[i] = roc(target~predict,data=evSW.train)$auc
  aucSelect[i] = roc(target~predict,data=evSW.select)$auc
  aucTest[i] = roc(target~predict,data=evSW.test)$auc
} 

## Plot in the same axes to compare
yLimit=range(c(min(aucTrain,aucSelect,0.55),max(aucTrain,aucSelect,0.7)))

plot(aucTrain,main="Logistic Regression with Stepwise (both)",type="b",col=2,
     xlab="Model size",ylab="AUC",ylim=yLimit)
par(new=T)
plot(aucSelect,type="b",col=3,axes=F,ylim=yLimit,xlab="",ylab="")
legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Selecting"),bty='n')

numVars = 6 # Select 8 variables for the final model
abline(v=numVars) 

## Show AUC result table
cbind(aucTrain,aucSelect,aucTest)

## -------------------------------------------------------
## Finalize model with new selected variables
## -------------------------------------------------------

finalVars = c(varSW[c(2:(numVars+1))],"active") # Take [numVars] variables
trainFinal = train[,finalVars]
testFinal = test[,finalVars]

modelFN = glm(active~.,data=trainFinal,family=binomial)
summary(modelFN)

# Evaluation final model
predictFN.train = predict(modelFN,newdata=trainFinal)
predictFN.test = predict(modelFN,newdata=testFinal)

evFN.train = cbind(predictFN.train,trainFinal$active)
colnames(evFN.train) = c("predict","target")
evFN.test = cbind(predictFN.test,testFinal$active)
colnames(evFN.test) = c("predict","target")

## Plot final model evaluation training vs. testing
plot(roc(target~predict,data=evFN.train),main="Logistic Regression with Stepwise ROC Curve", col=2)
plot(roc(target~predict,data=evFN.test),add=T,col=3)
legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Testing"),bty='n')

## =======================================================
## Business interpretation - Plotting 
## =======================================================

# install.packages("ROCR")
library(ROCR)

## Final model Lift graphs
pred <- prediction(evFN.train[,1],evFN.train[,2])
perf <- performance(pred,"lift","rpp")
plot(perf,main="Lift curve",col=2,ylim=c(1,20))

pred <- prediction(evFN.test[,1],evFN.test[,2])
perf <- performance(pred,"lift","rpp")
plot(perf,main="Lift curve",col=3,add=T,ylim=c(1,20))

legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Testing"),bty='n')
abline(v=0.2)
abline(h=2.1)

## Final model Cumulative gains graph
pred <- prediction(evFN.train[,1],evFN.train[,2])
perf <- performance(pred,"tpr","fpr")
plot(perf,main="Cumulative gains",col=2)

pred <- prediction(evFN.test[,1],evFN.test[,2])
perf <- performance(pred,"tpr","fpr")
plot(perf,main="Cumulative gains",col=3,add=T)

legend("bottomright",col=c(2:3),lwd=2,legend=c("Training","Testing"),bty='n')
abline(v=0.2)
abline(h=0.4)

## =======================================================
## Predicting on new dataset [Score]
## =======================================================

## Build datatable for Score
scoreTable = buildBasetable(score,c(7541),"lastcampaign")
scoreTable = scoreTable[,!(names(scoreTable) %in% c("campID","commID"))]

## Replace outliers
numericList = names(which(sapply(scoreTable,is.numeric)))
numericList = numericList[which(!(numericList %in% "donorID"))]
for (v in numericList) {
  UL = mean(scoreTable[,v]) + 3*sd(scoreTable[,v])
  LL = mean(scoreTable[,v]) - 3*sd(scoreTable[,v])
  scoreTable[scoreTable[,v]<LL,v] = LL
  scoreTable[scoreTable[,v]>UL,v] = UL 
}

# Create dummy variables
factorVars = names(which(sapply(scoreTable,is.factor)))
for (v in factorVars) {
  d = as.data.frame(dummy(v,data=scoreTable,sep="_"))
  scoreTable = cbind(scoreTable,d)
  scoreTable[,v] = NULL # Drop column v
}

## Make final prediction
predictScore = predict(modelFN,newdata=scoreTable,type="response")
result = cbind(scoreTable$donorID,predictScore)
colnames(result) = c("donorID","rank")
write.csv(result,file="finalScore_prob_team3.csv",row.names=F)
# write(result,file="finalScore_team3.txt",row.names=F,sep="\t")

## =======================================================
## Last edited on 4 April 2016. By Minh PHAN.
## =======================================================