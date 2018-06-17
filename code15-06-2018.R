load(file = 'C:/Users/Mouna/Desktop/20180314_pipe-delimited-export/code-données/trials1.Rda')
summary (trials1)## ici donne un résumé des données 
### transformer des variables en facteur important
library(sqldf)
trials2<-trials1

trials3<-trials2[c(which(trials2$outcome==0),which(trials2$outcome==2),which(trials2$outcome==1)[1:(85812)]),]
table (trials3$outcome)

# recodage de la variable outcome pour cart 

trials3$outcome1 <- trials3$outcome
trials3$outcome1[trials3$outcome %in% ('0')]<-"notcompleted"
trials3$outcome1[trials3$outcome %in% ('1')]<-"terminated"
trials3$outcome1[trials3$outcome %in% ('2')]<-"ongoing"
table (trials3$outcome1)

trialbase <- sqldf("select * 
               from trials3
                   where outcome1 in ('notcompleted','terminated')")

trialval<- sqldf("select * 
               from trials3
                where outcome1 in ('ongoing')")
# recodage de la variable outcome

trialbase$type <- factor (trialbase$type)
trialbase$phase_r <-factor(trialbase$phase_r)
trialbase$has_dmc <- factor (trialbase$has_dmc)
trialbase$has_expanded_access <- factor (trialbase$has_expanded_access)
trialbase$were_results_reported <- factor (trialbase$were_results_reported)
trialbase$outcome<- factor (trialbase$outcome)
trialbase$outcome1<- factor (trialbase$outcome1)
str(trialbase)
trialbase1<-trialbase[,-1]


## creation des partitions 
install.packages("caret")
library(caret)
set.seed(123)
part<-createDataPartition(y=trialbase1$outcome,p = .8,list = F)
train <- trialbase1[part,] #80% training set
test <- trialbase1[-part,] #20% test set
str(trialbase1)
table(train$outcome)
#On vérifie les proportions de la cible dans les deux sous échantillons ici on voit OK

ytrain <- trialbase1$outcome[part]
ytrain1 <- trialbase1$outcome1[part]
ytest  <- trialbase1$outcome[-part]
ytest1  <- trialbase1$outcome1[-part]
table(ytrain)/sum(table(ytrain))
table(ytest)/sum(table(ytest))

reg<- glm(formula =outcome~.,data=subset(train, select=-outcome1),family=binomial)
summary(reg) 

##utilisation cours text mining 

library(parallel)
#install.packages("doParallel")
library(doParallel)

detectCores()

cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)


objControl <- trainControl(method='cv', number=10, returnResamp='none', classProbs = TRUE, summaryFunction=twoClassSummary, allowParallel = TRUE, seeds = NA)
## ci dessus spécification du nombe de validation croisé

## CART


gridsearch <- expand.grid(cp=seq(0, 1, 0.05)) #
tune <- train(subset(train, select=- c(outcome,outcome1)),ytrain1,method = "rpart",tuneGrid=gridsearch, trControl =objControl,metric='ROC')
tune
plot(tune)
tune$bestTune

pred <- predict(object=tune$finalModel, test,type='class')## tes le modèle avec le meilleu paramétre 
head(pred)
pred

install.packages("e1071")
conf.mat <- confusionMatrix(pred, ytest)
conf.mat$overall
conf.mat$byClass
library(reshape2)
library(ggplot2)
cm.plot <- function(table_cm){
  tablecm <- round(t(t(table_cm) / colSums(as.matrix(table_cm))*100)) # crée les pourcentages
  tablemelt <- melt(tablecm)
  ggplot(tablemelt, aes(Reference, Prediction)) +
    geom_point(aes(size = value, color=value), alpha=0.8, show.legend=FALSE) +
    geom_text(aes(label = value), color="white") +
    scale_size(range = c(5,25)) +
    scale_y_discrete(limits = rev(levels(tablemelt$Prediction)))+
    theme_bw()
}
cm.plot(conf.mat$table)

##On peut également afficher les variables qui ont le plus contribué à la construction de l'arbre.

imp <- varImp(tune$finalModel)
impdf <- data.frame(names = row.names(imp), imp = imp[,1])
impdf <- impdf[order(impdf$imp, decreasing = TRUE),]
names(impdf)[2]<-colnames(imp)[1]
impdf[1:15,]

#On peut représenter l'arbre
install.packages("rpart.plot")
library(rpart.plot) 
## Loading required package: rpart
rpart.plot(tune$finalModel)

install.packages("pROC")
library(pROC)

pred.rpart <- predict(object=tune$finalModel, reg_log_val2,type='prob')
rocCurve.rpart   <- roc(response = ytest2, predictor = pred.rpart[, "X1"], levels = rev(levels(ytest2)))
plot(rocCurve.rpart, print.thres = "best")

rocCurve.rpart$auc

## randomforest 
library(randomForest)
gridsearch <- expand.grid(mtry = seq(30,200,50))
tune1 <- train(subset(train, select=- c(outcome,outcome1)),ytrain1,method = "rf",tuneGrid=gridsearch, trControl =objControl,metric='ROC')
tune1
plot(tune1)
tune1$bestTune

pred1 <- predict(object=tune1$finalModel, reg_log_val2,type='class')
conf.mat <- confusionMatrix(pred, ytest)

conf.mat$overall
conf.mat$byClass

imp1 <- varImp(tune1$finalModel)
impdf1 <- data.frame(names = row.names(imp), imp1 = imp1[,1])
impdf1 <- impdf1[order(impdf1$imp1, decreasing = TRUE),]
names(impdf1)[2]<-colnames(imp1)[1]
impdf1[1:15,]

pred.rf <- predict(object=tune$finalModel, reg_log_val2,type='prob')
rocCurve.rf <- roc(response = ytest, predictor = pred.rf[, "economie"])
rocCurve.rf$auc
