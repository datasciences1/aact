################################################################################################################################################################################################################################
################################################################################################################################################################################################################################

########################################################
## 
## AACT clinical data base query
## 
########################################################

#From txt file uploaded
setwd('D:/4. learning/05. STAT/04. DS-Training/0. Project/database/20170811_pipe-delimited-export')
#setwd('E:/4. learning/05. STAT/04. DS-Training/0. Project/database/20170811_pipe-delimited-export')

suppressWarnings(library(data.table))
suppressWarnings(library(readxl))
suppressWarnings(library(sqldf))


file <- "0.summary.xlsx"
tbl <- read_excel(path=file, col_names= TRUE, na="", n_max= Inf)

aact <- data.frame(name = tbl[which(tbl$Status=='Y'),]$Name,column = tbl[which(tbl$Status=='Y'),]$Columns)

for(i in 1:nrow(aact)){
      nm    <- paste("'",aact$name[i],"'",sep="")
      input <- paste("'",aact$name[i],".txt'",sep="")
      xpr   <- paste("c('",gsub(",","','",gsub(" ","",aact$column[i])),"')",sep="")
      nm   <- eval(parse(text=nm))
      input<- eval(parse(text=input))
      col   <- eval(parse(text=xpr))
      
      assign(nm, fread(input, header=TRUE, select= col ))
}


# Regroup countries
cntrs <- sqldf("select nct_id, 
                       case when name in ('American Samoa ','Guam','Northern Mariana Islands','United States','United States Minor Outlying Islands','Virgin Islands (U.S.)') then 'USA'
                            when name in ('Aruba','Netherlands Antilles')                                                                                                     then 'Netherland'
                            when name in ('Bermuda','Cayman Islands','Gibraltar','Montserrat','United Kingdom')                                                               then 'UK'
                            when name in ('Congo, The Democratic Republic of the','The Democratic Republic of the Congo')                                                     then 'Congo'
                            when name in ('Czechia','Czech Republic')                                                                                                         then 'Congo'
                            when name in ('Faroe Islands','Greenland')                                                                                                        then 'Denmark'
                            when name in ('French Polynesia','Guadeloupe','Martinique','New Caledonia','Réunion')                                                             then 'France'
                            when name in ('Holy See (Vatican City State)')                                                                                                    then 'Vatican'
                            when name in ('Hong Kong','Macau','Taiwan')                                                                                                       then 'China'
                            when name like '%Korea%'                                                                                                                          then 'North Korea'
                            when name in ('Former Yugoslavia and Macedonia','The Former Yugoslav Republic of')                                                                then 'Macedonia'
                            when name in ('Federated States of Micronesia','Nauru','Palau')                                                                                   then 'Macedonia'
                            when name in ('Palestinian Territories','Occupied and Palestinian Territory, Occupied')                                                           then 'Palestine'
                            when name in ('Syrian Arab Republic')                                                                                                             then 'Syria'
                            when name in ('Russian Federation')                                                                                                               then 'Russia'
                            else name end as country
                from countries ")


##############################################################################################
##############################################################################################

## start a model based on 5 variables (sponsor, nb on countries, in criteria, ex criteria, therapeutic area)
#derive in study ==> variable outcome as such
# 3 - completed	    ["Completed", "Approved for marketing"]
# 2 - ongoing		    ["Recruiting", "Not yet recruiting", "Active, not recruiting","Enrolling by invitation","Suspended"]
# 1 - unknown       ["Unknown status", "Available","No longer available","Temporarily not available"]
# 0 - not completed	["Terminated", "Withdrawn"]

trial <- sqldf("select nct_id, study_type, overall_status, phase, number_of_arms, has_dmc, substr(start_date,1,4) as stdat, 
                       case when overall_status in ('Terminated', 'Withdrawn','Withheld') then 'Stopped'
                            when overall_status in ('Approved for marketing','Completed') then 'Completed'
                            when overall_status in ('Recruiting', 'Not yet recruiting', 'Active, not recruiting','Enrolling by invitation','Suspended') then 'Ongoing'
                            when overall_status in ('Unknown status', 'Available','No longer available','Temporarily not available','') then 'Unknown' end as outcome,
                       case when overall_status in ('Terminated', 'Withdrawn','Withheld') then 0
                            when overall_status in ('Approved for marketing','Completed') then 1
                            when overall_status in ('Recruiting', 'Not yet recruiting', 'Active, not recruiting','Enrolling by invitation','Suspended') then 2
                            when overall_status in ('Unknown status', 'Available','No longer available','Temporarily not available','') then 0 end as outcome_n,
                       case when study_type = 'Expanded Access' then 0
                            when study_type = 'Interventional' then 1
                            when study_type in ('Observational', 'Observational [Patient Registry]') then 2
                            when study_type in ('Unknown','N/A','') then 3 end as type,
                       case when phase in ('Early Phase 1','Phase 1') then 1
                            when phase in ('Phase 3','Phase 2/Phase 3') then 3
                            when phase in ('Phase 2','Phase 1/Phase 2') then 2
                            when phase='N/A' then 0
                            when phase='Phase 4' then 4
                            end as phase_r
                from studies ")

region <- sqldf("select nct_id, count(*) as nb_rg from cntrs group by nct_id")
trial <- sqldf("select t.*, r.nb_rg from trial as t left join region as r on t.nct_id = r.nct_id ")

ae <- sqldf("select nct_id, count(*) as nb_ae from reported_events group by nct_id")
trial <- sqldf("select t.*, a.nb_ae from trial as t left join ae as a on t.nct_id = a.nct_id ")

trial$number_of_arms <- ifelse(trial$type=='Observational' & is.na(trial$number_of_arms),0, trial$number_of_arms)
trial$has_dmc <- ifelse(trial$has_dmc=='' ,'f',trial$has_dmc)
trial$stdat <- as.numeric(trial$stdat)

trial$stdat <- ifelse(trial$outcome=='Stopped' & is.na(trial$stdat),median(trial[trial$outcome=='Stopped',]$stdat,na.rm = T),trial$stdat)

trial$study_type <- NULL
trial$overall_status <- NULL
trial$phase <- NULL

#trial <- subset(trial, outcome_n!=3)


# trial
# nct_id, has_dmc, stdat, outcome, outcome_r, type, phase_r, 
# to be imputed: nb_rg*, nb_ae* and number_of_arms*
table(trial$number_of_arms,useNA = "always")
table(trial$nb_rg,useNA = "always")
trial[is.na(trial$nb_ae),]

# Calculation nb of inclusion criteria and exclusion criteria
eligibilities$id <- with(eligibilities, ave(nct_id, FUN = seq_along))
x <- strsplit(eligibilities$criteria,split = 'Exclusion Criteria:')

# take only a sample of values
#y <- as.data.frame(do.call(rbind,x))
y <- as.data.frame(matrix(unlist(x), nrow=251244, byrow= T))
names(y)[01]<-"inc"
names(y)[02]<-"exc"

y$inc <- as.character(y$inc)
y$exc <- as.character(y$exc)

y$ninc <- nchar(y$inc)
y$nexc <- nchar(y$exc)

## "Number. "
y$nb_inc <- lengths(regmatches(y$inc, gregexpr("\\d+\\.\\s+", y$inc, perl = TRUE)))
y$nb_exc <- lengths(regmatches(y$exc, gregexpr("\\d+\\.\\s+", y$exc, perl = TRUE)))

## " - "
y$nb_inc <- ifelse(y$nb_inc==0,lengths(regmatches(y$inc, gregexpr("\\s+\\-\\s+", y$inc, perl = TRUE))),y$nb_inc)
y$nb_exc <- ifelse(y$nb_exc==0,lengths(regmatches(y$exc, gregexpr("\\s+\\-\\s+", y$exc, perl = TRUE))),y$nb_exc)


y$nb_inc <- ifelse(y$nb_inc==0 & y$ninc>0,1,y$nb_inc)
y$nb_exc <- ifelse(y$nb_exc==0 & y$nexc>0,1,y$nb_exc)

y$id <- with(y, ave(inc, FUN = seq_along))

criteria <- sqldf("select nct_id, nb_inc, nb_exc from eligibilities as e left join y as y on e.id = y.id ")
#criteria <- sqldf("select nct_id, gender, healthy_volunteers as healthy_subj, minimum_age, maximum_age, nb_inc, nb_exc from eligibilities as e left join y as y on e.id = y.id ")
#criteria$gender <- ifelse(criteria$gender=='','All',criteria$gender)
#criteria$healthy_subj <- ifelse(criteria$healthy_subj=='Accepts Healthy Volunteers' | criteria$healthy_subj=='','Yes',criteria$healthy_subj)


trial <- sqldf("select t.*, c.nb_inc, c.nb_exc
                from trial as t left join criteria as c on t.nct_id = c.nct_id ")


#table(trial$nb_inc,useNA = "always")
#table(trial$nb_exc,useNA = "always")

### NOT DONE FOR THE MOMENT: maybe later

## Imputation nb_rg*, nb_ae* and number_of_arms*
# Number of arms
# trial[is.na(trial$number_of_arms),]
# for expanded access it is always NA, we will impute to zero
# trial$test <- ifelse(trial$type=='Expanded Access',0,trial$number_of_arms)
### NOT DONE FOR THE MOMENT: maybe later


trial <- sqldf("select * from trial where stdat is not null and number_of_arms is not null and nb_rg is not null and nb_ae is not null")

trial$type <- NULL
trial$has_dmc <- ifelse(trial$has_dmc=='t',1,0)
trial$nb_rg <- as.numeric(trial$nb_rg)
trial$nb_ae <- as.numeric(trial$nb_ae)


save(test,file="data.Rda")

#setwd('D:/4. learning/05. STAT/04. DS-Training/0. Project/database/20170811_pipe-delimited-export')
#load("data.Rda")

##
## Stat Descriptives
##

## Mettre ? jour la phase: early, no phase, 1, 2, 3 et 4

counts <- as.data.frame(table(trial$stdat))
names(counts)[1] <- "year"
names(counts)[2] <- "Nb_trial"

p<-ggplot(data=counts, aes(x=year, y=Nb_trial)) + geom_bar(stat="identity") + theme(panel.border = element_blank(),
                                                                                    panel.grid.major = element_blank(),
                                                                                    panel.grid.minor = element_blank(),
                                                                                    axis.text.x = element_text(angle=45))

      
p
#barplot(counts, main="Trial", xlab="Date", legend = rownames(counts), beside=TRUE)

suppressWarnings(library(ggplot2))
# Barplot basique



counts <- as.data.frame(table(trial$phase, trial$stdat))
names(counts)[1] <- "year"
names(counts)[2] <- "Nb_trial"
barplot(counts, main="Trial", xlab="Date", legend = rownames(counts), beside=TRUE)


barplot(table(trial$stdat, trial$phase), main="Trial", xlab="Date", legend = rownames(counts), beside=TRUE)



############################################
library(caret)
library(ROCR)
library(elasticnet)

train  <- subset(trial, outcome_n!=2)
valid  <- subset(trial, outcome_n==2)


part <-createDataPartition(y=train$outcome_n,p = .8,list = F)

training	<- train[part,] # 80% training set
test	    <- train[-part,] # 20% test set

trainingy <- as.factor(training[,"outcome_n"])
trainingx <- training[,-which(names(training) %in% c("nct_id","outcome_n", "outcome"))]

testy <- as.numeric(test[,"outcome_n"])
testx <- test[,-which(names(test) %in% c("nct_id","outcome_n", "outcome"))]

control   <- trainControl(method="repeatedcv", number=2, repeats=2)

##GLM
model_glm <- train(trainingx, trainingy, method = "glm", family=binomial(), trControl = control, preProc  = c("center","scale"))
model_glm
p <- predict(model_glm, testx)
pred <- prediction(as.numeric(p),as.numeric(test$outcome_n))
perf<-performance(pred,'tpr','fpr')
c<-confusionMatrix(p,as.factor(test$outcome_n))
c

##GLM AIC
model_aic <- train(trainingx, trainingy, method = "glmStepAIC", family=binomial(), trControl = control, preProc  = c("center","scale"))
model_aic
p <- predict(model_aic, testx)
pred <- prediction(as.numeric(p),as.numeric(test$outcome_n))
perf<-performance(pred,'tpr','fpr')
c<-confusionMatrix(p,as.factor(test$outcome_n))
c

#Neural Networks
nnetGrid <- expand.grid(.decay = seq(0, 0.1, .01), .size = c(3:10), .bag=FALSE)
model_nn <- train(trainingx, trainingy, method = "avNNet",repeats = 10, tuneGrid = nnetGrid, trControl = control,
                   preProc = c("center", "scale"),
                   linout = TRUE,
                   trace = FALSE,
                   MaxNWts = 10 * (ncol(trainningx) + 1) + 10 + 1,
                   maxit = 500)
model_nn
p <- predict(model_nn, testx)
pred <- prediction(as.numeric(p),as.numeric(test$outcome_n))
perf<-performance(pred,'tpr','fpr')
c<-confusionMatrix(p,as.factor(test$outcome_n))
c

#SVM
model_svm <- train(trainingx, trainingy, method = "svmPoly", preProc = c("center", "scale"), tuneLength = 14, trControl = control)

model_svm
p <- predict(model_svm, testx)
pred <- prediction(as.numeric(p),as.numeric(test$outcome_n))
perf<-performance(pred,'tpr','fpr')
c<-confusionMatrix(p,as.factor(test$outcome_n))
c

#Random Forest
model_rf <- train(trainingx,trainingy, method = "rf", ntrees = 10,importance = TRUE, preProc = c("center", "scale","medianImpute"),trControl = control)

model_rf
p <- predict(model_rf, testx)
pred <- prediction(as.numeric(p),as.numeric(test$outcome_n))
perf<-performance(pred,'tpr','fpr')
c<-confusionMatrix(p,as.factor(test$outcome_n))
c

