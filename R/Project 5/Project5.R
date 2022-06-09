#project 5)--------------------------------------------------------------------------------------------------------------------------------

#Logistic regression------------------------------------------------------------

setwd('D:/EDVANCER/R/Project 5')

library(data.table)

bf_test=data.frame(fread("bank_full_test.csv"))

bf_train=data.frame(fread("bank_full_train.csv"))

View(bf_train)

View(bf_test)

bf_train$data='train'    #add new col for identifying train data

bf_test$data='test'      #add new col for identifying test data

bf_test$y=NA


bf_all=rbind(bf_train,bf_test)   #combining both train and test in single dataset

View(bf_all)


glimpse(bf_all)

sapply(bf_all,function(x) sum(is.na(x)))     #apart from y,no NA values


bf_all=bf_all%>%
        select(-ID)


glimpse(bf_all)


table(bf_all$pdays)

table(bf_all$previous)


#we create flag variable for these 2 and delete original columns

bf_all=bf_all%>%
     mutate(pdays_1=ifelse(pdays==-1,1,0))

bf_all=bf_all%>%
        mutate(previous_0=ifelse(previous==0,1,0))

bf_all=bf_all%>%
        select(-pdays,-previous)


glimpse(bf_all)


cat_cols=c('job','marital','education','default','housing','loan','contact','month','poutcome')  #store all cat cols in single variable


CreateDummies=function(data,var,freq_cutoff=0){     #create dummies func
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep='_')
    name=gsub(" ",'',name)
    name=gsub('-','_',name)
    name=gsub('\\?','Q',name)
    name=gsub('<','LT_',name)
    name=gsub('\\+','',name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


#checking all cat cols for diff categories

table(bf_all$job)
table(bf_all$marital)
table(bf_all$education)
table(bf_all$default)
table(bf_all$housing)
table(bf_all$loan)
table(bf_all$contact)
table(bf_all$month)
table(bf_all$poutcome)


cat_cols


for(cat in cat_cols){
  
  bf_all=CreateDummies(bf_all,cat,100)
}


bf_all$y=as.numeric(bf_all$y=='yes')   #converting y into 0 and 1

glimpse(bf_all)


#as data prep is completed,we seperate all into train and test using our data column

bf_train=bf_all%>%
         filter(data=='train')%>%
         select(-data)

bf_test=bf_all%>%
  filter(data=='test')%>%
  select(-data)


#solving classification problem using logistic regression

s=sample(nrow(bf_train),0.7*nrow(bf_train))   #sampling our train data in 70-30 ratio
bf_train1=bf_train[s,]
bf_train2=bf_train[-s,]


#now time for model building on train1 data

lfit=lm(y~.-previous_0,data = bf_train1)      

summary(lfit)

sort(vif(lfit),decreasing = T)


lfit=lm(y~.-previous_0-poutcome_unknown,data = bf_train1)      

sort(vif(lfit),decreasing = T)

lfit=lm(y~.-previous_0-poutcome_unknown-month_may,data = bf_train1)      

sort(vif(lfit),decreasing = T)


lfit=lm(y~.-previous_0-poutcome_unknown-month_may-job_blue_collar,data = bf_train1)   #now vif <10   

sort(vif(lfit),decreasing = T)


lfit=glm(y~.-previous_0-poutcome_unknown-month_may-job_blue_collar,data = bf_train1,family='binomial')  #running logistic regression


log_lfit=step(lfit)   #it helps drop vars based on AIC score


formula(log_lfit)   #in logistic regression,we don't drop cols based on p-values


train1.log.model=glm(y ~ balance + day + duration + campaign + pdays_1 + job_student + 
                       job_housemaid + job_retired + job_admin. + job_management + 
                       marital_married + education_primary + housing_yes + loan_no + 
                       contact_unknown + contact_cellular + month_mar + month_sep + 
                       month_oct + month_jan + month_apr + month_nov + month_jun + 
                       month_aug + month_jul + poutcome_other + poutcome_failure,data = bf_train1,family = 'binomial')   #build final model on train1


library(pROC)

train2.prob.scores=predict(train1.log.model,newdata=bf_train2,type = 'response')   #predict values for train2

roc(bf_train2$y,train2.prob.scores)$auc      #auc:0.9064

train2.prob.scores


train.log.model=glm(y ~ balance + day + duration + campaign + pdays_1 + job_student + 
                      job_housemaid + job_retired + job_admin. + job_management + 
                      marital_married + education_primary + housing_yes + loan_no + 
                      contact_unknown + contact_cellular + month_mar + month_sep + 
                      month_oct + month_jan + month_apr + month_nov + month_jun + 
                      month_aug + month_jul + poutcome_other + poutcome_failure,data = bf_train,family = 'binomial')   #build final model on train

test.prob.scores=predict(train.log.model,newdata=bf_test,type = 'response')   #predict values for test

test.prob.scores


#now for hardclasses

train.prob.scores=predict(train.log.model,newdata=bf_train,type = 'response')   #prob scores on train data


real=bf_train$y

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,KS=99)


#for loop for various formulae calculations

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.prob.scores>cutoff)
  
  TP=sum(real==1 & predicted==1)
  
  TN=sum(real==0 & predicted==0)
  
  FP=sum(real==0 & predicted==1)
  
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  
  N=FP+TN
  
  KS=(TP/P)-(FP/N)
  
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
  
}

View(cutoff_data)

cutoff_data=cutoff_data[-1,]


#get max value of KS for cutoff calc

final_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]   #max KS-0.6729


#we compare our test scores along with cutoff and assign 1 and 0 for hardclasses

test.hardclass.scores=as.numeric(test.prob.scores>final_cutoff)




#RF-----------------------------------------------------------------------------

bf_train$y=as.factor(bf_train$y)

glimpse(bf_train)

param=list(mtry=c(5,10,20),              #RF parameters
           ntree=c(100,200,500),
           maxnodes=c(5,10,20),
           nodesize=c(1,2,5))


para_subset=function(param_list,n=10){           #subse of total combs
  
  exp_grid=expand.grid(param_list)
  
  s=sample(nrow(exp_grid),n)
  
  param_sub=exp_grid[s,]
  
  return(param_sub)
}


num_trials=10
subset_para=para_subset(param,num_trials)

subset_para


auc_func=function(y,yhat){             #func to calc auc score
  
  roccurve=pROC::roc(y,yhat)
  
  aucscore=pROC::auc(roccurve)
  
  return(aucscore)
}


myauc=0

for(i in 1:num_trials){
  
  print(paste0('Starting iteration',i))
  
  current_para=subset_para[i,]
  
  k=cvTuning(randomForest,
             y~.,
             data=bf_train,
             folds=cvFolds(nrow(bf_train),K=5,type = 'random'),
             tuning=current_para,
             seed=2,
             cost = auc_func,
             predictArgs = list(type='prob')
             )
  
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    
    print(current_para)
    
    myauc=score.this
    
    print(score.this)
    
    best_para=current_para
  }
  print('DONE')
}


myauc            #auc score after running above code

best_para        #best params

best_para=as.data.frame(best_para)

train.rf.model=randomForest(y~.,          #running randomforest on whole train data
                            data=bf_train,
                            mtry=best_para$mtry,
                            ntree=best_para$ntree,
                            maxnodes=best_para$maxnodes,
                            nodesize=best_para$nodesize)

test.prob.scores=predict(train.rf.model,newdata = bf_test,type='prob')[,2]     #predict on test data and only give prob of 1 


#prob scores are calc on test data,now for hardclasses

train.prob.scores=predict(train.rf.model,newdata = bf_train,type='prob')[,2]


real=bf_train$y

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,KS=99)


#for loop for various formulae calculations

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.prob.scores>cutoff)
  
  TP=sum(real==1 & predicted==1)
  
  TN=sum(real==0 & predicted==0)
  
  FP=sum(real==0 & predicted==1)
  
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  
  N=FP+TN
  
  KS=(TP/P)-(FP/N)
  
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
  
}

View(cutoff_data)

cutoff_data=cutoff_data[-1,]


final_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]    #max KS score-0.6853

final_cutoff


test.hardclass.scores=as.numeric(test.prob.scores>final_cutoff)

table(test.hardclass.scores)

getwd()

test.hardclassyon.scores=ifelse(test.hardclass.scores==1,'yes','no')

table(test.hardclassyon.scores)

write.table(test.hardclassyon.scores,file='Sayen_Das_P5_part2.csv',col.names = 'y',row.names = F)