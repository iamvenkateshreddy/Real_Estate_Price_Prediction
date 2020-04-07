getwd()
library(dplyr)
library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

#importing the test and train csv files
housing_train = read.csv('housing_train.csv',stringsAsFactors = F)
housing_test = read.csv('housing_test.csv',stringsAsFactors = F)

View(housing_train)
View(housing_test)

#creating data variable for test and train data
housing_test$data = 'test'
housing_train$data = 'train'

#creating Price column for test data
housing_test$Price = NA


#combining the test and train data
housing_all = rbind(housing_train,housing_test)

#Feature Scaling
#housing_all[,13:14] = scale(housing_all[,13:14])


summary(housing_all$Price)
summary(housing_train$Price)
#nrow(housing_test)

View(housing_all)
glimpse(housing_all)

#######################
for(col in names(housing_all)){
  
  ##replacing the NA with mean
  if(sum(is.na(housing_all[,col]))>0 & (col %in% c("Landsize","BuildingArea","YearBuilt","Bedroom2","Car","Bathroom"))){
    
    housing_all[is.na(housing_all[,col]),col]=round(mean(housing_all[,col],na.rm=T))
  }
  
}
############################
glimpse(housing_all)
#removig unwanted variable
#housing_all$Method = NULL
housing_all$Postcode = NULL
housing_all$Address = NULL
housing_all$CouncilArea = NULL
#housing_all$BuildingArea = NULL
#housing_all$Bathroom = NULL

table(housing_all$Suburb)
table(housing_all$Type)
table(housing_all$Rooms) #can remove number of rooms more than 5
table(housing_all$SellerG)
table(housing_all$Distance)
table(housing_all$Bedroom2) #can consider 1,2,3,4,5
table(housing_all$Bathroom) #can consider 1,2,3
table(housing_all$Car) #can consider 0,1,2,3,4,5
table(housing_all$Landsize) # 1985 NA  mean = 454.2212 median=337
table(housing_all$BuildingArea) #5269 NA are there mean=144.6354 median = 121 
table(housing_all$YearBuilt) #4660 NA are there mean=1961.238 median 1965
table(housing_all$CouncilArea)
table(housing_all$Method)



#checking for NA
apply(housing_all, 2, function(x) any(is.na(x)))

###lable encoding
#housing_all = housing_all %>% 
#  mutate(Type_h=as.numeric(Type=="h"),
#        Type_u=as.numeric(Type=="u")) %>%    
#  select(-Type)

#Crearing Dummies 
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

#housing_all = CreateDummies(housing_all,"Type",1000)
#housing_all = CreateDummies(housing_all,"Rooms",500)
#housing_all = CreateDummies(housing_all,"Bedroom2",400) #NA are there
#housing_all = CreateDummies(housing_all,"Bathroom",400) #NA are there
#housing_all = CreateDummies(housing_all,"Car",200) #NA are there
housing_all = CreateDummies(housing_all,'Suburb',100)
housing_all = CreateDummies(housing_all,"SellerG",50)
#housing_all = CreateDummies(housing_all,"CouncilArea",100)
housing_all = CreateDummies(housing_all,"Method",100)
housing_all = CreateDummies(housing_all,"Type",100)


summary(housing_all$Distance)
summary(housing_all$YearBuilt)
housing_all$Distance = 15 - housing_all$Distance
#housing_all$Distance  = scale(housing_all$Distance)
#housing_all$YearBuilt = scale(housing_all$YearBuilt)


glimpse(housing_all)


lapply(housing_all,function(x) sum(is.na(x)))

#table(ld_all$ID)

##removing the NA ids 
#housing_all=housing_all[!(is.na(housing_all$Price)),]

for(col in names(housing_all)){
  
  ##replacing the NA with mean
  if(sum(is.na(housing_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    housing_all[is.na(housing_all[,col]),col]=round(mean(housing_all[,col],na.rm=T))
  }
  
}

#Splitting the data
housing_train=housing_all %>% filter(data=='train') %>% select(-data)
housing_test=housing_all %>% filter(data=='test') %>% select(-data,-Price)

names(housing_train)
names(housing_test)


#taking the sample
set.seed(1)
s=sample(1:nrow(housing_train),0.80*nrow(housing_train))
housing_train1=housing_train[s,]
housing_train2=housing_train[-s,]


#param=list(mtry=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70),
#          ntree=c(50,100,200,500,600,650,700,750,800,850,900,950,1000),
#         maxnodes=c(5,10,15,20,30,50,70,90,120,150,170,180,190,200,210,220,230,240,250,
 #                    260,270,280,290),
 #         nodesize=c(1,2,5,10,13,15,20,25,30,35,40,45,50))

param=list(mtry=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70),
           ntree=c(50,100,200,500,600,650,700,800,900,1000,1200,1400,1500,1700,2000,2100,2200),
           maxnodes=c(5,10,15,20,30,50,70,90,120,150,170,180,190,200,210,220,230,240,250,
                      260,270,280,290,300,320,350),
           nodesize=c(1,2,5,10,13,15,20,25,30,35,40,45,50))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=50
my_params=subset_paras(param,num_trials)

# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =housing_train,
             tuning =params,
             folds = cvFolds(nrow(housing_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

ld.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=housing_test)

test.pred=predict(ld.rf.final,newdata = housing_test)
write.csv(test.pred,"mysubmission_223.csv",row.names = F)



212467/359000 
