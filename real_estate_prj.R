getwd()
library(dplyr)

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

#Removing the NA columns in council Area
#housing_all$CouncilArea_ = NULL

#nrow(housing_all)
#unique(housing_all$BuildingArea)
#summary(housing_all$BuildingArea)

#write.csv(housing_all,'housing_all.csv',row.names = F)

#housing_all$YearBuilt = housing_all$YearBuilt - 1830 #NA are there
#unique(housing_all$YearBuilt)
#summary(housing_all$YearBuilt)

#housing_all$Landsize
#summary(housing_all$Landsize)

#summary(housing_all$Car)
#round(mean(housing_all$Car_0,na.rm = T))

#Modifying the distance
summary(housing_all$Distance)
summary(housing_all$YearBuilt)
housing_all$Distance = 15 - housing_all$Distance
housing_all$Distance  = scale(housing_all$Distance)
housing_all$YearBuilt = scale(housing_all$YearBuilt)


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
set.seed(2)
s=sample(1:nrow(housing_train),0.75*nrow(housing_train))
housing_train1=housing_train[s,]
housing_train2=housing_train[-s,]

#fitting the data
fit=lm(Price~.,data=housing_train1)
summary(fit)

library(car)
# we'll take vif cutoff as 5

sort(vif(fit),decreasing = T)
#fit=lm(Price~.-Bedroom2_3-Car_2-Bathroom_1-Rooms_2,data=housing_train1)
fit=lm(Price~.-Bedroom2_3-Car_1,data=housing_train1)

fit=step(fit)

summary(fit)

######applying the decision tree

#library(tree)
#library(rpart)
#ld.tree=rpart(Price~.,data=housing_train1)
#ld.tree

## Visual Format

#plot(ld.tree)
#text(ld.tree)
#val.IR=predict(ld.tree,newdata = housing_train2)
#rmse_val=((val.IR)-(housing_train2$Price))^2 %>% mean() %>% sqrt()
#################
#predicting values on train2

val.pred=predict(fit,newdata=housing_train2)
head(val.pred)

#errors
errors=housing_train2$Price-val.pred
head(errors)

errors**2 %>% mean() %>% sqrt()  ###root mean sqaure error


### model for predcition on the entire data

fit.final=lm(Price ~ .,
                 data=housing_train)
fit.final=step(fit.final)

summary(fit.final)

test.pred=predict(fit.final,newdata=housing_test)

write.csv(test.pred,'housing_price_11.csv',row.names = F)

plot(fit.final,1)
# residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

output=summary(fit.final)
names(output)

output$coefficients[,4]


Score =212467/413741.6
Score

413741.6