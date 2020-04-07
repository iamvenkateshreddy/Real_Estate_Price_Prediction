library(dplyr)
h_test = read.csv('housing_test.csv',stringsAsFactors = F)
h_train = read.csv('housing_train.csv',stringsAsFactors = F)

h_test$Price = NA

h_all = rbind(h_test,h_train)

write.csv(h_all,'h_all.csv',row.names = F)

glimpse(h_all)

var(h_all$Price,na.rm = T)

mean(h_all$Price,na.rm = T)
?var

length(h_all$YearBuilt==NA)

table(h_all$YearBuilt)

var(1:10)
mean(1:10)

var(h_all$Price,na.rm = T)

length(h_all$YearBuilt==1960)

sum = h_all %>%
  group_by(YearBuilt) %>%
  summarise(y_sum = n())
View(sum)


h_all %>%
  group_by(Type) %>%
  summarise(av_p = mean(Price,na.rm = T))

1294320 - 901936

mean(1:3)
  
h_all %>%
  group_by(SellerG) %>%
  summarise(sum = sum(Price,na.rm = T)) %>%
  arrange(-sum)

 
 table(sum)
 
 
 h_all %>%
   group_by(CouncilArea) %>%
   summarise(avg = mean(Price,na.rm = T)) %>%
   arrange(-avg)
 
 
 h_all %>%
   group_by(CouncilArea) %>%
   summarise(var = var(Price,na.rm = T)) %>%
   arrange(-var)


 
 getwd()
length(unique(h_all$Postcode)) 

length(h_all$Distance)


hist(h_all$Distance,probability=T, 
     main="Histogram of normal data",
     xlab="Approximately normally distributed data") 
lines(density(h_all$Distance),col=8)

qqnorm(h_all$Distance,main="QQ plot of normal data",pch=19) 
qqline(h_all$Distance)


library(ggplot2)
df=data.frame(res=h_all$Distance)
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm ,args = list(mean=mean(df$res),sd=sd(df$res)),color="green")

#To check the noraml distribution of small sample
#shapiro.test(h_all$Distance)
#install.packages('ggpubr')
library("ggpubr")
ggdensity(h_all$Distance,
          main = "Density plot of Distance length",
          xlab = "Tooth length")


length(unique(h_all$Postcode))



