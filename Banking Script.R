xtrain = read.csv("bank-full_train.csv",stringsAsFactors = F)
xtest = read.csv("bank-full_test.csv",stringsAsFactors = F)
library(dplyr)
xtest$y = NA
xtrain$data = "train"
xtest$data= "test"
x = rbind(xtrain, xtest)
sum(x$data=="test")

outliers = function(v){
  q1 = quantile(x$age,0.25)
  q3 = quantile(x$age,0.75)
  iq_r = IQR(v)
  upper = q3 + 1.5*iq_r
  lower = q1 - 1.5*iq_r
  res = which((v<lower) | (v>upper)) #which gives the index where true is there, this can remove the outlier rows from the data frame
  return(res)
}
x = x %>%
  mutate(y = ifelse(y=="yes",1,0))
glimpse(x)

class(x$age)
range(x$age)
library(ggplot2)
ggplot(x, aes(x=age))+geom_density()

CreateDummies=function(data,var,freq_cutoff=100){
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
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
for_dummy_vars=c('marital','education','housing','loan','contact','month')

x = CreateDummies(x,"job")
x = CreateDummies(x,"marital")
x = CreateDummies(x,"education")
x = CreateDummies(x,"housing")
x = CreateDummies(x,"loan")
x = CreateDummies(x,"contact")
x = CreateDummies(x,"month")

x = x %>% select(-ID)
x = x %>% select(-poutcome)
glimpse(x$default)
x = x %>% mutate(
  default = ifelse(default=="yes",1,0)
)


length(i) #Oho so plenty of outliers!
library(ggplot2)
p = ggplot(x, aes(x=balance)) + geom_density()
#Most of them are concentrated on the lower end of
mean(x$balance[-i])
length(i)/nrow(x) #That's 20%, they're not outliers, the spread is that much that;s all

#Split the training data into validation
xtrain2 = x[x$data=="train",]
xtest2 = x[x$data=="test",]
s = sample(1:nrow(xtrain2), 0.8*nrow(xtrain2))
trainingdata = x[s,]
validationdata = x[-s,]
forvif = lm(y~.,data=x)
