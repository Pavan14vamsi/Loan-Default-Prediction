xtrain = read.csv("bank-full_train.csv",stringsAsFactors = F)
xtest = read.csv("bank-full_test.csv",stringsAsFactors = F)
library(dplyr)
xtest$y = NA
xtrain$data = 1
xtest$data = 0
x = rbind(xtrain,xtest)
glimpse(x)

removeOutliers = function(v){
  q1 = quantile(x$age,0.25)
  q3 = quantile(x$age,0.75)
  iq_r = IQR(v)
  upper = q3 + 1.5*iq_r
  lower = q1 - 1.5*iq_r
  res = which((v>lower) & (v<upper)) #which gives the index where true is there, this can remove the outlier rows from the data frame
  return(res)
}

#Data Preparation stage. Check every variable properly.
#Checking for missing values in all columns
lapply(x, function(a) sum(is.na(a))) #No missing values in any field

#The target variable should be a 1/0
x = x%>%
  mutate(y = ifelse(y=="yes",1,0))
#Checking age, the range, outlier.
class(x$age)
ind = removeOutliers(x$age)
#x = x[ind,] #Removed the rows which had outlier values of age.

#Checking for job, too many categories, so group them according to response similarity
table(x$job)
sort(round(tapply(x$y, x$job, mean, na.rm=T),3)*100)
x = x%>% mutate(
  job1 = as.numeric(job %in% c("blue-collar","housemaid")),
  job2 = as.numeric(job %in% c("services","entrepreneur")),
  job3 = as.numeric(job %in% c("unknown","technician")),
  job4 = as.numeric(job %in% c("self-employed","admin")),
  job5 = as.numeric(job %in% c("management")),
  job6 = as.numeric(job == "unemployed"),
  job7 = as.numeric(job == "retired"),
  job8 = as.numeric(job == "student")
  )

x$job = NULL #dont need this anymore.

#marital status is a categorical value, very few categories, so no need to check response rate.
table(x$marital)
xbackup = x
createDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
x = createDummies(x, "marital", 100)

#Education is a categorical value
table(x$education)
x = createDummies(x,"education")

#default is a binary variable
x = x %>%
  mutate(
    default = as.numeric(default == "yes")
  )

#balance is a numeric variable
class(x$balance)
range(x$balance) #Which is fine

#housing and loans are binary
table(x$housing)
table(x$loan)
x=x %>% mutate(
  housing = as.numeric(housing == "yes"),
  loan = as.numeric(loan == "yes")
)

#contact is categorical
table(x$contact)
x = createDummies(x, "contact")

#day is numeric
class(x$day) #all ok

mons = x$month
table(x$month)
#Month of the year is categorical. But it does have relative order, so I'm trying to keep it. I'll check with order and withut it too


x3 = createDummies(x2b,"month") #x3 has converted to categories and x2 has months in numbers


#Duration should be numeric
# xb = x
# x3b = x3
#range(x2b$dur) #It is fine

#Campaign should be numeric

class(x$campaign)

#pdays is numeric
class(x$pdays)

#previous shoyld be numeric
class(x$previous)
range(x$previous)

#putcome should be categorical
table(x$poutcome)
x = createDummies(x, "poutcome")

glimpse(x)
xtrain = x[x$data==1,]
xtest = x[x$data ==0,]
s = sample(1:nrow(xtrain), 0.8*nrow(xtrain))
x_validation = xtrain[-s,]
x_train  = xtrain[s,]

for_vif = lm(y~.-ID,data=x_train)
summary(for_vif)
for_vif2 = step(for_vif)
formula(for_vif2)

log_fit = glm(y ~ balance + housing + loan + day + month + duration + campaign + 
                job1 + job2 + job7 + job8 + marital_married + education_tertiary + 
                contact_unknown + poutcome_other + poutcome_failure + poutcome_unknown, data=x_train, family = "binomial")
summary(log_fit)
library(pROC)
val.score = predict(log_fit, newdata = x_validation, type = "response" )

#So now we have probability values in val.score, now I'll use the various metrics to choose the cut-off value
cutoff = 0.1 #I checked multiple cutoffs, rerunning the code multiple times until I found that this gives the highest ks score
predicted = as.numeric(val.score > cutoff)
TP = sum(x_validation$y==1 & predicted==1)
FP = sum(x_validation$y==0 & predicted==1)
TN = sum(x_validation$y==0 & predicted==0)
FN = sum(x_validation$y==1 & predicted==0)
P = TP + FN
N = TN + FP
Sn = TP/P
Sp = TN/N
dist = sqrt((1-Sp)**2 + (1-Sn)**2)
KS = Sn - (FP/N)
KS
accuracy = (TP + TN)/(P + N)

test_pred = predict(log_fit, newdata = xtest, type = "response")
final_pred = as.numeric(test_pred>cutoff)
final_pred
temp = data.frame(y=final_pred)
write.csv(temp, "/home/pavan117/Courses/Edvancer 53k course/1. R/Projects/Banking/result.csv", row.names = FALSE)
nrow(temp)


aaa = read.csv("uploaded.csv")
  