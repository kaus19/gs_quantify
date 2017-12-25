setwd("H:/GS")
install.packages('randomForest')
library(randomForest)
train=read.csv("H:/GS/gcTrianingSet.csv")
test=read.csv("H:/GS/gcPredictionFile.csv")
View(train)
summary(train)
train$a = train$initialUsedMemory+train$initialFreeMemory 
#total memory(initial used memory + initial free memory) before running a particular query) 
train$b = train$finalUsedMemory+train$finalFreeMemory  
#total memory(final used memory + final free memory) after running a particular query 
summary(train$a)
df <-subset(train,train$gcRun=="TRUE",drop=TRUE) 
#segregating that part of the dataset that has gcRun==TRUE 
View(df)

mean(train$initialUsedMemory)
mean(df$initialUsedMemory)

df$c=((df$finalUsedMemory-df$initialUsedMemory)+(df$gcInitialMemory-df$gcFinalMemory) ) 
#a new variable that defines the amount of memory each query takes for those queries which gives TRUE for garbage collector 
summary(df$c)

df1 <-subset(train,train$gcRun=="FALSE",drop=TRUE)
# A subset of those queries which give FALSE for garbage collector 
View(df1)
df1$c=(-df1$initialUsedMemory+df1$finalUsedMemory) 
#a new variable that defines the amount of memory each query takes for those queries which gives FALSE for garbage collector
summary(df1$c)

for(j in 1:2730)
{
  if(train$gcRun[j]=="TRUE")
    train$c[j]=(train$finalUsedMemory[j]-train$initialUsedMemory[j]+train$gcInitialMemory[j]-train$gcFinalMemory[j] ) 
  else
    train$c[j]=(train$finalUsedMemory[j]-train$initialUsedMemory[j] )
}
# a new variable that defines the amount of memory each query takes for all the training cases 
summary(train$c)

summary(df$initialUsedMemory+df$c)
thres=min(df$initialUsedMemory+df$c)
# Threshold for classification of garbage collector ( thres is the minimum of the sum of initially used memory and the memory of each query )
View(test)
for(j in 1:1625)
{
  tok <- subset(train,train$querytoken==test$querytoken[j],drop=TRUE)
  test$c[j]=tok$c[1]
  # In the subset of token( for each iteration tok contains only 1 query value and is used to determine best optimal value of query memory for a particular query ), only the initial values of each token is taken as best optimal query memory (since the tokens are initially used sequentially in the beggining, there would be less errors  as we move down the dataset our unexplained error increases)  
}

write.csv(df,"df.csv")
df2=read.csv("H:/GS/df.csv")
model1 <- randomForest(finalUsedMemory~cpuTimeTaken+c+initialUsedMemory,data=df,nodesize=18,ntree=55)
#random forest model applied for regression and final used memory as dependent variable with the  suitable parameters and independent variables taken
for(j in 1:1624)
{
  if((test1$initialUsedMemory[j]+test1$c[j])>thres)
  {test1$gcRun[j]="TRUE"
  #test1$initialUsedMemory[j+1]=predict(model1,newdata=data.matrix(test1[j,1:3]))
  a=4.247186+2.774299
  #test1$initialFreeMemory[j]= (a-test1$initialUsedMemory[j])
  # test1$initialFreeMemory[j+1]=predict(model1,newdata=data.frame(test1[j,]))
  test1$initialUsedMemory[j+1]=predict(model1,newdata=data.frame(test1[j,]))
  #test1$initialUsedMemory[j+1]= (a-test1$initialFreeMemory[j+1])                                                                                  
  }
  else
  {
    test1$initialUsedMemory[j+1]=test1$initialUsedMemory[j]+test1$c[j]
    test1$gcRun[j]="FALSE"
  }
}
#whole for loop consist of sequentially transversing through testing data set and filling initialUsedMemory  and gcRun using random forest model as regressor for initialUsedMemory and thres value as classifier 
table(test1$gcRun)
#to measure precision and recall of our results
a=4.247186+2.774299
#assumption that our total memory of the container is initial Used value o+ intial Free Value of the testing dataset before any query was run
test1$initialFreeMemory= (a-test1$initialUsedMemory)
#determining Initial free memory before each query was run.

write.csv(test1,"ans.csv")

