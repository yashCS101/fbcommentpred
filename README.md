# fbcommentpred
Capstone Projects(1st upload)
setwd("C:/Users/USER/Desktop/R Data Sets/4. Facebook Comment Volume Prediction")
fb_rawdata = readxl::read_xlsx("Training.xlsx")
summary(fb_rawdata)
str(fb_rawdata)
fb_rawdata$`Post Promotion Status`=as.factor(fb_rawdata$`Post Promotion Status`)
### Removing Feature 5 - Feature29 and id
  mydata = readxl::read_xlsx("mydatafb.xlsx")
  mydata=data.frame(mydata)
  attach(mydata)
  str(mydata)
  
  
########################################### Removing NAs
  sum(is.na(mydata))
  mydata = na.omit(mydata)

########################################## Univariate Analysis
str(mydata)
  
  ###Converting varialbes with binary value into categorical
  mydata$Post.Promotion.Status = as.factor(mydata$Post.Promotion.Status)
  mydata$Post.published.weekday = as.factor(mydata$Post.published.weekday)
  mydata$Base.DateTime.weekday = as.factor(mydata$Base.DateTime.weekday)
  str(mydata)
  mydata = mydata[,-13] ###Removing Post Promotion status as all the values are 0
  
  ###Boxplots
  boxplot(mydata[,-c(15,16,17)]) ### Evident outliesr in Page likes variable.
  
  ########################################## Outlier treatment
  
  numeric_cols = c(1,2,3,4,5,6,7,8,9,10,11,12,14)
  for (i in numeric_cols){
    x = mydata[,i]
    qnt = quantile(x, probs=c(.25, .75), na.rm = T)
    caps = quantile(x, probs=c(.05, .95), na.rm = T)
    H = 1.5 * IQR(x, na.rm = T)
    x[x < (qnt[1] - H)] = caps[1]
    x[x > (qnt[2] + H)] = caps[2]
    mydata[,i] = x
  }
  
  boxplot(Page.likes)
  boxplot(mydata[,-c(15,16,17)])

  ###Normal Dist. Check
  library(psych) 
  hist(`Page likes`)
  multi.hist(mydata[,-c(13,15,16,17)],density = FALSE,labels=TRUE,col = c("blue","light blue"), cex.main = 2 )
  hist(`Target Variable`)
  
  ###Amount of 0 comments
  sum(`Target Variable`=="0")/nrow(mydata) 
  
#############################################Bi Variate Analysis
  barplot(table(mydata$Page.likes,mydata$Page.Checkins),
          main = "Page likes vs Page Checkins")
  
  plot(mydata$Page.likes,mydata$Page.Checkins, main = "Page likes vs Page Checkins")
  
  barplot(table(mydata$Page.likes,mydata$Post.published.weekday),
          main = "Page likes vs Post Published Weekday")
  
  barplot(table(mydata$Page.likes,mydata$Base.DateTime.weekday),
          main = "Page likes vs Base Date weekday")
  
  plot(mydata$CC1,mydata$CC2, main = "CC1 vs CC2")
  
  plot(mydata$Page.Checkins,mydata$Page.Category, main = "Category vs Checkins")
  
  plot(mydata$Post.Length,mydata$Page.likes,main = "Length vs Likes")
  
  plot(mydata$Post.Length,mydata$Target.Variable, main = "Lenght vs Target")
  
  plot(mydata$Page.likes, mydata$Target.Variable, main = "Likes vs Target")
  
  ###Corplot
  library(corrplot)
  cor = cor(mydata[,-c(13,15,16,17)])
  corrplot(cor(mydata[,-c(13,14,15,16,17)]), method = "number", type = "lower")               
  
  ###Creating new variables for published and base date
  mydata$ppmonday = as.factor(ifelse(mydata$Post.published.weekday == "Monday",1,0))
  mydata$pptuesday = as.factor(ifelse(mydata$Post.published.weekday == "Tuesday",1,0))
  mydata$ppwednesday = as.factor(ifelse(mydata$Post.published.weekday == "Wednesday",1,0))
  mydata$ppthursday = as.factor(ifelse(mydata$Post.published.weekday == "Thursday",1,0))
  mydata$ppfriday = as.factor(ifelse(mydata$Post.published.weekday == "Friday",1,0))
  mydata$ppsat = as.factor(ifelse(mydata$Post.published.weekday == "Saturday",1,0))
  mydata$ppsun = as.factor(ifelse(mydata$Post.published.weekday == "Sunday",1,0))
  
  mydata$bdmon = as.factor(ifelse(mydata$Base.DateTime.weekday == "Monday",1,0))
  mydata$bdtue = as.factor(ifelse(mydata$Base.DateTime.weekday == "Tuesday",1,0))
  mydata$bdwed = as.factor(ifelse(mydata$Base.DateTime.weekday == "Wednesday",1,0))
  mydata$bdthu = as.factor(ifelse(mydata$Base.DateTime.weekday == "Thursday",1,0))
  mydata$bdfri = as.factor(ifelse(mydata$Base.DateTime.weekday == "Friday",1,0))
  mydata$bdsat = as.factor(ifelse(mydata$Base.DateTime.weekday == "Saturday",1,0))
  mydata$bdsun = as.factor(ifelse(mydata$Base.DateTime.weekday == "Sunday",1,0))
  
  ###Removing the orignal published and base data variables
  mydata = mydata[,-c(14,15)]
  library(caTools)
  set.seed(2004)
  split<-sample.split(mydata$Target.Variable,SplitRatio = 0.7)
  traindata = subset(mydata, split == T)
  testdata = subset(mydata, split == F)
  
  
  ###High co-relation beetween CC1 and CC4, removing CC4
  traindata = traindata[,-8]
  head(mydata)  
  str(traindata)
  
### Multi Liner Regression
multimodel = lm(Target.Variable~.,traindata ) ### Very Low R-squared value (0.17)
summary(multimodel)
vif(multimodel)

  ### Removing factors with low importance ### R-squared (0.27)
  multimodel2 = lm(Target.Variable~Page.talking.about+Page.Checkins+CC1+CC3+CC5+Base.Time+Post.Share.Count)
  summary(multimodel2)
  
  ### Checking for VIF values
  library(car)
  vif(multimodel2)
  
  ### Predicting on Test
  predmultimodel = predict(multimodel2,testdata)
  RMSEmulti = sqrt(mean((predmultimodel-testdata$Target.Variable)^2))
  RMSEmulti # 18.5469
  MAEmulti = mean(abs(predmultimodel-testdata$Target.Variable))
  MAEmulti # 7.585504
  
### CART
  library(rpart)
  library(rpart.plot)
  ### All Variables
  tree = rpart(formula = Target.Variable~., data = traindata)
  summary(tree)
  plotcp(tree)
  ptree = prune(tree,cp=0.025, "CP") ### Minimum cross validation error
  rpart.plot(ptree)
  rsq.rpart(ptree)
  
  ### Predicting on Test
  predtree = predict(ptree,testdata)
  RMSEtree = sqrt(mean((predtree-testdata$Target.Variable)^2))
  RMSEtree # 16.5203
  MAEtree = mean(abs(predtree-testdata$Target.Variable))
  MAEtree # 5.6177
  
  ### Selecting variables as per multiregression
  tree1 = rpart(formula = Target.Variable~Page.talking.about+CC1+CC5+Base.Time+Post.Share.Count, data = traindata)
  summary(tree1)
  plotcp(tree1)
  ptree1 = prune(tree1,cp=0.019, "CP") ### Minimum cross validation error
  rpart.plot(ptree1)
  rsq.rpart(ptree1)
  
  ### Predicting on Test
  predtree1 = predict(ptree1,testdata)
  RMSEtree1 = sqrt(mean((predtree1-testdata$Target.Variable)^2))
  RMSEtree1 # 16.7712
  MAEtree1 = mean(abs(predtree1-testdata$Target.Variable))
  MAEtree1 # 5.8308
  
### Random Forest
library(randomForest)
rf = randomForest(Target.Variable~., data = traindata, ntree = 501, importance = TRUE)
print(rf)
which.min(rf$mse) ### 183

prf0 = randomForest(Target.Variable~., data = traindata, ntree = 183, importance = TRUE)
prf0
### Predicting on Test
predrf0 = predict(prf0,testdata)
RMSErf0 = sqrt(mean((predrf0-testdata$Target.Variable)^2))
RMSErf0 # 15.8034
MAErf0 = mean(abs(predrf0-testdata$Target.Variable))
MAErf0 # 4.0914

###Adding mtry and nodesize
prf = randomForest(Target.Variable~., data = traindata, ntree = 183, mtry = 3, nodesize =10, importance = TRUE)
prf
### Predicting on Test
predrf = predict(prf,testdata)
RMSErf = sqrt(mean((predrf-testdata$Target.Variable)^2))
RMSErf #13.721
MAErf = mean(abs(predrf-testdata$Target.Variable))
MAErf #4.37

### Feature Importance
importance(prf)
varImpPlot(prf)

### Plotting Predicted vs actual
library(ggplot2)
ploty = data.frame(cbind(predrf,testdata$Target.Variable))
ggplot(aes(predrf,testdata$Target.Variable))
plot(testdata$Target.Variable,col="Red")
lines(testdata$Target.Variable,col="Red")
plot(predrf,col="Blue")
lines(predrf,col="Blue")
lines(testdata$Target.Variable,col="Red")



### Experimenting with limiting variables
prf1 = randomForest(Target.Variable~Page.talking.about+CC1+CC5+Base.Time+Post.Share.Count, data = traindata, ntree = 183, mtry = 3, nodesize =10, importance = TRUE)
prf1
### Predicting on Test
predrf1 = predict(prf1,testdata)
RMSErf1 = sqrt(mean((predrf1-testdata$Target.Variable)^2))
RMSErf1 # 16.1181
MAErf1 = mean(abs(predrf1-testdata$Target.Variable))
MAErf1 # 4.2959
