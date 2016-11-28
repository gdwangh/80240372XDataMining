setwd('D:/study/80240372XDataMining/ex/week3 decisiontrees')
setwd('D:/doc/study/80240372XDataMining/ex/week3 decisiontrees')

library(rpart)
library(datasets)
library(rpart.plot)
library(caTools)

# Train Classification Tree
ionosphere<-read.csv("ionosphere.data",head=FALSE)
names(ionosphere)[35] <- "class"
names(ionosphere)

# data(ionosphere)
nrow(ionosphere)

set.seed(1)

spl = sample.split(ionosphere$class, SplitRatio = 0.8)
ionosphereTrain = subset(ionosphere, spl==TRUE)
ionosphereTest = subset(ionosphere, spl==FALSE)

# Baseline
table(ionosphereTrain$class)  # b:101, g=180, È¡g
table(ionosphereTest$class)
base_err<-45/(25+45)  # 0.6428571

# 
ionoTree <- rpart(class~., data=ionosphereTrain,method = "class")
prp(ionoTree)

ionoTestPred<-predict(ionoTree, newdata=ionosphereTest, type = "class")
table(ionosphereTest$class, ionoTestPred)
tree1_err<-(19+40)/nrow(ionosphereTest)
tree1_err   # 0.8428571

# ¼ôÖ¦
ionoTree2<-prune(ionoTree, cp=0.1)
prp(ionoTree2)

ionoTest2_Pred<-predict(ionoTree2, newdata=ionosphereTest, type = "class")
table(ionosphereTest$class, ionoTest2_Pred)
tree2_err<-(22+39)/nrow(ionosphereTest)
tree2_err   # 0.8714286

#Specify to grow each tree using a minimum leaf size in leafs.
err1<-rep(0,15)

for (minleaf in c(1:15) ) {
  ct<-rpart.control(minbucket=minleaf)
  fit<-rpart(class~., data=ionosphereTrain, control=ct, method="class")
  
  pred<-predict(fit, newdata=ionosphereTest, type = "class")
  err1[minleaf]<-sum(pred==ionosphereTest$class)/nrow(ionosphereTest)
  cat(minleaf, ", err1=", err1[minleaf], "\n")
}



# Train Regression Tree
data(mtcars)
carsTree<-rpart(mpg~hp+wt, data=mtcars, method="anova")
prp(carsTree)

mse<-sqrt(sum(residuals(carsTree)^2)/nrow(mtcars))
mse



