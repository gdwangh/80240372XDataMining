library(MASS)
data(crabs)

seed(12345)

library(nnet)
nnetfit <- nnet(sex~sp+FL+RW+CL+CW+BD, data=crabs, size=10)
nnetfit$residuals


nnetfit2<-train(sex~sp+FL+RW+CL+CW+BD, data=crabs, method="nnet", tuneLength = 10)
nnetfit2$bestTune
nnetfit2$results

data <- crabs
data$spclass<-0
data[data$sp=='B', 'spclass']<-1
data$sexflass<-0
data[data$sexflass=='M', 'spclass']<-1

x_in <-as.matrix(data[,c(4:9)])
y_out<-as.matrix(data[,c(10)])


n_hidden<-3
delta0<-0.001
stepSize<-0.05
maxIter<-100000
nnetfit3<-trainAnn3(x_in, y_out, n_hidden, delta0, stepSize, maxIter) 
pw1<-nnetfit3[[4]] 
pw2<-nnetfit3[[5]]
x1<-cbind(rep(1,nrow(data)),x_in)

y1<-calANN3Out(x1, pw1,pw2)[[1]]
0.5* rowSums((y_out-y1)^2)

