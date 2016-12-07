library(MASS)
data(crabs)

seed(12345)

library(nnet)
nnetfit <- nnet(sex~sp+FL+RW+CL+CW+BD, data=crabs, size=10)
nnetfit$residuals


nnetfit2<-train(sex~sp+FL+RW+CL+CW+BD, data=crabs, method="nnet", tuneLength = 10)
nnetfit2$bestTune
nnetfit2$results
