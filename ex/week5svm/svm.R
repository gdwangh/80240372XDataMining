library(e1071)
data(iris)

plot(svm)

library(lattice)
xyplot(Petal.Length ~ Petal.Width, data = iris, groups = Species,auto.key=list(corner=c(1,0)))

subdata<-iris[iris$Species != 'virginica',]
subdata$Species = factor(subdata$Species)

s1<-svm( Species~Petal.Width+Petal.Length, data=subdata)
plot(s1, subdata, Petal.Length ~ Petal.Width)

s2<-svm(Species~., data=iris)
summary(s2)

x<-iris[,-5]
y<-iris[,5]
s3<-svm(x,y)
summary(s3)
pred <- predict(s3, x)
table(y, pred)

library(plotrix)
set.seed(1)
r<-sqrt(runif(100))
t<-2*pi*runif(100)
data1<-data.frame(x=r*cos(t), y=r*sin(t))

r2<-sqrt(runif(100)*3+1)
t2<-2*pi*runif(100)
data2<-data.frame(x=r2*cos(t2), y=r2*sin(t2))

xyMax<-max(abs(data1$x),abs(data2$x),abs(data1$y),abs(data2$y))

plot(data1$x, data1$y,type='p', col = "red", pch=16,xlab="x", ylab="y",
     xlim=c(-1*xyMax,xyMax),
     ylim=c(-1*xyMax,xyMax),
     asp=1)
points(data2$x, data2$y,type='p',col="blue",pch=16)

draw.circle(0,0,1)
draw.circle(0,0,2)

data3<-rbind(data1,data2)
theClass<-rep(1,200)
theClass[1:100]<--1

c1<-svm(data3, theClass)
plot(c1)
