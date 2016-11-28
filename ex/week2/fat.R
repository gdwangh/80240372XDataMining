setwd("D:/study/80240372XDataMining/ex/week2")
setwd("D:/doc/study/80240372XDataMining/ex/week2")

data1 <- read.csv("data.csv", stringsAsFactors=FALSE)

library(Rlof)

summary(data1)

# 检查行数、列数
nrow(data1)
ncol(data1)

# 删除多余无用的列
data2<-data1[1:38]
ncol(data2)

# 检查是否有缺失值 --- 没有
summary(data2)

# 编号 --- 部分编号重复，且重复的记录有些不像同一个人？
table(data2$编号)

# 性别
summary(data2$性别)
table(data2$性别)  # 0:1 = 1.85:1 不算不平衡
data2$性别 = as.factor(data2$性别)

data2['flag']='+'

Maledata = data2[data2$性别==0,]
FemaleData = data2[data2$性别==1,]

plot(data2$性别)

# 年龄
summary(data2$年龄)
table(data2$年龄)
hist(data2$年龄)

# 身高, 单位：cm
summary(data2$身高)
table(data2$身高)
hist(data2$身高)
boxplot(身高~性别,data=data2)
plot(data2$身高,data2$年龄,col=data2$性别)

# boxplot.stats(x, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# --- 四分法判断outlier值
boxplot.stats(Maledata$身高)$out
boxplot.stats(FemaleData$身高)$out

# 用3sigma法判断outlier值，没有outlier值
data2$身高[checkOutlet_sigma(data2$身高, 3, 'logic')]


# 用lof法判断outlier值
lof<-myLOF(data2$身高, k=3)
summary(lof)
plot(density(lof))
data2$身高[order(lof, decreasing=T)][1:5]

# 体重，单位:kg
summary(data2$体重)
table(data2$体重)
hist(data2$体重)
boxplot(体重~性别,data=data2,col=data2$性别) 

boxplot.stats(Maledata$体重)$out
boxplot.stats(FemaleData$体重)$out

lof<-myLOF(data2$体重, k=3)
summary(lof)
plot(density(lof))
data2$体重[order(lof, decreasing=T)][1:5]

# 部分体重有点太轻了 --> 推断 性别=1 --> Female, =0 -> Male
plot(data2$身高, data2$体重,col=data2$性别)

# 细胞内液
summary(data2$细胞内液)
boxplot(细胞内液~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$细胞内液)$out
boxplot.stats(FemaleData$细胞内液)$out

lof<-myLOF(data2$细胞内液, k=3)
summary(lof)
plot(density(lof))
data2$细胞内液[order(lof, decreasing=T)][1:5]

# 细胞外液
summary(data2$细胞外液)
boxplot(细胞外液~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$细胞外液)$out
boxplot.stats(FemaleData$细胞外液)$out


# 肌肉重
summary(data2$肌肉重)
boxplot(肌肉重~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$肌肉重)$out
boxplot.stats(FemaleData$肌肉重)$out

lof<-myLOF(data2$肌肉重, k=3)
summary(lof)
plot(density(lof))
data2$肌肉重[order(lof, decreasing=T)][1:5]

# 蛋白质
summary(data2$蛋白质)
boxplot(蛋白质~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$蛋白质)$out
boxplot.stats(FemaleData$蛋白质)$out

lof<-myLOF(data2$肌肉重, k=3)
summary(lof)
plot(density(lof))
data2$肌肉重[order(lof, decreasing=T)][1:5]

# 瘦体重
summary(data2$瘦体重)
boxplot(瘦体重~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$瘦体重)$out
boxplot.stats(FemaleData$瘦体重)$out

# 无机盐
summary(data2$无机盐)
boxplot(无机盐~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$无机盐)$out
boxplot.stats(FemaleData$无机盐)$out

lof<-myLOF(data2$无机盐, k=3)
summary(lof)
plot(density(lof))
data2$无机盐[order(lof, decreasing=T)][1:5]

# 脂肪重 --- 有负值，异常
summary(data2$脂肪重)
data2[data2$脂肪重<=0, 'flag']='-'

boxplot(脂肪重~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$脂肪重)$out
boxplot.stats(FemaleData$脂肪重)$out

lof<-myLOF(data2$脂肪重, k=3)
summary(lof)
plot(density(lof))
data2$脂肪重[order(lof, decreasing=T)][1:5]

# 脂肪百分比
summary(data2$脂肪百分比)
data2[data2$脂肪百分比<=0, 'flag']='-'

boxplot(脂肪百分比~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$脂肪百分比)$out
boxplot.stats(FemaleData$脂肪百分比)$out

lof<-myLOF(data2$脂肪百分比, k=3)
summary(lof)
plot(density(lof))
data2$脂肪百分比[order(lof, decreasing=T)][1:5]


# 腰臀比
summary(data2$腰臀比)
boxplot(腰臀比~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$腰臀比)$out
boxplot.stats(FemaleData$腰臀比)$out

# 肥胖度
summary(data2$肥胖度)
boxplot(肥胖度~性别,data=data2,col=data2$性别)   

  
# 体质指数
summary(data2$体质指数)
boxplot(体质指数~性别,data=data2,col=data2$性别)   

boxplot.stats(Maledata$体质指数)$out
boxplot.stats(FemaleData$体质指数)$out

# 肌肉控制
summary(data2$肌肉控制)
boxplot(肌肉控制~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$肌肉控制)$out
boxplot.stats(FemaleData$肌肉控制)$out

# 脂肪控制
summary(data2$脂肪控制)
boxplot(脂肪控制~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$脂肪控制)$out
boxplot.stats(FemaleData$脂肪控制)$out

# 体重控制
summary(data2$体重控制)
boxplot(体重控制~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$体重控制)$out
boxplot.stats(FemaleData$体重控制)$out


# 标准体重
summary(data2$标准体重)
boxplot(标准体重~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$标准体重)$out
boxplot.stats(FemaleData$标准体重)$out

# 目标体重
summary(data2$目标体重)
boxplot(目标体重~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$目标体重)$out
boxplot.stats(FemaleData$目标体重)$out

# 基础代谢率
summary(data2$基础代谢率)
boxplot(基础代谢率~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$基础代谢率)$out
boxplot.stats(FemaleData$基础代谢率)$out

# 躯干肌肉
summary(data2$躯干肌肉)
boxplot(躯干肌肉~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$躯干肌肉)$out
boxplot.stats(FemaleData$躯干肌肉)$out

# 躯干骨质
summary(data2$躯干骨质)
boxplot(躯干骨质~性别,data=data2,col=data2$性别) 
boxplot.stats(Maledata$躯干骨质)$out
boxplot.stats(FemaleData$躯干骨质)$out

# 躯干脂肪
summary(data2$躯干脂肪)
boxplot(躯干脂肪~性别,data=data2,col=data2$性别)
boxplot.stats(Maledata$躯干脂肪)$out
boxplot.stats(FemaleData$躯干脂肪)$out

# 躯干肌肉
summary(data2$躯干肌肉)
boxplot(躯干肌肉~性别,data=data2,col=data2$性别)
boxplot.stats(Maledata$躯干肌肉)$out
boxplot.stats(FemaleData$躯干肌肉)$out

# 躯干脂肪百分比
summary(data2$躯干脂肪百分比)
boxplot(躯干脂肪百分比~性别,data=data2,col=data2$性别)
data2[data2$躯干脂肪百分比<=0, 'flag']='-'
  
# 左上肢肌肉
#左上肢骨质
#左上肢脂肪
summary(data2[27:29])
data2[data2$左上肢肌肉<=0, 'flag']='-'
data2[data2$左上肢骨质<=0, 'flag']='-'


boxplot(左上肢肌肉~性别,data=data2,col=data2$性别)
boxplot(左上肢骨质~性别,data=data2,col=data2$性别)
boxplot(左上肢脂肪~性别,data=data2,col=data2$性别)

boxplot.stats(data2$左上肢肌肉)$out
boxplot.stats(data2$左上肢骨质)$out
boxplot.stats(data2$左上肢脂肪)$out


#右上肢肌肉
# 右上肢骨质
# 右上肢脂肪
summary(data2[30:32])
data2[data2$右上肢肌肉<=0, 'flag']='-'
data2[data2$右上肢骨质<=0, 'flag']='-'

boxplot(右上肢肌肉~性别,data=data2,col=data2$性别)
boxplot(右上肢骨质~性别,data=data2,col=data2$性别)
boxplot(右上肢脂肪~性别,data=data2,col=data2$性别)

boxplot.stats(data2$右上肢肌肉)$out
boxplot.stats(data2$右上肢骨质)$out
boxplot.stats(data2$右上肢脂肪)$out

#左下肢肌肉
#左下肢骨质
#左下肢脂肪
summary(data2[33:35])

boxplot(左下肢肌肉~性别,data=data2,col=data2$性别)
boxplot(左下肢骨质~性别,data=data2,col=data2$性别)
boxplot(左下肢脂肪~性别,data=data2,col=data2$性别)

boxplot.stats(data2$左下肢肌肉)$out
boxplot.stats(data2$左下肢骨质)$out
boxplot.stats(data2$左下肢脂肪)$out

data2[data2$左下肢肌肉>20, 'flag']='-'

#右下肢肌肉
#右下肢骨质
#右下肢脂肪
summary(data2[36:38])

boxplot(右下肢肌肉~性别,data=data2,col=data2$性别)
boxplot(右下肢骨质~性别,data=data2,col=data2$性别)
boxplot(右下肢脂肪~性别,data=data2,col=data2$性别)

boxplot.stats(data2$右下肢肌肉)$out
boxplot.stats(data2$右下肢骨质)$out
boxplot.stats(data2$右下肢脂肪)$out

data2[data2$右下肢肌肉==168, 'flag']='-'
data2[data2$右下肢肌肉==51, 'flag']='-'

# 多个字段关系检查
data2<-data2[data2$flag=='+',]

# 细胞内：细胞外
summary(data2$细胞内液/data2$细胞外液)
summary((data2$细胞内液+data2$细胞外液)/data2$体重)

# 肌肉重量 = 水分含量 + 蛋白质=5+6+8
diff <- data2$肌肉重-(data2$蛋白质+data2$细胞内液+data2$细胞外液)
hist(diff)

# 去脂体重＝肌肉重＋无机盐=7+10
diff <- data2$瘦体重-(data2$肌肉重+data2$无机盐)
summary(diff)
table(diff)


# 脂肪百分比 = 脂肪重/体重 * 100%
diff<-data2$脂肪百分比-data2$脂肪重/data2$体重*100
summary(diff)


# 肥胖度 = 实际体重/标准体重 * 100%
diff <- data2$肥胖度/100 * data2$标准体重 - data2$体重
summary(diff)

# 目标体重 = 体重 + 体重控制 
diff<-data2$体重 + data2$体重控制 - data2$目标体重
summary(diff)

# 标准体重 = 目标体重, 上下相差10%均为正常
diff <- data2$标准体重 - data2$目标体重
summary(diff)
  
# 肌肉重 - （ 躯干肌肉 + 左上肢肌肉+右上肢肌肉+左下肢肌肉+右下肢肌肉）
diff<- data2$肌肉重 - (data2$躯干肌肉 + data2$左上肢肌肉+data2$右上肢肌肉+data2$左下肢肌肉+data2$右下肢肌肉)
summary(diff)

# 骨质（无机盐）-（躯干骨质 +左上肢骨质+右上肢骨质+左下肢骨质+右下肢骨质）
diff<- data2$无机盐 - (data2$躯干骨质 + data2$左上肢骨质+data2$右上肢骨质+data2$左下肢骨质+data2$右下肢骨质)
summary(diff)

# 脂肪重 - （躯干脂肪+右上肢脂肪+左下肢脂肪+右下肢脂肪）
diff<- data2$脂肪重 - (data2$躯干脂肪 + data2$左上肢脂肪 + data2$右上肢脂肪+data2$左下肢脂肪+data2$右下肢脂肪)
summary(diff)

# 可以研究各属性对性别的区分度

# Min-max normalization：new_min=0, new_max=1
data3<-cbind(data2[,1:3], scale(data2[,4:38], 
                                center= apply(data2[,4:38], 2, min), 
                                scale = apply(data2[,4:38], 2, function(x) { max(x)-min(x)})
                               )
            )

library(ggplot2)
library(grid)

namelist<-c(3:4)
plist<-list()
for (colName in names(data3)[namelist]) {
  p<-ggplot(data3)+geom_density(aes_string(x=colName, colour="性别"))
  plist<-c(plist, list(p))
}
multiplot(plotlist=plist)


pdf("allplot.pdf",family="GB1");

for (i in seq(from=3, to=38, by = 4)) {
  namelist<-c(i:(i+3))
  plist<-list()
  for (colName in names(data3)[namelist]) {
    p<-ggplot(data3)+geom_density(aes_string(x=colName, colour="性别"))
    plist<-c(plist, list(p))
  }
  print(namelist)
  print(names(data3)[namelist])
  multiplot(plotlist=plist, cols=2)
}
dev.off();


pdf("boxplot.pdf",family="GB1");

for (i in seq(from=3, to=38, by = 4)) {
  namelist<-c(i:(i+3))
  plist<-list()
  for (colName in names(data3)[namelist]) {
    p<-ggplot(data3,aes_string(y=colName,x="性别"))+geom_boxplot()
    plist<-c(plist, list(p))
  }
  
  multiplot(plotlist=plist, rows=2, cols=2)
}
dev.off();

# PCA: 主成份分析
# p.cr<-princomp(data2[3:38])
# summary(p.cr)
# plot(p.cr)
# biplot(p.cr)
# 
# p2.cr<-prcomp(data2[3:38])
# summary(p2.cr)
# plot(p2.cr)
# biplot(p2.cr)
# 
# library(psych)  
# pc<-principal(data2[3:38])  

library(MASS)
plda<-lda(性别~., data=data2[,2:38])

ldacoef <-coef(plda)

# 影响最大的
row.names(ldacoef)[which.max(abs(ldacoef))]

pred = predict(plda, new_data = data3[,2:38])


plot(data3$性别, pred$x)
tmpdata = data.frame(gender = data3$性别, ld1 = pred$x)
ggplot(tmpdata)+geom_density(aes(x=LD1, colour=gender)) # 变换后

# 原始字段
ggplot(data3)+geom_density(aes_string(x="腰臀比", colour="性别"))

# 特征选择
library(caret)
subsets <- c(1,2,5,10,20,30)
ctrl= rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "final")
Profile = rfe(x=data3[,3:38], y=data3$性别, sizes = subsets, rfeControl = ctrl)
print(Profile)
plot(Profile)
Profile$optVariables  # 最优的是："基础代谢率"

p1<-ggplot(data3)+geom_density(aes_string(x="基础代谢率", colour="性别"))
p2<-ggplot(data3)+geom_density(aes_string(x="脂肪百分比", colour="性别"))
multiplot(p1,p2)

