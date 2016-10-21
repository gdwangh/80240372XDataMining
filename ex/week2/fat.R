setwd("D:/study/80240372XDataMining/ex/week2")

data <- read.csv("data.csv", stringsAsFactors=FALSE)

summary(data)

# 检查行数、列数
nrow(data)
ncol(data)

# 删除多余无用的列
data2<-data[1:38]
ncol(data)

# 编号 --- 部分编号重复，且重复的记录有些不像同一个人？
table(data2$编号)

# 性别
summary(data2$性别)
table(data2$性别)
data2$性别 = as.factor(data2$性别)
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

# 体重，单位:kg
summary(data2$体重)
table(data2$体重)
boxplot(体重~性别,data=data2,col=data2$性别)   

# 部分体重有点太轻了 --> 推断 性别=1 --> Female, =0 -> Male
plot(data2$身高, data2$体重,col=data2$性别)

# 标准体重
summary(data2$标准体重)
table(data2$标准体重)

# 体重控制 = 标准体重 - 体重
tzc = data2$标准体重 - data2$体重 - data2$体重控制
summary(tzc)  
data2[data2$标准体重 != data2$体重 - data2$体重控制]

# 细胞内液
summary(data2$细胞内液)
table(data2$细胞内液)
boxplot(data2$细胞内液)

# 细胞外液
summary(data2$细胞外液)
boxplot(data2$细胞外液)
