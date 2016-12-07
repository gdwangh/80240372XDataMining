# 三层神经网络：1个输入，1个隐藏，1个输出
# 输入
#    x_in：训练集中的输入，原始的x1~xn，不含x0=1
#    y_out：训练集之输出（可以多个）
#    n_hidden: 隐藏层节点个数
#    delta0: 结束阀值
#    stepSize: 学习步长
#    maxIter: 最大迭代次数
# 输出
#    list(isConv,iterNum, e, w1, w2)
#   其中：isConv --- 是否收敛
#         iterNum --- 迭代次数
#         e --- 当前error
#         w1 --- 输入层和中间层的系数
#         w2 --- 输出层和中间层的系数
trainAnn3<-function(x_in, y_out, n_hidden, delta0, stepSize, maxIter) {
  rownum <-nrow(x_in)
  x<-cbind(rep(1,rownum),x_in)
  
  n_in<-ncol(x_in)+1
  n_out<-ncol(y_out)
  
  # 初始化w:w0~wn
  # w1 <- matrix(rep(0,n_in*n_hidden), nrow=n_hidden, ncol=n_in) # 输入层与中间层
  # w2 <- matrix(rep(0,n_hidden*n_out), nrow=n_out, ncol=n_hidden)  # 中间层与输出层
  w1 <- matrix(runif(n_in*n_hidden), nrow=n_hidden, ncol=n_in) # 输入层与中间层
  w2 <- matrix(runif(n_hidden*n_out), nrow=n_out, ncol=n_hidden)  # 中间层与输出层
  
  stop<-FALSE
  iterNum<-0
  isConv<-FALSE
  while (!stop) {
    for (idx_row in c(1:rownum)) {
      # 中间层输入
      # netj<-x[idx_row,] %*% t(w1) 
      
      # 中间层输出
      # oj<-apply(netj, c(1,2),function(x) { 1/(1+exp(-x))} ) 
      
      # 输出层输入
      # netk<-oj %*% t(w2) 
      
      # 输出层的输出，也是整体输出
      # o<-apply(netk,c(1,2),function(x) { 1/(1+exp(-x))} ) 
      
      # 用函数代替上面各计算步骤
      out<-calANN3Out(x[idx_row,],w1, w2)
      o<-out[[1]]
      oj<-out[[2]]
      
      # 计算总体的输出误差对输出层输入的偏导
      delta_o<-o*(1-o)*(y_out[idx_row,]-o)
      
      # 中间层到输出层的w2
      delta_w2<-stepSize*t(delta_o)%*%oj
      w2<-w2+delta_w2
      
      # 计算总体的输出误差对中间层输入的偏导
      delta_j<-oj*(1-oj)*(delta_o %*% w2)
      
      # 计算输入层到中间层的w1
      delta_w1<-stepSize * t(delta_j) %*% x[idx_row,]
      w1<-w1+delta_w1
      
      # 判断是否满足收敛条件,满足则强行退出
      iterNum<-iterNum+1
      
      e<-0.5* rowSums((y_out[idx_row,]-o)^2)  # 计算方差
      isConv<-(e < delta0)
      
      stop<-(iterNum>maxIter) | isConv
      if (stop) break
    }  # for 
  }  # while 
  
  ret<-list(isConv,iterNum, e, w1, w2)
  return(ret)
}

# 计算神经网络输出值
# 输入
#   indata -- 输入,包含额外添加的x0
#   w1 --- 输入层和中间层的系数，包含额外添加的w0
#   w2 --- 中间层和输出层的系数，包含额外添加的w0
# 输出
#    list(out, outj)
#    out --- 输出层输出值
#    outj --- 中间层输出值

calANN3Out<-function(indata, w1, w2) {
  # 中间层输入
  netj<-indata %*% t(w1) 
  
  # 中间层输出
  oj<-apply(netj, c(1,2),g ) 
  
  # 输出层输入
  netk<-oj %*% t(w2) 
  
  # 输出层的输出，也是整体输出
  o<-apply(netk,c(1,2),g ) 
   
  ret = list(o, oj)
  return(ret)
}

#节点的阀值函数
g<-function(x) {
  return (1/(1+exp(-x)))
}

data <- matrix(c(0,0,1,0,1,1,1,0,1,1,1,0),nrow = 4, ncol = 3)
x_in <-as.matrix(data[,c(1,2)])
y_out<-as.matrix(data[,c(3)])
n_hidden<-2
delta0<-0.001
stepSize<-0.05
maxIter<-100000

out<-trainAnn3(x_in, y_out, n_hidden, delta0, stepSize, maxIter) 
pw1<-out[[4]] 
pw2<-out[[5]]

x1<-cbind(rep(1,nrow(data)),x_in)

y1<-calANN3Out(x1, pw1,pw2)[[1]]
y1
0.5* rowSums((y_out-y1)^2)

