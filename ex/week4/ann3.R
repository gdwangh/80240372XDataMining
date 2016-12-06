# 三层神经网络：1个输入，1个隐藏，1个输出
# 输入
#    x_in：训练集中的输入，原始的x1~xn，不含x0=1
#    y_out：训练集之输出（可以多个）
#    n_hidden: 隐藏层节点个数
#    delta0: 结束阀值
#    stepSize: 学习步长
ann3<-function(x_in, y_out, n_hidden, delta0, stepSize) {
  rownum <-dim(x_in)[1]
  n_in<-dim(x_in)[2]+1
  n_out<-dim(y_out)[2]
  
  rownum <-dim(x_in[1])
  # 初始化w:w0~wn
  w1 <- matrix(rep(0,n_in*n_hidden), nrow=n_hidden, ncol=n_in) # 输入层与中间层
  w2 <- matrix(rep(0,n_hidden*n_out), nrow=n_out, ncol=n_hidden)  # 中间层与输出层
  
  x <- cbind(rep(1, dim(x_in)[1]), x_in)
    
  stop<-False
  while (!stop) {
    for (idx_row in c(1:rownum) {
      # 中间层输入
      netj<-x[idx_row,] %*% t(w1) 
      
      # 中间层输出
      oj<-apply(netj, c(1,2),function(x) { 1/(1+exp(-x))} ) 
      
      # 输出层输入
      netk<-oj %*% t(w2) 
      
      # 输出层的输出，也是整体输出
      o<-apply(netk,c(1,2),function(x) { 1/(1+exp(-x))} ) 
       
      # 计算总体的输出误差对输出层输入的偏导
      delta_o<-o*(1-o)*(y_out[idx_row,]-o)
      
      # 中间层到输出层的w2
      delta_w2<-stepSize*t(delta_o)*oj
      w2<-w2+delta_w2
      
      # 计算总体的输出误差对中间层输入的偏导
      delta_j<-oj*(1-oj)*(delta_o %*% w2)
      
      # 计算输入层到中间层的w1
      delta_w1<-stepSize * t(delta_j) %*% x[idx_row,]
      w1<-w1+delta_w1
    }  # for
    
    stop = 
  }
}