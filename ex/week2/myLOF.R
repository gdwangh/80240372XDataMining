# 计算各点的lof值，LOF值越大表示是离群点的概率越大

myLOF<-function(data, k, cores=NULL) {
  if(is.null(k))
    stop('k is missing')
  
  if(!is.numeric(k))
    stop('k is not numeric')
  
  if(!is.numeric(cores) && !is.null(cores))
    stop('cores is not numeric')
  
  data <- as.matrix(data)
  
  if(!is.numeric(data))
    stop('the data contains non-numeric data type')
  
  v.k<-as.integer(k)
  if(max(v.k) >= dim(data)[1])
    stop('the maximum k value has to be less than the length of the data')
  
  # 计算distance_k(o)：o点到它的k个近邻的最大距离，包含了在这个范围内的所有点
  dist.k.nearest<-f.distance.k.neigbour(data, v.k, cores)
  
  p <- dim(dist.k.nearest)[2L]  # 原始记录的数量
  
  lrddata <- f.reachability(dist.k.nearest,ik)
    
  # 计算lof
  v.lof <- rep(0,p)
  for (i in 1:p) {
      lrd_A <- lrddata[i]
      
      # A的邻居B
      numneigh<-dist.k.nearest[1,i]
      B_id <- dist.k.nearest[3:(2+numneigh),i]
      lrd_B <- lrddata[B_id]
      v.lof[i] = sum(lrd_B)/lrd_A/numneigh
    }
    v.lof

}

f.distance.k.neigbour<-function(data, k, cores=NULL) {
  m.dist <- as.matrix(dist(data))
  num.col = ncol(m.dist)
  num.row = nrow(m.dist)
  
  l.knndist<-lapply(c(1:num.col),function(i)
  {
    idx = c(1:num.row)
    idx <- idx[idx!=i] # 去掉(i,i)
    
    uniq.dist<-unique(m.dist[idx,i])  # 相同距离看作1个
    order.x <- order(uniq.dist) 
    kdist<-uniq.dist[order.x[k]]  # 升序
    numnei <- sum(m.dist[idx,i] <= kdist)
    n.dist<- m.dist[idx,i][m.dist[idx,i]<= kdist]
    n.order<- as.integer(names(n.dist))
    
    data.frame(v.order = n.order, v.dist = n.dist)
  }) 

  rm(m.dist)
  
  # 最多邻居数
  maxnum <- max(unlist(lapply(l.knndist,function(x){dim(x)[1]})))
  
  registerDoParallel(cores=cores)
  i <- numeric()
  
  # 如果有记录存在距离相同的邻居时，最大邻居数>某条记录实际邻居数, 多出的补NA
  # 每列对应一个原始记录
  knndist <- foreach(i = 1:num.col, .combine=cbind) %dopar%
  {
    len <- dim(l.knndist[[i]])[1]
    # 邻居个数，最大邻居距离，邻居的序列号，邻居的距离
    c(k.num=len, k.max=max(l.knndist[[i]]$v.dist), k.ord=l.knndist[[i]]$v.order,rep(NA,(maxnum-len)),k.dist=l.knndist[[i]]$v.dist,rep(NA,(maxnum-len)))
  }
  
  stopImplicitCluster()
  
  knndist
  
}

# distance_k(A,B) = max(distance_k(B), d(A,B))
# distdata：列 - 原始记录，行 - 邻居序号，邻居的距离
f.reachability <- function(distdata,k)
{
  p <- dim(distdata)[2]  # 原始记录的个数
  lrd <- rep(0,p)
  
  dist.start <- as.integer((dim(distdata)[1]-2)/2)+3
  dist.end <- dim(distdata)[1]
  
  for (i in 1:p)
  {
    # compare the k-distance of each observation's kth neighbor
    # with the actual distance between each observation and its neighbors
    
    # A 的邻居B
    numneigh<-distdata[1,i]
    B_id <- distdata[3:(2+numneigh),i]
    
    # max(B的邻居最大距离, A到B的距离)
    dist_k_AB = apply(cbind(distdata[2,B_id], distdata[dist.start:(dist.start+numneigh-1),i]),1, max)
    
    #calculate reachability
    reach <- 1/(sum(dist_k_AB)/numneigh)
    lrd[i] <- reach
  }
  lrd
}

