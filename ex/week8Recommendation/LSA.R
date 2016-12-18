terms<-c('human','interface','computer','user','system','response','time','EPS','survey','trees','graph','minors')

docs<-c('c1','c2','c3','c4','c5','m1','m2','m3','m4')

X<-matrix(data=c(1.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,
                 1.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,
                 1.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
                 0.,  1.,  1.,  0.,  1.,  0.,  0.,  0.,  0.,
                 0.,  1.,  1.,  2.,  0.,  0.,  0.,  0.,  0.,
                 0.,  1.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,
                 0.,  1.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,
                 0.,  0.,  1.,  1.,  0.,  0.,  0.,  0.,  0.,
                 0.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,
                 0.,  0.,  0.,  0.,  0.,  1.,  1.,  1.,  0.,
                 0.,  0.,  0.,  0.,  0.,  0.,  1.,  1.,  1.,
                 0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  1.),
          ncol=length(docs), byrow=TRUE, dimnames=list(terms,docs))

# SVD分解, X=TSD', 
svdRes<-svd(X)
T<-svdRes$u
S<-diag(svdRes$d)
D<-svdRes$v

Y<-T %*% S %*% t(D)

# 降维k=2, S已按降序排序
T2<-T[,1:2]
S2<-S[1:2,1:2]
D2<-D[,1:2]

# X-hat
X_hat<-T2 %*% S2 %*% t(D2)

# 将每个term降维, 得到的是列矩阵，即每一列对应一个term
term2<-solve(S2) %*% t(D2) %*% t(X)

plot(x=term2[1,], y=term2[2,],col='red')
text(x=term2[1,], y=term2[2,], colnames(term2), pos=3,offset=0.5)

# 将每个文档降维
docs2<-solve(S2) %*% t(T2) %*% X
plot(x=docs2[1,], y=docs2[2,],col='red')
text(x=docs2[1,], y=docs2[2,], colnames(docs2), pos=3,offset=0.5)



# Cosine Similarity
library(lsa)

cosine(docs2)
cosine(term2)

cosine(X)  # 讲义上P23的Original
cosine(X_hat)   # 讲义P23的Transformed

# query
q<-c(1,0,0,0,0,1,0,0,0,0,0,0)
q_hat<-solve(S2) %*% t(T2) %*% q

cosine(cbind(q_hat, docs2))[1,]

############################################################################
# 用lsa包完全处理
library(lsa)

myLSAspace<-lsa(X, dims=2)
myLSAspace$tk
myLSAspace$dk
myLSAspace$sk

# X-hat
X_hat2<-as.textmatrix(myLSAspace)

# 从query获得一个对应的doc列
q2<-query("human response", rownames(X))

q2_hat<-fold_in(q2, myLSAspace)  # transform以后的q2

# q2与其他doc的cos相似度
cosine(cbind(X_hat2, q2_hat))['HUMAN RESPONSE',]  

