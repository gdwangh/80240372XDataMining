
# PCA 和 LDA Example

# PCA：

# Dataset
C1 = data.frame(x=c(1:5), y=c(2,3,3,5,5))
C2 = data.frame(x=c(1,2,3,3,5,6), y=c(0,1,1,2,3,5))

# PCA属于无监督，没有分类标签C1，C2，所以合并所有的数据来计算
C = rbind(C1,C2)

# Covariance of [c1; c2]
Z = cov(C)

# Eigenvectors and Eigenvalues of Z
res = eigen(Z)
D = res$values   # sorted in decreasing order
V = res$vectors

# 取最大的1个
PC_NUM = 1

# The direction of PCA projection
V[,0:PC_NUM]


# LDA

# LDA要求不同分类之间的距离最远，同一个分类内部分布最靠近，
# 所以不同类别之间的数据要分别计算
miu1 = t(colMeans(C1))
miu2 = t(colMeans(C2))

# C1和C2之间的距离Sb
Sb = (miu1 -  miu2) %*% t(miu1 - miu2)

# C1和C2的内部散布矩阵
# 从Si与cov（X）的公式，可得：Si = cov(Xi) * (n-1) --- n为数据分类Xi中数据个数
S1 = (nrow(C1)-1) * cov(C1)
S2 = (nrow(C2)-1) * cov(C2)

Sw = S1 + S2

S = solve(Sw) %*% Sb  # S^(-1) * S

# Eigenvectors and Eigenvalues of S
res_LDA = eigen(S)
D2 = res_LDA$values   # sorted in decreasing order
V2 = res_LDA$vectors

# The direction of LDA projection
V2[,0:PC_NUM]

# 另一种计算LDA的方法, Sw(-1) * (miu1 - miu2)
solve(Sw) %*% t(miu1-miu2)

# 两个答案的方向是一样的
res1 = V2[,0:PC_NUM]
res2 = solve(Sw) %*% t(miu1-miu2)

res1[1]/res2[1]
res1[2]/res2[2]


