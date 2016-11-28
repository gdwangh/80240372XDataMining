# 3*sigma or 6*sigma法，判断极限值
# input
#    data : 需要判断是否有极限值的数据
#    k：正常范围的上下限为k个sigma
#    type：
#        ‘B' -- 返回true or false，是否是极限值
#        ’M‘ --- 用最大最小值k*sigma or -k*sigma，替换极限值         
# output:
#   对应data的每个值，返回
#       True or False --- 是否是极限值
#     or
#       k*sigma or -k*sigma
checkOutlet_sigma<-function(data, k, out_type='logic') {
  m = mean(data)
  sd = sd(data)
  min = m - k * sd
  max = m + k * sd
  
  if (out_type == 'logic') {
    (data < min) | (data > max)
  }  else {
    sapply(data, function(x) { 
        if (x < min) min
        else 
          if (x > max) max
          else x
      })
  }
}