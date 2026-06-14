#https://www.cnblogs.com/squidGuang/p/9054301.html
#第一步：永远是下载用到的包（如果已经下载过就不用下载了），并引用
#install.packages("lavaan")
#install.packages("semPlot")
library(lavaan)
library(semPlot)

#第二步，导入数据
data1 <- read.csv("example1.csv")#这次这里可没有rownames =1

#第三步：根据自己的预测情况讲模型写出来
model <- '#measurement model 测量模型
          ind60 =~ x1 + x2 + x3
          dem60 =~ y1 + y2 + y3 + y4
          dem65 =~ y5 + y6 + y7 + y8
          
          # regressions 回归/路径模型
          dem60 ~ ind60
          dem65 ~ ind60 + dem60
   
          #residual correlations 残余相关
          y1 ~~ y5
          y2 ~~ y4 + y6
          y3 ~~ y7
          y4 ~~ y8
          y6 ~~ y8'

#第四步：拟合SEM
fit <- sem(model, data = data1)

#第五步：提取结果
summary(fit, standardized = TRUE)  

#第六步：验证结果的准确性
fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))

#第七步：作图
semPaths(fit, #上面跑出来的数据模型
         what = "std", #图中边的格式，#"path"，"std"，"est "，"cons "
         layout = "spring", #图的格式， tree  circle  spring  tree2  circle2
         fade = F, #褪色，按照相关度褪色
         residuals = F ,#残差/方差要不要体现在图中，可T和F
         nCharNodes	= 0
         
)



##################我们下面开始第二个例子，有新东西哦
#第二步，导入数据
data2 <- read.csv("example2.csv")#这次这里可没有rownames =1

#第三步：根据自己的预测情况讲模型写出来
model <- ' visual  =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed   =~ x7 + x8 + x9
           mind =~ visual + textual + speed
           speed ~ visual + textual
           x1 ~~ x7
           
'
#第四步：拟合SEM
fit <-sem(model, data = data2)

#第五步：提取结果
summary(fit, standardized = TRUE)  

#第六步：验证结果的准确性
fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))


#第七步：作图
semPaths(fit, #上面跑出来的数据模型
         what = "std", #图中边的格式，#"path"，"std"，"est "，"cons "
         layout = "tree", #图的格式， tree  circle  spring  tree2  circle2
         fade=F, #褪色，按照相关度褪色
         residuals = F, #残差/方差要不要体现在图中，可T和F
         nCharNodes	= 0
)


###############################
data3 <- read.csv("example3.csv")
colnames(data3)[1] <- "x1" 

#第三步：根据自己的预测情况讲模型写出来
model <- ' guangao  =~ x1 + x2
           huodong =~ y1 + y2
           jixing  =~ z1 + z2 + z3
          
           jixing ~ guangao + huodong
           guangao ~~ huodong 
           
           x1 ~~ x2
           y1 ~~ y2
           z1 ~~ z2 + z3
'
#第四步：拟合SEM
fit <-sem(model, data = data3)

#第五步：提取结果
summary(fit, standardized = TRUE)  

#第六步：验证结果的准确性
fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))


#第七步：作图
semPaths(fit, #上面跑出来的数据模型
         what = "std", #图中边的格式，#"path"，"std"，"est "，"cons "
         layout = "tree", #图的格式， tree  circle  spring  tree2  circle2
         fade=F, #褪色，按照相关度褪色
         residuals = F, #残差/方差要不要体现在图中，可T和F
         nCharNodes	= 0
)
