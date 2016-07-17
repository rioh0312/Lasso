library(glmnet)

# ワークスペースの設定
setwd("G:/Projects/Lasso")
getwd()
par(ps = 8)
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}

# データ入力
data1 <- read.csv("./data.csv") 
head(data1, 15) 
str(data1)

# glmnet()関数を使うためにnumeric型の行列に変換する
data3 <- data1
lstCol <- length(data1)
c_num <- 1
for(clm in data3[1:(lstCol-1)]){
  data3[c_num] <- as.numeric(clm)
  c_num <- c_num + 1
}
str(data3)

#説明変数の作成
exp_vars <- as.matrix(data3[3:11])

#目的変数の作成
target_var <- as.matrix(data3[1])

#-----------------------------------------------------------
#Ridge回帰
#・α= 0 とした場合、Ridge回帰となる
#・λの分割数はデフォルト100
#・family="gaussian"は最小二乗法、それ以外だと最尤法になる
#-----------------------------------------------------------
fitRidge1 <- glmnet( x=exp_vars, y=target_var, family="gaussian", alpha=0 )

#ペナルティと推定値のグラフをプロット
#plot(fitRidge1, xvar="norm", label=TRUE)
#plot(fitRidge1, xvar="lambda", label=TRUE)

#MSE(平均２乗誤差)のプロット
#左側の縦点線が、MSEが最小となるときのλの対数
#右側の点線が、上のMSEが最小となるときのMSEの上側1seとなるときのλの対数
fitRidgeCV1 <- cv.glmnet( x=exp_vars, y=target_var, family="gaussian", alpha=0)
#plot(fitRidgeCV1)

#選ばれたλを、ペナルティと推定値のグラフに描き入れてみる
#plot(fitRidge1, xvar="lambda", label=TRUE)
#abline(v=log(fitRidgeCV1$lambda.min), lty=2)

#MSEが最小となる時のλに対応するパラメータを出力するときは以下
#coef(fitRidgeCV1, s="lambda.min")

#MSEが最小となるときのMSEの上側1seとなるときのλに対応するパラメータを出力するときは以下
#coef(fitRidgeCV1, s="lambda.1se")

#推定したパラメータによる予測値を求める場合は以下
#pred_fitRidgeCV1 <- predict(fitRidgeCV1, s="lambda.min", newx=exp_vars)
#plot(pred_fitRidgeCV1)


#-----------------------------------------------------------
#Lasso回帰
#・α= 1 とした場合、Lasso回帰
#-----------------------------------------------------------
fitLasso1 <- glmnet( x=exp_vars, y=target_var, family="gaussian", alpha=1 )
fitLassoCV1 <- cv.glmnet( x=exp_vars, y=target_var, family="gaussian", alpha=1 )

#MSE(平均２乗誤差)のプロット
#plot(fitLassoCV1)

#MSEが最小
#log(fitLassoCV1$lambda.min)

#1 SEの位置
#log(fitLassoCV1$lambda.1se)

#ペナルティと推定値のグラフにラインを加えたもの
plot(fitLasso1, xvar="lambda", label=TRUE)
abline(v=log(fitLassoCV1$lambda.min), lty=2)
lbs_fun(fitLasso1)

#λが最小となる時の、パラメータ推定値
#coef(fitLassoCV1, s="lambda.min")

#予測の場合もRidgeと同様
#pred_fitLassoCV1 <- predict(fitLassoCV1, s="lambda.min", newx=exp_vars)


#-----------------------------------------------------------
#Elastic Net
#0 < α< 1 の場合、Elastic Net
#-----------------------------------------------------------
#fitEN1 <- glmnet( x=exp_vars, y=target_var, family="gaussian", alpha=0.5 )
#plot(fitEN1, xvar="lambda", label=TRUE)

#fitENCV1 <- cv.glmnet( x=exp_vars, y=target_var, family="gaussian", alpha=0.5 )
#plot(fitENCV1)

#plot(fitEN1, xvar="lambda", label=TRUE)
#abline(v=log(fitENCV1$lambda.min), lty=2)

#pred_fitENCV1 <- predict(fitENCV1, s="lambda.min", newx=exp_vars)
#plot(pred_fitENCV1)









