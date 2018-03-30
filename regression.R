library(data.table)
set.seed(12345)
N = 1000
e1 <- rnorm(N, 0, 1)
# y = 1 + 2x
x <- rep(seq(1, 100), 10)
x <- matrix(x)
y <- 3 * x + 1 + e1
y <- matrix(y)

model <- lm(y ~ x)

summary(model)
# 参数估计结果： y = 2.00032 * x + 1.01769  R2 = 0.9997  p-value < 2.2e-16


# simple regression: similar results --------------------------------------

# 如果是完全线性关系的话，迭代次数过多会梯度爆炸？，而且NA之前一步迭代的结果也不对。





# complex regression ------------------------------------------------------


