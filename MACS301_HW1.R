library(ggplot2)
#setting functions for each variable
bias <- function(x) 1/x
variance <- function(x) .005*x^2
variance_e <- function(x) 1
MSE_training <- function(x) -.0005*(x-12)^3 + .75
MSE_test <- function(x) .0003*(x - 8)^4 + 1.1
#plotting those functions on ggplot
ggplot(data = data.frame(x = c(0, 20)), aes(x)) + stat_function(fun = bias, aes(color = "bias")) + stat_function(fun = variance, aes(color = "variance")) + stat_function(fun = variance_e, aes(color = "variance_e")) + stat_function(fun = MSE_training, aes(color = "MSE_training")) + stat_function(fun = MSE_test, aes(color = "MSE_test")) + xlim(0, 20) + ylim(0, 2.5) + xlab("flexibility") + ylab("values") + scale_color_manual("Curves", values = c("red", "blue", "green", "orange", "purple"))



#(a) setting a random number generator seed
set.seed(500)
#(b) simulating a dataset of N = 200 with X1 and X2 as random uniform variables [0, 1]
X1 <- runif(200, -1, 1)
X2 <- runif(200, -1, 1)
X1
X2
#(c) calculating equation with epsilon
e <- rnorm(200, 0, sqrt(.25))
y <- X1 + X1^2 + X2 + X2^2 + e
y
#(d) calculate the probability of success bounded
y_prob <- exp(y)/(1 + exp(y))
y_prob
#(e) plot the points
plot <- plot(X1, X2, xlab = "X1", ylab = "X2", col = ifelse(y_prob > .5, 'blue', 'orange'))
plot
#(f) Bayes decision boundary
library(tidyverse)
df <- data.frame(X1, X2, y_prob)
boundary.X1 <- seq(-1, 1, by = .5)
boundary.X2 <- seq(-1, 1, by = .5)
grid <- expand.grid(X1 = boundary.X1, X2 = boundary.X2)
grid
grid <- mutate(grid, y = X1 + X1^2 + X2 + X2^2, z = exp(y)/(1 + exp(y)), upper = z > .5)
ggplot(grid, aes(X1, X2)) + geom_contour(aes(z = z, group = upper), bins = .5) + geom_point(data = df, aes(color = ifelse(y_prob > .5, 'success', 'failure'))) + labs(title = "Bayes Classifer")

                                                                                            
                                                                                            
                                                                                            
                                         