library(ggplot2)
X <- matrix(c(0,0,1, 0,1,1,1,0,1,1,1,1),ncol = 3,byrow = TRUE)
y <- c(0, 1, 1, 0)
cbind(X, y)
print(y)
rand_vector<- runif(ncol(X) * nrow(X))
rand_matrix<- matrix(rand_vector,nrow = ncol(X),ncol = nrow(X),byrow = TRUE)
my_nn<- list(input = X,weights1 = rand_matrix, weights2 = matrix(runif(4), ncol = 1),y = y,output = matrix(rep(0, times = 4),ncol = 1))
sigmoid <- function(x) {
       1.0 / (1.0 + exp(-x))
  }
sigmoid_derivative<- function(x) {
      x * (1.0 - x)
  }
loss_function<- function(nn) {
       sum((nn$y - nn$output) ^ 2)
   }
feedforward <- function(nn) {
         nn$layer1 <- sigmoid(nn$input %*% nn$weights1)
         nn$output<- sigmoid(nn$layer1 %*% nn$weights2)
         print(nn)
    }
backprop <- function(nn) {
         d_weights2 <- (
         t(nn$layer1) %*%
         (2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)))
         d_weights1 <- ( 2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)) %*% 
         t(nn$weights2)
         d_weights1 <- d_weights1 * sigmoid_derivative(nn$layer1)
         d_weights1 <- t(nn$input) %*% d_weights1
         nn$weights1 <- nn$weights1 + d_weights1
         nn$weights2 <- nn$weights2 + d_weights2
         print(nn)
        }
n <- 10
loss_df<- data.frame(iteration = 1:n,loss = vector("numeric", length = n))
for (i in seq_len(10)) {
        my_nn<- feedforward(my_nn)
        my_nn<- backprop(my_nn)
        loss_df$loss[i] <- loss_function(my_nn)
       }
data.frame("Predicted" = round(my_nn$output, 3),"Actual" = y)
ggplot(data = loss_df, aes(x = iteration, y = loss)) + geom_line()
