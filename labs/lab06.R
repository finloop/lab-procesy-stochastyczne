library(forecast)

M <- 100

# X - model ruchomej średnie AR(3)
# Y - model autokorelacji MA(3)
# Z - szym biały
# Q - model ARMA(3, 3)

X <- function(x, n) {
  for(t in length(x):n) {
    val <- 0.3*x[t-1] - 0.7*x[t-2] + 0.1*x[t-3] + rnorm(1)
    x <- append(x,val)  
  }
  x
}

Y <- function(x, n) {
  A <- rnorm(n)
  for(t in length(x):n) {
    val <- A[t] - 0.4*A[t-1] + 0.2*A[t-2] - 0.1*A[t-3]
    x <- append(x,val)
  }
  x
}

Z <- function(n) {
  rnorm(n)
}

Q <- function(x, n) {
  A <- rnorm(n)
  for(t in length(x):n) {
    val <- A[t] - 0.4*A[t-1] + 0.2*A[t-2] - 0.1*A[t-3] +
      0.3*x[t-1] - 0.7*x[t-2] + 0.1*x[t-3] + A[t]
    x <- append(x,val)
  }
  x
}

N <- 4
p <- palette(rainbow(N))
plot(X(c(1, -0.5, 1), 100), col=p[i], type='s')
for(i in 1:N){
  lines(X(c(1, -0.5, 1), 100), col=p[i], type='s')
}

plot(Y(c(1, -0.5, 1), 100), col=p[i], type='s')
for(i in 1:N){
  lines(Y(c(1, -0.5, 1), 100), col=p[i], type='s')
}

plot(Q(c(1, -0.5, 1), 100), col=p[i], type='s')
for(i in 1:N){
  lines(Q(c(1, -0.5, 1), 100), col=p[i], type='s')
}

# Poszukujemy rzędu modelu MA dla X
Acf(X(c(1, -0.5, 1), 5000), lag.max = 15) # rzędu 4

# Poszukujemy rzędu modelu AR dla X
Pacf(X(c(1, -0.5, 1), 5000), lag.max = 15) # rzędu 3

# Poszukujemy rzędu modelu AR dla Y
Pacf(Y(c(1, -0.5, 1), 1000), lag.max = 15)
# Rzędu 1

# Poszukujemy rzędu modelu AR dla Y
Z

######### auto.arima
X1 <- X(c(1, -0.5, 1), 1000)
Y1 <- Y(c(1, -0.5, 1), 1000)
Z1 <- Z(1000)
Q1 <- X(c(1, -0.5, 1), 10000)

auto.arima(X1)
auto.arima(Y1)
auto.arima(Z1)
auto.arima(Q1)

# Fit AR model or given order
ar(X1, order.max=3, aic=FALSE)
ma(Y1, order=3)

# Fit Arima
Arima(X1, c(3, 0, 0))
Arima(Y1, c(0, 0, 3))

Arima(Q1, c(3, 0, 3))
 