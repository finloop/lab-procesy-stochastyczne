# Przykładowy proces stochastyczny
X1 <- function(t) {
  cumsum(rnorm(t))
}

# Rysowanie realizacji obok siebie
N <-100
par(mfrow=c(2,2))
for(i in 1:4) {
  plot(X1(N), type='l')
}
par(mfrow=c(1,1))

# Rysowanie realizacji jedna na drugiej

p <- palette(rainbow(5))
plot(X1(N), type='l', col=p[1])
for(i in 2:5) {
  lines(X1(N), type='l', col=p[i])
}

# Rysowanie realizacji na sobie

# Generowanie setek realizaji i ich zapisanie
# w każdej kolumnie jest jedna realizacja
N <- 20 # Liczba realizacji
T <- 100 # Długość jednej realizacji 

# Generowanie realizacji w macierzy
data <- matrix(nrow = T, ncol = N)
for(ind in 1:N) {
  data[, ind] <- X1(T)  
}
mean(apply(data, 2, mean)) # Wartość oczekiwana procesów stochastycznych na bazie N realizacji
sd(apply(data, 2, mean)) # Sd wartości oczekiwanej

mean(apply(data, 2, var)) # Średnia wariancja proc stoch
sd(apply(data, 2, var)) # sd wariancji proc stoch

# Rysowanie wielu obok siebie
p <- palette(rainbow(N))
plot(data[,1], type='l', col=p[1], xlim = c(0, T), ylim = c(min(data), max(data)))
for(i in 2:N) {
  lines(X1(data[,i]), type='l', col=p[i])
}

############### Wyznaczanie rzędów AR i MA modeli ##############################
# X - model ruchomej średnie AR(3)
# Y - model autokorelacji MA(3)
library(forecast)

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

X1 <- X(c(1, -0.5, 1), 1000)
Y1 <- Y(c(1, -0.5, 1), 1000)

# Sprawdzanie rzędu MA
# https://towardsdatascience.com/identifying-ar-and-ma-terms-using-acf-and-pacf-plots-in-time-series-forecasting-ccb9fd073db8
Acf(X1, lag.max=10)
# Wybieram peak na lag=4, do rozważenia są jeszcze modele lag=2 oraz 6
Arima(X1, order=c(0, 0, 4), include.mean=FALSE)

# Sprawdzanie rzędu AR
Pacf(Y1, lag.max = 10)
Arima(X1, order=c(3, 0, 0), include.mean=FALSE)

# Auto arima
auto.arima(X1)
auto.arima(Y1)
