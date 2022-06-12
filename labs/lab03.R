# Zadanie 1
N <-100
par(mfrow=c(2,2))
plot(rnorm(N), type='l')
plot(rnorm(N), type='l')
plot(rnorm(N), type='l')
plot(rnorm(N), type='l')


# Zadanie 2
X2 <- function(t) {
  0.1*t+rnorm(length(t))/log(t+1)
}
par(mfrow=c(2,2))
plot(X2(c(1:N)), type='l')
plot(X2(c(1:N)), type='l')
plot(X2(c(1:N)), type='l')
plot(X2(c(1:N)), type='l')

# Zadanie 3
X3 <- function(t, s=2) {
  result <- rnorm(t, sd=s)
  result <- cumsum(result)
}
N <- 500
par(mfrow=c(2,2))
plot(X3(N), type='l')
plot(X3(N), type='l')
plot(X3(N), type='l')
plot(X3(N), type='l')

#### Zadanie 4 #### 
X4 <- function(t, s=2) {
  res <- rnorm(t, sd=s)
  div <- log(c(1:t) +1)
  res <- res / div
  res <- cumsum(res)
}

Y4 <- function(t, s=2) {
  res <- rnorm(t, sd=s)
  div <- c(1:t)^2
  res <- res / div
  res <- cumsum(res)
}

par(mfrow=c(1,1))
plot(X4(N), type='l')
for(i in 1:4) {
  lines(X4(N))
}


plot(Y4(N), type='l')
for(i in 1:9) {
  lines(Y4(N))
}

# Zadanie 5
x <-X3(N)
y <-X3(N) 
for(i in 1:N) {
  plot(x[1:i], y[1:i], type='l', xlim = c(min(x), max(x)))
  Sys.sleep(0.05)
}

# Zadanie 6
x <- sample(c(-1, 0, 1), N, TRUE)
x <- cumsum(x)
y <- sample(c(-1, 0, 1), N, TRUE)
y <- cumsum(y)
for(i in 1:N) {
  plot(x[1:i], y[1:i], type='l')
  Sys.sleep(0.05)
}

# Zadanie 7
# TODO doko?czyc to zadanie
a <- 50
b <- 60
alpha <- 3/2*pi - 2*pi*195/1440
c <- 2*pi/1440

lambda <- function(t) { 
  a*sin(alpha + c*t) + b
}

poission_rand <- function(t) {
  buff <- c()
  for (minute in t) {
    buff <- append(buff, rpois(1, lambda(minute)))
  }
  buff
}

plot(poission_rand(seq(0, 24*60)), type='l')

mean(poission_rand(seq(0, 24*60)))
sd(poission_rand(seq(0, 24*60)))

mean(lambda(seq(0, 24*60)))
sd(lambda(seq(0, 24*60)))

mean(poission_rand(rep(8*60+45, 100)))
sd(poission_rand(rep(8*60+45, 10000)))


lambda(8*60+45)
sqrt(lambda(8*60+45))



game <- function(pool = 8, limit = 1000) {
  g1_pool <- 0
  i <- 0
  
  while (TRUE) {
    i <- i + 1
    rzuty <- sample(1:6, 4, replace = TRUE)
    g1 <- sum(rzuty[1:2])
    g2 <- sum(rzuty[3:4])
    
    # Rule 1
    if (g1 == g2 & g1 > 4) {
      
    } else if (any(c(g1, g2) == 2) |
               (g1 == g2 & g1 <= 4)) {
      return(c(g1_pool, i))
    } else if (g1 == 12 | g2 == 12) {
      if (g1 == 12) {
        return(c(pool + g1_pool, i))
      }
      return(c(g1_pool, i))
    } else if (any(c(g1, g2) == 10) | any(c(g1, g2) == 11)) {
      pool <- pool * 2
      if (g1 == 10 | g1 == 11) {
        g1_pool <- g1_pool + 0.25 * pool
        pool <- 0.75 * pool
      } else {
        pool <- 0.75 * pool
      }
    }
  }
}

wyniki <- c()
ii <- c()
for(i in 1:100000) {
  g <- game()
  wyniki <- append(wyniki, g[1])
  ii <- append(ii, g[2])
}
mean(wyniki)
max(wyniki)
max(ii)
