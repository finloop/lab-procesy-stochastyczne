library("mosaicCalc")

F = antiD(
  2/(sigma*sqrt(2*pi)) * 
    exp(
      -x^2/(2*sigma^2)
        ) ~ x)
for (i in seq(4.4,4.5,0.01)) {
  cat(i, F(x = 3, sigma = i), "\n")
}

2*(pnorm(3,sd=4.44) - pnorm(0,sd=4.44))

xx = seq(0, 100,0.1)
yy = F(x = xx, sigma=4.44)
plot(xx, yy, 'l')

#################################################
P <- function(k, p=0.5, n=3) {
  return(choose(n, k)*
           p^(k) * 
           (1-p)^(n-k)
         )
} 
n = 25
k = 1
sum(P(seq(k, n, 1),p=1/10, n=n)) 
