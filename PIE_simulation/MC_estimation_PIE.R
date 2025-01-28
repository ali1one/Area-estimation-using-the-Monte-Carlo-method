set.seed(123)

mc.pi_1 <- function(n)
{
  if(n <= 1)
  {
    stop("Error")
  }
  
  x <- runif(n, min = 0, max = 1)
  y <- runif(n, min = 0, max = 1)
  
  inside <- ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
  
  inside <- sum(inside)
  
  resultat <- (inside / n) * 4
  
  return(resultat)
}
no_of_iterations <- c(10, 100, 1000, 10000, 100000, 1000000)
res <- lapply(no_of_iterations, mc.pi_1)
names(res) <- no_of_iterations
res
mc.pi_2<- function(n)
{
  df <- data.frame(x <- runif(n, min = 0, max = 1),
                   y <- runif(n, min = 0, max = 1))
  df$points <- 1:n
  
  df$inside = ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
  
  df$PIE <- 4 * (cumsum(df$inside) / df$points)
  
  df$ERR <- abs(df$PIE/pi - 1)
  
  return(df)
}



####################################

n = 10000

x<- runif(n, -1, 1)
y<- runif(n, -1, 1)

vec <- which(sqrt(x^2 + y^2) <= 1)
vec1 <- which(sqrt(x^2 + y^2) > 1)
r <- 1

angles <- seq(0, 2 * pi, length.out = n + 1)

x1 <- r * cos(angles)
y1 <- r * sin(angles)

df <- data.frame(x1, y1)

#par(mfrow = c(1, 1))
plot(df$x1, df$y1, type = "l")
points(x[vec], y[vec], col='firebrick', pch=20)
points(x[-vec], y[-vec], col='darkblue', pch=20)
