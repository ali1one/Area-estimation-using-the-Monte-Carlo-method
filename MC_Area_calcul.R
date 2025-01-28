# @ : Youssef AIT ALI
# Seconde year bachelor at Nice University (MASS)

# Monte Carlo simulation

mc.pi <- function(n) {
  
  # This function return an approximation of the value
  # pi using the monte carlo simulation
  
  if(n <= 0)
  {
    stop("n must be positif number")
  }
  
  inside_cercle = 0
  for(i in 1:n)
  {
    x <- runif(1, 0, 1) #
    y <- runif(1, 0, 1) #
    
    distance <- x**2 + y**2
    
    if(distance <= 1)
    {
      inside_cercle <- inside_cercle + 1
    }
  }
  
  pi <- (inside_cercle / n) * 4
  
  return(pi)
}

#""""""""""""""""""""""""""""""""""""""""""""""""#
mc.pi_optm <- function(n){
  
  # This function is the same as mc.pi
  # but optimized
  
  if(n <= 1)
  {
    strop("")
  }
  
  cords <- matrix(runif(2*n, 0, 1), ncol = 2)
  
  distances <- cords[, 1]**2 + cords[, 2]**2
  
  inside_circle <- sum(distances <= 1)
  
  return((inside_circle/ n) * 4)
}
#""""""""""""""""""""""""""""""""""""""""""""""""#
# we test our function

system.time(mc.pi(1000000))
system.time(mc.pi_optm(1000000))

# PIE ESTIMATION
# we going to create a matrix to save the different estimation of PIE

p <- 6
t <- 50

PIE <- matrix(0, t, p)

for(j in 1:p)
{
  n <- 10**j
  
  for(i in 1:t)
  {
    PIE[i, j] <- mc.pi(n)
  }
}
#""""""""""""""""""""""""""""""""""""""""""""""""#
# Optimization of PIE ESTIMATION
for(i in 1:p)
{
  n = 10**i
  PIE[,i] <- replicate(t, mc.pi_optm(n))
}
#""""""""""""""""""""""""""""""""""""""""""""""""#
# MEAN TIME

tE <- numeric(p)

for(j in 1:p)
{
  n <- 10**j
  times <- numeric(t)
  
  for(i in 1:t)
  {
    start_time <- Sys.time()
    PIE[i, j] <- mc.pi(n)
    end_time <- Sys.time()
    times[i] <- as.numeric(end_time - start_time)
  }
  
  tE[j] <- mean(times)
}
#""""""""""""""""""""""""""""""""""""""""""""""""#
# Optimization of the mean time

tE <- numeric(p)
for(i in 1:p)
{
  n <- 10**i
  times <- numeric(t)
  start_time <- Sys.time()
  PIE[, i] <- replicate(t, mc.pi_optm(n))
  end_time <- Sys.time()
  tE[i] <- as.numeric(end_time - start_time)
}

# ERROR SIMULATION

ERR <- abs(PIE/pi - 1)*100

# ERROR VISUALISATION

par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1)
boxplot(ERR, main='Relative error on PI (10**points)', log='y', xlab='#points', ylab='Rel. Error')
plot(10 ** (1:p), tE, type='b', main='the mean time of a simulation', log='x', xlab='#points', ylab='Time (seconde)')

#------------------ PART 2 ------------------# 

# POLYGON

creer_polygone <- function (x,y) {
  matrix(c(x, x[1], y, y[1]), ncol=2,dimnames=list(c(), c("x","y")))
}

square <- creer_polygone(c(10,10,90,90), c(30, 70, 70, 30))
baterfly <- creer_polygone(c(10,90,10,90), c(30,70,70,30))
diamond <- creer_polygone(c(50,10,50,90),c(30,50,70,50))


plot(carre, type = "l")
lines(baterfly-1, type = "b", col="firebrick")
lines(diamond, type="l", col="darkblue")

# POLYGON GENERATOR

# 1- Regular polygon

reg_poly <- function(n , r = 1){
  
  # This function return a data frame
  # with the corrdonnes of a regular polygon
  # centred around a circle of r = 1
  if(n <= 2 || r <= 0){
    stop("NO")
  }
  angles <- seq(0, 2 * pi, length.out = n + 1)
  
  x <- r * cos(angles)
  y <- r * sin(angles)
  
  return(data.frame(x, y))
  
}

#---------- Test reg_poly function
plot(reg_poly(6), type = "l")

# 2-Polygon suprise
x <- c(0,0,9,11,11,9,8,11,9,6,3,3,8,9,9,8,2,2)
y <- c(0,12,12,10,7,5,5,0,0,5,5,7,7,8,9,10,10,0)
surprise <- creer_polygone(x,y)
plot(surprise,col="black", type="l")

# Approximation

# 1-Step 1
boite <- function(polygon){
  
  # this function return a data frame with
  # the max min of X and y
  
  # output : 2x2 matrix
  
  if(is.null(dim(polygon)) || nrow(polygon) < 3){
    stop("NO")
  }
 x <- c(min(polygon[,1]), max(polygon[,1]))
 y <- c(min(polygon[,2]), max(polygon[,2]))
 
 return(data.frame(x, y))
}

#---------- Test boite function
boite(baterfly)

# 2-Step 2
points_aleatoires <- function(n, bo)
{
  x <- runif(n, bo[1,1], bo[2,1])
  y <- runif(n, bo[1,2], bo[2,2])
  return(matrix(c(x,y), ncol = 2, dimnames = list(c(), c("x", "y"))))
}

#---------- Test points_aleatoires function
bo <- boite(baterfly)
n = 5
points_aleatoires(n, bo)


################
# 
appartient_poly <- function(point, polygone) {
  
  # this function verify is our point is inside the polygon
  # by testing the intersection between the polygon segment and the horizontal half right
  # output : if the number of intersection is impair we return true, false otherwise
  
  n <- nrow(polygone)
  x <- polygone[, "x"]
  y <- polygone[, "y"]
  x0 <- point[1]
  y0 <- point[2]
  
  intersections <- 0
  
  for (i in 1:n) {
    x1 <- x[i]
    y1 <- y[i]
    x2 <- x[(i %% n) + 1]
    y2 <- y[(i %% n) + 1]
    
    if (((y1 <= y0 && y2 > y0) || (y1 > y0 && y2 <= y0)) &&
        (x1 + (y0 - y1) / (y2 - y1) * (x2 - x1) <= x0)) {
      intersections <- intersections + 1
    }
  }
  
  return(intersections %% 2 == 1)
}

# Fonction principale pour vérifier l'appartenance des points au polygone
appartient <- function(points, polygone) {
  n_points <- nrow(points)
  result <- logical(n_points)
  
  for (i in 1:n_points) {
    result[i] <- appartient_poly(points[i, ], polygone)
  }
  
  return(result)
}

################

## Réaliser un test de la fonction
carre <- creer_polygone(c(0, 0, 1, 1), c(0, 1, 1, 0))
cc <- seq(from=-0.25,to=1.25,by=0.25)
points <- do.call(rbind,lapply(cc, FUN=cbind, cc,deparse.level = 0))
pin <- appartient(points,carre);


## Dessiner le résultat du test
par(mar=c(2,2,3,2)+0.1)
plot(carre, type='l', main="Test de la fonction appartient", xlim=range(carre[,1],points[,1]), ylim=range(carre[,2],points[,2]))
points(points[pin,1], points[pin,2], col='firebrick', pch=20)
points(points[!pin,1], points[!pin,2], col='darkblue', pch=20)

# 5.3. Méthode de Monte Carlo et calcul approché de l’aire d’un polygone
mc.poly <- function(n, polygon)
{
  # defined our rectangle
  rectangle <- boite(polygon)
  
  # generate random point
  random_points <- points_aleatoires(n, rectangle)
  
  # verify if this points are inside the polygon
  pin <- appartient(random_points,polygon)
  
  # the air of the rectangle
  ractangle_air <- (rectangle[2, 1] - rectangle[1, 1]) * (rectangle[2, 2] - rectangle[1, 2])
  
  #plot(polygon, type='l', main="Test de la fonction appartient", xlim=range(polygon[,1],random_points[,1]), ylim=range(polygon[,2],random_points[,2]))
  #points(random_points[pin,1], random_points[pin,2], col='firebrick', pch=20)
  #points(random_points[!pin,1], random_points[!pin,2], col='darkblue', pch=20)
  
  return((sum(pin) / n) * ractangle_air)
}

#----------- Test mc.poly function
mc.poly(10, diamond)
mc.poly(100, diamond)
mc.poly(10000, diamond)

# THE EXACT AIR OF POLYGON


air_poly <- function(polygon)
  
  # we forme triangle and we calculate the air of each until we pass all the polygon point
{
  n <- nrow(polygon)
  air <- 0
  
  for(i in 1:n)
  {
    xi <- polygon[i, 1]
    yi <- polygon[i, 2]
    
    if(i < n)
    {
      x1 <- polygon[i + 1, 1]
      y1 <- polygon[i + 1, 2]
    }
    else
    {
      x1 <- polygon[i, 1]
      y1 <- polygon[i, 2] 
    }
    
    air <- air + ((xi * y1) - (x1 * yi))
  }
  
  return(abs(air) / 2)
}

# 7- Simulations : aire approchée versus aire exacte
t <- 50
p <- 5

Simu_Air <- matrix(0, t, p)

polygon <- reg_poly(7)

Exact_air <- air_poly(polygon)

for(i in 1:p)
{
  n = 10**i
  Simu_Air[, i] <- replicate(t, mc.poly(n, polygon))
  Exact_air[, i] <- replicate(t, air_poly(polygon))
}

ERR_Polygon <- abs((Simu_Air / Exact_air) - 1) * 100

#par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1)
boxplot(ERR_Polygon, main='Relative error on PI (10**points)', log='y', xlab='#points', ylab='Rel. Error')
#plot(10 ** (1:p), tE, type='b', main='the mean time of a simulation', log='x', xlab='#points', ylab='Time (seconde)')

# 8- MORE REGULAR POLYGON

reg_poly_1 <- function(n, r = 1, x = 0, y = 0, alpha = 0) {

  angle_rotation <- 2 * pi / n
  
  x1 <- x + r * cos(alpha)
  y1 <- y + r * sin(alpha)
  
  sommets <- matrix(0, n+1, 2)
  sommets[1, ] <- c(x1, y1)
  sommets[n+1, ] <- c(x1, y1)
  
  for (i in 2:n) {
    x_i <- x1 * cos(angle_rotation) - y1 * sin(angle_rotation)
    y_i <- x1 * sin(angle_rotation) + y1 * cos(angle_rotation)
    
    sommets[i, ] <- c(x_i + x, y_i + y)
    
    x1 <- x_i
    y1 <- y_i
  }
  
  return(sommets)
}

test = reg_poly_1(5)
plot(test, type = 'l')
