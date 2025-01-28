# @ : Youssef AIT ALI
# Seconde year bachelor at Nice University (MASS)

# Monte Carlo simulation

# 1- POLYGON

creer_polygone <- function (x,y) {
  matrix(c(x, x[1], y, y[1]), ncol=2,dimnames=list(c(), c("x","y")))
}

square <- creer_polygone(c(10,10,90,90), c(30, 70, 70, 30))
baterfly <- creer_polygone(c(10,90,10,90), c(30,70,70,30))
diamond <- creer_polygone(c(50,10,50,90),c(30,50,70,50))

# Drawing the polygon
plot(square, type = "l")
lines(baterfly-1, type = "b", col="firebrick")
lines(diamond, type="l", col="darkblue")

# 2- POLYGON GENERATOR

# Regular polygon

reg_poly <- function(n , r = 1){
  
  # This function return a data frame
  # with the corrdonnes of a regular polygon
  # centred around a circle of r = 1
  if(n <= 2 || r <= 0){
    stop("The polygone must have more than 2 points")
  }
  angles <- seq(0, 2 * pi, length.out = n + 1)
  
  x <- r * cos(angles)
  y <- r * sin(angles)
  
  return(data.frame(x, y))
  
}

#---------- Test reg_poly function
plot(reg_poly(3), type = "l")

# Surprise

x <- c(0,0,9,11,11,9,8,11,9,6,3,3,8,9,9,8,2,2)
y <- c(0,12,12,10,7,5,5,0,0,5,5,7,7,8,9,10,10,0)
surprise <- creer_polygone(x,y)
plot(surprise,col="black", type="l")

# 3- AREA APPROXIMATION

# -------------- #Uniform distribution# -------------- #
boite <- function(polygon){
  
  # this function return a data frame with
  # the max min of X and y
  
  # output : 2x2 matrix
  
  if(is.null(dim(polygon)) || nrow(polygon) < 3){
    stop("The polygone must have at less 3 points")
  }
  x <- c(min(polygon[,1]), max(polygon[,1]))
  y <- c(min(polygon[,2]), max(polygon[,2]))
  
  return(data.frame(x, y))
}

#---------- Test boite function
boite(baterfly)
# -------------------------------------------------- #

# --------------- make random points based on --------------- #
# --------------- the rectangle aroud the polygon ------------#
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
# ----------------------------------------------------------- #

# Points are inside the polygon ? #
# ----------------> Start
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
# ----------------> End

# ----------------> Start Function Test

carre <- creer_polygone(c(0, 0, 1, 1), c(0, 1, 1, 0))
cc <- seq(from=-0.25,to=1.25,by=0.25)
points <- do.call(rbind,lapply(cc, FUN=cbind, cc,deparse.level = 0))
pin <- appartient(points,carre);


## Dessiner le résultat du test
par(mar=c(2,2,3,2)+0.1)
plot(carre, type='l', main="Test de la fonction appartient", xlim=range(carre[,1],points[,1]), ylim=range(carre[,2],points[,2]))
points(points[pin,1], points[pin,2], col='firebrick', pch=20)
points(points[!pin,1], points[!pin,2], col='darkblue', pch=20)
# ----------------> End Function Test

# Monte Carlo simulation to compute the polygon area
# ----------------> Start
mc.poly <- function(n, polygon)
{
  # defined our rectangle
  rectangle <- boite(polygon)
  
  # generate random point
  random_points <- points_aleatoires(n, rectangle)
  
  # verify if this points are inside the polygon
  pin <- appartient(random_points,polygon)
  
  # the area of the rectangle
  ractangle_air <- (rectangle[2, 1] - rectangle[1, 1]) * (rectangle[2, 2] - rectangle[1, 2])
  
  return((sum(pin) / n) * ractangle_air)
}

mc.poly_draw <- function(n, polygon)
{
  # defined our rectangle
  rectangle <- boite(polygon)
  
  # generate random point
  random_points <- points_aleatoires(n, rectangle)
  
  # verify if this points are inside the polygon
  pin <- appartient(random_points,polygon)
  
  # the air of the rectangle
  ractangle_air <- (rectangle[2, 1] - rectangle[1, 1]) * (rectangle[2, 2] - rectangle[1, 2])
  
  plot(polygon, type='l', main="Test de la fonction appartient", xlim=range(polygon[,1],random_points[,1]), ylim=range(polygon[,2],random_points[,2]))
  points(random_points[pin,1], random_points[pin,2], col='firebrick', pch=20)
  points(random_points[!pin,1], random_points[!pin,2], col='darkblue', pch=20)
  
  return((sum(pin) / n) * ractangle_air)
}
# ----------------> End

# ----------------> start Function Test
par(mfrow = c(3, 1))

mc.poly_draw(10, diamond)
mc.poly_draw(100, diamond)
mc.poly_draw(10000, diamond)
# ----------------> End Function Test

# How to compute the exact areat of a polygon ? #

# ----------------> Start
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
# ----------------> End
# ----------------> Start Test
# We compute the area using monte carlo simulation and we compaire to the exact
# area of the polygon

Area_simulation<- function(n_col, n_ligne, polygon_point)
{
  Simu_Air <- matrix(0, n_ligne, n_col)
  
  polygon <- reg_poly(polygon_point)
  
  Exact_air <- air_poly(polygon)
  
  times <- numeric(n_col)
  
  for(i in 1:n_col)
  {
    n = 10**i
    start_run <- Sys.time()
    Simu_Air[, i] <- replicate(n_ligne, mc.poly(n, polygon))
    end_run <- Sys.time()
    
    times[i] <- as.numeric(end_run - start_run)
  }
  
  ERR_Polygon <- abs((Simu_Air / Exact_air) - 1) * 100
  result = list(Estimation = Simu_Air, Times = times, Error = ERR_Polygon)
  return(result)
}

res_sim <- Area_simulation(50, 4, 6)

boxplot(ERR_Polygon, main='Relative error on PI (10**points)', log='y', xlab='#points', ylab='Rel. Error')

