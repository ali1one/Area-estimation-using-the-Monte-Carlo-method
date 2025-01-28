# @ : Youssef AIT ALI
# Seconde year bachelor at Nice University (MASS)
# 

# Goal : Simulate an approximation value for PIE using Monte Carlo method

#library(ggplot2)

rm(list = ls())
set.seed(123)

Circle <- function(n, r = 1)
{
  # Example of Monte Carlo simulation for a circle
  # This example will help as to understand how we can
  # compute an approximate value of pie
  
  # n ; Number of iteration (simulation)
  # r : is a the circle ray
  
  # ---> 1 - Let's draw a circle
  angles <- seq(0, 2 * pi, length.out = n + 1)
  
  x1 <- r * cos(angles)
  y1 <- r * sin(angles)
  
  # ---> 2 - Generate a couple of random points
  
  x_random <- runif(n, -r, r)
  y_random <- runif(n, -r, r)
  
  # ---> 3 - Verify if our couple points are inside the circle
  
  # This will give us a list of position where (x,y) couple are inside the circle
  inside_circle <- which(sqrt(x_random^2 + y_random^2) <= r)
  
  # Visualization
  plot(x1, y1, type ="l") # Show the circle
  points(x_random[inside_circle], y_random[inside_circle], col='firebrick', pch=20) # Show the points inside the circle
  points(x_random[-inside_circle], y_random[-inside_circle], col='darkblue', pch=20) # Show the points outside the circle
  
}

## Observation #################################################################
# After this example, we can see clairly how we can compute
# the pie value approximatly
# we can see that we create a circle inside a square
# and by showing the point inside and outside the circle
# we can find a relation ship that help us to get an estimation
# of pie value.
# (area of circle / area square) = π∗r^2 / 4∗r^2 = π/4
# π ≈ 4∗ (simulated points within circle / total number of simulated points)
################################################################################

mc.pi <- function(n, r = 1)
  
  # The function is a simulation of monte carlo simulation
  # to compute an approximation of pie value
  
  # param : 
  # n ; Number of iteration (simulation)
  # r : is a the circle ray
{
  if(n <=1)
  {
    stop("The simulation number must be greater than 1")
  }
  
  cords <- matrix(runif(2*n, 0, r), ncol = 2)
  
  distances <- cords[, 1]**2 + cords[, 2]**2
  
  inside_circle <- sum(distances <= r)
  
  return((inside_circle/ n) * 4)
}

# PIE simulation

PIE_simulation<- function(n_ligne, n_col)
{
  # this function help us to understand more the Monte Carlo
  # simulation.
  # the goal is to create a matrix and times running vector
  # to visualize the evolution of pie estimation
  
  # param
  # n_ligne :  number of time we simulate the pie value
  # n_col : number of points used for the simulation
  
  PIE <- matrix(0, n_ligne, n_col)
  times <- numeric(n_col)
    
  for(i in 1:n_col)
  {
    start_run <- Sys.time()
    PIE[,i] <- replicate(n_ligne, mc.pi(10**i))
    end_run <- Sys.time()
    times[i] <- as.numeric(end_run - start_run)
  }
  
  ERR <- abs(PIE / pi - 1) * 100
  
  results <- list(estimation = PIE, tE = times, error = ERR)
  
  return(results)
}

## Observation #################################################################
# Using more points brings our estimation closer to the real value of pi, but 
# it also results in longer execution times.
################################################################################

# Implementing result

resultat_PIE_Simulation <- function(n_iteration_points)
{
  df <- data.frame(iteration<- 1:n_iteration_points)
  
  #df$iteration <- 1:n_iteration_points
  
  df$x <- runif(n_iteration_points, 0, 1)
  df$y <- runif(n_iteration_points, 0, 1)
  
  df$inside_circle <- ifelse(sqrt(df$x^2 + df$y^2) <= 1, 1, 0)
  
  df$PIE <- 4 * cumsum(df$inside_circle) / df$iteration
  
  df$ERR <- abs(df$PIE / pi - 1)
  
  colnames(df)[colnames(df) == colnames(df)[1]] <- "Iteration"
  
  return(df)
  
}

## main #################################
# ---> Circle visualization
par(mfrow = c(3, 1))
Circle(50)
Circle(500)
Circle(5000)

# ---> PIE estimation
points_iteration = c(10, 100, 1000, 10000, 100000, 1000000, 10000000)
res <- sapply(points_iteration, function(n) mc.pi(n))
names(res) <- points_iteration
par(mfrow = c(1,1))
plot(points_iteration, res, type = "l") # 

# ---> PIE simulation Monte Carlo
n_ligne = 200
n_col = 6

Simulation_resultat <- PIE_simulation(n_ligne, n_col) # We get our simulation

PIE <- Simulation_resultat$estimation # Defining our observation matrix

tE <- Simulation_resultat$tE # Execution time

ERR <- Simulation_resultat$error # Error between the real value of pi and our simulation matrix

# ---> ERROR AND SPEED VISUALIZATION
par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1)
boxplot(ERR, main='Relative error on PI (10**points)', log='y', xlab='#points', ylab='Rel. Error')
plot(10 ** (1:n_col), tE, type='b', main='the mean time of a simulation', log='x', xlab='#points', ylab='Time (seconde)')

# ---> Data set PIE Simulation
res <- resultat_PIE_Simulation(10**6)

ggplot(res, aes(x = Iteration, y = PIE)) +
  geom_line(col = "blue") +
  geom_hline(yintercept = pi) +
  ylim(c(3, 3.3)) +
  labs(title = expression(paste("Approximation of ", pi)),
       x = "number of points",
       y = expression(paste("estimated value of ",pi)))

# ---> Save data
col_names_for_data <- 10^(1:6)

PIE <- as.data.frame(PIE)
ERR <- as.data.frame(ERR)

colnames(PIE) <- col_names_for_data
colnames(ERR) <- col_names_for_data

write.csv2(PIE, file = "PIE.csv", row.names = FALSE)
write.csv2(ERR, file = "ERR.csv", row.names = FALSE)
write.csv2(res, file = "MC_simulation_for_PIE.csv", row.names = FALSE)