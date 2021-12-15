

## Building a random walk function from the ground up?
##, need x and y coordinates

# Random Walk Version One

x_start = 0
y_start = 0

n_steps = 1000

step_data = matrix(0, ncol = 2, nrow = n_steps +1)
step_data[1, 1] = x_start
step_data[1,2] = y_start

step_data

for(i in 1:n_steps)
{
  

x_current = step_data[i, 1]
y_current = step_data[i, 2]

x_next = x_current + runif(n = 1, min = -1, max = 1)
y_next = y_current + runif(n = 1, min = -1, max = 1)

step_data[i + 1, 1] = x_next
step_data[i + 1, 2] = y_next

}
# Plot Random Walk

plot(step_data[,1], step_data[,2], type = "l")



## Random Walk Function Version 1

x_start = 0
y_start = 0

n_steps = 1000


r_walk_1 = function(x_start, y_start, n_steps)

{
  
  step_data = matrix(0, ncol = 2, nrow = n_steps +1)
  step_data[1, 1] = x_start
  step_data[1,2] = y_start
  
  for(i in 1:n_steps)
  {
    
    x_current = step_data[i, 1]
    y_current = step_data[i, 2]
    
    x_next = x_current + runif(n = 1, min = -1, max = 1)
    y_next = y_current + runif(n = 1, min = -1, max = 1)
    
    step_data[i + 1, 1] = x_next
    step_data[i + 1, 2] = y_next
    
  }
return(step_data)
}

coords_walk_1 =
  r_walk_1(0,0,1000)
plot(coords_walk_1, type = "l")


## linear function

slope = 1
intercept = 1
x = 1:5

y = slope * x + intercept

y

linear = function(slope, intercept, x) ## order matters within the function
{
  y = slope * x + intercept
  return(y)
}
linear(
  slope = 1,
  intercept = 1, 
  x = 1:5)

require(palmerpenguins)
boxplot(body_mass_g ~ x, data = penguins)

## come back and finish the palmer penguins part

bp_sex = function(dat)
{
  boxplot(
    body_mass_g ~ sex,
    data = droplevels
  )
  
}

