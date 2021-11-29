data("iris")
head(iris)
iris$Sepal.Width
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length, main="A Floral Scatter Plot",
    xlab="Sepal width", ylab="Sepal length")

data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
##abline(v=3.057, 5.84, col="purple")
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
lines(iris$Sepal.Width)
lines=function(x,x1,y1,slope)
lm(x1, y1, slope)
line_point_slope(x, x1, y1, slope)

plot(x = iris$Sepal.Width, y = iris$Sepal.Length, main="A Floral Scatter Plot",
     xlab="Sepal width", ylab="Sepal length")
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.5), 
  add = TRUE)
#####################

c(1, 2, 3)
"c(1, 2, 3)"
