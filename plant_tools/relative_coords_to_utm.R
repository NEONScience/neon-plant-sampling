# the unit circle, distance/azimuth problem

radians = function(degrees) {
  rad = (degrees*pi)/180
  return(rad) 	
}

deg = function(radians) {
  degrees = (radians*180)/pi
  return(degrees)
}

plot(sin(radians(0:360)), type = "l")
lines(cos(radians(0:360)), type = "l", col = "blue")

##########################################
########################################## This is the basic logic of converting relative x,y to Utm_E/Utm_N
##########################################
############ azimuth 0 = true north = "up"
# # initial data set, points all around the origin - two points 180 degrees apart to test visually
rel = data.frame(azimuth = c(0,45,275,180, 95, 45, 225, 225), distance = c(2,10,9.3,12, 5, 2.82, 5, 10))
# 
# calculate x,y coordinates from distance and radians angle
rel$x = with(rel, distance*sin(radians(azimuth))) # sin() gives the x coord because the unit circle is rotated 90 degrees
rel$y = with(rel, distance*cos(radians(azimuth)))

##########################################
##########################################
##########################################

# plot it out
plot(rel$x,rel$y, xlim=c(-10,10), ylim=c(-10,10), main = "(x,y) = (sin, cos)")
abline(h=0)
abline(v=0)
abline(0,1) # 1:1 slope line
points(0,5, col ="red", pch=2) # north
# ############

############
# # initial data set, points all around the origin - two points 180 degrees apart to test visually
rel2 = data.frame(azimuth = c(0,45,275,180, 95, 45, 225, 225), distance = c(2,10,9,12, 5, 3, 5, 10))
# 
# calculate x,y coordinates from distance and radians angle
rel2$x = with(rel2, distance*cos(radians(azimuth))) # sin() gives the x coord because the unit circle is rotated 90 degrees
rel2$y = with(rel2, distance*sin(radians(azimuth))) 

# plot it out
plot(rel2$x,rel2$y, xlim=c(-10,10), ylim=c(-10,10), main = "(x,y) = (cos, sin)")
abline(h=0)
abline(v=0)
abline(0,1) # 1:1 slope line
points(0,5, col ="red", pch=2) # north (x = 0, y = 5)

#############
## Draw a unit circle; radius = 5
############ 
radius = 5
azi <- seq(0,360,by = 5)
dis <- rep(radius, length(azi))

rx <- dis*cos(radians(azi))
ry <- dis*sin(radians(azi))

plot(rx,ry, type = "l")
abline(h=0);abline(v=0)


############## SCENARIO -- one point with coordinates (2, -2)
# if point = (x,y) = (2, -2) i.e. quadrant 2 i.e. lower-right hand corner
# sqrt(2^2 + 2^2) = distance from origin = hypotenuse
d = 5
# # calculate coordinates, as angle from x-axis -- have to flip the sign of sin() else it returns a positive y-coordinate
# x1 = cos(radians(45))*d # x-coordinate
# y1 = -sin(radians(45))*d # y-coordinate, result needs to be multiplied by -1 to put it in the right place
# points(x1,y1, col = "red") # plot out the computed point
# # 
# # calculate coordinates, angle from y-axis
# x2 = cos(radians(135))*d # x-coordinate -- this gives a negative x-coordinate, which is WRONG
# y2 = sin(radians(135))*d # y-coordinate -- this gives a positive y-coordinate, which is WRONG
# points(x2,y2, col = "red") # plot out the computed point, it's in the opposite quadrant now

# calculate coordinates as the angle from the y-axis, but flip the sin/cos functions -- no need to flip signs or functions
x3 = sin(radians(135))*d # x-coordinate -- this gives a negative x-coordinate, which is CORRECT
y3 = cos(radians(135))*d # y-coordinate -- this gives a positive y-coordinate, which is CORRECT
points(x3,y3, col = "blue", pch = 2, cex = 1) # plot out the computed point, it's in the correct quadrant

# calculate the same coordinate but in quadrant 4, as the total angle from the y-axis -- no need to flip signs or functions
x4 = sin(radians(15))*d # x-coordinate -- this gives a negative x-coordinate, which is correct
y4 = cos(radians(15))*d # y-coordinate -- this gives a positive y-coordinate, which is correct
points(x4,y4, col = "blue", pch = 2, cex = 1) # plot out the computed point, this is in the correct quadrant

# calculate coordinate (-2, 2) but in quadrant 4, as the angle from the x-axis -- have to flip signs
x5 = -cos(radians(45))*d # x-coordinate -- this gives a negative x-coordinate, which is correct
y5 = sin(radians(45))*d # y-coordinate -- this gives a positive y-coordinate, which is correct
points(x4,y4, col = "darkgreen", pch = 2, cex = 4) # plot out the computed point, also in the correct quadrant




