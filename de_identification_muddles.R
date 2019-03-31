## get a collection of points
library(ithir)
data("HV_subsoilpH")
str(HV_subsoilpH)

dat.xy<- HV_subsoilpH[,1:2]
xyData<- dat.xy

set.seed(666)
#randomisation of step
step.random<- sample(1:3, 3, replace = F)
step.random

### Step 3: Y coordinate leap
y.leap<- sample(-999999:999999, 1)
y.leap

dat.xy.copy[,2]<- (dat.xy.copy[,2] + y.leap)



#### Step 1: rotation randomisation
## get an extent 
x.extent<- c(min(dat.xy.copy[,1]),max(dat.xy.copy[,1]) )
y.extent<- c(min(dat.xy.copy[,2]),max(dat.xy.copy[,2]) )
mid.area<- c((((x.extent[2] - x.extent[1])*0.5)+ x.extent[1]), (((y.extent[2] - y.extent[1])*0.5)+y.extent[1]))
mid.area
plot(dat.xy.copy, col="black", cex=0.5)
points(mid.area[1], mid.area[2], col="red")

## find the mid point (origin)
dists<- spDistsN1(pts = as.matrix(dat.xy.copy), pt = as.matrix(mid.area), longlat = FALSE) # the distance from function
origin.point<- dat.xy.copy[which(dists == min(dists)),]
origin.point
points(origin.point, col="blue")

## do the rotation

# getting prepped for the change of angle
x<- t(dat.xy.copy[,1])
y<- t(dat.xy.copy[,2])
v = rbind(x,y)


x_center = origin.point[1]
y_center = origin.point[2]

#create a matrix which will be used later in calculations
center <-  v
center[1,]<- as.matrix(x_center)
center[2,]<- as.matrix(y_center)
center[1:2,1:10]

deg<- sample(1:359,1, replace = F)
theta = (deg * pi)/180      # express in radians

R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
R

# do the rotation...
s = v - center    # shift points in the plane so that the center of rotation is at the origin
so = R%*%s           # apply the rotation about the origin
vo = so + center   # shift again so the origin goes back to the desired center of rotation

# pick out the vectors of rotated x- and y-data
x_rotated = vo[1,]
y_rotated = vo[2,]
points(x_rotated, y_rotated, col="blue", cex=0.25)

dat.xy.copy[,1]<- x_rotated
dat.xy.copy[,2]<- y_rotated


### Step 2: X coordinate lead
# X leap
x.leap<- sample(-999999:999999, 1)
x.leap
dat.xy.copy[,1]<- dat.xy.copy[,1] + x.leap


plot(dat.xy.copy)


### back-transform to the original cooridnates
# step 3,1,2
# step 2,1,3

# step 2
dat.xy.copy[,1]<- dat.xy.copy[,1] - x.leap


# step 1



# getting prepped for the change of angle
x<- t(dat.xy.copy[,1])
y<- t(dat.xy.copy[,2])
v = rbind(x,y)


x_center = origin.point[1]
y_center = origin.point[2]

#create a matrix which will be used later in calculations
center <-  v
center[1,]<- as.matrix(x_center)
center[2,]<- as.matrix(y_center)
center[1:2,1:10]

b.deg<- 360 - deg 
theta = (b.deg * pi)/180      # express in radians
theta

R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
R

# do the rotation...
s = v - center    # shift points in the plane so that the center of rotation is at the origin
so = R%*%s           # apply the rotation about the origin
vo = so + center   # shift again so the origin goes back to the desired center of rotation

# pick out the vectors of rotated x- and y-data
x_rotated = vo[1,]
y_rotated = vo[2,]

dat.xy.copy[,1]<- x_rotated
dat.xy.copy[,2]<- y_rotated


# step 3
dat.xy.copy[,2]<- (dat.xy.copy[,2] - y.leap)

plot(dat.xy)
points(dat.xy.copy, cex=0.25, col="red")

