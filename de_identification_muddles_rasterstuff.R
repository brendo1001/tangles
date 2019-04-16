## get a collection of points
library(ithir)
data("hunterCovariates_sub")
str(hunterCovariates_sub)
raster_object<- hunterCovariates_sub

## convert raster object to table format
tempD <- data.frame(cellNos = seq(1:ncell(raster_object)))
vals <- as.data.frame(getValues(raster_object))
tempD<- cbind(tempD, vals)
tempD <- tempD[complete.cases(tempD), ]
cellNos <- c(tempD$cellNos)
gXY <- data.frame(xyFromCell(raster_object, cellNos, spatial = FALSE))

#data("HV_subsoilpH")
#str(HV_subsoilpH)
#xyData<- as.matrix(HV_subsoilpH[,1:2])
#xyData<- xyData[1:15,]

##
xyData<- as.matrix(gXY)
xyData<- xyData[1:1000,]
dat.xy.copy<- xyData



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
origin.point<- c(origin.point[1,])
points(origin.point[1], origin.point[2], col="blue", cex=5)

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

deg<- 270
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
dist(dat.xy.copy[500:550,1:2])
plot(xyData)
plot(dat.xy.copy)
tempE<- cbind(dat.xy.copy, tempD)
str(tempE)
r3<- rasterFromXYZ(tempE[,c(1,2,4)])
plot(r3)


##determine the readings taken from the same location
#MAKE A REGULAR GRID ()
xseq<-seq(min(dat.xy.copy[,1]),max(dat.xy.copy[,1]),26) # 25m res 
yseq<-seq(min(dat.xy.copy[,2]),max(dat.xy.copy[,2]),26)
XX<-rep(xseq,length(yseq))
st<-1
en<-length(xseq)
XX<-as.data.frame(XX)
XX$Y<- 0
for (i in 1:length(yseq)){
  XX$Y[st:en]<- yseq[i]
  st=en+1
  en<- st+(length(xseq)-1)}
XX<-cbind(seq(1,nrow(XX),1), XX) 
names(XX)<- c("FID","X", "Y")
r1<- rasterFromXYZ(XX[,c(2,3,1)])
plot(r1)

tempE<- cbind(dat.xy.copy, tempD)
str(tempE)
coordinates(tempE)<- ~ x + y
str(tempE)
r2<- rasterize(tempE, r1, "Terrain_Ruggedness_Index")
plot(r2)

writeOGR(tempE, "C:/Users/MAL181/Dropbox/2019/rmuddles/deIDent", "test", "ESRI Shapefile")
tempE<- SpatialPoints(tempE)

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

