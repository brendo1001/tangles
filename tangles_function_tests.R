## test it out
# source the functions
## test it out with point data
# tangles
setwd("/home/malone/Dropbox/2019/rmuddles/deIDent/")
library(ithir);library(digest)
data("HV_subsoilpH")
str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
xyData<- as.matrix(dat.xy)
tangles.out<- tangles(data = xyData, depth = 3, rasterdata = FALSE, raster_object = FALSE)
tangles.out  
str(tangles.out)  
head(tangles.out[[1]])
# tangler
tangled.origi<- tangler(data = as.matrix(tangles.out[[1]]), tanglerInfo = tangles.out[[2]], raster_object = FALSE, stub = "hv1")


data("HV_subsoilpH")
str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
xyData<- as.matrix(dat.xy)
detangler.dat<- readRDS("detangler_1465351c42949cc8db6d6d3e3f15e65a39211560207d4c12607aaafe7dfa8f1b.rds")
str(detangler.dat)

plot(xyData)
tangled.origi<- tangler(data = xyData, tanglerInfo = detangler.dat, raster_object = FALSE, stub = "hv1")
head(tangled.origi)



## test it out with raster data
library(raster)
data("hunterCovariates_sub")
str(hunterCovariates_sub)
raster_object<- hunterCovariates_sub
tangles.out<- tangles(data = hunterCovariates_sub, depth = 10, rasterdata = TRUE, raster_object = TRUE)
tangles.out  
str(tangles.out) 

#tangler
# tangler
data("hunterCovariates_sub")
str(hunterCovariates_sub)
detangler.dat<- readRDS("detangler_77f23ff80a5320acda37bd03085f9e91dfadce3a66ce955928cba757a1d17964.rds")
str(detangler.dat)

tangled.origi<- tangler(data = hunterCovariates_sub, tanglerInfo = detangler.dat, raster_object = TRUE, stub = "hv1")
tangled.origi
plot(tangled.origi[[1]])


## Tangle points then rasters
# plot original points
plot(hunterCovariates_sub[[1]])
plotxydat<- as.data.frame(xyData)
coordinates(plotxydat)<- ~ X + Y
plot(plotxydat,add=T)

# tangle the points
data("HV_subsoilpH")
str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
xyData<- as.matrix(dat.xy)
tangles.out<- tangles(data = xyData, depth = 5, rasterdata = TRUE, raster_object = FALSE) 
tangles.out  

# tangle raster data based on tangled points
data("hunterCovariates_sub")
str(hunterCovariates_sub)


tangled.origi<- tangler(data = hunterCovariates_sub, tanglerInfo = tangles.out[[2]], raster_object = TRUE, stub = "hv1")
tangled.origi
plot(tangled.origi[[1]])

# overlay tangle points
tp<- as.data.frame(tangles.out[[1]])
coordinates(tp)<- ~ X + Y
plot(tp, add=T)


## detangles
# points 
xyData<- as.matrix(tangles.out[[1]])
point_detang<- detangles(data=xyData, tanglerInfo=tangles.out[[2]], raster_object = FALSE, stub = "hv_fix", hash_key = "9ec140a1ed5ff8f80cbd10c7f92abe6a595d2b6ea85c719c3593d0ccc693c179")

#rasters
raster_detang<- detangles(data=tangled.origi, tanglerInfo=tangles.out[[2]], raster_object = TRUE, stub = "hv_fix", hash_key = "9ec140a1ed5ff8f80cbd10c7f92abe6a595d2b6ea85c719c3593d0ccc693c179")
plot(raster_detang[[1]])

pdat<- as.data.frame(point_detang)
coordinates(pdat)<- ~X + Y
plot(pdat, add=T)



## POINT DATA tangles
library(ithir);library(digest)
data("HV_subsoilpH")
str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
xyData<- as.matrix(dat.xy)
# deidentify with 5 levels of abstraction
tangles.out<- tangles(data = xyData, depth = 5, rasterdata = TRUE, raster_object = FALSE)


# tangle associated RASTER DATA
library(raster)
data("hunterCovariates_sub")
tangled.origi<- tangler(data = hunterCovariates_sub, tanglerInfo = tangles.out[[2]], raster_object = TRUE, stub = "hv1")

# PLOT real
par(mfrow=c(1,2))
plot(hunterCovariates_sub[[1]])
coordinates(dat.xy)<- ~ X + Y
plot(dat.xy, add=T)

# PLOT deidentified
plot(tangled.origi[[1]])
ndat.xy<- as.data.frame(tangles.out[[1]])
coordinates(ndat.xy)<- ~ X + Y
plot(ndat.xy, add = T)
