## Function for tangling raster objects
## Given an existing tangles object, raster objects are transformed accordingly


## Inputs
# raster_data: raster object (works with rasters and raster stacks)
# tanglerInfo: object the was saved from the tangle function that has the steps and parameters need to untangle.

## Outputs
# 1. The transformed raster object (same data, but transfomed coordinates)

tangler_raster<- function(raster_object=NULL, tanglerInfo = NULL){
  
  ###### Internalised Step Functions
  ## Step 1 (shifting X)
  leap_Xr<- function(xyData=NULL, r.num=NULL){
    xyData[,1]<- xyData[,1] + r.num
    return(xyData)
  }
  ## End Step 1 (shifting X)
  
  ## Step 2 (shifting Y)
  leap_Yr<- function(xyData=NULL, r.num=NULL){
    xyData[,2]<- xyData[,2] + r.num
    return(xyData)
  }
  ## End Step 2 (shifting Y)
  
  ## Step 3 (data rotation)
  rotate_XYr<- function(xyData=NULL, deg=NULL, origin.point=NULL){
    
    ## Prep data for rotation
    x<- t(xyData[,1])
    y<- t(xyData[,2])
    v = rbind(x,y)
    
    x_center = origin.point[1]
    y_center = origin.point[2]
    
    #create a matrix which will be used later in calculations
    center <-  v
    center[1,]<- as.matrix(x_center)
    center[2,]<- as.matrix(y_center)
    
    theta = (deg * pi)/180      # express in radians
    
    R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
    
    # do the rotation...
    s = v - center    # shift points in the plane so that the center of rotation is at the origin
    so = R%*%s           # apply the rotation about the origin
    vo = so + center   # shift again so the origin goes back to the desired center of rotation
    
    # pick out the vectors of rotated x- and y-data
    xyData<- cbind(vo[1,], vo[2,])
    return(xyData)
  }
  ## END Step 3 (data rotation)
  
  
  ########################################## END internalised step functions
  
  ## convert raster object to table format
  tempD <- data.frame(cellNos = seq(1:ncell(raster_object)))
  vals <- as.data.frame(getValues(raster_object))
  tempD<- cbind(tempD, vals)
  tempD <- tempD[complete.cases(tempD), ]
  cellNos <- c(tempD$cellNos)
  gXY <- data.frame(xyFromCell(raster_object, cellNos, spatial = FALSE))
  #tempD<- cbind(gXY, tempD)
  
  ##
  xyData<- as.matrix(gXY)
  
  ## do the spatial transfomrations
  for (i in 1:nrow(tanglerInfo$unpicker)){
    seq.step<- tanglerInfo$unpicker$step[i]
    
    # if step 1
    if(seq.step == 1){
      step1.out<- leap_Xr(xyData = xyData , r.num = tanglerInfo$unpicker$leap_dist[i])
      # save outputs
      xyData<- step1.out}
    
    # if step 2
    if(seq.step == 2){
      step2.out<- leap_Yr(xyData = xyData, r.num = tanglerInfo$unpicker$leap_dist[i])
      # save outputs
      xyData<- step2.out}
    
    # if step 3
    if(seq.step == 3){
      step3.out<- rotate_XYr(xyData = xyData, deg = tanglerInfo$unpicker$degree[i], origin.point = c(tanglerInfo$unpicker$origin_X[i], tanglerInfo$unpicker$origin_Y[i]))
      # save outputs
      xyData<- step3.out}}
  
  xyData<- as.data.frame(xyData)
  names(xyData)<- c("X", "Y")
  
  # join to covariates
  tempD<- cbind(xyData, tempD)
  head(tempD)
  
  # rasterise
  if(nlayers(raster_object) == 1){
    ras1<- rasterFromXYZ(tempD[,c(1,2,4)],digits=1)
  }
  
  
  
  hash.out<- tanglerInfo$hash
  ## Need capture output to save hash key to a readme file
  
  # write revised coordinates to file
  nm2<- paste0(getwd(), "/tanglerXY_", hash.out, ".rds")
  saveRDS(object = xyData, file = nm2)

  return(xyData)}
  
#### END

## test it out
setwd("Z:/Dropbox/2019/rmuddles/deIDent/")
library(ithir);library(digest);library(raster)
data("hunterCovariates")
raster_object<- hunterCovariates

str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
xyData<- as.matrix(dat.xy)
detangler.dat<- readRDS("detangler_94d5dc62251dc75932a791eda6b756b2d0b4400606375fc79adb13742c1c6365.rds")
str(detangler.dat)

plot(hunterCovariates[[1]])
tangled.ras<- tangler_raster(raster_object = hunterCovariates, tanglerInfo = detangler.dat)

#test
t.dat<- readRDS("tangledXY_7cf86c89cca6207f3eff300e550d87fea09b076d090345edd77b8d8fc7fb0aaa.rds")
plot(t.dat, cex=0.23)
points(tangled.origi, col="red")  





