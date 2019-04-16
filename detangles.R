## Function for untangling transformed coordinates back to original XY coordinates
## Reverses the steps of the tangle function



## Inputs
# tranData: 2 column MATRIX of transformed spatial coordinates
# tanglerInfo: object the was saved from the tangle function that has the steps and parameters need to untangle.

## Outputs
# The original spatial point pattern


detangles<- function(xyData=NULL, tanglerInfo=NULL){
  
  ###### Internalised Step Functions
  ## Step 1 (shifting X)
  leap_Xba<- function(xyData=NULL, r.num=NULL){
    xyData[,1]<- xyData[,1] - r.num
    return(xyData)}
  ## End Step 1 (shifting X)
  
  ## Step 2 (shifting Y)
  leap_Yba<- function(xyData=NULL, r.num=NULL){
    xyData[,2]<- xyData[,2] - r.num
    return(xyData)}
  ## End Step 2 (shifting Y)
  
  ## Step 3 (data rotation)
  rotate_XYba<- function(xyData=NULL, deg=NULL, origin.point=NULL){
    
    ## Prep data for rotation
    x<- t(xyData[,1])
    y<- t(xyData[,2])
    v = rbind(x,y)
    
    #origin point
    x_center = origin.point[1]
    y_center = origin.point[2]
    
    #create a matrix which will be used later in calculations
    center <-  v
    center[1,]<- as.matrix(x_center)
    center[2,]<- as.matrix(y_center)
    
    bdeg<- 360 - deg  # choose a random orientation
    theta = (bdeg * pi)/180      # express in radians
    
    R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
    
    # do the rotation...
    s = v - center    # shift points in the plane so that the center of rotation is at the origin
    so = R%*%s           # apply the rotation about the origin
    vo = so + center   # shift again so the origin goes back to the desired center of rotation
    
    # pick out the vectors of rotated x- and y-data
    xyData<- cbind(vo[1,], vo[2,])
    return(xyData)}
  ## END Step 3 (data rotation)
  
  
  ########################################## END internalised step functions
  
  
  
  ## cycle through the step sequence
  for (i in 1:nrow(tanglerInfo$unpicker)){
    jp<- nrow(tanglerInfo$unpicker) - (i-1)
    seq.step<- tanglerInfo$unpicker$step[jp]
    
    # if step 1
    if(seq.step == 1){
      step1.out<- leap_Xba(xyData = xyData , r.num = tanglerInfo$unpicker$leap_dist[jp])
      # save outputs
      xyData<- step1.out}
    
    # if step 2
    if(seq.step == 2){
      step2.out<- leap_Yba(xyData = xyData, r.num = tanglerInfo$unpicker$leap_dist[jp])
      # save outputs
      xyData<- step2.out}
    
    # if step 3
    if(seq.step == 3){
      step3.out<- rotate_XYba(xyData = xyData, deg = tanglerInfo$unpicker$degree[jp], origin.point = c(tanglerInfo$unpicker$origin_X[jp], tanglerInfo$unpicker$origin_Y[jp]))
      # save outputs
      xyData<- step3.out}}
  
  xyData<- as.data.frame(xyData)
  names(xyData)<- c("X", "Y")
  
  hash.out<- tanglerInfo$hash
  ## Need capture output to save hash key to a readme file
  
  
  # write revised coordinates to file
  nm2<- paste0(getwd(), "/detangledXY_", hash.out, ".rds")
  saveRDS(object = xyData, file = nm2)

  return(xyData)}
  
#### END

## test it out
setwd("Z:/Dropbox/2019/rmuddles/deIDent/")
library(ithir);library(digest)
data("HV_subsoilpH")
str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
plot(dat.xy)
xyData<- as.matrix(dat.xy)
tangles.out<- tangles(xyData = xyData, depth = 3)
tangles.out
tangles.out[[2]$`hash`]

##detangles
tangled.dat<- readRDS("tangledXY_ca24e91b5a6d8ea35abd660692413d7a303e3fefadf20ca9b499f5a7490e6180.rds")
detangler.dat<- readRDS("detangler_ca24e91b5a6d8ea35abd660692413d7a303e3fefadf20ca9b499f5a7490e6180.rds")
str(detangler.dat)
xyData<- as.matrix(tangled.dat)
#plot(xyData)
#origXY<- detangles(xyData = xyData, tanglerInfo = detangler.dat)

# test
#plot(dat.xy, cex=0.25)
#points(origXY, col="red")


