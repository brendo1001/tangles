## Function for tangling original XY coordinates
## There are 3 types of transformation:
# Shifting up or down (step1)
# Shifting left ot right (step2)
# Rotating coordinates about an origin (step3)
# Given a selected number of depths, a step sequence is randomised
# Spatial coordinates are transformed at each step and are then used as input for the following step
# until we have reached the maximum depth. 

## Inputs
# xyData: 2 column MATRIX of spatial coordinates
# depth: the number of transformations to perfrom. Default is three

## Outputs
# A list object that contains
# 1. The transformed coordinates
# 2. A seperate list object to be used for untangling the transformed coordinates
# The outputs are written to file to the working direcotry with file stub manes of tangledXY and detangler respectively.
# These files have a commmon hash key as part of their filename.
# The hash key is generated from the detangler object using the sha256 hash algorithm

tangles<- function(xyData=NULL, depth=3){
  
  ###### Internalised Step Functions
  ## Step 1 (shifting X)
  leap_X<- function(xyData=NULL){
    r.num<- sample(-999999:999999, 1)
    xyData[,1]<- xyData[,1] + r.num
    return(list(xyData, r.num))
  }
  ## End Step 1 (shifting X)
  
  ## Step 2 (shifting Y)
  leap_Y<- function(xyData=NULL){
    r.num<- sample(-999999:999999, 1)
    xyData[,2]<- xyData[,2] + r.num
    return(list(xyData, r.num))
  }
  ## End Step 2 (shifting Y)
  
  ## Step 3 (data rotation)
  rotate_XY<- function(xyData=NULL){
    # pick a point at random from the dataset
    row.sample<- sample(1:nrow(xyData),1)
    origin.point<- xyData[row.sample,]
    
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
    
    deg<- sample(1:359,1, replace = F) # choose a random orientation
    theta = (deg * pi)/180      # express in radians
    
    R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
    
    # do the rotation...
    s = v - center    # shift points in the plane so that the center of rotation is at the origin
    so = R%*%s           # apply the rotation about the origin
    vo = so + center   # shift again so the origin goes back to the desired center of rotation
    
    # pick out the vectors of rotated x- and y-data
    xyData<- cbind(vo[1,], vo[2,])
    return(list(xyData,origin.point,deg))
  }
  ## END Step 3 (data rotation)
  
  ## letters and numbers randomisation
  
  ########################################## END internalised step functions
  
  ## randomise the sequence of steps with defined depth
  step.random<- sample(1:3, depth, replace = T)
  
  ## cycle through the step sequence
  s3.cnt<- 1
  seq.mat<- matrix(NA, nrow = depth, ncol = 6)
  for (i in 1:depth){
    seq.step<- step.random[i]
    
    # if step 1
    if(seq.step == 1){
      step1.out<- leap_X(xyData = xyData)
      # save outputs
      xyData<- step1.out[[1]]
      seq.mat[i,1]<- seq.step
      seq.mat[i,2]<- step1.out[[2]]}
    
    # if step 2
    if(seq.step == 2){
      step2.out<- leap_Y(xyData = xyData)
      # save outputs
      xyData<- step2.out[[1]]
      seq.mat[i,1]<- seq.step
      seq.mat[i,2]<- step2.out[[2]]}
    
    # if step 3
    if(seq.step == 3){
      step3.out<- rotate_XY(xyData = xyData)
      # save outputs
      xyData<- step3.out[[1]]
      seq.mat[i,1]<- seq.step
      seq.mat[i,3:4]<- step3.out[[2]]
      seq.mat[i,5]<- step3.out[[3]]
      seq.mat[i,6]<- s3.cnt
      s3.cnt<- s3.cnt + 1}
  }
  
  seq.dat<- as.data.frame(seq.mat)
  names(seq.dat)<- c("step", "leap_dist", "origin_X", "origin_Y", "degree", "s3_count")
  
  xyData<- as.data.frame(xyData)
  names(xyData)<- c("X", "Y")
  
  # generate a hash
  hash.out<- digest(seq.dat ,"sha256") # first try
  deTangler<- list(hash = hash.out, step_sequence= step.random, unpicker= seq.dat)
  
  # write de-tangler to file
  nm1<- paste0(getwd(), "/detangler_", hash.out, ".rds")
  saveRDS(object = deTangler, file = nm1)
  
  # write revised coordinates to file
  nm2<- paste0(getwd(), "/tangledXY_", hash.out, ".rds")
  saveRDS(object = xyData, file = nm2)

  return(list(xyData, deTangler))}
  
#### END

## test it out
#setwd("/home/malone/Dropbox/2019/rmuddles/deIDent/")
#library(ithir);library(digest)
#data("HV_subsoilpH")
#str(HV_subsoilpH)
#dat.xy<- HV_subsoilpH[,1:2]
#xyData<- as.matrix(dat.xy)
#tangles.out<- tangles(xyData = xyData, depth = 6)
  
  





