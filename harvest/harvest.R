# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "harvest",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9006"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "harvest.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("minVolume", "numeric", 45, 0, NA, "Minimum volume in m^3/ha for eligibility to harvest"),
    defineParameter("minAge", "numeric", 50, 0, NA, "Minimum age to harvest"),
    defineParameter("startTime", "numeric", 0, 0, NA, "Start time"),
    defineParameter("returnInterval", "numeric", 1, 0, 0, "waddya think?"),
    defineParameter("minBlockSize", "numeric", 20, 1, NA, "minimum block size (ha)"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = c("ageMap","yieldTables","ignitionLoci"),
    objectClass = c("RasterLayer","list", "vector"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("disturbanceMap", "noCanGo"),
    objectClass = c("RasterLayer","rasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.harvest = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- harvestInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$harvest$startTime,"harvest","harvest")
    sim <- scheduleEvent(sim, params(sim)$harvest$.plotInitialTime, "harvest", "plot")
    sim <- scheduleEvent(sim, params(sim)$harvest$.saveInitialTime, "harvest", "save")
    
  } else if (eventType == "plot") {
    Plot(sim$noCanGo, legendRange=0:1)
    Plot(sim$disturbanceMap, legendRange=0:2,zero.color="white")
    sim <- scheduleEvent(sim, time(sim)+params(sim)$harvest$.plotInterval, "harvest", "plot")
  } else if (eventType == "save") {
  } else if (eventType == "harvest") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- updateNoGo(sim)
    sim$disturbanceMap[] <- 0  #reset this so it is cummulative over years between plots
    sim$disturbanceMap[sim$ignitionLoci] <- 1 #should safetycheck this
    sim$harvestState$volCut <- 0.0
    
    #sim<-sequentialCut(sim)
    #sim<-sequenceOldest(sim)
    sim<-blockOldest(sim)
    sim$harvestStats$volume <- c(sim$harvestStats$volume,sim$harvestState$volCut)
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    sim <- scheduleEvent(sim, time(sim) + params(sim)$harvest$returnInterval,"harvest","harvest")

  } 
  else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
harvestInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  #should make these data frames, not lists...too awkward
  sim$harvestState<-list(idx=1,volCut=0)
  sim$harvestStats<-list(area=numeric(0),volume=numeric(0),vPerHa=numeric(0))
  
  N<-sim$mapDim
  x<-raster::extent(c(0,N-1,0,N-1))
  sim$disturbanceMap<-raster(x,nrows=N, ncols=N,vals=0)
  setColors(sim$disturbanceMap, n=3) <- c("white", "blue","red")
  
  sim$noCanGo<-raster(x,nrows=N, ncols=N,vals=0)
  setColors(sim$noCanGo, n=2) <- c("white","black")
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

updateNoGo<-function(sim){
  #browser()
  sim$noCanGo[] <- 0
  minAge <- params(sim)$harvest$minAge
  if (!is.na(minAge) && minAge>0){
    bad <- which(sim$ageMap[] < minAge)
    sim$noCanGo[bad]<-1
  }
  minVolume <-  params(sim)$harvest$minVolume
  if (!is.na(minVolume) && minVolume>0){
    #at some point, need to make this object orientd wif methods
    v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
    n<-length(v)
    x<-sim$ageMap[]  #this solves the problem of 0 ages
    x<-ifelse(x==0,1,x)
    x<-ifelse(x<n,x,n)
    x<-v[x]
    bad <- which(x < minVolume)
    sim$noCanGo[bad]<-1
  }
  
  return(invisible(sim))
}


blockOldest <- function(sim){
  x <- order(sim$ageMap[], decreasing=TRUE)
  x <- x[which(sim$noCanGo[x]==0)] 
  v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
  n<-length(v)
  idx <- 1
  while (sim$harvestState$volCut < sim$annualCut ){
    cell <- x[idx]
    if (sim$noCanGo[cell] == 0){
     # browser()
      # 9*6.25 = 56.25ha, an acceptable max block size for now (60ha is sometimes used in Alberta)
      block <- adjacent(sim$ageMap, cell, directions=8, pairs=FALSE, include=TRUE)
      operable <- sim$noCanGo[block]
      block <- block[!operable]   #length of block >= 1
      #other constraints on blocks: age range, type mix, adjacency.
      if (length(block)*sim$cellSize > params(sim)$harvest$minBlockSize){
        age <- sim$ageMap[block]
        age <- ifelse(age < n, age, n)
        cut <- sum(v[age]) * sim$cellSize
        sim$harvestState$volCut <- sim$harvestState$volCut + cut
        sim$ageMap[block] <- 0
        sim$disturbanceMap[block] <- 2
      }
    }
    idx <- idx + 1
    if (idx > length(x)){
      warning("ran out of cells")
      break
    }
  }
return(invisible(sim))  
}



sequenceOldest <- function(sim){
  x <- order(sim$ageMap[], decreasing=TRUE)
  x <- x[which(sim$noCanGo[x]==0)] 
  v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
  n<-length(v)
  idx <- 1
  while (sim$harvestState$volCut < sim$annualCut ){
    cell <- x[idx]
    age <- sim$ageMap[cell]
    age <- ifelse(age < n, age, n)
    cut <- v[age] * sim$cellSize
    sim$harvestState$volCut <- sim$harvestState$volCut + cut
    sim$ageMap[cell] <- 0
    sim$disturbanceMap[cell] <- 2
    idx <- idx + 1
    if (idx > length(x)){
      warning("ran out of cells")
      break
    }
  }
  return(invisible(sim))  
}

sequentialCut<-function(sim){
  
  N <- prod(dim(sim$noCanGo)[1:2])
  idx0 <- sim$harvestState$idx
  idx <-idx0
  v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
  n<-length(v)
  while (sim$harvestState$volCut < sim$annualCut ){
    
    if (sim$noCanGo[idx]==0){
      age<-sim$ageMap[idx]
      age<-min(age,n)
      sim$harvestState$volCut <- sim$harvestState$volCut + (v[age] * sim$cellSize)
      sim$disturbanceMap[idx] <- 2  #fix the magic numbers
      sim$ageMap[idx] <- 0
    }
    
    idx <- ifelse(idx==N,1,idx+1)
    if (idx == idx0){
      warning("ran out of harvestable cells")?
      break
    }
  }
  sim$harvestState$idx <- idx
  return(invisible(sim))
}

### template for save events
harvestSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
harvestPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}



### add additional events as needed by copy/pasting from above

