
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "stateVars",
  description = "keep track of stat transitions affecting multiple moduls; also classification",
  keywords = c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9006"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit =  "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "stateVars.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("startTime", "numeric", 0, 0, NA, "Start time"),
    defineParameter("returnInterval", "numeric", 1, 0, NA, "waddya think?"),
    defineParameter("cutPersistanceTime", "numeric", 30, 0, 30, "For how many years do cut blocks affect birds?"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = c("disturbanceMap", "ageMap"),
    objectClass = c("RasterLayer","RasterLayer"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("harvestStateMap","heightMap"),
    objectClass = c("RasterLayer","RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.stateVars = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$stateVarsInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$stateVars$startTime,"stateVars","update")
    sim <- scheduleEvent(sim, params(sim)$stateVars$.plotInitialTime, "stateVars", "plot")
    sim <- scheduleEvent(sim, params(sim)$stateVars$.saveInitialTime, "stateVars", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, params(sim)$stateVars$.plotInitialTime, "stateVars", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "stateVars", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "update") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    
    sim$harvestStateMap <- sim$harvestStateMap - 1
    x <- which(sim$disturbanceMap[] == 2)
    sim$harvestStateMap[x] <- params(sim)$stateVars$cutPersistanceTime
    
    sim<-heightFromAge(sim)
    
    sim <- scheduleEvent(sim, time(sim) + params(sim)$stateVars$returnInterval, "stateVars", "update")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event2") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "stateVars", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else {
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


heightFromAge <-function(sim){
  x <- sim$ageMap[]/ (sim$ageMap[] + 35)
  sim$heightMap <- 80 * x
  return(invisible(sim))
}

stateVarsInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  N<-sim$mapDim
  x<-raster::extent(c(0,N-1,0,N-1))
  sim$harvestStateMap<-raster(x,nrows=N, ncols=N,vals=0)
  setColors(sim$disturbanceMap, n=10) <- c("green","red")
  sim$heightMap<-raster(x,nrows=N, ncols=N,vals=0)
  setColors(sim$heightMap, n=10) <- c("white","blue")
  sim<-heightFromAge(sim)
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
stateVarsSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
stateVarsPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
stateVarsEvent1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
stateVarsEvent2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above

