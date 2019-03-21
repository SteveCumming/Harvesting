
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "Hanzlik",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Steve", "Cumming"), "Last", email="stevec@sbf.ulval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit =  "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Hanzlik.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("replanInterval", "numeric", 10, NA, NA, "How frequently do we replan?"),
    defineParameter("startTime", "numeric", 0, 0, 1e10, "How frequently do we replan?"),
    defineParameter("rationPeriodMultiplier", "numeric", 1, 0, 10, "multiply rotation age (R) by this to change Vm/R")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "yieldTables", objectClass = "RasterLayer", desc = "stand volume-age Tables"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "stand age")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "hanzlikPars", objectClass = "list", desc = "structures defining AAC"),
    createsOutput(objectName = "annualCut", objectClass = "numeric", "AAC value in m^3")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Hanzlik = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
  eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime, "Hanzlik", "plan")
    },
    plan = {
      sim <- Plan(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$replanInterval, "Hanzlik", "plan")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}



calcHanzlik <- function(ytM){
  # assumes yt is just a vector of volumes in 1 yr age classes starting at 1
  # Vm/R + I
  #browser()
  #for now, use last collumn as total.
  if (is.matrix(ytM)){
    idx <- dim(ytM)[2]
    yt <- ytM[idx]
  }
  else 
    stop("illegal yield table: matrix expected")
  n <- length(yt)
  vt<- yt/1:n
  #plot(vt)
  R <- order(vt,decreasing=TRUE)[1] #trouve le age de la culmination
  inc <- c(0, diff(yt))
  tmp <- yt[R:n]/(P(sim)$rationPeriodMultiplier*R)     #contribution of each age class to Vm/R in m^3/ha
  tmp <- c(inc[1:R-1], tmp)
  
  return(list(R=R,I=inc,hVec=tmp))   #should not alter Vm after initial plan?

}

### template initialization
Init <- function(sim) {
 
  sim$hanzlikPars <- lapply(sim$yieldTables, calcHanzlik) # we assume there is a list these structures

  return(invisible(sim))
}


Plan <- function(sim) {
  #browser()
  hVec<-sim$hanzlikPars[[1]]$hVec   #for now, assume annualCut has at least one object, and take the 1st
                                    #Comment peut-on le generaliser pour plusieurs courbes de rendemment?
  nh <- length(hVec)
  res<-rep(0,nh)
  x<-tabulate(sim$ageMap[])   #error here: what is max age?
  nx<-length(x)
  if (nx <= nh){
    res[1:nx] <- x
  }
  else {
    res <- x[1:nh]
    x[nh] <- x[nh] + sum(x[nh+1:nx]) #accumulate any "missing ones"
  }

  res<-sum(res*hVec) * sim$cellSize
  print(sprintf("%d AAC = %5.1f\n",time(sim),res/1e3))
  sim$annualCut <- res  
  return(invisible(sim))
}



