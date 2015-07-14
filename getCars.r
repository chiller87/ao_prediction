getCars <- function(overview, manufacturer, model, addJahr=TRUE, jahrWeight=0.9) {	
  # select instances of given car model only	  
  cars <- subset(overview, MANUFACTURER==manufacturer & MODEL==model)

  metadata <- cars[,1:16]

  if (addJahr) {	
    jahr <- as.numeric(substring(cars$FIRST_REGISTRATION_DATE, 7, 8))
    jahr <- (jahr +2000 - 100*(jahr>15)) * jahrWeight
    jahr[is.na(jahr)] <- median(jahr) 
    metadata <- cbind(metadata, jahr)
  }

  # separate the region columns
  regions <- cars[,17:ncol(cars)]

  # create binarized copy of regions 
  cat("binarizing region data ...\n")
  regions.binary <- regions
  regions.binary[regions.binary > 0.0] <- 1.0 
  region.names <- colnames(regions)
  region.bin.names <- paste(region.names,"bin",sep=".")
  colnames(regions.binary) <- region.bin.names

  region.bin.names.non.na <- region.bin.names[1:(length(region.bin.names)-1)]
  region.names.non.na <- region.names[1:(length(region.names)-1)]

  # summarize all costs related to individual regions (except NA Region) in an additional column
  region.sum <- rowSums(cars[, region.names.non.na])
	
  # summarize the count of affected regions (except NA Region) in an additional column
  region.count <- rowSums(regions.binary[, region.bin.names.non.na])

  # create a complete binarized data frame with metadata and aggregate costs
  complete <- cbind(metadata, regions, region.sum, region.count, regions.binary)	
  
  cars <- list(data=complete, region.names = region.names, region.names.non.na = region.names.non.na, region.bin.names = region.bin.names, region.bin.names.non.na = region.bin.names.non.na)
  
  return(cars)
} 