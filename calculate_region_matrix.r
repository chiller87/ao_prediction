#
# calculateRegionMatrix
#
# args:
#   (data)    car.model          = komplette Fahrzeugdaten im overview_* Format
#   (numeric) threshold_quantile = Perzentil, das als "leicht beschaedigt" gilt
#   (char)    damage_classes     = Beruecksichtigung von Schadensklasse (none, total, byregion)
#   (bool)    distorted  	     = Soll auch eine gestörte Variante der der Regionsmatrix berechnet werden?
#   (numeric) PZ = Wahrscheinlichkeit, dass eine 0-Region als eine 1- oder 2-Region fehlklassifiziert wird
#   (numeric)    PNZ            = Die nicht-0-Regionen werden wie folgt gestört: auf die wahren Kosten  ein um 0 normalverteilter Fehler addiert. Dadurch wird die Wahrscheinlichkeit, dass Regionen nahe des Thresholds fehlklassifiziert werden, größer. PNZ bestimmt die Größe des Fehlers. 
#
# returns:
#    list mit folgenden Eintraegen:
#      regions = ungestörte Regionsmatrix binären Einträgen
#      regions.sev = Regionsmatrix mit Schadensklassen (optional: nur wenn damage_classes != "none")
#      regions.distorted = gestörte Schadensmatrix (optional: nur wenn distorted == TRUE)
#      diffs = 3x3-Matrix mit den Anzahlen der Unterschiede zwischen der nicht gestörten und gestörten Version der Regionsmatrix. diffs[i,j] = x bedeutet: es gibt x Regionen, die in der originalen Matrix eine i-1 und in der gestörten Matrix eine j-1 haben (optional: nur wenn damage_classes != "none" und distorted == TRUE).
#      diffsNull = prozentualer Anteil Störungen von Typ 0<->{1,2}
#      diffsNonNull = prozentualer Anteil der Störungen vom Typ 1<->2
calculateRegionMatrix <- function(car.model, damage_classes = "byregion", threshold_quantile = 0.5, damage_class_values=c(0,1,2), distorted=FALSE, PZ=0.01, PNZ=0.001) {

  region_costs <- car.model[,17:(ncol(car.model)-1)]
  regions <- car.model[,17:(ncol(car.model)-1)]
  regions[regions != 0] <- 1
  
#   # entferne nicht vorkommende Regionen
#   N <- apply(regions>0,2,sum)
#   existent_regions <- (N > 0)
#   region_costs <- region_costs[,N>0]
#   regions <- regions[,N>0]
	
  total_repair_costs <- car.model$TOTAL_REPAIR_COSTS
  
  if (damage_classes != "none") {
  
    # create the damage severity matrix
    regions.sev <- region_costs
    colnames(regions.sev) <- paste(colnames(region_costs),"sev",sep=".")

   
    # bestimme die Schwellwerte fuer jede Region
    # Quantile nur bzgl Fahrzeugen mit Regionskosten <> 0.0 bestimmen
    region_costs_NA <- region_costs
    region_costs_NA[regions==0] <- NA
    region_thresholds <- apply(region_costs_NA, 2, quantile, probs=threshold_quantile, na.rm=TRUE)
  
    if (damage_classes == "total") {
      total_threshold <- quantile(total_repair_costs, probs=threshold_quantile)
      regions.sev <- (region_costs > 0) * (1+(total_repair_costs > total_threshold))
      regions.sev[is.na(region_costs)] <- 0
    } else if (damage_classes == "byregion") {    
      regions.sev <- (region_costs > 0) + t(t(region_costs) > region_thresholds)
      regions.sev[is.na(regions.sev)] <- 0
    }	
  }
  

  if (distorted) {
    # create a distorted version 
    regions.sev.dist <- regions.sev
    # colnames(region_distorted_severity_matrix) <- paste(colnames(region_matrix),"dsev",sep=".")
  
    # # method 1
    # # create a distorted version of the cost matrix by adding a random variation
    # # only for original entries with costs > 0
    # region_costs.dist <- region_costs  
    # for (i in 1:ncol(region_costs)) {
      # x <- length(region_costs[,i][region_costs[,i]>0])
	  # region_costs.dist[region_costs[,i]>0,i] <- region_costs[region_costs[,i]>0,i] + rnorm(x,0, PNZ * sqrt(region_thresholds[i]))		  
    # }  
	# # Korrigiere eventuell negativ gewordene Kosten
    # region_costs.dist[region_costs.dist<0] <- 0
  
    # # Neuberechnung der Schadensklassen auf den gestörten Regionskosten
    # # todo: total_repair_costs.dist anpassen	
    # total_repair_costs.dist <- total_repair_costs
    # if (damage_classes == "total") {
      # total_threshold <- quantile(total_repair_costs, probs=threshold_quantile)	 
      # regions.sev.dist <- (region_costs.dist > 0) * (1+(total_repair_costs.dist > total_threshold))
      # regions.sev.dist[is.na(region_costs)] <- 0
    # } else if (damage_classes == "byregion") {  
      # # originale region_thresholds an gestörte Daten anpassen 	
	  # region_costs_NA.dist <- region_costs.dist
      # region_costs_NA.dist[region_costs.dist==0] <- NA
	  # region_thresholds.dist <- apply(region_costs_NA.dist, 2, quantile, probs=threshold_quantile, na.rm=TRUE)
      # regions.sev.dist <- (region_costs.dist > 0) + t(t(region_costs.dist) > region_thresholds.dist)
      # regions.sev.dist[is.na(regions.sev.dist)] <- 0
    # }
	
	
	# method 2
	# vary the orginal 1's  
	# 95% der Störungen sollen 1->2-Störungen, 5% 1->0-Störungen sein 
	x <- length(regions.sev[regions.sev==1]) 	
    regions.sev.dist[regions.sev==1] <- sample(c(1,0,2), x, replace=T, prob=c(1-PNZ, 0.05*PNZ,0.95*PNZ))
	
	# vary the orginal 2's  
	# 98% der Störungen sollen 2->1-Störungen, 2% 2->1-Störungen sein 
	x <- length(regions.sev[regions.sev==2]) 	
    regions.sev.dist[regions.sev==2] <- sample(c(2,0,1), x, replace=T, prob=c(1-PNZ, 0.02*PNZ,0.98*PNZ))

    # vary the orginal 0's 
	# 95% der Störungen sollen 0->1-Störungen, 5% 0->2-Störungen sein 
    x <- length(regions.sev[regions.sev==0]) 	
    regions.sev.dist[regions.sev==0] <- sample(c(0,1,2), x, replace=T, prob=c(1-PZ, 0.95*PZ,0.05*PZ))
  
    # Statistiken
    diffs <- matrix(numeric(0), 3, 3)
    colnames(diffs) <- (0:2)
    rownames(diffs) <- (0:2)
    for (i in 0:2) {
      for (j in 0:2) {
	    diffs[i+1,j+1] <- length(regions.sev[regions.sev==i &  regions.sev.dist == j]) 
	  }
    }  
	diffs0 <- (diffs[1,2] + diffs[1,3] + diffs[2,1] + diffs[3,1])/sum(diffs)
	diffsNon0 <- (diffs[2,3] + diffs[3,2])/sum(diffs)
    result <- list(regions=regions, regions.sev=regions.sev, regions.sev.dist=regions.sev.dist, diffs=diffs, diffs0=diffs0, diffsNon0=diffsNon0)
  }
  else {
    if (damage_classes=="none") {
		result <- list(regions=regions)
    }
	else {
	  result <- list(regions=regions, regions.sev = regions.sev)
	}
  }    

  return(result)
}
