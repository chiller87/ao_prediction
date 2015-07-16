

# prepares data for model.name car model according to some.params


prepare_data_isolated <- function(car.model, model.name, region.costs=FALSE, some.params=NA) {
    
    # initialize default params
    params <- list(
        # additional metadata
        metadata=NA, 
        # Gewichtung des Baujahrs (wenn 0.0, wird Baujahr nicht beruecksichtigt)
        weight_jahr= 0.0, # TODO
        # Gewichtung der Leistung (wenn 0.0, wird Leistung nicht beruecksichtigt)
        weight_leistung= 0.0, # TODO
        # Perzentil, das als "leicht beschaedigt" gilt
        threshold_quantile=0.5,
        # Beruecksichtigung von Schadensklasse (none, total, byregion)
        damage_classes="none",
        # Sollen die Abstände auf Basis einer gestörte Variante der der Regionsmatrix berechnet werden?
        distorted=FALSE, # TODO
        # Wahrscheinlichkeit, dass eine 0-Region als eine 1- oder 2-Region fehlklassifiziert wird
        PZ = 0.001, # TODO
        # Störung der nicht-0-Regionen (Details siehe calculate_region_matrix.r)
        PNZ=0.01
    )
    
    
    # override default params with those, that are specified in some.params
    if (!identical(some.params,NA)) {
        for (n in names(some.params)) {
            if (!(n %in% names(params))) {
                stop("unknown parameter: ", n, "\n  allowed parameters: ", paste(names(params),collapse=",")) 
            }
            params[n] <- some.params[n]
        }
    }
    
    
    
    # extract regions matrix
    region_costs <- car.model[,17:(ncol(car.model)-1)]
    region_costs[region_costs == 0] <- NA
    regions <- car.model[,17:(ncol(car.model)-1)]
    regions[regions != 0] <- 1
    
    #print(paste("num of cols (", model.name, ") = ", ncol(regions), sep=""))
    # entferne nicht vorkommende Regionen
#     N <- apply(regions>0,2,sum)
#     existent_regions <- (N > 0)
#     region_costs <- region_costs[,N>0]
#     regions <- regions[,N>0]
    
    total_repair_costs <- car.model$TOTAL_REPAIR_COSTS
    
    # if damage classes should be used, apply them
    if (params$damage_class != 'none') 
    {
        res <- calculateRegionMatrix(car.model, damage_classes=params$damage_classes, threshold_quantile=params$threshold_quantile, distorted=params$distorted, PZ=params$PZ, PNZ=params$PNZ)
        regions <- data.frame(res$regions.sev)  
        
        
        if (params$distorted) {
            regions.dist <- res$regions.sev.dist
            params$diffsNull <- res$diffsNull
            params$diffsNonNull <- res$diffsNonNull		
        }
    }
    
    #print(paste("num of cols (", model.name, ") = ", ncol(regions), sep=""))

    # append year
    if(params$weight_jahr != 0.0) {
        jahr <- as.numeric(substring(car.model$FIRST_REGISTRATION_DATE, 7, 8))
        jahr <- (jahr +2000 - 100*(jahr>15))*params$weight_jahr
        regions <- cbind(regions, jahr)
    }
    
    # append kw
    if(params$weight_leistung != 0.0) {
        leistung <- car.model$LEISTUNG * params$weight_leistung
        leistung[is.na(leistung)] <- median(leistung, na.rm=TRUE)
        regions <- cbind(regions, leistung)
    }
    
    explanatory_variables <- colnames(regions)
    
    
    columns.to.drop <- c()
    # append engine type
    param <- "ENGINE_TYPE_GROUP"
    if(param %in% params$metadata) {
        engine.types.range <- c("BENZIN", "DIESEL", "GAS", "NA")
        
        # append engine types (names) to regions matrix
        explanatory_variables <- append(explanatory_variables, param)
        regions <- cbind(regions, car.model[param])
        
        # engine.types.vectors <- list()
        for(engine.type in engine.types.range) {
            if(engine.type != "NA") {
                # create vector with zeros
                vec <- c(rep(0, times=nrow(regions)))
                # set appropriate indices to one
                vec <- replace(vec, regions[param]==engine.type, 1)
                # append vec as column to regions
                regions <- cbind(regions, vec)
                # add explanatory variable
                explanatory_variables <- append(explanatory_variables, paste("ET_", engine.type, sep=""))
            }
            else {
                # create vector with zeros
                vec <- c(rep(0, times=nrow(regions)))
                # set appropriate indices to one
                vec <- replace(vec, is.na(regions[param]), 1)
                # append vec as column to regions
                regions <- cbind(regions, vec)
                # add explanatory variable
                explanatory_variables <- append(explanatory_variables, paste("ET_", engine.type, sep=""))
            }
        }
        
        columns.to.drop <- c(columns.to.drop, param)
    }
    
    
    
    # append model
    param <- "MODEL"
    if(param %in% params$metadata) {
        model.names.range <- c("GOLF", "SERIES 3", "POLO", "C-CLASS", "ASTRA", "PASSAT")
        
        # append models (names) to regions matrix
        explanatory_variables <- append(explanatory_variables, param)
        regions <- cbind(regions, car.model[param])
        
        for(model in model.names.range) {
            # create vector with zeros
            vec <- c(rep(0, times=nrow(regions)))
            # set appropriate indices to one
            vec <- replace(vec, regions[param]==model, 1)
            # append vec as column to regions
            regions <- cbind(regions, vec)
            # add explanatory variable
            explanatory_variables <- append(explanatory_variables, paste("M_", model, sep=""))
        }
        
        columns.to.drop <- c(columns.to.drop, param)
    }
    
    
    # append manufacturer
    param <- "MANUFACTURER"
    if(param %in% params$metadata) {
        manu.names.range <- c("VOLKSWAGEN", "BMW", "MERCEDES", "OPEL")
        
        # append manu (names) to regions matrix
        explanatory_variables <- append(explanatory_variables, param)
        regions <- cbind(regions, car.model[param])
        
        for(manu in manu.names.range) {
            # create vector with zeros
            vec <- c(rep(0, times=nrow(regions)))
            # set appropriate indices to one
            vec <- replace(vec, regions[param]==manu, 1)
            # append vec as column to regions
            regions <- cbind(regions, vec)
            # add explanatory variable
            explanatory_variables <- append(explanatory_variables, paste("MF_", manu, sep=""))
        }
        
        columns.to.drop <- c(columns.to.drop, param)
    }
    
    
    
    
    colnames(regions) <- explanatory_variables
    
#     # weitere Metadaten
#     if (!identical(params$metadata,NA)) {
#         explanatory_variables <- append(explanatory_variables, params$metadata)
#         regions <- cbind(regions, car.model[params$metadata])
#         if (params$distorted) {
#             regions.dist <- cbind(regions.dist, car.model[params$metadata])
#         }
#     }
    
    # true costs
    if (region.costs) {
        tcost <- rowSums(region_costs, na.rm=TRUE)
        response_variable <- "region_sum"	
    } else {
        tcost <- total_repair_costs		
        response_variable <- "TOTAL_REPAIR_COSTS"	
    }
    
    explanatory_variables <- append(explanatory_variables, response_variable)
    
    # add target variable to regions matrix
    regions <- cbind(regions, tcost)
    #colnames(regions)[ncol(regions)] <- response_variable
    colnames(regions) <- explanatory_variables
    
    if (params$distorted) {
        regions.dist <- cbind(regions.dist, tcost)
        colnames(regions.dist)[ncol(regions.dist)] <- response_variable
    }
    
    ecost <- vector('numeric',length(tcost))  # Speicherallokierung
    
    
    write.table(regions,file=paste("auswertung/last_used_regions_", model.name, ".csv", sep=""), sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)
    
    # remove categorized columns
    regions <- regions[,!(names(regions) %in% columns.to.drop)]
    explanatory_variables <- !(explanatory_variables %in% columns.to.drop)
    
    #print(paste("num of cols (", model.name, ") = ", ncol(regions), sep=""))
    
    return(list(regions=regions, tcost=tcost, ecost=ecost))
    
}



