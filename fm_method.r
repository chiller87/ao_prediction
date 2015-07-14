#
# lm_method
#
# args:
#   (data)    car.model = komplette Fahrzeugdaten im overview_* Format
#   (bool) region.costs = schaetze Regionskosten statt totaler Kosten
#   (list)  some.params = Parameter die Werte aus der Menge von params ueberschrieben sollen
#   (vector)      train = boolscher Vektor der zum Training zu nehmenden Zeilen aus car.model
#
# returns:
#    data.frame mit tcost ("true cost") und ecost ("estimated cost") fuer alle Fahrzeuge
#    Achtung: fuer zurueckgewiesene Fahrzeuge sind die Kosten NA
#

fm_method <- function(car.model, model.name, region.costs=FALSE, some.params=NA, train=NA, combined=FALSE)
{

    # initialize default params
    params <- list(
        # method for FM parameter learning
        method="mcmc",
    	# Anzahl Faktoren
    	k=8,
    	# Anzahl Iterationen
    	iter=2000,
    	# output filename
    	stdev=0.1,
        # regulation (only affect ALS and SGD)
        reg=0,
        # learning rate (only affect SGD)
        lr=0.001,
    	# 
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
    
    
    
    
    # initialize output filename
    outfile = "prediction"
    
    
#     # extract regions matrix
# 	region_costs <- car.model[,17:(ncol(car.model)-1)]
#     region_costs[region_costs == 0] <- NA
# 	regions <- car.model[,17:(ncol(car.model)-1)]
# 	regions[regions != 0] <- 1
# 	
# 	
# 	# entferne nicht vorkommende Regionen
#     N <- apply(regions>0,2,sum)
#     existent_regions <- (N > 0)
#     region_costs <- region_costs[,N>0]
#     regions <- regions[,N>0]	
# 	
# 	
#     
# 	total_repair_costs <- car.model$TOTAL_REPAIR_COSTS
# 	
# 	# if damage classes should be used, apply them
# 	if (params$damage_class != 'none') 
# 	{
# 	  res <- calculateRegionMatrix(car.model, damage_classes=params$damage_classes, threshold_quantile=params$threshold_quantile, distorted=params$distorted, PZ=params$PZ, PNZ=params$PNZ)
# 	  regions <- data.frame(res$regions.sev)  
# 	  
# 	  
# 	  if (params$distorted) {
# 	    regions.dist <- res$regions.sev.dist
# 	    params$diffsNull <- res$diffsNull
# 		params$diffsNonNull <- res$diffsNonNull		
# 	  }
# 	}	
# 	
# 	# zusaetzliche Metadaten
# 	
# 	# fuege Baujahr und Leistung hinzu 
# 	# wird nicht berücksichtigt, wenn weight_jahr und weight_leistung == 0
# 	jahr <- as.numeric(substring(car.model$FIRST_REGISTRATION_DATE, 7, 8))
#     jahr <- (jahr +2000 - 100*(jahr>15))*params$weight_jahr
#     leistung <- car.model$LEISTUNG * params$weight_leistung
#     leistung[is.na(leistung)] <- median(leistung, na.rm=TRUE)
#     regions <- cbind(regions, jahr, leistung)
#     explanatory_variables <- colnames(regions)
# 	
# 	# weitere Metadaten
# 	if (!identical(params$metadata,NA)) {
# 		explanatory_variables <- append(explanatory_variables, params$metadata)
# 		regions <- cbind(regions, car.model[params$metadata])
# 		if (params$distorted) {
# 		  regions.dist <- cbind(regions.dist, car.model[params$metadata])
# 		}
# 	}
# 	
# 	# true costs
#     if (region.costs) {
#         tcost <- rowSums(region_costs, na.rm=TRUE)
# 		response_variable <- "region_sum"	
#     } else {
#         tcost <- total_repair_costs		
# 		response_variable <- "TOTAL_REPAIR_COSTS"	
#     }
# 	
#     # add target variable to regions matrix
# 	regions <- cbind(regions, tcost)
# 	colnames(regions)[ncol(regions)] <- response_variable
# 	
# 	if (params$distorted) {
# 	  regions.dist <- cbind(regions.dist, tcost)
# 	  colnames(regions.dist)[ncol(regions.dist)] <- response_variable
# 	}
# 	
# 	ecost <- vector('numeric',length(tcost))  # Speicherallokierung	
	
    
    prepare.params <- list(
        metadata=params$metadata, 
        weight_jahr=params$weight_jahr, # TODO
        weight_leistung=params$weight_leistung, # TODO
        threshold_quantile=params$threshold_quantile,
        damage_classes=params$damage_classes,
        distorted=params$distorted, # TODO
        PZ=params$PZ, # TODO
        PNZ=params$PNZ
    )
    data <- prepare_data_isolated(car.model, model.name=model.name, region.costs=region.costs, some.params=prepare.params)
    
	
	# specify strata
	stratum_count <- 5
    stratum_size <- nrow(car.model) %/% stratum_count
    considered_row_count <- stratum_count * stratum_size
	
    # convert regions to matrix
	regions.matrix <- as.matrix(data$regions)
	
	# create empty data frame for best iteration per strata
	# best.data <- data.frame("model"=NA, "method"=NA, "k"=NA, "iter"=NA, "best_iter"=NA, "stdev"=NA, "reg"=NA, "lr"=NA, "strata_nr"=NA, "rmse"=NA, "mae"=NA)[numeric(0),]
	
	
	i <- 0	
	while ( i < stratum_count ) {
		ind <- (i*stratum_size + 1):((i+1)*stratum_size)
		# cat("Test indices: ", ind[1], "-", ind[length(ind)], "...")
		
		# create test and training sets
		if (!identical(train,NA)) {
		    train2 <- train
		    train2[ind] <- FALSE
			training <- regions.matrix[train2,]
        } else {
		    training <- regions.matrix[-ind,]
		}
		if (params$distorted) {
		  test <- regions.dist.matrix[ind,]
		}
		else {
		  test <- regions.matrix[ind,]
		}
		
		# define train data
		x.training <- training[,1:(ncol(training)-1)]
		y.training <- training[,ncol(training)]
		#print("data: ")
		#print(x.training)
		#break
		
		# write the training data as file in sparse libsvm format
		write.matrix.csr(x=x.training, "training.libfm", y=y.training) 
		
		# define test data
		x.test <- test[,1:(ncol(test)-1)]
		y.test <- test[,ncol(test)]
		
		# write the training data as file in sparse libsvm format
		write.matrix.csr(x=x.test, "test.libfm", y=y.test)
		
		
		# naming logfile, to have one for each libfm scenario
# 		logname <- paste("auswertung/", model.name, "/fm/", sep="")
# 		logname <- paste(logname, params["method"], "_k", params["k"], "_i", params["iter"], "_s", params["stdev"], sep="")
# 		if(params["method"] == 'als') {
# 		    logname <- paste(logname, "_r", params["reg"], sep="")
# 		} else if(params["method"] == 'sgd') {
# 		    logname <- paste(logname, "_lr", params["lr"], sep="")
# 		} else if(params["method"] != 'mcmc'){
# 		    print(paste("unknown mathod: ", params["method"], sep=""))
# 		    stop()
# 		}
# 		logname <- paste("'", logname, "_sc", i, ".log", "'", sep="")
		
		
		# init parts for libfm execution
		libfm.cmd <- "mylibfm -task r"
		libfm.train <- "-train training.libfm"
		libfm.test <- "-test test.libfm"
		libfm.out <- paste("-out ", outfile, sep="")
		libfm.dim <- paste("-dim 1,1,", params["k"], sep="")
		libfm.iter <- paste("-iter ", params["iter"], sep="")
		libfm.method <- paste("-method ", params["method"], sep="")
		libfm.stdev <- paste("-init_stdev ", params["stdev"], sep="")
		libfm.reg <- paste("-regular ", params["reg"], sep="")
		libfm.lr <- paste("-learn_rate ", params["lr"], sep="")
		libfm.rlog <- ""#paste("-rlog ", logname, sep="")
		
		#cmd <- "libfm -task r -train training.libfm -test test.libfm -dim '1,1,12' -iter 2000 -method mcmc -init_stdev 0.1 -out output"
		cmd <- paste(libfm.cmd, libfm.train, libfm.test, libfm.out, libfm.dim, libfm.iter, libfm.method, libfm.stdev, libfm.reg, libfm.lr, libfm.rlog, sep=" ")
		print(paste("command: ", cmd, sep=""))
		system(cmd)
		
		
# 		# open connection to file
#         con <- file(logname, open="r")
# 		# read lines from logfile
# 		lines <- readLines(con)
# 		# close connection
# 		close(con)
# 		# sort lines of logfile by RMSE (ascending)
# 		lines <- sort.int(lines)
# 		# write sorted lines back to file
# 		write(lines, logname)
# 		
# 		# get best result of this run
# 		best.row <- lines[1]
# 		best.row.vec <- unlist(strsplit(best.row, split="\t"))
# 		best.rmse <- best.row.vec[1]
# 		best.mae <- best.row.vec[2]
# 		best.iter <- best.row.vec[3]
# 		
# 		# write result with stratum number to data frame
# 		best.data.entry <- c(model.name, params$method, params$k, params$iter, best.iter, params$stdev, params$reg, params$lr, i, best.rmse, best.mae)
# 		best.data[nrow(best.data)+1,] <- best.data.entry
		
		
		prediction <- read.table(outfile)
		
		data$ecost[ind] <- as.numeric(prediction[,1])
		
		i <- i + 1		
	}	
	
	# added best result to return statement
	#return(list(result=data.frame(tcost=tcost,ecost=ecost),params=params, fmres=best.data))
	return(list(result=data.frame(tcost=data$tcost,ecost=data$ecost),params=params))
}