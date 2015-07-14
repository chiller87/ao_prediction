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



fm_method_combined <- function(all.models, some.params=NA, train=NA)
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
        PNZ=0.01,
        # whether to use other models for training or not
        combined=FALSE
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
    

#     prepare.params <- list(
#         metadata=params$metadata, 
#         weight_jahr=params$weight_jahr, # TODO
#         weight_leistung=params$weight_leistung, # TODO
#         threshold_quantile=params$threshold_quantile,
#         damage_classes=params$damage_classes,
#         distorted=params$distorted, # TODO
#         PZ=params$PZ, # TODO
#         PNZ=params$PNZ
#     )
#     
#     data <- prepare_data_isolated(car.model, region.costs=region.costs, some.params=prepare.params, train=train)
    
    
    train.others <- c()

    if(combined == TRUE) {
        for(model in all.models$names) {
        
            #write.table(all.models$data[[model]]$regions,file=paste("auswertung/data_", model, ".csv", sep=""),
            #            sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)
            
            if(model != all.models$curr.model) {
                train.others <- rbind(train.others, all.models$data[[model]]$regions)
            }
        }
        train.others <- as.matrix(train.others)
    }
    
    
    
    #write.table(train.others, file="auswertung/data_others.csv", append=FALSE, sep=",", col.names = TRUE, row.names = FALSE)
    
    # training <- train.others
    # nrows.others <- nrow(train.others)
    
    
    regions <- all.models$data[[all.models$curr.model]]$regions
    
    # specify strata
    stratum_count <- 5
    stratum_size <- nrow(regions) %/% stratum_count
    considered_row_count <- stratum_count * stratum_size
    
    # convert regions to matrix
    regions.matrix <- as.matrix(regions)
    
    
    # create empty data frame for best iteration per strata
    # best.data <- data.frame("model"=NA, "method"=NA, "k"=NA, "iter"=NA, "best_iter"=NA, "stdev"=NA, "reg"=NA, "lr"=NA, "strata_nr"=NA, "rmse"=NA, "mae"=NA)[numeric(0),]
    
    tcost <- all.models$data[[all.models$curr.model]]$tcost
    ecost <- vector('numeric',length(tcost))
    
    i <- 0	
    while ( i < stratum_count ) {
        ind <- (i*stratum_size + 1):((i+1)*stratum_size)
        # cat("Test indices: ", ind[1], "-", ind[length(ind)], "...")
        
        # create test and training sets
        if (!identical(train,NA)) {
            train2 <- train
            train2[ind] <- FALSE
            #training <- regions.matrix[train2,]
            training <- rbind(train.others, regions.matrix[train2,])
        } else {
            #training <- regions.matrix[-ind,]
            training <- rbind(train.others, regions.matrix[-ind,])
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
        
        #write.table(training, file="auswertung/used_train_data.csv", append=FALSE, sep=",", col.names = TRUE, row.names = FALSE)
        
        # write the training data as file in sparse libsvm format
        write.matrix.csr(x=x.training, "training.libfm", y=y.training) 
        
        # define test data
        x.test <- test[,1:(ncol(test)-1)]
        y.test <- test[,ncol(test)]
        
        #write.table(test, file="auswertung/used_test_data.csv", append=FALSE, sep=",", col.names = TRUE, row.names=FALSE)
        
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
        libfm.rlog <- "" #paste("-rlog ", logname, sep="")
        
        cmd <- paste(libfm.cmd, libfm.train, libfm.test, libfm.out, libfm.dim, libfm.iter, libfm.method, libfm.stdev, libfm.reg, libfm.lr, libfm.rlog, sep=" ")
        print(paste("command to execute: ", cmd, sep=""))
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
        
        #ecost.ind <- nrows.others + ind
        ecost[ind] <- as.numeric(prediction[,1])
        
        i <- i + 1
    }	
    
    # added best result to return statement
    #return(list(result=data.frame(tcost=tcost,ecost=ecost),params=params, fmres=best.data))
    return(list(result=data.frame(tcost=tcost,ecost=ecost),params=params))
}