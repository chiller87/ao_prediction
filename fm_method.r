#
# fm_method (combined)
#
# args:
#   (data)   all.models = all model names, current model, fore every model: raw_data (overview_* format), data (used for libfm)
#   (list)  some.params = Parameter die Werte aus der Menge von params ueberschreiben sollen
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

    # initialize train data of other models
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
    
    # get regions matrix of current model
    regions <- all.models$data[[all.models$curr.model]]$regions
    
    # specify strata
    stratum_count <- 5
    stratum_size <- nrow(regions) %/% stratum_count
    considered_row_count <- stratum_count * stratum_size
    
    # convert regions to matrix
    regions.matrix <- as.matrix(regions)
    
    # initialize true and expected costs
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
            training <- rbind(train.others, regions.matrix[train2,])
        } else {
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

        
        
        # init parts for libfm execution
        libfm.cmd <- "libfm -task r"
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

        # read predicted data
        prediction <- read.table(outfile)
        
        # write prediction to expected costs
        ecost[ind] <- as.numeric(prediction[,1])
        
        i <- i + 1
    }	
    
    return(list(result=data.frame(tcost=tcost,ecost=ecost),params=params))
}