#
# Testlauf zur Messung von Fehlern
#

if (!exists("print_dist", mode = "function")) {
    source("display_functions.r")
}
if (!exists("edit_error", mode = "function")) {
    source("editing.r")
}
if (!exists("calculateRegionMatrix", mode = "function")) {
    source("calculate_region_matrix.r")
}
if (!exists("save_dist", mode = "function")) {
    source("save_result.r")
}
if (!exists("save_best_iter", mode = "function")) {
    source("save_best_fm_result.r")
}
    source("fm_method.r")
    source("fm-prepare_data.r")



# Auswahl des Modells
#--------------------------------------------



method.name <- "FM"
fm.method.range <- c("mcmc")
model.name.range <- c("GOLF") # c("GOLF", "SERIES 3", "POLO", "C-CLASS", "ASTRA", "PASSAT")
k.range <- c(20) #seq(10, 40, 10) #c(30)
stdev.range <- c(0.0) #seq(0.0, 1.0, 0.2) # seq(0.0, 0.5, 0.1) #seq(0, 0.5, 0.1)
iter.range <- c(1200) # seq(400, 2000, 400) 
w_year.range <- c(1.0) #c(1.0, 0.0)
w_kw.range <- c(1.0) #c(1.0, 0.0)
region.costs.range <- c(FALSE)
PZ.range <- c(0) #,0.005, 0.01, 0.015, 0.02, 0,025)
PNZ.range <- c(0) #, 0.001, 0.002)
damage_classes.range <- c('byregion') # c('none','byregion')

for (model.name in model.name.range) {
    for (region.costs in region.costs.range) {
    for (fm.method in fm.method.range) {
    for (iter in iter.range) {
    for (k in k.range) {
    for (stdev in stdev.range) {
    for (w_year in w_year.range) {
    for (w_kw in w_kw.range) {
    for (PZ in PZ.range) {
    for (PNZ in PNZ.range) {
    for (damage_classes in damage_classes.range) {
        # nur bei Modellwechsel Daten laden
        if (exists("car.model") && exists("last.model")) {
            if (last.model != model.name) {
                rm(car.model)
            }
        }
        last.model = model.name
        if (!exists("car.model")) {
            car.model <- read.table(paste("../../data/overview_",model.name,".csv",sep = ""), sep = ",", header = TRUE)
        }
        
        # Pruning der Trainingsdaten
        #train <- edit_error(car.model, times.median=3.0, error.is.re=FALSE)
        #cat("Anzahl beim Training entfernt:", sum(!train), "von", length(train))
        train <- NA
        
        result <- fm_method(car.model, model.name=model.name, region.costs = region.costs,
                list(
                    method=fm.method, k=k, iter=iter, stdev=stdev, weight_leistung=w_kw, weight_jahr=w_year,
                    damage_classes=damage_classes, metadata=c("ENGINE_TYPE_GROUP", "MODEL")
                ), train = train
            )
        
        res <- result$result
        ae <- abs(res$ecost - res$tcost)
        ae <- ae[!is.na(ae)]
        re <- ae / res$tcost
        re <- re[!is.na(re)]
        # print_dist(ae,re,paste("FM ", what,"\n",paramstr,sep=""))
        titlestring <- sprintf("%s\n%s, region.costs=%i\n%s", method.name, model.name, region.costs, build_paramstr(result$params))
        print_dist(ae,re,titlestring)
        cat(sprintf("rejected: %4.2f%%\n", 100 * sum(is.na(res$ecost)) / length(res$tcost)))
        
        save_dist(region.costs, result, model.name, method.name)
        #save_best_iter(result, model.name)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
}


print("done!")


