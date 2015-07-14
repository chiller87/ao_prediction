# 
# Testlauf zur Messung von Fehlern
#

if (!exists("print_dist", mode="function")) {
    source("display_functions.r")
}
if (!exists("edit_error", mode="function")) {
    source("editing.r")
}
if (!exists("calculateRegionMatrix", mode="function")) {
    source("calculate_region_matrix.r")
}
source("eba_method.r")


# Auswahl des Modells
#--------------------------------------------

method.name <- "FM"
model.name.range <- c("GOLF", "SERIES 3", "POLO", "C-CLASS", "ASTRA", "PASSAT")
k.range = c(30)
region.costs.range <- c(FALSE)
PZ.range <- c(0) #,0.005, 0.01, 0.015, 0.02, 0,025)
PNZ.range <- c(0) #, 0.001, 0.002)
damage_classes.range <- c('none')

for (model.name in model.name.range) {
  
  for (region.costs in region.costs.range) {
  for (k in k.range) {  
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
    car.model <- read.table(paste("../../data/overview_",model.name,".csv",sep=""), sep=",", header=TRUE)  
  }

  # Pruning der Trainingsdaten
  #train <- edit_error(car.model, times.median=3.0, error.is.re=FALSE)
  #cat("Anzahl beim Training entfernt:", sum(!train), "von", length(train))
  train <- NA

  result <- eba_method(car.model, region.costs=FALSE, list(k=30,distance_type="jaccard",weight_leistung=0.0,weight_jahr=0.0, damage_classes=damage_classes), train=train)
  
  res <- result$result
  ae <- abs(res$ecost-res$tcost)
  ae <- ae[!is.na(ae)]
  re <- ae/res$tcost
  re <- re[!is.na(re)]
  #print_dist(ae,re,paste("EBA ", what,"\n",paramstr,sep=""))
  titlestring <- sprintf("%s\n%s, region.costs=%i\n%s", method.name, model.name, region.costs, build_paramstr(result$params))
  print_dist(ae,re,titlestring)
  cat(sprintf("rejected: %4.2f%%\n", 100*sum(is.na(res$ecost))/length(res$tcost)))
  
  save_dist(region.costs, result, model.name, method.name)
  }
  }
  }
  }
  }
}

