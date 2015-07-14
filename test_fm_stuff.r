
# logname <- "auswertung/GOLF/fm/mcmc_k25_i2000_s0.2_sc6.log_test"
# 
# con <- file(logname, open="r")
# # sort lines of logfile
# lines <- readLines(con)
# close(con)
# #print(lines)
# lines <- sort.int(lines)
# #print(lines)
# write(lines, logname)
# 
# 
# best.row <- lines[1]
# best.row.vec <- unlist(strsplit(best.row, split="\t"))
# best.iter <- best.row.vec[3]
# 
# print(paste("best result at ", best.iter, sep=""))
# 
# libfm.iter <- paste("-iter ", best.iter, sep="")
# 
# 
# iter.range <- seq(2000, 100, -100)
# 
# best.data.names <- c("model", "method", "k", "best_iter", "stdev", "reg", "lr", "rmse", "mae")
# best.data <- data.frame("model"=NA, "method"=NA, "k"=NA, "best_iter"=NA, "stdev"=NA, "reg"=NA, "lr"=NA, "strata_nr"=NA, "rmse"=NA, "mae"=NA)[numeric(0),]
# 
# entry <- c("GOLF", "mcmc", 30, 1243, 0.2, 1, 0.003, 1.543, 125.048)
# entry2 <- c("GOLF", "mcmc", 2, 5001, 0.5, 0.3, 0.000432, 0, 10.5643, 198.4012)
# best.data[nrow(best.data)+1,] <- entry2
# 
# nrow(best.data)


# # extract model names
# car.names.all <- car.model[,4]
# car.names.uni <- unique(car.names.all)
# car.names.bin <- matrix(nrow = length(car.names.all), ncol = length(car.names.uni), data=0)
# is <- seq(1, length(car.names.all), 1)
# for(i in is) {
#     row <- i
#     col <- which(car.names.uni==name)
#     car.names.bin[row, col] <- 1
#     print(paste("i=", i, sep=""))
# }
# 
# # combine binary model matrix with regions matrix
# fm.matrix <- cbind(car.names.bin, regions)


# 
# # load all models
# all.models <- list()
# all.models$names <- c("GOLF", "SERIES 3", "POLO", "C-CLASS", "ASTRA", "PASSAT")
# all.models$data <- list()
# all.models$indicators <- list()
# #model <- "C-CLASS"
# for(model in all.models$names) {
#     all.models$indicators[[model]] <- all.models$names
#     all.models$indicators[[model]][all.models$indicators[[model]] != model] <- 0
#     all.models$indicators[[model]][all.models$indicators[[model]] == model] <- 1
#     
#     all.models$data[[model]] <- read.table(paste("../../data/overview_",model,".csv",sep = ""), sep = ",", header = TRUE)
#     #all.models$data[[model]] <- cbind(curr.model.indicator, all.models.data[[model]])
# }
# 
# foo <- c()
# foo <- c(foo, 0, 3, 1, 5)

# engine.types.names <- c("BENZIN", "DIESEL", "GAS", "NA")
# columns.to.drop <- c()
# 
# params <- list(        
#     metadata=c("ENGINE_TYPE_GROUP"), 
#     weight_jahr= 0.0, # TODO
#     weight_leistung= 0.0, # TODO
#     threshold_quantile=0.5,
#     damage_classes="none",
#     distorted=FALSE, # TODO
#     PZ = 0.001, # TODO
#     PNZ=0.01
# )
# 
# region.costs <- FALSE
# train <- NA
# 
# car.model <- read.table(paste("../../data/overview_GOLF.csv",sep=""), sep=",", header=TRUE)
# 
# region_costs <- car.model[,17:(ncol(car.model)-1)]
# region_costs[region_costs == 0] <- NA
# regions <- car.model[,17:(ncol(car.model)-1)]
# regions[regions != 0] <- 1
# 
# # entferne nicht vorkommende Regionen
# N <- apply(regions>0,2,sum)
# existent_regions <- (N > 0)
# region_costs <- region_costs[,N>0]
# regions <- regions[,N>0]
# 
# total_repair_costs <- car.model$TOTAL_REPAIR_COSTS
# 
# explanatory_variables <- colnames(regions)
# 
# 
# 
# # categorize data
# # weitere Metadaten
# explanatory_variables <- append(explanatory_variables, params$metadata)
# regions <- cbind(regions, car.model[params$metadata])
# 
# columns.to.drop <- c(columns.to.drop, "ENGINE_TYPE_GROUP")
# 
# engine.types.vectors <- list()
# for(engine.type in engine.types.names) {
#     #engine.type <- "BENZIN"
#     vec <- c(rep(0, times=nrow(regions)))
#     vec <- replace(vec, regions["ENGINE_TYPE_GROUP"]==engine.type, 1)
#     regions <- cbind(regions, vec)
#     explanatory_variables <- append(explanatory_variables, paste("ET_", engine.type, sep=""))
# }
# 
# colnames(regions) <- explanatory_variables
# 
# drop <- c("ENGINE_TYPE_GROUP", "ET_GAS")
# 
# res <- is.na(regions["ENGINE_TYPE_GROUP"])
# 
# regions <- regions[,!(names(regions) %in% drop)]
# 
# head(regions["ENGINE_TYPE_GROUP"])





# 
# 
# 
# 
# 
# 
# 
# source("fm-prepare_data.r")
# source("fm_method.r")
# 
# 
# method.name <- "FM"
# 
# 
# # load all models
# # create data structure
# all.models <- list()
# all.models$names <- c("GOLF", "SERIES 3", "POLO", "C-CLASS", "ASTRA", "PASSAT")
# all.models$raw_data <- list()
# all.models$data <- list()
# for(model in all.models$names) {
#     # read data from file
#     all.models$raw_data[[model]] <- read.table(paste("../../data/overview_",model,".csv",sep = ""), sep = ",", header = TRUE)
# }
# 
# 
# 
# 
# # initialize ranges (FM training)
# fm.method <- c("mcmc")
# model.name <- c("GOLF")
# k <- 20 
# stdev <- 0.0 
# iter <- 1200
# 
# 
# # initialize ranges (data representations)
# w_year <- 1.0 
# w_kw <- 1.0
# damage_classes <- 'byregion'
# region.costs <- FALSE
# PZ <- 0
# PNZ <- 0
# 
# metadata <- c("ENGINE_TYPE_GROUP")
# 
# 
# 
# 
#     
# all.models$curr.model <- model.name
# 
# 
# # Pruning der Trainingsdaten
# #train <- edit_error(car.model, times.median=3.0, error.is.re=FALSE)
# #cat("Anzahl beim Training entfernt:", sum(!train), "von", length(train))
# 
# train <- NA
# 
# # prepare data per model according to data representation
# for (model in all.models$names) {
#     all.models$data[[model]] <- prepare_data_isolated(
#         all.models$raw_data[[model]],
#         region.costs = region.costs,
#         list(
#             weight_leistung = w_kw,
#             weight_jahr = w_year,
#             damage_classes = damage_classes,
#             metadata = metadata,
#             PZ = PZ,
#             PNZ = PNZ
#         )
#     )
# }
# 
# 
# result <- fm_method(
#     all.models = all.models,
#     some.params = NA,
#     train = train,
#     combined = FALSE
# )
# 
# res <- result$result
# ae <-
#     abs(res$ecost - res$tcost)
# ae <- ae[!is.na(ae)]
# re <- ae / res$tcost
# re <- re[!is.na(re)]
# # print_dist(ae,re,paste("FM ", what,"\n",paramstr,sep=""))
# titlestring <-
#     sprintf(
#         "%s\n%s, region.costs=%i\n%s", method.name, model.name, region.costs, build_paramstr(result$params)
#     )
# print_dist(ae,re,titlestring)
# cat(sprintf("rejected: %4.2f%%\n", 100 * sum(is.na(res$ecost)) / length(res$tcost)))
# 
# save_dist(region.costs, result, model.name, method.name)
# #save_best_iter(result, model.name)
# 
# 
# 
# 
# print("done!")



train <- c()

toadd <- matrix(nrow=5, ncol=8, data=5)

test <- rbind(train, toadd)


















