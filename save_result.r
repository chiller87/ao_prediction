save_dist <- function(region.costs, result, model, method.name) {
  res = result$result
  params = result$params
  #print(params)
  ae <- abs(res$ecost-res$tcost)
  ae <- ae[!is.na(ae)]
  re <- ae/res$tcost
  re <- re[!is.na(re)]
  #save_dist <- function(ae, re, parameter, title,model, reject) {
 # cat("\n",title,"\n",sep="")
  probs_ae <- c(200,300,400,500)
  cdf_ae <- ecdf(ae)(probs_ae)
  probs_re <- c(0.1,0.2,0.3,0.4,0.5)
  cdf_re <- ecdf(re)(probs_re)
  mae <- mean(ae)
  medae <- median(ae)
  mre <- mean(re)
  medre <- median(re)
  reject <- 100*sum(is.na(res$ecost))/length(res$tcost)
  maere.names <- c("mean(AE)", "median(AE)","P(AE<200)", "P(AE<300)", "P(AE<400)", "P(AE<500)")
  maere.names <- c(maere.names, "mean(RE)", "median(RE)","P(RE<0.10)", "P(RE<0.20)", "P(RE<0.30)", "P(RE<0.40)","P(RE<0.50)","reject")
  maere <- c(mae, medae, cdf_ae, mre, medre, cdf_re, reject)
  names(maere) <- maere.names
  #print(maere)
  
  params$metadata <- paste(params$metadata, collapse=" ")
  data_f <- t(c(model,params,region.costs,mae, medae, cdf_ae, mre, medre, cdf_re, reject))

  print("save data to csv")
  write.table(data_f,file=paste("auswertung/",model,"/",paste(model,"test",method.name,sep="_"),".csv",sep=""), sep=",",append=TRUE, col.names=FALSE, row.names=FALSE)
  #  write.table(data_f,file=paste("auswertung/",model"",model,"parameter",title,".csv",sep=""), sep=",",append=TRUE, col.names=FALSE, row.names=FALSE)
  
  #write.table(data_f,file="scripts/dalitz/neu/text.csv", sep=",",append=TRUE, col.names=FALSE, row.names=FALSE)
  return(data.frame(data_f))
  
}