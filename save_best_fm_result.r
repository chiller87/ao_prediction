save_best_iter <- function(result, model) {
  res = result$fmres
  params = result$params
  
  print("save data to csv")
  write.table(res,file=paste("auswertung/",model,"/",paste(model,"test",method.name, "libfm",sep="_"),".csv",sep=""), sep=",",append=TRUE, col.names=FALSE, row.names=FALSE)

  return(res)
  
}