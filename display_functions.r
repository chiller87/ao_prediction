#
# Hilfsfunktionen zur Ausgabe und Visualisierung der Fehlerverteilungen:
#
#   plot_dist & print_dist
#

# zeichnet Verteilungskurve der Fehler
plot_dist <- function(data, title, to=NA, xlab="") {
  if (is.na(to)) {
    plot(density(data), main=title, xlab=xlab)
  } else {
    plot(density(data, to=to), main=title, xlab=xlab)
  }    
  m = mean(data)
  M = median(data)
  abline(v=m, col="red")
  abline(v=M, col="blue")
  legend("topright", c(sprintf("mean (%4.3f)      ", m), sprintf("median (%4.3f)      ", M)), col=c("red","blue"), lty=c(1,1))
}

# gibt Kennzahlen aus
print_dist <- function(ae, re, title) {
    cat("\n",title,"\n",sep="")
    probs_ae <- c(200,300,400,500)
    cdf_ae <- ecdf(ae)(probs_ae)
    cat(sprintf("AE: mean=%.2f, median=%.2f,", mean(ae), median(ae)))
    for (i in 1:length(probs_ae)) {
        cat(sprintf("P(AE<%d)=%5.4f, ", probs_ae[i], cdf_ae[i]))
    }
    cat("\nAE (CSV): ")
    cat(mean(ae), median(ae), cdf_ae, sep=";")
    cat("\n")
    probs_re <- c(0.1,0.2,0.3,0.4,0.5)
    cdf_re <- ecdf(re)(probs_re)
    cat(sprintf("RE: mean=%.4f, median=%.4f,", mean(re), median(re)))
    for (i in 1:length(probs_re)) {
        cat(sprintf("P(RE<%3.2f)=%5.4f, ", probs_re[i], cdf_re[i]))
    }
    cat("\nRE (CSV): ")
    cat(mean(re), median(re), cdf_re, sep=";")
    cat("\n")
}

# bereitet Parameterliste als String auf
build_paramstr <- function(params) {
    paramstr <- paste(names(params)[1],"=",params[[1]], sep="")
    for (i in 2:length(params)) {
        paramstr <- paste(paramstr, ",", names(params)[i], "=", params[[i]], sep="")
    }
    return(paramstr)
}
