# 
# Methoden zum Entfernen bestimmter Fahrzeuge aus den Testdaten
#
#   edit_regions - Entfernen aufgrund Regionen
#   edit_error   - Entfernen aufgrund Fehler (AE, RE) von EBA
#

#
# edit_regions
#
# args:
#   (data)     car.model = komplette Fahrzeugdaten im overview_* Format
#   (int)    min.regions = Anzahl Regionen, die mindestens beschaedigt sein muessen
#   (string) not.regions = Vector mit Namen der Regionen, die nicht vorkommen duerfen (NA, wenn nicht)
#
# returns:
#    boolscher Vektor aller Fahrzeuge mit TRUE = Bedingung erfuellt
#
edit_regions <- function(car.model, min.regions=1, not.regions=NA)
{
    regions <- car.model[,17:(ncol(car.model)-1)]
    regions[regions != 0] <- 1
    n_regions <- rowSums(regions)
    condition <- (n_regions >= min.regions)
    if (!is.na(not.regions)) {
        condition <- condition & (rowSums(regions[not.regions]) < 1)
    }
    return(condition)
}

#
# edit_error
#
# args:
#   (data)       car.model = komplette Fahrzeugdaten im overview_* Format
#   (numeric) times.median = Fehler (AE oder RE) muss kleiner sein als Median * times.median
#   (bool)     error.is.re = TRUE fuer RE, FALSE fuer AE
#
# returns:
#    boolscher Vektor aller Fahrzeuge mit TRUE = Bedingung erfuellt
#
edit_error <- function(car.model, times.median=2.0, error.is.re=TRUE, params=NA)
{
    if (!exists("eba_method", mode="function")) {
        source("eba_method.r")
    }
    res <- eba_method(car.model, some.params=params)
    error <- abs(res$result$ecost - res$result$tcost)
    if (error.is.re) {
        error <- error / res$result$tcost
    }
    condition <- (error < (median(error)*times.median))
    return(condition)
}


