#########################
###  K-ANONYMIZE BDD  ###
#########################

#' k-anonymize the generated dataset
#'
#' @param data \code{data.table}. The demo dataset to be k-anonymize.
#' @param k \code{integer}. The number of indivuals per k-anonymization groups.
#' @param method \code{character}. The method to be used for k-anonymization (See \code{\link{sdcMicro}}).
#'
#' @return the anonymized dataset
#' @export
#' 
#' @import data.table sdcMicro rAmCharts manipulateWidget
k_anonymize_bdd <- function(data, 
                            k = 10,
                            method = "mdav",
                            sensitive = c("Cancer", "Cirrhose", "Diabete", "Hypertension", "Immuno deficience", "Pathologie respiratoire")) {
  
  # remove useless columns
  data[, c("Nom", "freq. depl. cut", "poids cut") := NULL]
  
  # recode 'Date de naissance' as an age for anonymization
  data[, `Date de naissance` := as.numeric(as.Date(data$`Date de naissance`, format = "%d/%m/%Y", origin = lubridate::origin))]
  # replace Poids + Taille by IMC
  data[, IMC := Poids/(Taille/100)**2][, c("Poids", "Taille") := NULL]
  
  # /!\ The MDAV algorithm work on numeric variables only, hence we will binarize the character variables
  # binarize character variables
  to_dummy <- function(data, cols) {
    data <- data.table(data)
    for (col in cols) {
      mods <- unique(as.vector(data[[col]]))
      for (mod in mods) {
        data[, paste0(col, "_", mod) := as.numeric(get(col) == mod)]
      }
      data[, (col) := NULL]
    }
    data
  }
  data_bin <- to_dummy(data[, -sensitive, with=F], 
                       cols = names(which(sapply(data, function(x) is.character(x) || is.logical(x)))))
  # center + scale
  data_bin <- data_bin[, (names(data_bin)) := lapply(.SD, function(x) (x-mean(x))/sd(x))]
  
  # anonymize the binarized data using sdcMicro 
  data_bin_anonym <- sdcMicro::microaggregation(obj = as.data.frame(data_bin),
                                                variables = names(data_bin), 
                                                aggr = k, 
                                                method = method)$mx
  
  # retrieve anonymization groups
  anonymization_groups <- data.table(data_bin_anonym)[, group := .GRP, by = c(names(data_bin_anonym))]$group
  data_anonym <- data[, "group" := anonymization_groups]
  
  # anonymize idv in each group :
  # _numeric variables : use mean
  # _character variables : use mode
  cols_num <- names(which(sapply(data[, -sensitive, with=F], is.numeric)))
  data_anonym[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]
  data_anonym[, (cols_num) := lapply(.SD, function(x) mean(x)), .SDcols = cols_num, by = "group"]
  cols_char <- names(which(sapply(data[, -sensitive, with=F], is.character)))
  data_anonym[, (cols_char) := lapply(.SD, as.character), .SDcols = cols_char]
  data_anonym[, (cols_char) := lapply(.SD, function(x) names(which.max(table(x)))), .SDcols = cols_char, by = "group"]
  data_anonym[, (cols_char) := lapply(.SD, as.factor), .SDcols = cols_char]
  # [DEV] data_anonym[, "group" := NULL]
  
  # recode back Date de naissance
  data_anonym[, `Date de naissance` := as.Date(data_anonym$`Date de naissance`, origin = lubridate::origin)]
  # add back comorbidities
  data_anonym[, (sensitive) := data[, sensitive, with = F]]
  
  return(data_anonym)
}
