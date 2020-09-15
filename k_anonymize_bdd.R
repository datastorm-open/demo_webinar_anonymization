#########################
###  K-ANONYMIZE BDD  ###
#########################

# using sdcMicro + mdav algorithm
require(data.table)
require(sdcMicro)
require(rAmCharts)
require(manipulateWidget)

# PRISE EN COMPTE DE LA DATE DE NAISSANCE
# TEST SANS LES VARIABLES NOMINALES

### anonymization
source("bdd/script_bdd.R", local = FALSE, encoding = "utf-8")
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
data_bin <- to_dummy(data[, -c("Cancer", "Cirrhose", "Diabete", "Hypertension", "Immuno deficience", "Pathologie respiratoire")], 
                     cols = names(which(sapply(data, function(x) is.character(x) || is.logical(x)))))
# center + scale
data_bin <- data_bin[, (names(data_bin)) := lapply(.SD, function(x) (x-mean(x))/sd(x))]


# choose method (to create clusters) and k (number of observation per cluster)
method <- "mdav"
k = 10

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
cols_num <- names(which(sapply(data[, -c("Cancer", "Cirrhose", "Diabete", "Hypertension", "Immuno deficience", "Pathologie respiratoire")], is.numeric)))
data_anonym[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]
data_anonym[, (cols_num) := lapply(.SD, function(x) mean(x)), .SDcols = cols_num, by = "group"]
cols_char <- names(which(sapply(data[, -c("Cancer", "Cirrhose", "Diabete", "Hypertension", "Immuno deficience", "Pathologie respiratoire")], is.character)))
data_anonym[, (cols_char) := lapply(.SD, as.character), .SDcols = cols_char]
data_anonym[, (cols_char) := lapply(.SD, function(x) names(which.max(table(x)))), .SDcols = cols_char, by = "group"]
data_anonym[, (cols_char) := lapply(.SD, as.factor), .SDcols = cols_char]
# data_anonym[, "group" := NULL]

# recode back Date de naissance
data_anonym[, `Date de naissance` := as.Date(data_anonym$`Date de naissance`, origin = lubridate::origin)]



### check results
labels = c("Direction commerciale" = "Commerce", 
           "Direction des opérations" = "Opérationnel", 
           "Direction financières & RH" = "Finance & RH", 
           "Direction R&D" = "R&D")
dt = data.table(table(data$Departement))
colnames(dt) = c("label", "value")
dt$label = unname(labels[dt$label])
pie_original <- plot(amPie(dt, 
                           main=paste0("Répartition des employés au sein des départements avant anonymisation")))
dt = data.table(table(data_anonym$Departement))
colnames(dt) = c("label", "value")
dt$label = unname(labels[dt$label])
pie_anonym <- plot(amPie(dt, 
                         main=paste0("Répartition des employés au sein des départements avant anonymisation (k=", k, ")")))
combineWidgets(list = list(pie_original, pie_anonym))

dt = data.table(table(data$Statut))
colnames(dt) = c("label", "value")
pie_original <- plot(amPie(dt, 
                           main = paste0("Répartition des employés suivant leur statut avant anonymisation (k=", k, ")")))
dt = data.table(table(data_anonym$Statut))
colnames(dt) = c("label", "value")
pie_anonym <- plot(amPie(dt, 
                            main = paste0("Répartition des employés suivant leur statut après anonymisation (k=", k, ")")))
combineWidgets(list = list(pie_original, pie_anonym))

df = data.frame(table(cut(data$`Volume horaire`, 
                          seq(floor(min(data$`Volume horaire`)/5)*5, ceiling(max(data$`Volume horaire`)/5)*5, 5), 
                          include.lowest = T, right = F)))
plt_original <- amBarplot("Var1", "Freq", df, horiz = T,
                          xlab="Volume horaire réel", ylab="Fréquence", show_values = T,
                          main="Répartition du volume horaire moyen effectué")
plt_original@dataProvider <- lapply(1:nrow(df), function(x) {plt_original@dataProvider[[x]]$color <- colorRampPalette(c("#edd7e8", "#5c0047"))(nrow(df))[x] ; plt_original@dataProvider[[x]]})
df = data.frame(table(cut(data_anonym$`Volume horaire`, 
                          seq(floor(min(data_anonym$`Volume horaire`)/5)*5, ceiling(max(data_anonym$`Volume horaire`)/5)*5, 5), 
                          include.lowest = T, right = F)))
plt_anonym <- amBarplot("Var1", "Freq", df, horiz = T,
                        xlab="Volume horaire réel", ylab="Fréquence", show_values = T,
                        main="Répartition du volume horaire moyen effectué")
plt_anonym@dataProvider <- lapply(1:nrow(df), function(x) {plt_anonym@dataProvider[[x]]$color <- colorRampPalette(c("#edd7e8", "#5c0047"))(nrow(df))[x] ; plt_anonym@dataProvider[[x]]})
combineWidgets(list = list(plot(plt_original), plot(plt_anonym)))

plt_original <- rAmCharts::amHist(data$`Freq. deplacements`, breaks = seq(0, ceiling(max(data$`Freq. deplacements`)/5)*5, 5),
                                  col = "#5c0047", border = "white",
                                  xlab="Fréquence de déplacement (Jour/An)", 
                                  ylab="Fréquence", 
                                  main="Répartition de la fréquence de déplacement")
plt_anonym <- rAmCharts::amHist(data_anonym$`Freq. deplacements`, breaks = seq(0, ceiling(max(data$`Freq. deplacements`)/5)*5, 5),
                                col = "#5c0047", border = "white",
                                xlab="Fréquence de déplacement (Jour/An)", 
                                ylab="Fréquence", 
                                main="Répartition de la fréquence de déplacement")
combineWidgets(list = list(plot(plt_original), plot(plt_anonym)))

