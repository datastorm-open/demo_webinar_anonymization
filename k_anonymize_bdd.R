#########################
###  K-ANONYMIZE BDD  ###
#########################

# using sdcMicro + mdav algorithm
require(data.table)
require(FactoMineR)
require(sdcMicro)
require(rAmCharts)
require(manipulateWidget)

# PRISE EN COMPTE DE LA DATE DE NAISSANCE
# TEST SANS LES VARIABLES NOMINALES

### anonymization
source("bdd/script_bdd.R", local = FALSE, encoding = "utf-8")
data[, c("Nom", "freq. depl. cut", "poids cut", "Cancer", "Cirrhose", "Diabète", "Hypertension", "Immuno-déficience", "Pathologie respiratoire") := NULL]

# recode Date de naissance
data[, `Date de naissance` := as.numeric(as.Date(data$`Date de naissance`, format = "%d/%m/%Y", origin = lubridate::origin))]
# replace Poids + Taille by IMC
data[, IMC := Poids/(Taille/100)**2][, c("Poids", "Taille") := NULL]

# /!\ The MDAV algorithm work on numeric variables only, hence we will transform the dataset by applying a PCA
# we will anonymize all the components, get the anonymization groups, and then transform the idv in each group

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
data_pca <- to_dummy(data, cols = names(which(sapply(data, function(x) is.character(x) || is.logical(x)))))

# transform data with a CAH to get only numeric variables
data_pca <- FactoMineR::PCA(X = data.frame(data_pca),
                            scale.unit = F,
                            graph = F,
                            ncp = 20)$ind$coord

# chose method (to form clusters) and k (number of observation per cluster)
method <- "mdav"
k = 10

# anonymize the PCA using sdcMicro 
data_pca_anonym <- sdcMicro::microaggregation(obj = as.data.frame(data_pca),
                                              variables = names(data_pca), 
                                              aggr = k, 
                                              method = method)$mx

# retrieve anonymization groups
anonymization_groups <- data.table(data_pca_anonym)[, group := .GRP, by = c(names(data_pca_anonym))]$group
data_anonym <- copy(data)[, "group" := anonymization_groups]

# anonymize idv in each group :
# _numeric variables : use mean
# _character variables : use mode
cols_num <- names(data)[sapply(data, is.numeric)]
data_anonym[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]
data_anonym[, (cols_num) := lapply(.SD, function(x) mean(x)), .SDcols = cols_num, by = "group"]
cols_char <- setdiff(names(data), cols_num)
data_anonym[, (cols_char) := lapply(.SD, as.character), .SDcols = cols_char]
data_anonym[, (cols_char) := lapply(.SD, function(x) names(which.max(table(x)))), .SDcols = cols_char, by = "group"]
data_anonym[, (cols_char) := lapply(.SD, as.factor), .SDcols = cols_char]
data_anonym[, "group" := NULL]

# recode back Date de naissance
# recode Date de naissance
data_anonym[, `Date de naissance` := as.Date(data_anonym$`Date de naissance`, origin = lubridate::origin)]


### check results
labels = c("Direction commerciale" = "Commerce", 
           "Direction des opérations" = "Opérationnel", 
           "Direction financières & RH" = "Finance & RH", 
           "Direction R&D" = "R&D")
dt = data.table(table(data$Département))
colnames(dt) = c("label", "value")
dt$label = unname(labels[dt$label])
pie_original <- plot(amPie(dt, 
                           main=paste0("Répartition des employés au sein des départements avant anonymisation (k=", k, ")")))
dt = data.table(table(data_anonym$Département))
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

plt_original <- rAmCharts::amHist(data$`Freq. déplacements`, breaks = seq(0, ceiling(max(data$`Freq. déplacements`)/5)*5, 5),
                                  col = "#5c0047", border = "white",
                                  xlab="Fréquence de déplacement (Jour/An)", 
                                  ylab="Fréquence", 
                                  main="Répartition de la fréquence de déplacement")
plt_anonym <- rAmCharts::amHist(data_anonym$`Freq. déplacements`, breaks = seq(0, ceiling(max(data$`Freq. déplacements`)/5)*5, 5),
                                col = "#5c0047", border = "white",
                                xlab="Fréquence de déplacement (Jour/An)", 
                                ylab="Fréquence", 
                                main="Répartition de la fréquence de déplacement")
combineWidgets(list = list(plot(plt_original), plot(plt_anonym)))