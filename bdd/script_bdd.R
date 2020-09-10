###############################
###  Generate QCM database  ###
###############################

require(data.table)



set.seed(100)



### bdd co-morbidités
n = 10000

liste_prenoms <- fread("bdd/prenom.csv")[sum >= 500, ]
liste_noms <- fread("bdd/patronymes.csv")[count >= 100, ]
gen_nom <- paste0(sample(unlist(mapply(rep, liste_prenoms$prenom, times = liste_prenoms$sum, USE.NAMES = F)), size = n, replace = T), " ", sample(unlist(mapply(rep, liste_noms$patronyme, times = liste_noms$count, USE.NAMES = F)), size = n, replace = T))

gen_sexe <- sample(c("F", "H"), size = n, replace = T)

gen_taille <- round(sapply(gen_sexe, function(x) {if (x == "F") {rnorm(1, 165, 6)} else {rnorm(1, 176, 6)}}, USE.NAMES = F))

imcs <- c(16, 18.5, 24.9, 29.9, 39.9, 45)
imc_tranche <- sample(c("18.5", "24.9", "29.9", "39.9", "45"), size = n, replace = T, prob = c(3.5, 49.2, 32.3, 13.8, 1.2))
imc <- sapply(imc_tranche, function(x) {runif(1, imcs[which(x == imcs) - 1], imcs[which(x == imcs)])}, USE.NAMES = F)
gen_poids <- round(imc*(gen_taille/100)**2, 1)

age <- round(pmin(pmax(18, rnorm(n, 45, 10)), 81))
mois <- sample(1:12, size = n, replace = T)
max_jour <- list("1" = 31, "2" = 28, "3" = 31, "4" = 30, "5" = 31, "6" = 30, "7" = 31, "8" = 31, "9" = 30, "10" = 31, "11" = 30, "12" = 31)
gen_date_naissance <- paste0(sapply(mois, function(x) {sample(1:max_jour[[x]], 1)}, USE.NAMES = F), "/", mois, "/", 2020 - age)

gen_enfants_charge <- sapply(age, function(x) {pmax(0, round(rnorm(1, 1-(40-40)**2/175, 1.1)))}, USE.NAMES = F)

departements <- c("Direction commerciale", "Direction financières & RH", "Direction des opérations", "Direction R&D")
gen_departement <- sample(departements, size = n, replace = T, prob = c(25, 40, 20, 15))

statuts <- c("Cadre", "Directeur", "Stagiaire/alternant", "Technicien")
statuts_probs <- list("Direction commerciale" = c(50, 20, 15, 15), "Direction des opérations" = c(40, 10, 20, 30), "Direction R&D" = c(45, 20, 15, 20), "Direction financières & RH" = c(40, 10, 25, 25))
gen_statut <- mapply(function(x, y) {sample(c("Cadre", "Directeur", "Stagiaire/alternant", "Technicien"), size = 1, prob = statuts_probs[[x]]*c(max(age)**3, y**3, max(age)**3/(1 + (y>30)*y**1.5), max(age)**3))}, USE.NAMES = F, gen_departement, age)

deplacement_freq_departement <- list("Direction commerciale" = 10, "Direction financières & RH" = 3, "Direction des opérations" = 5, "Direction R&D" = 6)
deplacement_freq_status <- list("Cadre" = 7, "Directeur" = 10, "Stagiaire/alternant" = 1, "Technicien" = 2)
gen_freq_deplacement <- round(abs(unlist(mapply(function(x, y) {rnorm(1, ((deplacement_freq_departement[[x]]*deplacement_freq_status[[y]])**2)/200, deplacement_freq_departement[[x]]*deplacement_freq_status[[y]]/20)}, gen_departement, gen_statut, USE.NAMES = F))))
gen_freq_deplacement_cut <- cut(gen_freq_deplacement, breaks = seq(0, ceiling(max(gen_freq_deplacement)/5)*5, 5), include.lowest = T, right = F,
                                labels = paste0("[", seq(0, ceiling(max(gen_freq_deplacement)/5)*5, 5)[- length(seq(0, ceiling(max(gen_freq_deplacement)/5)*5, 5))], "-", seq(0, ceiling(max(gen_freq_deplacement)/5)*5, 5)[-1]-1, "]"))

volume_status <- list("Cadre" = 40, "Directeur" = 45, "Stagiaire/alternant" = 35, "Technicien" = 35)
gen_volume_horaire <- round(sapply(gen_statut, function(x) {rnorm(1, volume_status[[x]], (volume_status[[x]] - 32)/2)}, USE.NAMES = F)/5)*5

eligibilite_departement <- list("Direction commerciale" = 5, "Direction financières & RH" = 8, "Direction des opérations" = 5, "Direction R&D" = 10)
eligibilite_status <- list("Cadre" = 8, "Directeur" = 5, "Stagiaire/alternant" = 4, "Technicien" = 2)
gen_eligibilite_teletravail <- c(T, F)[round(abs(unlist(mapply(function(x, y) {round((eligibilite_departement[[x]]/10 + eligibilite_status[[y]]/10 + runif(1, 0, 1))/3)}, gen_departement, gen_statut, USE.NAMES = F)))) + 1]

gen_pathologie_resp <- sapply(age, function(x) {if (x < 40) {sample(c(T, F), size = 1, replace = T, prob = c(1, 49*10))} else {sample(c(T, F), size = 1, replace = T, prob = c(1, 49*9/10))}}, USE.NAMES = F)
gen_cancer <- mapply(function(x, y) {if (x) {sample(c(T, F), size = 1, replace = T, prob = c(2*y**2, 6*mean(age)**2))} else {sample(c(T, F), size = 1, replace = T, prob = c(2*y**2, 59*mean(age)**2))}}, USE.NAMES = F, gen_pathologie_resp, age)
gen_cirrhose <- sapply(age, function(x) {if (x < 40) {sample(c(T, F), size = 1, replace = T, prob = c(1, 330*10))} else {sample(c(T, F), size = 1, replace = T, prob = c(1, 330*9/10))}}, USE.NAMES = F)
gen_diabete <- sapply(imc, function(x) {sample(c(T, F), size = 1, prob = c(1+(x > 25)*(1 + x-25)**3, 19))}, USE.NAMES = F)
gen_hypertension <- sapply(gen_diabete, function(x) {if (x) {sample(c(T, F), size = 1, replace = T, prob = c(2, 1))} else {sample(c(T, F), size = 1, replace = T, prob = c(1, 20))}}, USE.NAMES = F)
gen_immunodeficience <- sapply(gen_cancer, function(x) {if (x) {sample(c(T, F), size = 1, replace = T, prob = c(1, 3))} else {sample(c(T, F), size = 1, replace = T, prob = c(1, 460))}}, USE.NAMES = F)

data <- data.table("Nom" = gen_nom,
                   "Sexe" = gen_sexe,
                   "Date de naissance" = gen_date_naissance,
                   "Taille" = gen_taille,
                   "Poids" = gen_poids,
                   "Enfants a charge" = gen_enfants_charge,
                   "Département" = gen_departement,
                   "Statut" =  gen_statut,
                   "Freq. deplacements" = gen_freq_deplacement,
                   "Volume horaire" = gen_volume_horaire,
                   "Eligibilite teletavail" = gen_eligibilite_teletravail,
                   "Cancer" = gen_cancer,
                   "Cirrhose" = gen_cirrhose,
                   "Diabete" = gen_diabete,
                   "Hypertension" = gen_hypertension,
                   "Immuno deficience" = gen_immunodeficience,
                   "Pathologie respiratoire" = gen_pathologie_resp,
                   "freq. depl. cut" = gen_freq_deplacement_cut,
                   "poids cut" = round(gen_poids))