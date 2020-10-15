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

age <- round(pmin(pmax(18, rnorm(n, 45, 10)), 81))
mois <- sample(1:12, size = n, replace = T)
max_jour <- list("1" = 31, "2" = 28, "3" = 31, "4" = 30, "5" = 31, "6" = 30, "7" = 31, "8" = 31, "9" = 30, "10" = 31, "11" = 30, "12" = 31)
gen_date_naissance <- paste0(sapply(mois, function(x) {sample(1:max_jour[[x]], 1)}, USE.NAMES = F), "/", mois, "/", 2020 - age)

gen_taille <- round(sapply(gen_sexe, function(x) {if (x == "F") {rnorm(1, 165, 6)} else {rnorm(1, 176, 6)}}, USE.NAMES = F))

imcs <- c(16, 18.5, 24.9, 29.9, 39.9, 45)
imc_tranche <- sample(c("18.5", "24.9", "29.9", "39.9", "45"), size = n, replace = T, prob = c(3.5, 49.2, 32.3, 13.8, 1.2))
imc <- sapply(imc_tranche, function(x) {runif(1, imcs[which(x == imcs) - 1], imcs[which(x == imcs)])}, USE.NAMES = F)
gen_poids <- round(imc*(gen_taille/100)**2, 1)

gen_total_produits <- sapply(age, function(x) {round(max(1, rnorm(1, (max(age) - 10 - x)/26, (52 - abs(30 - x))/25)))}, USE.NAMES = F)

pratiques <- c("Professionnelle", "Intensive", "Régulière", "Occasionnelle")
gen_pratique_declaree <- sapply(age, function(x) {sample(pratiques, size = 1, prob = c(500*abs(max(age) + 1 - x)**2/x**2, 1200*abs(max(age) + 1 - x)/x, 3*abs(min(age) - 1 - x)*x, 1/1000*abs(min(age) - 1 - x)**2*x**2))}, USE.NAMES = F)

activites <- c("Marche", "Course", "Cycle", "Natation")
activites_probs <- list("Professionnelle" = c(5, 50, 35, 10), "Intensive" = c(7, 45, 35, 10), "Régulière" = c(10, 60, 20, 10), "Occasionnelle" = c(40, 40, 15, 5))
gen_activite_principale <- sapply(gen_pratique_declaree, function(x, y) {sample(activites, size = 1, prob = activites_probs[[x]])}, USE.NAMES = F)

activites_mensuelle_freq_pratique <- list("Professionnelle" = 10, "Intensive" = 6, "Régulière" = 3, "Occasionnelle" = 1)
gen_freq_activites_mensuelles <- pmin(pmax(round(sapply(gen_pratique_declaree, function(x) {rnorm(1, activites_mensuelle_freq_pratique[[x]]*3, activites_mensuelle_freq_pratique[[x]]/2)}, USE.NAMES = F)), 1), 50)
gen_freq_activites_mensuelles_cut <- cut(gen_freq_activites_mensuelles, breaks = seq(0, ceiling(max(gen_freq_activites_mensuelles)/5)*5, 5), include.lowest = T, right = F,
                                labels = paste0("[", seq(0, ceiling(max(gen_freq_activites_mensuelles)/5)*5, 5)[- length(seq(0, ceiling(max(gen_freq_activites_mensuelles)/5)*5, 5))], "-", seq(0, ceiling(max(gen_freq_activites_mensuelles)/5)*5, 5)[-1]-1, "]"))

volume_activites <- list("Marche" = 1, "Course" = 1.5, "Cycle" = 2.3, "Natation" = 1.9)
gen_volume_horaire_mensuel <- round(unlist(mapply(function(x, y) {rnorm(1, volume_activites[[x]]*y, sqrt(volume_activites[[x]]*y))}, gen_activite_principale, gen_freq_activites_mensuelles, USE.NAMES = F))/5)*5

profil_public_pratique <- list("Professionnelle" = 80, "Intensive" = 60, "Régulière" = 25, "Occasionnelle" = 10)
gen_profil_public <- sapply(gen_pratique_declaree, function(x) sample(c(T, F), size = 1, prob = c(profil_public_pratique[[x]], 100 - profil_public_pratique[[x]])), USE.NAMES = F)

risque_tachycardie_pratique <- list("Professionnelle" = 100, "Intensive" = 70, "Régulière" = 20, "Occasionnelle" = 10)
gen_risque_tachycardie <- mapply(function(x, y) {sample(c(T, F), size = 1, replace = T, prob = c(x/2-17 + risque_tachycardie_pratique[[y]], 500))}, age, gen_pratique_declaree, USE.NAMES = F)
gen_risque_hypertension <- sample(c(T, F), size = n, replace = T, prob = c(1, 3))

strava_pratique <- list("Professionnelle" = 70, "Intensive" = 75, "Régulière" = 10, "Occasionnelle" = 5)
gen_strava <- sapply(gen_pratique_declaree, function(x) sample(c(T, F), size = 1, prob = c(strava_pratique[[x]], 100 - strava_pratique[[x]])), USE.NAMES = F)
twitter_pratique <- list("Professionnelle" = 85, "Intensive" = 50, "Régulière" = 5, "Occasionnelle" = 1)
gen_twitter <- sapply(gen_pratique_declaree, function(x) sample(c(T, F), size = 1, prob = c(twitter_pratique[[x]], 100 - twitter_pratique[[x]])), USE.NAMES = F)
facebook_pratique <- list("Professionnelle" = 95, "Intensive" = 90, "Régulière" = 5, "Occasionnelle" = 1)
gen_facebook <- sapply(gen_pratique_declaree, function(x) sample(c(T, F), size = 1, prob = c(facebook_pratique[[x]], 100 - facebook_pratique[[x]])), USE.NAMES = F)

optin_pratique <- list("Professionnelle" = 99, "Intensive" = 90, "Régulière" = 66, "Occasionnelle" = 33)
gen_opt_in <- sapply(gen_pratique_declaree, function(x) sample(c(T, F), size = 1, prob = c(optin_pratique[[x]], 100 - optin_pratique[[x]])), USE.NAMES = F)

data <- data.table("Nom" = gen_nom,
                   "Sexe" = gen_sexe,
                   "Date de naissance" = gen_date_naissance,
                   "Taille" = gen_taille,
                   "Poids" = gen_poids,
                   "Total produits" = gen_total_produits,
                   "Pratique declaree" = gen_pratique_declaree,
                   "Activite principale" =  gen_activite_principale,
                   "Freq. activites mensuelles" = gen_freq_activites_mensuelles,
                   "Volume horaire mensuel" = gen_volume_horaire_mensuel,
                   "Profil public" = gen_profil_public,
                   "Risque tachycardie" = gen_risque_tachycardie,
                   "Risque hypertension" = gen_risque_hypertension,
                   "Strava" = gen_strava,
                   "Twitter" = gen_twitter,
                   "Facebook" = gen_facebook,
                   "Opt_in" = gen_opt_in,
                   "freq. activites mensuelles cut" = gen_freq_activites_mensuelles_cut)