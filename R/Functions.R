library(tidyr)
library(dplyr)  


## Fonctions ----
# Modele liste d'espece ----
# Modele liste d'espece ----
# fonction contenant les modeles a repeter 100 fois
fun_fit <- function(k,matrice_tot, i) {
  # Echantillonnage des 0
  test1 <-
    matrice_tot[sample(row.names(matrice_tot),
                       nrow(matrice_tot),
                       replace = T), ]
  # Modele nul
  z1 <- test1[, i]
  glm_nul1 <-
    glm(z1 ~ 1, family = "binomial", data = test1)
  
  # Modele avec variables environnementales seules
  glm_envir1 <-
    step(
      glm_nul1,
      ~ .  + Dist_lisiere_act + Dist_route + FORMATION + STRUCTURE + Altitude + poly(Altitude, 2, raw = T) + Pente + poly(Pente, 2, raw = T)
      + exposition + poly(exposition, 2, raw = T) + pH + poly(pH, 2, raw = T)
      + phosphore + poly(phosphore, 2, raw = T)
      + Azote + poly(Azote, 2, raw = T)
      + limons + argile,
      direction = "both",
      trace = 0
    )
  # Ajout du type de foret comme predicteur
  glm_type_F1 <- update(glm_envir1, ~ . + TYPE_FORET)
  
  ## Remplissage du tableau result_repet.
  
  # Si AIC du modele avec le type de foret <AIC du modele avec variable envir, il ya a un effet du type de foret, Detla AIC fixe a 5.
  result_repet <- c(ifelse(AIC(glm_type_F1) + 5 < AIC(glm_envir1), 1, 0),
                    glm_type_F1[["coefficients"]][["TYPE_FORETForet recente"]],
                    AIC(glm_envir1),
                    AIC(glm_type_F1))
  names(result_repet) <- c("Effet_FA", "Coef_FR", "AIC_envir", "AIC_FA")
  return(as.data.frame(t(result_repet)))
}

FUN_RES_SP <- function(i, N_resample, matrice_tot){
  
  
  # Boucle pour repeter 100 fois les modeles en echantillonant aleatoirement avec remise.
  result_list <- lapply(1:N_resample,
                        FUN = fun_fit, matrice_tot, i)
  
  res <- dplyr::bind_rows(result_list)
  
  ## Remplissage de la matrice SORTIE_sp:
  SORTIE_sp <-
    data.frame(matrix(0, nrow = 1, ncol = 14))
  names(SORTIE_sp) <-
    c("Occurence",
      "nb_effet",
      "effet_type_F",
      "coef_mean",
      "coef_type_F_95inf",
      "coef_type_F_95sup",
      "AIC_envir_mean",
      "AIC_envir_95inf",
      "AIC_envir_95sup",
      "AIC_type_F_mean",
      "AIC_type_F_95inf",
      "AIC_type_F_95sup","diff_mean","diff_95")
  SORTIE_sp$Occurence[1] <-
    sum(as.numeric(paste(matrice_tot[, i])))
  # Abondance de l'espece dans le jeux de donnees

  # Moyenne des coeficients
  SORTIE_sp$coef_mean[1] <-   mean(res[, "Coef_FR"])
  SORTIE_sp$AIC_envir_mean[1] <-   mean(res[, 3])
  SORTIE_sp$AIC_type_F_mean[1] <-   mean(res[, 4])
  SORTIE_sp$diff_mean[1] <-   mean(res[, 3]-res[,4])
  
  # Valeur min et max des coefficients et des AIC.
  SORTIE_sp$nb_effet[1] <- sum(res[, 1])
  SORTIE_sp$coef_type_F_95inf[1] <- mean(res[, 2])-1.98*(sd(res[, 2]))
  SORTIE_sp$coef_type_F_95sup[1] <- mean(res[, 2])+1.98*(sd(res[, 2]))
  SORTIE_sp$AIC_envir_95inf[1] <- mean(res[, 2])-1.98*(sd(res[, 3]))
  SORTIE_sp$AIC_type_F_95inf[1] <- mean(res[, 4])-1.98*(sd(res[, 4]))
  SORTIE_sp$AIC_envir_95sup[1] <- mean(res[, 3])+1.98*(sd(res[, 3]))
  SORTIE_sp$AIC_type_F_95sup[1] <- mean(res[, 4])+1.98*(sd(res[, 4]))
  SORTIE_sp$diff_95[1] <-   quantile(x=(res[, 3]-res[,4]),0.05)
  SORTIE_sp$effet_type_F[1] <- ifelse(SORTIE_sp$diff_95[1]>=5,1,0)
  
  return(SORTIE_sp)
}

# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste <- function(pres_abs_sp, matrice_tot, N_resample = 100) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- foreach(i = 1:(length(pres_abs_sp)),
                         .export = c("FUN_RES_SP", "fun_fit"),
                         .packages = "igraph") %dopar% {
                           FUN_RES_SP(i, N_resample, matrice_tot)
                         }
  res <- dplyr::bind_rows(result_list)
  return(res)
}

# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_listeb <- function(pres_abs_sp, matrice_tot, N_resample = 100) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  library(tidyr)
  library(dplyr)  
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- mclapply(1:(length(pres_abs_sp)),
                          FUN_RES_SP, N_resample, matrice_tot,
                          mc.cores = 5)
  
  res <- dplyr::bind_rows(result_list)
  return(res)
}


# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste2 <- function(pres_abs_sp, matrice_tot, N_resample = 100) {
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- lapply(1:(length(pres_abs_sp)),
                        FUN = FUN_RES_SP, N_resample, matrice_tot)
  
  res <- dplyr::bind_rows(result_list)
  return(res)
}

# matrice de presence/pseudo_absence
matrice_sp <- function(data) {
  sp_occ <- data.frame(table(data$CdNom)) #comptage sp
  sp_occ <-
    sp_occ[sp_occ$Freq >= 20, ] # selection sp presentes au moins 20 fois
  occ_20 <- merge(data, sp_occ, by.x = "CdNom", by.y = "Var1")
  pres_abs1 <-
    as.data.frame(tapply(rep(1, dim(occ_20)[1]), list(occ_20$X_Y, occ_20$CdNom),
                         function(x) {
                           max(x, na.rm = F)
                         }))
  pres_abs1[is.na(pres_abs1)] = 0
  all_data_envir <-
    dplyr::select(occ_20, ESSENCE:FORMATION, X_Y:Grossier)
  pres_abs <-
    merge(pres_abs1, all_data_envir, by.x = 0, by.y = "X_Y")
}



## Function to format data

format_data <- function(path, Groupe_Select = "Plantes"){
  PN_Point <-
    read.csv(path,
             row.names = "X",
             header = T)
  PN_Point<-PN_Point[PN_Point$FORMATION %in% c("Coniferes","Melange","Feuillus"),]
  
  PN_Point <- drop_na(PN_Point, pH)
  PN_Point <- dplyr::select(PN_Point, -Dist_lisiere_anc)
  colnames(PN_Point)[names(PN_Point) %in% "distance_FA"] <- "Dist_lisiere_anc"
  PN_Point$Dist_route <- log(PN_Point$Dist_route + 1)
  PN_Point$Dist_lisiere_act <- log(PN_Point$Dist_lisiere_act + 1)
  
  list_select <- list("Plantes" = c("Angiospermes", "Gymnospermes"),
                      "Mammiferes" = c("Mammiferes"),
                      "Reptiles" = c("Reptiles"),
                      "Arthros" = c("Arachnides", "Insectes"),
                      "Oiseaux" = c("Oiseaux"))
  
  PN_Select <-
    PN_Point[PN_Point$Groupe %in% list_select[[Groupe_Select]],]
  #PN_Select <- subset(PN_Select, Source != "ONCFS")
  matrice_Select <-
    matrice_sp(PN_Select) # matrice presence-absence avec variables
  pres_abs_Select <-
    dplyr::select(matrice_Select,-c("ESSENCE":"Grossier")) # matrice de presence-absence seules
  
  return(list(mat = matrice_Select, pres_abs = pres_abs_Select[, -1]))  
}


Fun_Fit_Parc_Group <- function (Parc = "PNV", Groupe_Select = "Plantes"){
  list_df <- format_data(path = file.path("data",paste0(Parc, "_DATA_POINTS1.csv")), 
                         Groupe_Select = "Plantes")
  
  start.time <- Sys.time()
  ResFit <-
    Analyse_listeb(list_df$pres_abs, list_df$mat, N_resample = 100)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,"_Sorties")))
  print("done")
}


Fun_Fit_Parc_Group_Seq <- function (Seq_Sel, Parc = "PNV", Groupe_Select = "Plantes"){
  list_df <- format_data(path = file.path("data",paste0(Parc, "_DATA_POINTS1.csv")), 
                         Groupe_Select = "Plantes")
  ncols <- ncol(list_df$pres_abs)
  
  0:9*floor(ncols/10)+1
  c(1:9*floor(ncols/10), ncols)
  start.time <- Sys.time()
  ResFit <-
    Analyse_liste2(list_df$pres_abs, list_df$mat, N_resample = 100)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,"_Sorties_", Seq_Sel, ".csv")))
  print("done")
}

