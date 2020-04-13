library(tidyr)
library(dplyr)


## Fonctions ----
# Modele liste d'espece ----
# Modele liste d'espece ----
# fonction contenant les modeles a repeter N_resample fois
fun_fit <- function(k,matrice_tot, i) {
  # Echantillonnage des 0
  #  resampling only on absence data 
  matrice_tot$response <- matrice_tot[, i+1]
  test1 <- matrice_tot[matrice_tot$response == 1, ]
  test0t <- matrice_tot[matrice_tot$response == 0, ]
  test0 <-
    test0t[sample(row.names(test0t),
                       min(10000, nrow(test0t)),
                       replace = TRUE), ]
  test_fit <- rbind(test1,test0)
  # BASIC GLM
  # Modele nul
  glm_nul1 <-
    glm(response ~ 1, family = "binomial", data = test_fit)

  # Modele avec variables environnementales seules
  start_time <- Sys.time()
  glm_envir1 <-
    step(glm_nul1,
      ~ .  + Dist_lisiere_act  + Dist_route+ FORMATION + STRUCTURE + Altitude +
        poly(Altitude, 2) + Pente + poly(Pente, 2)
      + exposition + poly(exposition, 2) + pH + poly(pH, 2)
      + phosphore + poly(phosphore, 2)
      + Azote + poly(Azote, 2)
      + limons + argile,
      direction = "both",
      trace = 0)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  # 
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
  # Boucle pour repeter N_resample fois les modeles en echantillonant aleatoirement avec remise.
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
    sum(as.numeric(paste(matrice_tot[, i+1])))
  # Abondance de l'espece dans le jeux de donnees

  # Moyenne des coeficients
  SORTIE_sp$coef_mean[1] <-   mean(res[, "Coef_FR"])
  SORTIE_sp$AIC_envir_mean[1] <-   mean(res[, "AIC_envir"])
  SORTIE_sp$AIC_type_F_mean[1] <-   mean(res[, "AIC_FA"])
  SORTIE_sp$diff_mean[1] <-   mean(res[, "AIC_envir"]-res[,"AIC_FA"])

  # Valeur min et max des coefficients et des AIC.
  SORTIE_sp$nb_effet[1] <- sum(res[, "Effet_FA"])
  SORTIE_sp$coef_type_F_95inf[1] <- quantile(res[, "Coef_FR"], probs = 0.025)
  SORTIE_sp$coef_type_F_95sup[1] <- quantile(res[, "Coef_FR"], probs = 0.975)
  SORTIE_sp$AIC_envir_95inf[1] <- quantile(res[, "AIC_envir"], probs = 0.025)
  SORTIE_sp$AIC_type_F_95inf[1] <- quantile(res[, "AIC_FA"], probs = 0.025)
  SORTIE_sp$AIC_envir_95sup[1] <- quantile(res[, "AIC_envir"], probs = 0.975) 
  SORTIE_sp$AIC_type_F_95sup[1] <- quantile(res[, "AIC_FA"], probs = 0.975)  
  SORTIE_sp$diff_95[1] <-   quantile(x=(res[, "AIC_envir"]-res[,"AIC_FA"]),0.05)
  SORTIE_sp$effet_type_F[1] <- ifelse(SORTIE_sp$diff_95[1]>=5,1,0)

  return(SORTIE_sp)
}



# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste <- function(n_start, n_end, matrice_tot, N_resample = 3) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- foreach(i = n_start:n_end,
                         .export = c("FUN_RES_SP", "fun_fit"),
                         .packages = "igraph") %dopar% {
                           FUN_RES_SP(i, N_resample, matrice_tot)
                         }
  res <- dplyr::bind_rows(result_list)
  return(res)
}

# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_listeb <- function(n_start,n_end, matrice_tot, N_resample = 20) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  library(tidyr)
  library(dplyr)
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)

  result_list <- mclapply(X = n_start:n_end,
                          FUN = FUN_RES_SP, N_resample = N_resample, matrice_tot =  matrice_tot,
                          mc.cores = 5)

  res <- dplyr::bind_rows(result_list)
  return(res)
}


# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste2 <- function(n_start, n_end, matrice_tot, N_resample = 20) {
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- lapply(n_start:n_end,
                        FUN = FUN_RES_SP, N_resample, matrice_tot)

  res <- dplyr::bind_rows(result_list)
  return(res)
}

# matrice de presence/pseudo_absence
matrice_sp <- function(data) {
  sp_occ <- data.frame(table(data$CdNom)) #comptage sp
  sp_occ <- sp_occ[sp_occ$Freq >= 20, ]
           # selection sp presentes au moins 20 fois
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
  PN_Point <- read.csv(path, row.names = "X",header = T)
  PN_Point<-PN_Point[PN_Point$FORMATION %in% c("Coniferes","Melange",
                                               "Feuillus"),]

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
  matrice_Select <-
    matrice_sp(PN_Select) # matrice presence-absence avec variables
  pres_abs_Select <-
      dplyr::select(matrice_Select,-c("ESSENCE":"Grossier"))
                     # matrice de presence-absence seules

  return(list(mat = matrice_Select, pres_abs = pres_abs_Select[, -1]))
}


Fun_Fit_Parc_Group <- function (Parc = "PNV", Groupe_Select = "Plantes"){
    list_df <- format_data(path = file.path("data",paste0(Parc,
                                                       "_DATA_POINTS1.csv")),
                         Groupe_Select = Groupe_Select)

  start.time <- Sys.time()
  ResFit <-
    Analyse_listeb(1, ncol(list_df$pres_abs), list_df$mat, N_resample = 20) 
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)

  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,"_Sorties.csv")))
  print("done")
}

Fun_Fit_Parc_Group_NoPar <- function (Parc = "PNV", Groupe_Select = "Plantes"){
  list_df <- format_data(path = file.path("data",paste0(Parc,
                                                        "_DATA_POINTS1.csv")),
                         Groupe_Select = Groupe_Select)
  
  start.time <- Sys.time()
  ResFit <-
    Analyse_liste2(1, ncol(list_df$pres_abs), list_df$mat, N_resample = 20) 
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,"_Sorties.csv")))
  print("done")
}

Fun_Fit_Parc_Group_Seq <- function (Seq_Sel, Parc = "PNV",
                                    Groupe_Select = "Plantes"){
    list_df <- format_data(path = file.path("data",paste0(Parc,
                                                        "_DATA_POINTS1.csv")),
                         Groupe_Select = Groupe_Select)
  ncols <- ncol(list_df$pres_abs)
  sel_start <- (0:9*floor(ncols/10)+1)[Seq_Sel]
  sel_end  <- c(1:9*floor(ncols/10), ncols)[Seq_Sel]
  start.time <- Sys.time()
  ResFit <-
      Analyse_liste2(sel_start, sel_end ,
                     list_df$mat, N_resample = 20)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)

  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,
                                       "_Sorties_", Seq_Sel, ".csv")))
  print("done")
}



Merge_Seq_Output <- function(Parc = "PNV",
                           Groupe_Select = "Plantes"){
  list_df <- vector("list")  
  for ( i in 1:10){
   list_df[[i]]  <- read.csv(file.path("output", paste0(Parc,"_", Groupe_Select,
                                       "_Sorties_", i, ".csv")))
  }  
  print("read")
  res <- dplyr::bind_rows(list_df)
  write.csv(res,
            file.path("output", paste0(Parc,"_", Groupe_Select,
                                       "_Sorties", ".csv")))
  return(res)
}

Get_Nspecies_Parc_Group <- function (){
  Parc_seq <- c("PNV", "PNE", "PNP", "PNC", "PNM")
  Groupe_Select_seq = c( "Reptiles", "Plantes", "Oiseaux", "Mammiferes", "Arthros")
  mat <- matrix(NA, nrow = length(Parc_seq), ncol = length(Groupe_Select_seq))
  rownames(mat) <- Parc_seq
  colnames(mat) <- Groupe_Select_seq
  for (p in Parc_seq){
    for (g in Groupe_Select_seq){
      list_df <- format_data(path = file.path("data",paste0(p,
                                                            "_DATA_POINTS1.csv")),
                             Groupe_Select = g)
      mat[p, g] <- ncol(list_df$pres_abs)
    }
  }
  return(mat)
}


Read_All_Output <- function(){
  Parc_seq <- c("PNV", "PNE", "PNP", "PNC", "PNM")
  Groupe_Select_seq = c( "Reptiles", "Plantes", "Oiseaux", "Mammiferes", "Arthros")
  list_df <- vector("list")  
  i <- 1
  mat <- matrix(NA, nrow = length(Parc_seq), ncol = length(Groupe_Select_seq))
  rownames(mat) <- Parc_seq
  colnames(mat) <- Groupe_Select_seq
  for (p in Parc_seq){
    for (g in Groupe_Select_seq){
      output <- read.csv(file.path("output", paste0(p,"_", g,
                                 "_Sorties", ".csv")))
      output$Parc <- p
      output$Group_Select <- g
      list_df[[i]] <- output 
      i <- i + 1  
      mat[p, g] <- nrow(output)
    }
  }
  print(mat)
  res <- dplyr::bind_rows(list_df)
  return(res)
}  

Fun_Plot_ALL <- function(df){
library(ggplot2)

ggplot(df, aes(x=Group_Select, y=coef_mean, fill=Parc)) + 
  geom_boxplot(outlier.shape = NA) +theme_bw() +
  scale_y_continuous(limits = quantile(df$coef_mean, c(0.05, 0.95)))

ggplot(df, aes(x=Group_Select, y=diff_mean, fill=Parc)) + 
  geom_boxplot(outlier.shape = NA) +theme_bw() +
  scale_y_continuous(limits = quantile(df$diff_mean, c(0.1, 0.9)))

}
  