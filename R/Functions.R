library(tidyr)
library(dplyr)


## Fonctions ----
# Modele liste d'espece ----
# Modele liste d'espece ----
# fonction contenant les modeles a repeter 100 fois
fun_fit <- function(k,matrice_tot, i) {
  # Echantillonnage des 0
  #  resampling only on absence data 
  matrice_tot$response <- matrice_tot[, i+1]
  test1 <- matrice_tot[matrice_tot$response == 1, ]
  test0t <- test[test$response == 0, ]
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
  end_time - start_time
  
  # 
  # Ajout du type de foret comme predicteur
  glm_type_F1 <- update(glm_envir1, ~ . + TYPE_FORET)
  summary(glm_type_F1)

  # Infinitely Weighted Logistic Regression
  Pres <- test_fit$response
  up.wt = (10^6)^(1 - test_fit$response)
  test_fit$up.wt <- up.wt
  glm_nul1b <- glm(response ~ 1, family = "binomial", data = test_fit, weights = up.wt)
  
  start_time <- Sys.time()
  glm_envir1b <-
    step(glm_nul1b,
         ~ .  + Dist_lisiere_act + Dist_route + FORMATION + STRUCTURE + Altitude +
           poly(Altitude, 2) + Pente + poly(Pente, 2)
         + exposition + poly(exposition, 2) + pH + poly(pH, 2)
         + phosphore + poly(phosphore, 2)
         + Azote + poly(Azote, 2)
         + limons + argile,
         direction = "both",
         trace = 0)
  end_time <- Sys.time()
  end_time - start_time

  # Ajout du type de foret comme predicteur
  glm_type_F1b <- update(glm_envir1b, ~ . + TYPE_FORET)
  summary(glm_type_F1b)
  
  # Downweighted Poisson Regression   
  p.wt = rep(1.e-6, length(test_fit$response))
  area <- 519803770 # area in m2 of grid cell on Vanoise national park to update with good mùeasure for each park
  p.wt[Pres == 0] = area/sum(test_fit$response == 0)
  test_fit$p.wt <- p.wt
  glm_nul1c <- glm(response/p.wt ~ 1, data = test_fit,family = poisson(), weights = p.wt)
    
  start_time <- Sys.time()
  glm_envir1c <-
    step(glm_nul1c,
         ~ .  + Dist_lisiere_act + Dist_route + FORMATION + STRUCTURE + Altitude +
           poly(Altitude, 2) + Pente + poly(Pente, 2)
         + exposition + poly(exposition, 2) + pH + poly(pH, 2)
         + phosphore + poly(phosphore, 2)
         + Azote + poly(Azote, 2)
         + limons + argile,
         direction = "both",
         trace = 0)
  end_time <- Sys.time()
  end_time - start_time
  
  # Ajout du type de foret comme predicteur
 glm_type_F1c <- update(glm_envir1c, ~ . + TYPE_FORET)
 summary(glm_type_F1c)  

 
 library(glmnet)
 #convert training data to matrix format
 s_time <- Sys.time()
 x <- model.matrix(response~  Dist_lisiere_act + Dist_route + FORMATION + STRUCTURE + Altitude +
                     poly(Altitude, 2) + Pente + poly(Pente, 2)+ 
                     exposition + poly(exposition, 2) + pH + poly(pH, 2) +
                     phosphore + poly(phosphore, 2)+
                     Azote + poly(Azote, 2)+
                     limons + argile,test_fit)[, -1]
 #perform grid search to find optimal value of lambda
 #family= binomial => logistic regression, alpha=1 => lasso
 cvfit = cv.glmnet(x, test_fit$response, family = "binomial", type.measure = "auc")
 #plot result
 var_lasso <- as.matrix(coef(cvfit, s = "lambda.1se"))
 vars <- gsub( '(2\\)).*', '\\1', row.names(var_lasso)[var_lasso>0])
 vars <- vars[! vars == "(Intercept)"]

 x1 <- model.matrix(eval(parse(text=paste("response ~  ", paste(c(vars),  collapse = " + ")))),
                         data = test_fit)[, -1] 
 x2 <- model.matrix(eval(parse(text=paste("response ~  ", paste(c("TYPE_FORET", vars),  collapse = " + ")))),
                    test_fit)[, -1] 
 glmnet_envir1l <- glmnet(x1, test_fit$response, family = "binomial",
                        data = test_fit, lambda = 0)
 glmnet_type_F1l <- glmnet(x2, test_fit$response, family = "binomial",
                        data = test_fit, lambda = 0)
 
 AIC_glmnet <- function(fit){
   tLL <- fit$nulldev - deviance(fit)
   k <- fit$df
   AIC <- -tLL+2*k
   AIC   
 }
 AIC_glmnet(glmnet_envir1l)
 AIC_glmnet(glmnet_type_F1l)
 AIC_glmnet(glmnet_type_F1l) + 5 <  AIC_glmnet(glmnet_envir1l)
 e_time <- Sys.time()
 e_time - s_time
 
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
    sum(as.numeric(paste(matrice_tot[, i+1])))
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
Analyse_listeb <- function(n_start,n_end, matrice_tot, N_resample = 3) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  library(tidyr)
  library(dplyr)
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- mclapply(n_start:n_end,
                          FUN_RES_SP, N_resample, matrice_tot,
                          mc.cores = 5)

  res <- dplyr::bind_rows(result_list)
  return(res)
}


# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste2 <- function(n_start, n_end, matrice_tot, N_resample = 3) {
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
  PN_Point <-
    read.csv(path,
             row.names = "X",
             header = T)
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
    Analyse_listeb(1,  ncol(list_df$pres_abs), list_df$mat, N_resample = 20)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)

  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,"_Sorties")))
  print("done")
}


Fun_Fit_Parc_Group_Seq <- function (Seq_Sel, Parc = "PNV",
                                    Groupe_Select = "Plantes"){
    list_df <- format_data(path = file.path("data",paste0(Parc,
                                                        "_DATA_POINTS1.csv")),
                         Groupe_Select = "Plantes")
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

