library(tidyr)
library(dplyr)


## Fonctions ----
# Modele liste d'espece ----
# Modele liste d'espece ----
# fonction contenant les modeles a repeter N_resample fois
fun_fit <- function(k,matrice_tot, i, Groupe_Select ) {
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
  if (Groupe_Select %in% c("Reptiles", "Oiseaux", "Mammiferes", "Arthros") ){ 
  glm_envir1 <-
    step(glm_nul1,
      ~ .  + Dist_lisiere_act  + FORMATION + STRUCTURE + 
        Altitude + I(Altitude^2) + 
        exposition + I(exposition^2) ,
      direction = "both",
      trace = 0)
   }else{
     glm_envir1 <-
       step(glm_nul1,
            ~ .  + Dist_lisiere_act  + FORMATION + STRUCTURE + 
              Altitude + I(Altitude^2) + 
              exposition + I(exposition^2) +
              pH + I(pH^2) +
              Azote + I(Azote^2),
            direction = "both",
            trace = 0)
   }
  end_time <- Sys.time()
  print(end_time - start_time)
  
  # 
  # Ajout du type de foret comme predicteur
  glm_type_F1 <- update(glm_envir1, ~ . + TYPE_FORET)
  glm_type_F1_B <- update(glm_nul1, ~ . + TYPE_FORET)
  
  # resume du modele pour rÈcuperer les z value
  sglm_type_F1<-summary(glm_type_F1)
  sglm_type_F1_B<-summary(glm_type_F1_B)
  
  ## Remplissage du tableau result_repet.
  result_repet <- c(names(matrice_tot)[i+1], 
                    k,
                    sglm_type_F1$coefficients["TYPE_FORETForet recente","Estimate"],
                    sglm_type_F1$coefficients["TYPE_FORETForet recente","Std. Error"],
                    sglm_type_F1$coefficients["TYPE_FORETForet recente","z value"],
                    sglm_type_F1_B$coefficients["TYPE_FORETForet recente","Estimate"],
                    sglm_type_F1_B$coefficients["TYPE_FORETForet recente","Std. Error"],
                    sglm_type_F1_B$coefficients["TYPE_FORETForet recente","z value"],
                    AIC(glm_nul1),
                    AIC(glm_envir1),
                    AIC(glm_type_F1),
                    AIC(glm_type_F1_B),
                    AIC(glm_envir1) - AIC(glm_type_F1))
  
  
  names(result_repet) <- c("CdNom","N_resample",
                           "estimate_E", "estim_sd_E","z_value_E",
                           "estimate_B", "estim_sd_B","z_value_B",
                           "AIC_null", "AIC_envir", 
                           "AIC_FA_E", "AIC_FA_B",
                           "delta AIC")
  return(as.data.frame(t(result_repet)))
}


FUN_RES_SP <- function(i, N_resample, matrice_tot, Groupe_Select){
  # Boucle pour repeter N_resample fois les modeles en echantillonant aleatoirement avec remise.
  result_list <- lapply(1:N_resample,
                        FUN = fun_fit, matrice_tot, i, Groupe_Select)

  res <- dplyr::bind_rows(result_list)

  return(res)
}



# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste <- function(n_start, n_end, matrice_tot, Groupe_Select, N_resample = 20) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- foreach(i = n_start:n_end,
                         .export = c("FUN_RES_SP", "fun_fit"),
                         .packages = "igraph") %dopar% {
                           FUN_RES_SP(i, N_resample, matrice_tot, Groupe_Select)
                         }
  res <- dplyr::bind_rows(result_list)
  return(res)
}

# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_listeb <- function(n_start,n_end, matrice_tot, Groupe_Select, N_resample = 20) {
  # Packages ----
  library(MuMIn)
  library(doParallel)
  library(igraph)
  library(tidyr)
  library(dplyr)
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)

  result_list <- mclapply(X = n_start:n_end,
                          FUN = FUN_RES_SP, N_resample = N_resample, 
                          matrice_tot =  matrice_tot, Groupe_Select,
                          mc.cores = 5)

  res <- dplyr::bind_rows(result_list)
  return(res)
}


# Fonction pour tester effet du type de foret pour chaque espece.
Analyse_liste2 <- function(n_start, n_end, matrice_tot, Groupe_Select, N_resample = 20) {
  # boucle for pour appliquer les instructions pour chaque espece (chaque colonne de ma matrice de pres/abs)
  result_list <- lapply(n_start:n_end,
                        FUN = FUN_RES_SP, N_resample, matrice_tot, Groupe_Select)

  res <- dplyr::bind_rows(result_list)
  return(res)
}

# matrice de presence/pseudo_absence

matrice_sp <- function(data,N=20) {
  sp_occ <- data.frame(table(data$CdNom)) #comptage sp
  sp_occ <- sp_occ[sp_occ$Freq >= N, ]
  if(nrow(sp_occ)>1){
  occ_20 <- merge(data, sp_occ, by.x = "CdNom", by.y = "Var1")
  pres_abs1 <-dplyr::select(occ_20,CdNom,X_Y)%>%cbind(.,nb=1)
  pres_abs1<-aggregate(nb~X_Y+CdNom,pres_abs1,sum)
  pres_abs1<-spread(pres_abs1,CdNom,nb,fill=0)
  pres_abs1 <- mutate_all(pres_abs1[, -1], function(x) {
    ifelse(x > 0, 1, 0)
  })
  pres_abs1<-cbind(pres_abs1,"X_Y"=unique(occ_20$X_Y))
  all_data_envir <-
    dplyr::select(occ_20, ESSENCE:FORMATION, X_Y:Grossier)
  pres_abs <-
    merge(pres_abs1, all_data_envir, by= "X_Y")
  }else{
  pres_abs <- NA  
  }
  return(pres_abs)
}


## Function to format data

format_data <- function(path, Groupe_Select = "Plantes",N=20){
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
                      "Oiseaux" = c("Oiseaux"),
                      "Mousses"=c("Bryidae","Hepatiques et Anthocerotes"),
                      "Pteridophytes"=c("Pteridophytes"),
                      "Lichens"=c("Lichens"))
  
 if(Groupe_Select == "Champignons"){
   PN_Select <-
      PN_Point[PN_Point$Regne %in% "Fungi",]
  }else{
  PN_Select <-
    PN_Point[PN_Point$Groupe %in% list_select[[Groupe_Select]],]
  }
  if(nrow(PN_Select)>1){
  matrice_Select <-
    matrice_sp(PN_Select,N) # matrice presence-absence avec variables
  if(is.data.frame(matrice_Select)){
  pres_abs_Select <-
    dplyr::select(matrice_Select,-c("ESSENCE":"Grossier"))
  pres_abs_Select <- pres_abs_Select[, -1]
  # matrice de presence-absence seules
  }else{
    pres_abs_Select <- NA  
  }}else{
    pres_abs_Select <- NA  
    matrice_Select <- NA
  }
  print(dim( pres_abs_Select))
  return(list(mat = matrice_Select, pres_abs = pres_abs_Select))
}


Fun_Fit_Parc_Group <- function (Parc = "PNV", Groupe_Select = "Plantes"){
    list_df <- format_data(path = file.path("data",paste0(Parc,
                                                       "_DATA_POINTS1.csv")),
                         Groupe_Select = Groupe_Select)

  start.time <- Sys.time()
  ResFit <-
    Analyse_listeb(1, ncol(list_df$pres_abs), list_df$mat, Groupe_Select, N_resample = 20) 
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
    Analyse_liste2(1, ncol(list_df$pres_abs), list_df$mat, Groupe_Select, N_resample = 20) 
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,"_Sorties.csv")))
  print("done")
}

Fun_Fit_Parc_Group_Seq <- function (Seq_Sel, Parc = "PNV",
                                    Groupe_Select = "Plantes",
                                    Ncut=5){
    list_df <- format_data(path = file.path("data",paste0(Parc,
                                                        "_DATA_POINTS1.csv")),
                         Groupe_Select = Groupe_Select)
  ncols <- ncol(list_df$pres_abs)
  sel_start <- (0:(Ncut-1)*floor(ncols/Ncut)+1)[Seq_Sel]
  sel_end  <- c(1:(Ncut-1)*floor(ncols/Ncut), ncols)[Seq_Sel]
  start.time <- Sys.time()
  ResFit <-
      Analyse_liste2(sel_start, sel_end ,
                     list_df$mat, Groupe_Select, N_resample = 20)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)

  write.csv(ResFit,
            file.path("output", paste0(Parc,"_", Groupe_Select,
                                       "_Sorties_", Seq_Sel, ".csv")))
  print("done")
}



Merge_Seq_Output <- function(Parc = "PNV",
                           Groupe_Select = "Plantes", Ncut = 5){
  list_df <- vector("list")  
  for ( i in 1:Ncut){
   list_df[[i]]  <- read.csv(file.path("output", paste0(Parc,"_", Groupe_Select,
                                       "_Sorties_", i, ".csv")))
  }  
  print("read")
  res <- dplyr::bind_rows(list_df)
  write.csv(res,
            file.path("output", paste0(Parc,"_", Groupe_Select,
                                       "_Sorties", ".csv")),row.names = FALSE)
  return(res)
}

Get_Nspecies_Parc_Group <- function (){
  Parc_seq <- c("PNV", "PNE", "PNP", "PNC", "PNM")
  Groupe_Select_seq = c( "Reptiles", "Plantes", "Oiseaux", "Mammiferes", "Arthros", 
                         "Mousses","Pteridophytes", "Lichens", "Champignons")
  
  mat <- matrix(NA, nrow = length(Parc_seq), ncol = length(Groupe_Select_seq))
  rownames(mat) <- Parc_seq
  colnames(mat) <- Groupe_Select_seq
  for (p in Parc_seq){
    for (g in Groupe_Select_seq){
      list_df <- format_data(path = file.path("data",paste0(p,
                                                            "_DATA_POINTS1.csv")),
                             Groupe_Select = g)
      if(!is.data.frame(list_df$pres_abs)){
      mat[p, g] <- 0  
      }else{
      mat[p, g] <- ncol(list_df$pres_abs)
      }
    }
  }
  return(mat)
}


Read_All_Output <- function(){
  Parc_seq <- c("PNV", "PNE", "PNP", "PNC", "PNM")
  Groupe_Select_seq = c( "Reptiles", "Plantes", "Oiseaux", "Mammiferes", "Arthros",
                         "Mousses","Pteridophytes", "Lichens", "Champignons")
  list_df <- vector("list")  
  i <- 1
  mat <- matrix(NA, nrow = length(Parc_seq), ncol = length(Groupe_Select_seq))
  rownames(mat) <- Parc_seq
  colnames(mat) <- Groupe_Select_seq
  for (p in Parc_seq){
    for (g in Groupe_Select_seq){
      list_df_t <- format_data(path = file.path("data",paste0(p,
                                                            "_DATA_POINTS1.csv")),
                             Groupe_Select = g)
      
      output <- read.csv(file.path("output", paste0(p,"_", g,
                                 "_Sorties", ".csv")))
      output$SpeciesCode <- names(list_df_t$pres_abs)
      output$Parc <- p
      output$Group_Select <- g
      list_df[[i]] <- output 
      i <- i + 1  
      mat[p, g] <- nrow(output)
    }
  }
  print(mat)
  names(list_df) <- paste(rep(Parc_seq, each = length(Groupe_Select_seq)), 
                           rep(Groupe_Select_seq, times =length(Parc_seq)))
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
 

PCAmix_Parc <- function (Parc = "PNV"){
   path <- file.path("data",paste0(Parc,"_DATA_POINTS1.csv"))
   PN_Point <- read.csv(path, row.names = "X",header = T)
   PN_Point<-PN_Point[PN_Point$FORMATION %in% c("Coniferes","Melange",
                                                "Feuillus"),]
   PN_Point <- drop_na(PN_Point, pH)
   PN_Point <- dplyr::select(PN_Point, -Dist_lisiere_anc)
   colnames(PN_Point)[names(PN_Point) %in% "distance_FA"] <- "Dist_lisiere_anc"
   PN_Point$Dist_route <- log(PN_Point$Dist_route + 1)
   PN_Point$Dist_lisiere_act <- log(PN_Point$Dist_lisiere_act + 1)

   VarsQual <- c( "FORMATION", "STRUCTURE")
   VarsQuant <- c("Dist_lisiere_act" , "Dist_route","Altitude",
                  "Pente", "exposition", "pH" , "phosphore",  "Azote", "limons", "argile")
   Vars <- c(VarsQual,VarsQuant)
   cor_mat <- cor(PN_Point[, VarsQuant])
   print(cor_mat)
   library(PCAmixdata)
   print(Parc)
   print(table(factor(PN_Point$FORMATION)))
   print(table(factor(PN_Point$STRUCTURE)))

res <- PCAmix(PN_Point[, VarsQuant], PN_Point[, VarsQual])
pdf(paste0("figures/",Parc, "_PCAmix.pdf"), width = 11, height = 11)
par(mfrow = c(2,2))
plot(res, choice = "sqload", main = Parc)
plot(res, choice = "levels")
plot(res, choice = "cor")
dev.off()
}


Plot_PCAmix_All <- function(){
  Parc_seq <- c("PNV", "PNE", "PNP", "PNC", "PNM")
  lapply(Parc_seq, PCAmix_Parc)
}


