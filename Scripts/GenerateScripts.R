# Create R script
Parc_seq <- c("PNV", "PNE", "PNP", "PNC", "PNM")
Groupe_Select_seq = c( "Reptiles", "Plantes", "Oiseaux", "Mammiferes", "Arthros", 
                       "Mousses","Pteridophytes", "Lichens", "Champignons")

x <- readLines("Scripts/PNV_Reptiles_NoPar.R")
for (i in Parc_seq[-1]){
f <- gsub("PNV", i, "Scripts/PNV_Reptiles_NoPar.R")  
y <- gsub( "PNV", i, x )
cat(y, file=f, sep="\n")
}

for (i in Parc_seq){
  f <- gsub("PNV", i, "Scripts/PNV_Mammiferes_NoPar.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Reptiles", "Mammiferes", y )
  cat(y, file=f, sep="\n")
}

for (i in Parc_seq){
  f <- gsub("PNV", i, "Scripts/PNV_Mousses_NoPar.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Reptiles", "Mousses", y )
  cat(y, file=f, sep="\n")
}

for (i in Parc_seq){
  f <- gsub("PNV", i, "Scripts/PNV_Pteridophytes_NoPar.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Reptiles", "Pteridophytes", y )
  cat(y, file=f, sep="\n")
}

for (i in Parc_seq[3:5]){
  f <- gsub("PNV", i, "Scripts/PNV_Lichens_NoPar.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Reptiles", "Lichens", y )
  cat(y, file=f, sep="\n")
}

for (i in Parc_seq[3:5]){
  f <- gsub("PNV", i, "Scripts/PNV_Champignons_NoPar.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Reptiles", "Champignons", y )
  cat(y, file=f, sep="\n")
}

x <- readLines("Scripts/PNV_Plantes_Seq.R")
for (i in Parc_seq[-1]){
  f <- gsub("PNV", i, "Scripts/PNV_Plantes_Seq.R")  
  y <- gsub( "PNV", i, x )
  cat(y, file=f, sep="\n")
}

for (i in Parc_seq){
  f <- gsub("PNV", i, "Scripts/PNV_Oiseaux_Seq.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Plantes", "Oiseaux", y )
  cat(y, file=f, sep="\n")
}

for (i in Parc_seq){
  f <- gsub("PNV", i, "Scripts/PNV_Arthros_Seq.R")  
  y <- gsub( "PNV", i, x )
  y <- gsub( "Plantes", "Arthros", y )
  cat(y, file=f, sep="\n")
}
