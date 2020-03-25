# Fit PNV Arthros

if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

source("R/Functions.R")
Fun_Fit_Parc_Group_Seq(Seq_Sel = index, Parc = PN, Groupe_Select = GR)
  
