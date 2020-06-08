# TODO write in drake
source("R/Functions.R")

library(ggplot2)
library(dplyr)
library(drake)



plan <- drake_plan(res = Read_All_Output(),
                   res_summarise = format_res(res),
                   table_new_null = table(res_summarise$p_delta.AIC>0.95, res_summarise$p_delta.AIC.null>0.95)/nrow(res_summarise),
                   table_new_old = table(res_summarise$p_delta.AIC>0.95, res_summarise$p_delta.AIC.old>0.95)/nrow(res_summarise)
                   )


make(plan)

loadd(res)

loadd(table_new_null)
loadd(table_new_old)

# Plot Delta AIC between methods
# New vsold
ggplot(res, aes(x = delta.AIC, y = delta.AIC.old))+geom_point()+
  geom_abline(slope = 1, intercept = 0,color = "red")+
  facet_wrap(Parc ~ Group_Select, scales="free", nrow = 5)

# only plantes
ggplot(res[res$Group_Select == "Plantes",], aes(x = delta.AIC, y = delta.AIC.old))+geom_point()+
  geom_abline(slope = 1, intercept = 0,color = "red")+
  facet_wrap(Parc ~ ., scales="free", nrow = 5)

# new vs null

ggplot(res, aes(x = delta.AIC, y = delta.AIC.null))+geom_point()+
  geom_abline(slope = 1, intercept = 0,color = "red")+
  geom_hline(yintercept = 5)+ geom_vline(xintercept = 5)+
  facet_wrap(Parc ~ Group_Select, scales="free", nrow = 5)

ggplot(res[res$Group_Select == "Plantes",], aes(x = delta.AIC, y = delta.AIC.null))+geom_point()+
  geom_abline(slope = 1, intercept = 0,color = "red")+
  geom_hline(yintercept = 5)+ geom_vline(xintercept = 5)+
  facet_wrap(Parc ~ ., scales="free", nrow = 5)

# Parameters estimates

ggplot(res[res$Group_Select == "Plantes",], aes(x =  estimate_E, y =  estimate_Eold))+geom_point()+
  geom_abline(slope = 1, intercept = 0,color = "red")+
  facet_wrap(Parc ~ ., scales="free", nrow = 5)

ggplot(res[res$Group_Select == "Plantes",], aes(x =  estimate_E, y =  estimate_B))+geom_point()+
  geom_abline(slope = 1, intercept = 0,color = "red")+
  facet_wrap(Parc ~ ., scales="free", nrow = 5)
