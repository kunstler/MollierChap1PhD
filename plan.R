# TODO write in drake
source("R/Functions.R")

library(ggplot2)
library(dplyr)
library(drake)



plan <- drake_plan(res = Read_All_Output(),
                   res_m = merge_res(res, Taxref, BDC_statut),
                   BDC_statut = Data_PN_BDC_statut(),
                   Taxref = read.csv("data/Taxref_listes.csv"),
                   res_summarise = format_res(res, Taxref, BDC_statut),
                   table_new_null = table(res_summarise$ancient_forest_sp, res_summarise$ancient_forest_sp.null)/nrow(res_summarise),
                   table_new_old = table(res_summarise$ancient_forest_sp, res_summarise$ancient_forest_sp.old)/nrow(res_summarise)
                   )

make(plan)

loadd(res)
loadd(res_summarise)

loadd(table_new_null)
loadd(table_new_old)

# Plot Delta AIC between methods
# New vs old
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

pdf("Estimate_E_Null.pdf")
for (g in unique(res_summarise$Group_Select)){
dft <- res_summarise[res_summarise$Group_Select == g,]
print(ggplot(dft,
       aes(x =  mean.estimate, y =  mean.estimate.null))+
    geom_errorbarh(aes(xmin = ql.estimate , xmax= qh.estimate))+
    geom_errorbar(aes(ymin = ql.estimate.null , ymax= qh.estimate.null))+
  geom_abline(slope = 1, intercept = 0,color = "red")+
    facet_wrap(Parc ~ ., scales="free", nrow = 3)+
    theme(legend.position = "none") + ggtitle(g))
}
dev.off()

pdf("Estimate_E_Old.pdf")
for (g in unique(res_summarise$Group_Select)){
dft <- res_summarise[res_summarise$Group_Select == g,]
print(ggplot(dft,
       aes(x =  mean.estimate, y =  mean.estimate.old))+
    geom_errorbarh(aes(xmin = ql.estimate , xmax= qh.estimate))+
    geom_errorbar(aes(ymin = ql.estimate.old , ymax= qh.estimate.old))+
  geom_abline(slope = 1, intercept = 0,color = "red")+
    facet_wrap(Parc ~ ., scales="free", nrow = 3)+
    theme(legend.position = "none") + ggtitle(g))
}
dev.off()


## test effect statut protection

loadd(res_m)

library(lme4)
library(lmerTest)

fit <- lmer(estimate_E ~ factor(Menac.e) +(1 | Nom_Latin:PN), data = res_m)
summary(fit)

fit <- lmer(estimate_E ~ factor(Patrimoniale) +(1 | Nom_Latin:PN), data = res_m)
summary(fit)

ggplot(res_m, aes(x=factor(Menac.e), y=estimate_E)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = quantile(res_m$estimate_E, c(0.025, 0.975)))

ggplot(res_m, aes(x=factor(Patrimoniale), y=estimate_E)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = quantile(res_m$estimate_E, c(0.025, 0.975)))
