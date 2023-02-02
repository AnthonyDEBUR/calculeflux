library(calculeflux)
library(tools4DCE)

##### Chargement donnees ####
datafile <- system.file("nitrates.csv", package = "calculeflux")
nitrates <- read.csv2(datafile, encoding = "UTF-8")
nitrates$DatePrel<-nitrates$DatePrel%>%as.Date()

datafile <- system.file("debit.csv", package = "calculeflux")
debit <- read.csv2(datafile, encoding = "UTF-8")
debit$date_obs_elab<-debit$date_obs_elab%>%as.Date()


##### Calcul de l'hydraulicité par année hydrologique #####

dates<-c(2002,2022)
Surface_BV_ha<-1013400

date_debut_calcul=paste0(seq(dates[1],dates[2]-1),"-10-01")%>%as.Date
date_fin_calcul=paste0(seq(dates[1]+1,dates[2]),"-09-30")%>%as.Date

annees_hydro<-data.frame(date_debut_calcul=date_debut_calcul,
                         date_fin_calcul=date_fin_calcul,
                         nom=paste0(seq(dates[1],dates[2]-1),
                                    "-",
                                    seq(dates[1]+1,dates[2])))



hydraulicite<-lapply(seq(dates[1]:(dates[2]-1)),
            function(x){calcule_hydraulicite(debit,
                                             date_debut_calcul=annees_hydro$date_debut_calcul[x],
                                             date_fin_calcul=annees_hydro$date_fin_calcul[x])
            }
)


hydraulicite<-unlist(hydraulicite)
hydraulicite<-data.frame(annee=annees_hydro$nom, hydraulicite=hydraulicite)

ggplot(hydraulicite, aes(annee, hydraulicite)) +
  geom_bar(stat="identity") +
  ylab("hydraulicite") +
  geom_hline(yintercept=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))



##### Calcul des lames d'eau annuelles #####



##### Calcul des flux par année hydrologique #####

Flux_NO3<-lapply(seq(dates[1]:(dates[2]-1)),
            function(x){calcule_flux(
              nitrates,
              col_dates_anal = "DatePrel",
              col_analyses = "RsAna",
              debit,
              methode = "M6",
              date_debut_calcul=annees_hydro$date_debut_calcul[x],
              date_fin_calcul=annees_hydro$date_fin_calcul[x]
            )}
)


Flux_NO3<-unlist(Flux_NO3)
Flux_NO3<-data.frame(annee=annees_hydro$nom,
                     flux_t_an=Flux_NO3/1000,
                     flux_surf_kgN_ha_an=Flux_NO3/Surface_BV_ha/4.427)



Flux_NO3<-left_join(Flux_NO3, hydraulicite, by="annee")
Flux_NO3$flux_pondere_hydraulicite<-Flux_NO3$flux_surf_kgN_ha_an/Flux_NO3$hydraulicite


library(ggplot2)


ggplot(Flux_NO3, aes(annee, flux_t_an)) +
  geom_bar(stat="identity") +
  ylab("Flux en t NO3/an") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  ggtitle("Flux NO3 en t/an")


ggplot(Flux_NO3, aes(annee, flux_surf_kgN_ha_an)) +
  geom_bar(stat="identity") +
  ylab("Flux en t N-NO3/(ha.an)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  ggtitle("Flux surfacique en N-NO3", subtitle="Méthode M6 (macroflux)")


ggplot(Flux_NO3, aes(annee, flux_pondere_hydraulicite)) +
  geom_bar(stat="identity") +
  ylab("Flux en t N-NO3/(ha.an)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  ggtitle("Flux pondéré par hydraulicité en N-NO3", subtitle="Méthode M6 (macroflux)")



# calcul flux NO3 méthode M5

Flux_NO3_M5<-lapply(seq(dates[1]:(dates[2]-1)),
                 function(x){calcule_flux(
                   nitrates,
                   col_dates_anal = "DatePrel",
                   col_analyses = "RsAna",
                   debit,
                   methode = "M5",
                   date_debut_calcul=annees_hydro$date_debut_calcul[x],
                   date_fin_calcul=annees_hydro$date_fin_calcul[x]
                 )}
)


Flux_NO3_M5<-unlist(Flux_NO3_M5)
Flux_NO3_M5<-data.frame(annee=annees_hydro$nom,
                     flux_t_an_M5=Flux_NO3_M5/1000,
                     flux_surf_kgN_ha_an_M5=Flux_NO3_M5/Surface_BV_ha/4.427)

Flux_NO3<-left_join(Flux_NO3, Flux_NO3_M5, by="annee")

ggplot(Flux_NO3, aes(annee, flux_surf_kgN_ha_an_M5)) +
  geom_bar(stat="identity") +
  ylab("Flux en t N-NO3/(ha.an)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  ggtitle("Flux surfacique en N-NO3", subtitle="Méthode M5")


comparaison<-pivot_longer(Flux_NO3,
                          cols=c(flux_surf_kgN_ha_an,flux_surf_kgN_ha_an_M5),
                          )

comparaison$methode<-ifelse(comparaison$name=="flux_surf_kgN_ha_an", "M6", "M5")

ggplot(comparaison, aes(annee, value, fill=methode)) +
  geom_bar(stat="identity", position = "dodge2") +
  ylab("Flux en t N-NO3/(ha.an)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  ggtitle("Flux surfacique en N-NO3", subtitle="Comparaison méthode M5 avec méthode M6 (macroflux)")


##### ESSAI SUR D'AUTRES STATIONS #####

analyses_bassin <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_bassin.rds")



code_station<-






