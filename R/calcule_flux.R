# WARNING - Generated by {fusen} from dev/flat_full.Rmd: do not edit by hand

#' @importFrom utils globalVariables
utils::globalVariables(c("DatePrel", "jour_mois"))


#' calcule_flux
#'
#' Calculate flux with one of the methods lists in https://revue-set.fr/article/view/6291
#' 
#' M1 : Product of means of sampled Ci and Qi (Preston et al., 1989)
#' M2 : Mean of instantaneous fluxes : Fi = Ci x Qi (Preston et al., 1989)
#' M3 : Constant concentration hypothesis around sample (Meybeck et al., 1994)
#' M4 : Product of means of sampled Ci and annual discharge Q (Shih et al., 1994)
#' M5 :Flow-weighted mean concentration (Littlewood, 1992)
#' M6 : Linear interpolation of C (Moatar and Meybeck, 2005)
#' M9 : Méthode M2 corrigée par un facteur multiplicatif prenant en compte les débits aux instants où l’on n’effectue pas de prélèvements (Cooper, 2004)
#' 
#' Dates date_debut_calcul and date_fin_calcul are included in calculus.
#' if more than one result of analysis or flow rate is available for a same day, then the mean of all non-NA values is used.
#' 
#' @param analyses dataframe with the concentrations required for calculus. Concentration in mg/L
#' @param col_dates_anal Name of the column with date of measurement of concentrations. By default : DatePrel
#' @param col_analyses Name of the column with values of concentrations. RsAna by default.
#' @param debit dataframe with daily mean flow rate in L/s
#' @param col_dates_debit Name of the column with the date of flow rate. Default : date_obs_elab
#' @param col_debits Name of the column with the flowrate values. Default : resultat_obs_elab
#' @param methode Code of the method for calculus according https://revue-set.fr/article/view/6291 (default : "M6")
#' @param date_debut_calcul Date of begining of calculus (for instance "2010-10-01" to start calculus from 1st of october 2010)
#' @param date_fin_calcul Date of ending of calculus (for instance "2011-09-30" to end calculus to 30 of september 2011)
#'
#' @return
#' Flux for all the dataset in kg / duration of the measurement (i.e. max(date of flow rate) - min(date of flow rate))
#' @export
#'
#' @examples
#'
#' date_debut_calcul<-"2007-01-01"%>%as.Date()
#' date_fin_calcul<-"2007-12-31"%>%as.Date()
#'   
#' datafile <- system.file("nitrates.csv", package = "calculeflux")
#' nitrates <- read.csv2(datafile, encoding = "UTF-8")
#' nitrates$DatePrel<-nitrates$DatePrel%>%as.Date()
#'
#' datafile <- system.file("debit.csv", package = "calculeflux")
#' debit <- read.csv2(datafile, encoding = "UTF-8")
#' debit$date_obs_elab<-debit$date_obs_elab%>%as.Date()
#'
#'
#'
#' calcule_flux(
#'   analyses=nitrates,
#'   col_dates_anal = "DatePrel",
#'   col_analyses = "RsAna",
#'   debit=debit,
#'   methode = "M6",
#'   date_debut_calcul=date_debut_calcul,
#'   date_fin_calcul=date_fin_calcul
#' )
#'
#' methodes<-c("M1", "M2", "M3", "M4", "M5", "M6", "M9")
#'
#' tmp<-lapply(methodes,
#' function(x){calcule_flux(
#'   nitrates,
#'   col_dates_anal = "DatePrel",
#'   col_analyses = "RsAna",
#'   debit,
#'   methode = x,
#'   date_debut_calcul=date_debut_calcul,
#'   date_fin_calcul=date_fin_calcul
#' )}
#' )
#'
#'
#' tmp<-unlist(tmp)
#' tmp<-data.frame(methode=methodes, flux=tmp/1000)
#'
#' library(ggplot2)
#'
#' ggplot2::ggplot(tmp, ggplot2::aes(methode, flux)) + 
#'   ggplot2::geom_bar(stat="identity") + ggplot2::ylab("Flux en kg NO3/an")
#'
#' # calcul du flux annuel méthode M6
#'
#' calcul_flux_annuel<-function(annee)
#' {
#'   calcule_flux(
#'   nitrates,
#'   col_dates_anal = "DatePrel",
#'   col_analyses = "RsAna",
#'   debit,
#'   methode = "M6",
#'   date_debut_calcul=paste0(annee,"-10-01")%>%as.Date(),
#'   date_fin_calcul=paste0((annee+1),"-10-01")%>%as.Date()
#' )
#'  
#' }
#'
#' annees<-seq(2002,2021)
#' tmp<-lapply(annees, calcul_flux_annuel)
#'
#' tmp<-unlist(tmp)
#' flux_vilaine_bdhydro<-data.frame(annee_hydro=paste0(annees, " - ", annees+1), flux=tmp/1000)
#'
#'
#' ggplot2::ggplot(flux_vilaine_bdhydro, ggplot2::aes(annee_hydro, flux)) +
#'   ggplot2::geom_bar(stat="identity") +
#'   ggplot2::ylab("flux (t NO3/an)") + 
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5)) +
#'   ggplot2::ggtitle("Flux de nitrates calcul\u00e9s sur la Vilaine \u00e0 Rieux",
#'           subtitle="NO3 station 04216000 / station hydrom\u00e9trique J930061101")
#'
#'
#' # calcul du flux bimensuels pour chaque année
#'
#'
#' library(zoo)
#' DateSeq_fin_mois <- function(st, en, freq) {
#'   st <- as.Date(as.yearmon(st)) 
#'   en <- as.Date(as.yearmon(en)) 
#'   as.Date(as.yearmon(seq(st, en, by = paste(as.character(12/freq), "months"))), frac = 1)
#'
#' }
#'
#' DateSeq_debut_mois <- function(st, en, freq) {
#'   st <- as.Date(as.yearmon(st))
#'   en <- as.Date(as.yearmon(en)) 
#'   as.Date(as.yearmon(seq(st, en, by = paste(as.character(12/freq), "months"))), frac = 0)
#'
#' }
#'
#' # # dates de chaque fin de mois
#' # DateSeq_fin_mois(as.Date("2002-01-01"),as.Date("2022-09-30"),12)
#'
#' # dates de debut de mois, un mois sur 2
#' dates_debut_mois<-DateSeq_debut_mois(as.Date("2002-10-01"),as.Date("2022-08-01"),6)
#'
#' # dates des fins de mois 1 mois sur 2
#' dates_fin_mois<-DateSeq_fin_mois(as.Date("2002-11-30"),as.Date("2022-09-30"),6)
#'
#' # mois periode 
#' mois_periode<-paste0(format(dates_debut_mois, "%b"),"-",format(dates_fin_mois, "%b"))
#' mois_periode<-mois_periode%>%factor(levels=unique(mois_periode))
#'
#' # annnee hydro
#' an_hydro_periode<-ifelse(format(dates_debut_mois, "%m")%>%as.numeric()>=10,
#'                          format(dates_debut_mois, "%Y")%>%as.numeric(),
#'                          format(dates_debut_mois, "%Y")%>%as.numeric()-1)
#'
#' an_hydro_periode<-paste0(an_hydro_periode, " - ", an_hydro_periode+1)
#'
#' periodes<-data.frame(debut=dates_debut_mois,
#'                      fin=dates_fin_mois,
#'                      mois_periode=mois_periode,
#'                      an_hydro_periode=an_hydro_periode)
#'
#'
#' tmp<-lapply(seq(1,nrow(periodes)),
#' function(x){
#'   calcule_flux(
#'   nitrates,
#'   col_dates_anal = "DatePrel",
#'   col_analyses = "RsAna",
#'   debit,
#'   methode = "M6",
#'   date_debut_calcul=periodes$debut[x],
#'   date_fin_calcul=periodes$fin[x]
#' )
#'   }
#' )
#'
#'
#' flux<-unlist(tmp)
#' resultats<-cbind(periodes, flux)
#'
#'
#'
#' ggplot2::ggplot(resultats, ggplot2::aes(mois_periode, flux)) +
#'   ggplot2::geom_bar(stat="identity") +
#'   ggplot2::ylab("Flux en kg NO3/an") + 
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5)) +
#'   ggplot2::facet_wrap(an_hydro_periode~.)+
#'   ggplot2::ggtitle("Flux de nitrates calcul\u00e9s sur la Vilaine \u00e0 Rieux",
#'           subtitle="NO3 station 04216000 / station hydrom\u00e9trique J930061101")
#'
#'
#'
#'
#' @importFrom rlang .data

calcule_flux <- function(analyses, 
                         col_dates_anal="DatePrel", 
                         col_analyses="RsAna", 
                         debit, 
                         col_dates_debit="date_obs_elab",
                         col_debits="resultat_obs_elab",
                         methode="M6",
                         date_debut_calcul,
                         date_fin_calcul
                         ) {

  if (!is.data.frame(analyses)) {
    stop("analyses should be dataframe")
  }
  if (!is.character(col_dates_anal)) {
    stop("col_dates_anal should be character")
  }
  if (!is.character(col_analyses)) {
    stop("col_analyses should be character")
  }
  if (!is.data.frame(debit)) {
    stop("debit should be dataframe")
  }
  if (!is.character(col_dates_debit)) {
    stop("col_dates_debit should be character")
  }
  if (!is.character(col_debits)) {
    stop("col_debits should be character")
  }
  if (!is.character(methode)) {
    stop("methode should be character")
  }
  if (!(col_dates_anal %in% names(analyses))) {
    stop("col_dates_anal must be one of the column name from data.frame analyses")
  }
  if (!(col_analyses %in% names(analyses))) {
    stop("col_analyses must be one of the column name from data.frame analyses")
  }
  if (!(col_dates_debit %in% names(debit))) {
    stop("col_dates_debit must be one of the column name from data.frame debit")
  }
  if (!(col_debits %in% names(debit))) {
    stop("col_debits must be one of the column name from data.frame debit")
  }
  if (!(methode %in% c("M1", "M2", "M3", "M4", "M5", "M6", "M9"))) {
    stop("methode is not a defined method code")
  }
  if (!any(class(analyses[[col_dates_anal]]) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    stop("Class of analysis dates should be date, POSIXct or POSIXlt")
  }
  if (!any(class(debit[[col_dates_debit]]) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    stop("Class of flow rates dates should be date, POSIXct or POSIXlt")
  }
  if (!is.numeric(analyses[[col_analyses]])) {
    stop("Class of analysis results should be numeric")
  }
  if (!class(debit[[col_debits]]) %in% c("numeric")) {
    stop("Class of analysis results should be numeric")
  }
  if (!any(class(date_debut_calcul) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    stop("Class of date_debut_calcul should be date, POSIXct or POSIXlt")
  }
  if (!any(class(date_fin_calcul) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))) {
    stop("Class of date_fin_calcul should be date, POSIXct or POSIXlt")
  }

  # date_debut_calcul<-'2007-01-01'
  # date_fin_calcul<-'2007-12-31'

  date_debut_calcul<-date_debut_calcul%>%as.Date()
  date_fin_calcul<-date_fin_calcul%>%as.Date()
  
  if (date_debut_calcul>=date_fin_calcul) {
    stop("date_debut_calcul must be strictly lower than date_fin_calcul")
  }
  

  # on retient la moyenne des résultats d'analyse si plusieurs résultats sont disponibles à la même date

  analyses<-analyses%>%dplyr::group_by(base::get(col_dates_anal))%>%dplyr::summarise(RsAna=mean(.data[[col_analyses]], na.rm=T))
  names(analyses)<-c("DatePrel", "RsAna")
  
  # on retient la moyenne des résultats de debits si plusieurs résultats sont disponibles à la même date
  debit<-debit%>%dplyr::group_by(get(col_dates_debit))%>%dplyr::summarise(QJM=mean(.data[[col_debits]], na.rm=T))
  names(debit)<-c("DatePrel", "QJM")
  
  # nb de jours de la période consideree
  nb_jours<-length(seq(from = date_debut_calcul, to = date_fin_calcul, by = 'day'))

  # suppression des donnees non renseignees
  analyses<-analyses[!is.na(analyses[["RsAna"]]),]
  analyses[["DatePrel"]]<-as.Date(analyses[["DatePrel"]])
  debit[["DatePrel"]]<-as.Date(debit[["DatePrel"]])
  
  CiQi<-dplyr::inner_join(analyses, debit, by=c("DatePrel")) 

  
  Flux<-NA
  
  if(methode=="M1")
  {
    # Product of means of sampled Ci and Qi
    CiQi<-CiQi%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # n = Number of chemical analyses
    n<-nrow(CiQi)
    SomCi<-sum(CiQi[["RsAna"]], na.rm=TRUE)
    SomQi<-sum(CiQi[["QJM"]], na.rm=TRUE)

    Flux<-0.0864*nb_jours*(SomCi/n*SomQi/n)

  }

    if(methode=="M2")
  {
    # Mean of instantaneous fluxes Fi = Ci x Qi
    CiQi<-CiQi%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # n = Number of chemical analyses
    n<-nrow(CiQi)
    CiQi$CiQi<-CiQi[["RsAna"]]*CiQi[["QJM"]]

    Flux<-0.0864*nb_jours/n*sum(CiQi$CiQi, na.rm=T)

  }
  
      if(methode=="M3")
  {
    # Constant concentration hypothesis around sample

    analyses0<-analyses%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # calculus of each Mean discharge for interval between samples i and i - 1
    intervalles<-c(as.Date("1900-01-01"), analyses0[["DatePrel"]])%>%unique()%>%sort
    debit0<-debit%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    debit0$periode<-NA
    for(i in 2:length(intervalles))
    {

        debit0$periode<-ifelse(debit0$DatePrel>intervalles[i-1] &
                                debit0$DatePrel<=intervalles[i],
                              i,
                              debit0$periode)


    }
    debit0$periode<-intervalles[debit0$periode]
    debit0<-debit0%>%dplyr::group_by(.data$periode)%>%dplyr::summarise(Qmoy=mean(.data$QJM, na.rm=T))
    CiQii_1<-dplyr::inner_join(analyses0, debit0, by=c("DatePrel"="periode"))

    Kprime<-nb_jours/nrow(CiQii_1)*0.0864

    Flux<-Kprime*sum(CiQii_1$RsAna*CiQii_1$Qmoy, na.rm=TRUE)
  
  }
  
     if(methode=="M4")
  {
    # Product of means of sampled Ci and annual discharge Q

    analyses0<-analyses%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)
    debit0<-debit%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # estimation volume annuel ecoule
    V<-mean(debit0$QJM, na.rm=T)*nb_jours
    Cmoy<-mean(analyses0$RsAna, na.rm=T)

  Flux<-0.0864*V*Cmoy
  
  }
  

       if(methode=="M5")
  {
    # Flow-weighted mean concentration

    analyses0<-analyses%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)
    debit0<-debit%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)
    CiQi0<-CiQi%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # estimation volume annuel ecoule
    V<-mean(debit0$QJM, na.rm=T)*nb_jours


  Flux<-0.0864*V*sum(CiQi0$RsAna*CiQi0$QJM)/sum(CiQi0$QJM, na.rm=T)

  }
  

        if(methode=="M6")
  {
    #  Linear interpolation of C

     debit0<-debit%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # # calculus of concentration variations between 2 mesurements
    #  analyses$RsAna_j_1<-c(NA,analyses$RsAna[1:length(analyses$RsAna)-1])
    #   analyses$j_1<-c(as.Date("1800-01-01"),analyses$DatePrel[1:length(analyses$DatePrel)-1])
    #   analyses$delta_j<-as.numeric(difftime(analyses$DatePrel,analyses$j_1))
    #   analyses$gradient_con<-(analyses$RsAna-analyses$RsAna_j_1)/analyses$delta_j
    #   
      # 
      # CiQi<-CiQi[c("DatePrel", "gradient_con")]
     
  
      # Fonction pour effectuer l'interpolation linéaire
interpolation_lineaire <- function(x) {
  x$DatePrel <- as.numeric(x$DatePrel)  # Convertir la date en format numérique pour l'interpolation
  x <- x[order(x$DatePrel), ]  # Trier les données par date_mesure
  
  # Utiliser la fonction 'approx' pour l'interpolation linéaire
  interp_data <- approx(x$DatePrel, 
                        x$RsAna, 
                        xout = as.numeric(seq(min(analyses$DatePrel, na.rm=T), max(analyses$DatePrel, na.rm=T), "day")), 
                        method = "linear")
  
  # Remplacer les valeurs manquantes par les valeurs interpolées
  resultats_365_j<-data.frame(DatePrel=seq(min(analyses$DatePrel, na.rm=T), max(analyses$DatePrel, na.rm=T), "day"))
  resultats_365_j<-resultats_365_j%>%dplyr::left_join(debit, by="DatePrel")
  resultats_365_j$RsAna<-interp_data$y
  
  
  return(resultats_365_j)
}


resultats_365_j<-interpolation_lineaire(analyses)

    # resultats_365_j<-data.frame(DatePrel=seq(min(analyses$DatePrel, na.rm=T), max(analyses$DatePrel, na.rm=T), "day"))
    # resultats_365_j<-resultats_365_j%>%dplyr::left_join(debit, by="DatePrel")
    # resultats_365_j<-resultats_365_j%>%dplyr::left_join(analyses, by="DatePrel")
    # #resultats_365_j<-resultats_365_j%>%dplyr::left_join(CiQi, by="DatePrel")
    # 
    #  # filling missing concentrations
    #  resultats_365_j$gradient_con<-zoo::na.locf(resultats_365_j$gradient_con, fromLast = TRUE)
    #  last_result<-NA
    #  for(i in nrow(resultats_365_j):1)
    #  {
    #    if(is.na(resultats_365_j$RsAna[i])){resultats_365_j$RsAna[i]<-last_result-resultats_365_j$gradient_con[i]}
    #     last_result<-resultats_365_j$RsAna[i]
    #  }

     # keep only data of considered period
     resultats_365_j<-resultats_365_j%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)
     resultats_365_j$CiQi<-resultats_365_j$QJM*resultats_365_j$RsAna

     Flux<-0.0864*nb_jours/nrow(resultats_365_j)*sum(resultats_365_j$CiQi, na.rm=T)
        } 
  

          if(methode=="M9")
  {
      # Mean of instantaneous fluxes Fi = Ci x Qi corrected according Cooper, 2004
    CiQi<-CiQi%>%subset(DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)

    # n = Number of chemical analyses
    CiQi$CiQi<-CiQi[["RsAna"]]*CiQi[["QJM"]]

    Q_sans_mesure<-debit%>%subset(!(DatePrel%in%analyses$DatePrel) & DatePrel>=date_debut_calcul & DatePrel<=date_fin_calcul)
    f_correct<-(sum((Q_sans_mesure$QJM)^2, na.rm=TRUE))/(sum((CiQi$QJM)^2, na.rm=TRUE))

    Flux<-0.0864*sum(CiQi$CiQi, na.rm=T)*(1+f_correct)
        } 
  
    
  return(Flux)
}



