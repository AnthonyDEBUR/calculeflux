% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcule_flux_annuels.R
\name{calcule_flux_annuels}
\alias{calcule_flux_annuels}
\title{calcule_flux_annuels}
\usage{
calcule_flux_annuels(
  annees,
  mois_debut = 10,
  analyses0,
  col_dates_anal = "DatePrel",
  col_analyses = "RsAna",
  debit0,
  col_dates_debit = "date_obs_elab",
  col_valeurs_debits = "resultat_obs_elab",
  methode = "M6",
  out = "flux_hydrau_pond",
  minimum_rs_ana = 6,
  minimum_winter_rs = 4
)
}
\arguments{
\item{annees}{: vector with first and last years (in numeric) to treat. If you want results for
years 2010-2011, 2011-2012 and 2012-2013, annees should be equal to c(2010,2012)}

\item{mois_debut}{: numeric indicating wich month to start the hydraulic year
with. Default is 10 (for october).}

\item{analyses0}{: data.frame with nitrates results for the different dates (mg/L)}

\item{col_dates_anal}{: character indicating the name of the column of analyses
in which dates are stored. Default = "DatePrel"}

\item{col_analyses}{: character indicating the name of the column of analyses
in which results are stored. Default = "RsAna"}

\item{debit0}{: data.frame with daily flow rates results for the different dates (L/s)}

\item{col_dates_debit}{: character indicating the name of the column of debit
in which dates are stored. Default = "date_obs_elab"}

\item{col_valeurs_debits}{: character indicating the name of the column of debit
in which results are stored. Default = "resultat_obs_elab"}

\item{methode}{: methode used to calculate fluxes. Default = "M6".
See calcule_flux for more details.}

\item{out}{: required output. Values can be : "flux" : only flux are returned,
"hydrau" : only hydraulicity is returned, and "flux_hydrau_pond" : flux, hydraulicity,
ponderate flux by hydraulicity and 90 percentile are returned.}

\item{minimum_rs_ana}{: minimum number of data of concentration in one hydraulic
year to make the calculus. Default = 6 : Calculus are not done for years with
strictly less than 6 data of concentrations}

\item{minimum_winter_rs}{: minimum number of data of concentration in one hydraulic
year during winter period (october to april) to make the calculus. Default = 4 :
Calculus are not done for years with strictly less than 4 data of concentrations}
}
\value{
data.frame with columns :
\itemize{
\item annee_hydro : hydraulical year
\item N_RsAna : number of data of concentration
\item N_Qjm : number of daily flow rate
\item nb_jours : number of days during the hydraulical year
\item debit_an : annual flow rate in m3/year
\item flux : flux in kg/year (if analyses in mg/L)
\item Cmoy :mean of concentration = flux / debit_tot in mg/L
\item hydraulicity : hydraulicity
\item flux_pond : flux ponderate by hydraulicity in kg/year (if analyses in mg/L)
\item P90 : 90 percentile of NO3 calculated according "Arrete ministeriel du 25 janvier 2010"
}
}
\description{
This function calculate ech annual fluxes of a period.
For a same date if their are more than one analysis or flow rate values, then
calculus is done by considering the mean (excluding NA) of all the values for the day.
}
\examples{
   

datafile <- system.file("nitrates.csv", package = "calculeflux")
nitrates <- read.csv2(datafile, encoding = "UTF-8")
nitrates$DatePrel<-nitrates$DatePrel\%>\%as.Date()


datafile <- system.file("debit.csv", package = "calculeflux")
debit <- read.csv2(datafile, encoding = "UTF-8")
debit$date_obs_elab<-debit$date_obs_elab\%>\%as.Date()

annees=c(1971,2014)
 

flux_vilaine<-calcule_flux_annuels(annees=c(2010,2015),
                     mois_debut=10,
                     analyses0=nitrates,
                     debit0=debit,
                     minimum_rs_ana = 9,
                      minimum_winter_rs = 4
                     )



}
