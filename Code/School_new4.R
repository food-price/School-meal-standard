##The defintive r script for running PIF Analysis. Hopefully, using this will cut down on time spent changing things around for various 
##sensitivity / alternative analysis.

rm(list=ls(all=TRUE)) # remove all data from memory
library(plyr)
library(data.table)
library(dplyr)

file_location = "C:/Users/lwang18/Box/Projects/10_School meal standard/CRA model"
#file_location = "/cluster/tufts/kimlab/lwang18/schoolmealstandard"

setwd(file_location)

setwd(file_location)
#define results version indicator
year.vec.string<-"withwaste_best"
# if(length(year.vec) > 1) {
#   year.vec.string<-paste(year.vec[1], "-", year.vec[length(year.vec)],sep="")
# }else{
#   year.vec.string<-as.character(year.vec)}
# set.seed(1)

version.date<-"20220629" #this is for exposure data

#number of probalistic simulation
nsim1 <-500
#numbers of stratas defined
n.strata<-48
nsim2<-n.strata
nsim3<-1  # number of exposure files
n.mediated.effects<-2
n.sims<-nsim1

covar.vec<-c("Age", "Sex", "Race") ##Don't mess with the order age, sex, race, edu
### MODIFY THIS VECTOR WITH THE FULL LIST OF RISK FACTORS THAT YOU WANT TO RUN


rfvec<-c(  "wg", "addsug", "sodium","rfg")
### List of disease outcomes
diseases<-c("CHD", "DB","IST", "HST", "CC", "LVC", "PC", "UC", "OC", "GC", "KC", "APCA", "BC", "ECA", "MMC", "MPLC","Larynx","SCNC","TC")
### List of diseases with BMI medicated effects
disease_bmimed<-c("CHD", "DB","IST", "HST","CC",  "LVC", "PC", "UC", "OC", "GC", "KC", "APCA", "BC", "MMC","ECA","SCNC","TC")
diseases_medBMI.vec<-paste(disease_bmimed, "_medBMI", sep="")
### List of diseases with blood pressure mediated effects
diseases_medSBP.vec<-c("CHD_medBP", "HST_medBP", "IST_medBP")
#import main input file
#This input file includes dietary intakes by strata
expos = read.csv(file="Inputs/NHANES1318_waste_best.csv", header=T)  #Read CSV file of exposure
Food.to.BMI.effects<-read.csv(file="Inputs/food.to.BMI.effects.final.csv", header=T)
Food.to.SBP.effects<-read.csv(file="Inputs/food.to.SBP.effects.csv", header=T)
expos$Mean_alt<-expos$Mean_alt

#Diet disease RR, by population subgroup, and by riskfactor, and diseases
rrtotal = read.csv(file="Inputs/RR_CVDcancer_20220628.csv", header=T)
# relative risks dataset--make sure to retain the row/column format of this file in any future versions of it
theomin = read.csv(file="Inputs/TMRED_gram.csv", header=T)   
#population size
#pop<-read.csv(paste("All_Sites_Incidence_", incidence.version.date, ".csv", sep=""))
pop<-read.csv(paste("Inputs/Allmort3", ".csv", sep=""))
#Mortality
Mort=pop<-read.csv(paste("Inputs/Allmort3", ".csv", sep=""))


#############################################
#Input processing for later use###############
#############################################
#covariate vectors
covar.vec.string<-paste(covar.vec, sep="", collapse="")
n.covar<-length(covar.vec)
#disease vectors
diseases.vec<-c(diseases, diseases_medBMI.vec, diseases_medSBP.vec)
n.diseases<-length(diseases)
num.diseases<-length(diseases.vec)
diseases.vec.mn<-paste(diseases.vec, "mn", sep="") 
diseases.vec.se<-paste(diseases.vec, "se", sep="") 


expos<-expos[order(expos$riskfactor),]
#unique(expos$riskfactor)
expos$sd<-expos$sigma_u_wgt ##change this line if you want to use total sd or unweighted sd or something else for sd.
expos$sd.alt<-expos$sigma_u_wgt ##change this line if you want to use total sd or unweighted sd or something else for sd.
expos$se.alt<-expos$se ##change this line if you want to use total sd or unweighted sd or something else for sd.
expos$age.mid<-0
expos$age.mid[expos$age==1]<- median(c(25,35))
expos$age.mid[expos$age==2]<- median(c(35,45))
expos$age.mid[expos$age==3]<- median(c(45,55))
expos$age.mid[expos$age==4]<- median(c(55,65))
expos$age.mid[expos$age==5]<- median(c(65,75))
expos$age.mid[expos$age==6]<- median(c(75,100))
expos.rf=length(unique(expos$riskfactor)) #number of risk factors in the file (e.g., 1 if there is only one risk factor)

pop<-pop[pop$disease=="CHD",c("subgroup_id", "female", "age", "race", "disease", "Population")] ##get rod pf repeat subgroups with !duplicated(pop$subgroup_id)
pop$population=pop$Population
pop$population.se<-0

#pre-simulate mortality/incidence numbers, to be used in CRA simulations, as well for calculating reverse-engineered PAFs
observed.mort.draws<-matrix(data=NA, nrow=dim(Mort)[1], ncol=nsim1)
for(i in 1:dim(Mort)[1])
{
  temp<-rnorm(n=nsim1, mean=Mort$Population[i]*Mort$Crude_Rate[i]/100000, sd=Mort$Population[i]*Mort$crude_SE[i]/100000)
  temp[temp<0]<-0
  observed.mort.draws[i,]<-temp
}
observed.mort.draws<-cbind(Mort, observed.mort.draws)

medBMI.observed.mort.draws<-observed.mort.draws
medBMI.observed.mort.draws$disease<-paste(observed.mort.draws$disease, "_medBMI", sep="")

medSBP.observed.mort.draws<-observed.mort.draws
medSBP.observed.mort.draws$disease<-paste(observed.mort.draws$disease, "_medBP", sep="")

observed.mort.draws<-rbind(observed.mort.draws, medBMI.observed.mort.draws,medSBP.observed.mort.draws)
write.csv(x=observed.mort.draws, file=paste("Inputs/observed.cancer.mort.draws",  year.vec.string, nsim1, ".csv", sep="_"))

source("Code/Complete.PIF.Analysis.PartOne.r") #Actually runs the simulations and calculates PIF for nsim1 simulations

source("Code/Complete.PIF.Analysis.partTwo.redone3.r")

