##Complete.PAF.Analysis.partTwo.redone.r
#library(dplyr)
library(data.table)

#file_location = "C:/Users/lwang18/Box/Projects/School meal standard/CRA model"
#setwd(file_location)
#n.sims=1000
#nsim1 <-1000
#year.vec.string="2019_sen2"

#covar.vec<-c("Age", "Sex", "Race") ##Don't mess with the order age, sex, race, edu
#covar.vec.string<-paste(covar.vec, sep="", collapse="")
#nsim2<-length(expos$Mean)/expos.rf
#nsim3<-1  # number of exposure files
#n.mediated.effects<-2

source("Code/sum.by.strata.redone.r")

#import all attributable moratlity 
allmort<-read.csv(file=paste("Outputs/all.mort.draws", year.vec.string, covar.vec.string, nsim1, ".csv", sep="_"), header=T)
allmort<-allmort[order(allmort$riskfactor, allmort$outcome, allmort$female, allmort$age, allmort$race),]
outcome<-unique(allmort$outcome)

totalmort<-read.csv(file=paste("Inputs/observed.cancer.mort.draws",  year.vec.string, nsim1, ".csv", sep="_"))
totalmort$outcome<-totalmort$disease
totalmort<-totalmort[order(totalmort$disease, totalmort$female, totalmort$age, totalmort$race),]

##Summary 1: summarize for each direct and indirect effects on disease, for each dietary factor 
totalmort_1<-totalmort[totalmort$disease %in% outcome,]

unique(totalmort_1$outcome)
sum.stats.byOverall.attr.mort0<-Sum.by.overall(allmort=allmort, totalmort=totalmort_1)
#check<-sum.stats.byOverall.attr.mort0[[1]]

write.csv(x=sum.stats.byOverall.attr.mort0[[1]], file=paste("Outputs/summarystats_attributable_USmortality0_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)

##################Summary 2: summarize for each disease, combining direct and indirect effects########################

###prepare data for jointing direct and BMI mediated effects for each disease type
allmort<-allmort %>%
  mutate(outcomet= case_when(grepl("CHD", outcome) ~ "CHD",
                             grepl("DB", outcome) ~ "DB",
                             grepl("IST", outcome) ~ "IST",
                             grepl("HST", outcome) ~ "HST",
                             grepl("SCC", outcome) ~ "SCC",
                             grepl("SCNC", outcome) ~ "SCNC",
                             grepl("ECA", outcome) ~ "ECA",
                             grepl("EC", outcome) ~ "EC",
                             grepl("CC", outcome) ~ "CC",
                             grepl("SC", outcome) ~ "SC",
                             grepl("LVC", outcome) ~ "LVC",
                             grepl("APCA", outcome) ~ "APCA",
                             grepl("PC", outcome) ~ "PC",
                             grepl("UC", outcome) ~ "UC",
                             grepl("OC", outcome) ~ "OC",
                             grepl("GC", outcome) ~ "GC",
                             grepl("KC", outcome) ~ "KC",
                             grepl("BC", outcome) ~ "BC",
                             grepl("TC", outcome) ~ "TC",
                             grepl("MPLC", outcome) ~ "MPLC",
                             grepl("Larynx", outcome) ~ "Larynx",
                             grepl("MMC", outcome) ~ "MMC"
  ))

cols.to.sum<-paste("V", 1:n.sims, sep="")
allmort.dt<-as.data.table(allmort)
# joint direct and BMI mediated effects for each disease type
allmort_t<-allmort.dt[,lapply(.SD, sum), by=c("age","female","race", "outcomet", "riskfactor"), .SDcols=cols.to.sum]
allmort_t$outcome=allmort_t$outcomet
outcome1<-unique(allmort_t$outcomet)
totalmort_t<-totalmort[totalmort$disease %in% outcome1,]

##Summary 2.1: summarize for each disease, for each dietary factor, for overall population 

sum.stats.byOverall.attr.mort<-Sum.by.overall(allmort=allmort_t, totalmort=totalmort_t)

write.csv(x=sum.stats.byOverall.attr.mort[[1]], file=paste("Outputs/summarystats_attributable_USmortality_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)

# write.csv(x=sum.stats.byOverall.attr.mort[[2]], file=paste("Outputs/attributable_USmortality_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#           row.names=FALSE)
# write.csv(x=sum.stats.byOverall.attr.mort[[3]], file=paste("Outputs/total_USmortality_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#           row.names=FALSE)
#write.csv(x=sum.stats.byOverall.attr.mort[[4]], file=paste("Outputs/RE_PIFs_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#           row.names=FALSE)


### PIFs by strata

strata.combos<-c("age","female")
sum.stats.byX.attr.mort<-Sum.by.strata(allmort=allmort_t, totalmort=totalmort_t, covar=strata.combos)
write.csv(x=sum.stats.byX.attr.mort[[4]], file=paste("Outputs/draws/RE_PIFs_draws0_", "by_", paste(strata.combos, sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""), 
            row.names=FALSE)
  


##Summary 2.2: summarize for each disease, joining dietary factors, for overall population 

###Joint PIFs across dietary factors
PAF.draws<-data.frame(sum.stats.byOverall.attr.mort[[4]])
PAF.draws$overall=1
strata="overall"
joint.PAFs.all.draws<-calc.joint.PAFs(PAF.draws=PAF.draws, strata=strata)
#write.csv(x=joint.PAFs.all.draws, file=paste("Outputs/joint_PIFs_all_draws_", "overall", "_", year.vec.string, ".csv", sep=""))

###Joint mortality across dietary factors
total.mort<-sum.stats.byOverall.attr.mort[[3]]
total.mort$overall=1
joint.mort.all.draws<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws, total.mort.draws=total.mort,"overall")
#write.csv(x=joint.mort.all.draws, file=paste("Outputs/joint_mort_all_draws_", "overall", "_", year.vec.string, ".csv", sep=""))

###summarize 
sum.stats.byOverall.joint.attr.mort<-Sum.by.overall.joint(allmort=joint.mort.all.draws, totalmort=totalmort_t)

write.csv(x=sum.stats.byOverall.joint.attr.mort[[1]], file=paste("Outputs/summarystats_joint_attributable_USmortality_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)
#write.csv(x=sum.stats.byOverall.joint.attr.mort[[2]], file=paste("Outputs/joint_attributable_USmortality_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#          row.names=FALSE)
#write.csv(x=sum.stats.byOverall.joint.attr.mort[[3]], file=paste("Outputs/RE_joint_PIFs_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#         row.names=FALSE)


##Summary 2.3: summarize for each disease, for populationsubgroups

strata<-names(allmort)[1:(which(names(allmort)=="mean..food.")-1)] # take names of strata as used in the output file (eg age, female, race, rather than Age, Sex, Race)
##order should match with covar.vec
for(i in 1:length(strata))
{
  x<-combn(strata, i)
  combos.for.i<-split(x, rep(1:ncol(x), each = nrow(x))) #https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
  strata.combos<-c(strata.combos, combos.for.i)
}

###Joint PIFs by strata
for(i in 1:length(strata.combos))
{
  sum.stats.byX.attr.mort<-Sum.by.strata(allmort=allmort_t, totalmort=totalmort_t, covar=strata.combos[[i]])
  write.csv(x=sum.stats.byX.attr.mort[[3]], file=paste("Outputs/draws/RE_PIFs_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""), 
            row.names=FALSE)
  
  PAF.draws<-data.frame(sum.stats.byX.attr.mort[[4]]) 
  strata<-strata.combos[[i]]
  joint.PAFs.all.draws<-calc.joint.PAFs(PAF.draws=PAF.draws, strata=strata)
  total.mort<-sum.stats.byX.attr.mort[[3]]
  joint.mort.all.draws<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws, total.mort.draws=total.mort, 
                                        strata=strata)
  sum.stats.byX.joint.attr.mort<-Sum.by.strata.joint(allmort=joint.mort.all.draws, totalmort= total.mort, covar=strata.combos[[i]])
  
  #write.csv(x=sum.stats.byX.joint.attr.mort[[1]], file=paste("Outputs/summarystats_joint_attributable_USmortality_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""), 
  #          row.names=FALSE)
  # write.csv(x=sum.stats.byX.joint.attr.mort[[2]], file=paste("Outputs/draws/joint_attributable_USmortality_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""), 
  #           row.names=FALSE)
  write.csv(x=sum.stats.byX.joint.attr.mort[[3]], file=paste("Outputs/draws/RE_joint_PIFs_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""), 
            row.names=FALSE)
}

################
####combine DALYs at this level,sinceDALYs are avaiable for each disease outcomes by age and se #####
##############

DALYs<-read.csv(file="Inputs/DALYs.csv")
DALYs$female<-DALYs$sex_id-1
DALYs$DALY<-DALYs$x
DALY1<-DALYs %>%
  mutate(outcome2= case_when(outcome %in%  c("CHD",  "IST", "HST") ~"CVD", 
                             outcome %in%  c("DB") ~"DB", 
                             outcome %in% c("EC", "CC", "SC", "ECA", "SCC", "SCNC", "LVC", "PC", "UC", "OC", "GC", "KC", "APCA", "BC", "TC", "MPLC","Larynx","MMC") ~"Cancer"))
###Import PIFs by age and sex
# allPIFs<-read.csv(file=paste("Outputs/draws/RE_PIFs_draws_by_age_female_", year.vec.string ,".csv", sep=""), header=T)
#  all.DALY1<-merge(allPIFs, DALY1, by.x=c("age","female","outcome"), by.y= )
#  all.DALY1[,cols.to.sum]<-all.DALY1[,cols.to.sum]*all.DALY1$DALY
#  all.DALY1.dt<-data.table(all.DALY1)


###############################################################################################
########Summary 3: summarize for CVD, DB and cancer############################################
###############################################################################################


###3.0 prepare data for for jointing disease types to cancer, CVD and diabetes
allmort<-allmort %>%
  mutate(outcome2= case_when(outcomet %in%  c("CHD",  "IST", "HST") ~"CVD", 
                             outcomet %in%  c("DB") ~"DB", 
                             outcomet %in% c("EC", "CC", "SC", "ECA", "SCC", "SCNC", "LVC", "PC", "UC", "OC", "GC", "KC", "APCA", "BC", "TC", "MPLC","Larynx","MMC") ~"Cancer"))
allmort.dt<-as.data.table(allmort)
# joint disease types to cancer, CVD and diabetes
allmort_t2<-allmort.dt[,lapply(.SD, sum), by=c("age","female","race", "outcome2", "riskfactor"), .SDcols=cols.to.sum]

outcome2<-unique(allmort_t2$outcome2)

totalmort_t<-totalmort_t %>%
  mutate(outcome2= case_when(outcome %in%  c("CHD",  "IST", "HST") ~"CVD", 
                             outcome %in%  c("DB") ~"DB", 
                             outcome %in% c("EC", "CC", "SC", "ECA", "SCC", "SCNC", "LVC", "PC", "UC", "OC", "GC", "KC", "APCA", "BC", "TC", "MPLC","Larynx","MMC") ~"Cancer"))

totalmort.dt<-as.data.table(totalmort_t)
cols.to.sum.totalmort<-paste("X", 1:n.sims, sep="")
totalmort_t2<-totalmort.dt[,lapply(.SD, sum), by=c("age","female","race", "outcome2"), .SDcols=cols.to.sum.totalmort]

allmort_t2$outcome=allmort_t2$outcome2
totalmort_t2$outcome=totalmort_t2$outcome2

allmort_t3<-allmort.dt[,lapply(.SD, sum), by=c("age","female","race", "riskfactor"), .SDcols=cols.to.sum]
totalmort.dt2<-as.data.table(totalmort_t2)
totalmort_t3<-totalmort.dt2[,lapply(.SD, sum), by=c("age","female","race"), .SDcols=cols.to.sum.totalmort]
allmort_t3$outcome="all" 
totalmort_t3$outcome="all" 
################
##3.1 summarize for CVD, DB and cancer,for each dietary factor, for overall population 
################

sum.stats.byOverall.attr.mort2<-Sum.by.overall(allmort=allmort_t2, totalmort=totalmort_t2)

sumstat.overall.bydiet<-data.frame(sum.stats.byOverall.attr.mort2[[1]])
sumstat.mort.draw.bydiet<-data.frame(sum.stats.byOverall.attr.mort2[[2]])
sumstat.mort.draw.bydiet1<-sumstat.mort.draw.bydiet[!sumstat.mort.draw.bydiet$riskfactor %in% c("GLwheat","GLrice","rfg","wg"),]

sumstat.PAF.draw.bydiet1<-data.frame(sum.stats.byOverall.attr.mort2[[4]])
## effects for dietary factors expect for whole grain related for overall population 
sumstat.overall.bydiet1<-sumstat.overall.bydiet[!sumstat.overall.bydiet$riskfactor %in% c("GLwheat","GLrice","rfg","wg"),]
sumstat.PAF.draw.bydiet1<-sumstat.PAF.draw.bydiet1[!sumstat.PAF.draw.bydiet1$riskfactor %in% c("GLwheat","GLrice","rfg","wg"),]

## joining effects related to whole grain rule for overall population 
###Joint PIFs across dietary factors
PAF.draws<-data.frame(sum.stats.byOverall.attr.mort2[[4]])
PAF.draws<-PAF.draws[PAF.draws$riskfactor %in% c("GLwheat","GLrice","rfg","wg"),]
PAF.draws$overall=1
strata="overall"
joint.PAFs.all.draws.wg<-calc.joint.PAFs(PAF.draws=PAF.draws, strata=strata)
#write.csv(x=joint.PAFs.all.draws, file=paste("Outputs/joint_PIFs_all_draws2_", "overall", "_", year.vec.string, ".csv", sep=""))
###Joint mortality across dietary factors

total.mort<-data.frame(sum.stats.byOverall.attr.mort2[[3]])
total.mort$overall=1
joint.mort.all.draws.wg<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws.wg, total.mort.draws=total.mort,"overall")


#write.csv(x=joint.mort.all.draws, file=paste("Outputs/joint_mort_all_draws2_", "overall", "_", year.vec.string, ".csv", sep=""))

###summarize 
sum.stats.byOverall.joint.attr.mort.wg<-Sum.by.overall.joint(allmort=joint.mort.all.draws.wg, totalmort=totalmort_t2)
sumstat.overall.bydiet.wg<-data.frame(sum.stats.byOverall.joint.attr.mort.wg[[1]])
sumstat.overall.bydiet.wg$riskfactor="wg"

sumstat.overall.bydiet2<-rbind(sumstat.overall.bydiet1, sumstat.overall.bydiet.wg)


write.csv(x=sumstat.overall.bydiet2, file=paste("Outputs/summarystats2_bydiet", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)

##Mortality
joint.mort.all.draws.wg$riskfactor="wg"
sumstat.mort.draw.bydiet2<-rbind(sumstat.mort.draw.bydiet1, joint.mort.all.draws.wg[,-1])
sumstat.mort.draw.bydiet2$result<-"mort"
sumstat.mort.draw.bydiet2<-as.data.table(sumstat.mort.draw.bydiet2)
sumstat.mort.draw.bydiet2.tot<-sumstat.mort.draw.bydiet2[,lapply(.SD, sum), by="riskfactor", .SDcols=cols.to.sum]

sumstat.mort.draw.bydiet2.tot$result<-"mort"
sumstat.mort.draw.bydiet2.tot$outcome<-"overall"
#PIF
sumstat.PAF.draw.wg<-data.frame(sum.stats.byOverall.joint.attr.mort.wg[[3]])
sumstat.PAF.draw.wg$riskfactor="wg"

sumstat.PAF.draw.bydiet<-rbind(sumstat.PAF.draw.bydiet1, sumstat.PAF.draw.wg)
sumstat.PAF.draw.bydiet$result="PIF"


##overall direct medical costs for the disease
## Cancer costs 183 in 2015 from source file inflated to 220 in 2021 (inflation rate :1.041*1.018*1.02*1.04=1.12417)
##CVD costs:380 in 2019 , 414 in 2021--> inflation considered in the projection 
##Diabetes costs: 237  in 2017 from source file inflated to 268 in 2021
costsdiseases<-c(414,  220, 268 )
prod_costs<-c(337, 147, 101 )

#DALYs<-  c(17267000, 16472000, 4461000)

###summarize 


cols<-c("outcome","riskfactor","result")
sumstat.PAF.draw.medcost<-data.table(cbind(sumstat.PAF.draw.bydiet[,cols], sumstat.PAF.draw.bydiet[,cols.to.sum]*costsdiseases))
sumstat.PAF.draw.medcost$result<-"medcost"
sumstat.PAF.draw.medcost.tot<-sumstat.PAF.draw.medcost[,lapply(.SD, sum), by="riskfactor", .SDcols=cols.to.sum]
sumstat.PAF.draw.medcost.tot$result<-"medcost"
sumstat.PAF.draw.medcost.tot$outcome<-"overall"

sumstat.PAF.draw.prodcost<-data.table(cbind(sumstat.PAF.draw.bydiet[,cols], sumstat.PAF.draw.bydiet[,cols.to.sum]*prod_costs))
sumstat.PAF.draw.prodcost$result<-"prodcost"
sumstat.PAF.draw.prodcost.tot<-sumstat.PAF.draw.prodcost[,lapply(.SD, sum),by="riskfactor", .SDcols=cols.to.sum]
sumstat.PAF.draw.prodcost.tot$result<-"prodcost"
sumstat.PAF.draw.prodcost.tot$outcome<-"overall"

sumstat.PAF.draw.totalcosts<-rbind(sumstat.PAF.draw.medcost, sumstat.PAF.draw.medcost.tot, sumstat.PAF.draw.prodcost, sumstat.PAF.draw.prodcost.tot)

sumstat.PAF.draw.totalcost.tot<-sumstat.PAF.draw.totalcosts[,lapply(.SD, sum),  by=c("riskfactor","outcome"), .SDcols=cols.to.sum]
sumstat.PAF.draw.totalcost.tot$result<-"totalcosts"
sumstat.PAF.draw.totalcost.tot

###summarize by CVD DB and cancer, and dietary factors
###Import joint PIFs by age and sex
allPIFs<-read.csv(file=paste("Outputs/draws/RE_PIFs_draws0_by_age_female_", year.vec.string ,".csv", sep=""), header=T)
all.joint.DALY1<-merge(allPIFs, DALY1, by.x=c("age","female","outcome"), by.y=c("age","female","outcome") )
all.joint.DALY1[,cols.to.sum]<-all.joint.DALY1[,cols.to.sum]*all.joint.DALY1$DALY
all.joint.DALY1[is.na(all.joint.DALY1)]<-0
all.joint.DALY1 <-all.joint.DALY1 %>%
  mutate(riskfactor= case_when(riskfactor %in%  c("GLwheat","GLrice","rfg","wg") ~"wg",
                               TRUE ~ riskfactor) )

all.joint.DALY1.dt<-data.table(all.joint.DALY1)

sumstat.PAF.draw.joint.DALYS<-all.joint.DALY1.dt[,lapply(.SD, sum), by=c("outcome2","riskfactor"), .SDcols=cols.to.sum]
sumstat.PAF.draw.joint.DALYS$result<-"DALYs"
colnames(sumstat.PAF.draw.joint.DALYS)[1] <- "outcome"

sumstat.PAF.draw.joint.DALYS.tot<-sumstat.PAF.draw.joint.DALYS[,lapply(.SD, sum),by=c("riskfactor"), .SDcols=cols.to.sum]
sumstat.PAF.draw.joint.DALYS.tot$result<-"DALYs"
sumstat.PAF.draw.joint.DALYS.tot$outcome<-"overall"



sumstat.alldraw<-rbind(sumstat.mort.draw.bydiet2, sumstat.mort.draw.bydiet2.tot, sumstat.PAF.draw.bydiet,
  sumstat.PAF.draw.joint.DALYS, sumstat.PAF.draw.joint.DALYS.tot,
  sumstat.PAF.draw.medcost, sumstat.PAF.draw.medcost.tot, 
                       sumstat.PAF.draw.prodcost, sumstat.PAF.draw.prodcost.tot,sumstat.PAF.draw.totalcost.tot)

sum.stats.all<-sum.stats.maker(allmort=sumstat.alldraw)

write.csv(x=sum.stats.all, file=paste("Outputs/summarystats_allresult_joint_bydiet", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)


##Summary 3.1.2: summarize for CVD, DB and cancer joining dietary factors, for overall population 

###Joint PIFs across dietary factors
PAF.draws<-data.frame(sum.stats.byOverall.attr.mort2[[4]])
PAF.draws$overall=1
strata="overall"
joint.PAFs.all.draws2<-calc.joint.PAFs(PAF.draws=PAF.draws, strata=strata)
#write.csv(x=joint.PAFs.all.draws, file=paste("Outputs/joint_PIFs_all_draws_", "overall", "_", year.vec.string, ".csv", sep=""))

###Joint mortality across dietary factors
total.mort<-sum.stats.byOverall.attr.mort2[[3]]
total.mort$overall=1
joint.mort.all.draws2<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws2, total.mort.draws=total.mort,"overall")
#write.csv(x=joint.mort.all.draws, file=paste("Outputs/joint_mort_all_draws_", "overall", "_", year.vec.string, ".csv", sep=""))

###summarize 
sum.stats.byOverall.joint.attr.mort2<-Sum.by.overall.joint(allmort=joint.mort.all.draws2, totalmort=total.mort)

write.csv(x=sum.stats.byOverall.joint.attr.mort2[[1]], file=paste("Outputs/summarystats_joint_attributable_USmortality2_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)
#write.csv(x=sum.stats.byOverall.joint.attr.mort[[2]], file=paste("Outputs/joint_attributable_USmortality_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#          row.names=FALSE)
#write.csv(x=sum.stats.byOverall.joint.attr.mort[[3]], file=paste("Outputs/RE_joint_PIFs_draws_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
#         row.names=FALSE)
###summarize DALY by CVD DB and cancer by dietary factors 
# sumstat.PAF.draw.DALYS<-all.DALY1.dt[,lapply(.SD, sum), by=c("riskfactor","outcome2"), .SDcols=cols.to.sum]
# sumstat.PAF.draw.DALYS$result<-"DALYs"
# colnames(sumstat.PAF.draw.DALYS)[1] <- "outcome"

###summarize by CVD DB and cancer, jointing dietary factors
###Import joint PIFs by age and sex
allPIFs<-read.csv(file=paste("Outputs/draws/RE_joint_PIFs_draws_by_age_female_", year.vec.string ,".csv", sep=""), header=T)
all.joint.DALY1<-merge(allPIFs, DALY1, by.x=c("age","female","outcome"), by.y=c("age","female","outcome") )
all.joint.DALY1[,cols.to.sum]<-all.joint.DALY1[,cols.to.sum]*all.joint.DALY1$DALY
all.joint.DALY1[is.na(all.joint.DALY1)]<-0
all.joint.DALY1.dt<-data.table(all.joint.DALY1)

sumstat.PAF.draw.joint.DALYS<-all.joint.DALY1.dt[,lapply(.SD, sum), by=c("outcome2"), .SDcols=cols.to.sum]
sumstat.PAF.draw.joint.DALYS$result<-"DALYs"
colnames(sumstat.PAF.draw.joint.DALYS)[1] <- "outcome"

sumstat.PAF.draw.joint.DALYS.tot<-sumstat.PAF.draw.joint.DALYS[,lapply(.SD, sum), .SDcols=cols.to.sum]
sumstat.PAF.draw.joint.DALYS.tot$result<-"DALYs"
sumstat.PAF.draw.joint.DALYS.tot$outcome<-"overall"



################
####Economic outcomes, only avaiable at overall population level#####
##############

##overall direct medical costs for the disease
## Cancer costs 183 in 2015 from source file inflated to 220 in 2021 (inflation rate :1.041*1.018*1.02*1.04=1.12417)
##CVD costs:380 in 2019 , 414 in 2021--> inflation considered in the projection 
##Diabetes costs: 237  in 2017 from source file inflated to 268 in 2021
costsdiseases<-c(414,  220, 268 )
prod_costs<-c(337, 147, 101 )

#DALYs<-  c(17267000, 16472000, 4461000)

###summarize 

sumstat.PAF.draw<-sum.stats.byOverall.joint.attr.mort2[[3]]
sumstat.PAF.draw.mort<-sum.stats.byOverall.joint.attr.mort2[[2]]
sumstat.PAF.draw.mort$result<-"Mortality"
sumstat.PAF.draw.mort.tot<-sumstat.PAF.draw.mort[,lapply(.SD, sum),  .SDcols=cols.to.sum]
sumstat.PAF.draw.mort.tot$result<-"Mortality"
sumstat.PAF.draw.mort.tot$outcome<-"overall"



sumstat.PAF.draw.medcost<-cbind(sumstat.PAF.draw[,1], sumstat.PAF.draw[,-1]*costsdiseases)
sumstat.PAF.draw.medcost$result<-"medcost"
sumstat.PAF.draw.medcost.tot<-sumstat.PAF.draw.medcost[,lapply(.SD, sum),  .SDcols=cols.to.sum]
sumstat.PAF.draw.medcost.tot$result<-"medcost"
sumstat.PAF.draw.medcost.tot$outcome<-"overall"

sumstat.PAF.draw.prodcost<-cbind(sumstat.PAF.draw[,1], sumstat.PAF.draw[,-1]*prod_costs)
sumstat.PAF.draw.prodcost$result<-"prodcost"
sumstat.PAF.draw.prodcost.tot<-sumstat.PAF.draw.prodcost[,lapply(.SD, sum),  .SDcols=cols.to.sum]
sumstat.PAF.draw.prodcost.tot$result<-"prodcost"
sumstat.PAF.draw.prodcost.tot$outcome<-"overall"

sumstat.PAF.draw.totalcosts<-rbind(sumstat.PAF.draw.medcost, sumstat.PAF.draw.medcost.tot, sumstat.PAF.draw.prodcost, sumstat.PAF.draw.prodcost.tot)

sumstat.PAF.draw.totalcost.tot<-sumstat.PAF.draw.totalcosts[,lapply(.SD, sum),  by=c("outcome"), .SDcols=cols.to.sum]
sumstat.PAF.draw.totalcost.tot$result<-"totalcosts"

sumstat.PAF.draw$result<-"PAF"


sumstat.alldraw<-rbind(sumstat.PAF.draw,sumstat.PAF.draw.mort,
                       sumstat.PAF.draw.mort.tot, sumstat.PAF.draw.joint.DALYS, sumstat.PAF.draw.joint.DALYS.tot, 
                       sumstat.PAF.draw.medcost, sumstat.PAF.draw.medcost.tot, sumstat.PAF.draw.prodcost, sumstat.PAF.draw.prodcost.tot
                       ,sumstat.PAF.draw.totalcost.tot)
sum.stats.all<-sum.stats.maker(allmort=sumstat.alldraw)
write.csv(x=sum.stats.all, file=paste("Outputs/summarystats_allresult_joint_", "by_", "Overall", "_", year.vec.string, ".csv", sep=""), 
          row.names=FALSE)

################
##3.2 get sum stats by all strata combos, by dietary factors and joint dietary factors
###########################
###generate list strata combinations

for(i in 1:length(strata.combos))
{
  #########summarize for for each dietary factor###############3
  sum.stats.byX.attr.mort<-Sum.by.strata(allmort=allmort_t2, totalmort=totalmort_t2, covar=strata.combos[[i]])
  
  write.csv(x=sum.stats.byX.attr.mort[[1]], file=paste("Outputs/summarystats_attributable_USmortality2_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
            row.names=FALSE)
  # write.csv(x=sum.stats.byX.attr.mort[[2]], file=paste("Outputs/draws/attributable_USmortality2_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
  #           row.names=FALSE)
  # write.csv(x=sum.stats.byX.attr.mort[[3]], file=paste("Outputs/draws/total_USmortality2_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
  #           row.names=FALSE)
  # write.csv(x=sum.stats.byX.attr.mort[[4]], file=paste("Outputs/draws/RE_PIFs_draws2_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
  #           row.names=FALSE)
  
  ###joint dietary factors
  PAF.draws<-data.frame(sum.stats.byX.attr.mort[[4]])
  
  strata<-strata.combos[[i]]
  joint.PAFs.all.draws<-calc.joint.PAFs(PAF.draws=PAF.draws, strata=strata)
  #write.csv(x=joint.PAFs.all.draws, file=paste("Outputs/joint_PIFs_all_draws_", paste(strata, collapse="_"), "_", year.vec.string, ".csv", sep=""))
  
  total.mort<-sum.stats.byX.attr.mort[[3]]
  
  joint.mort.all.draws<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws, total.mort.draws=total.mort,
                                        strata=strata)
  
  sum.stats.byX.joint.attr.mort<-Sum.by.strata.joint(allmort=joint.mort.all.draws, totalmort= total.mort, covar=strata.combos[[i]])
  
  write.csv(x=sum.stats.byX.joint.attr.mort[[1]], file=paste("Outputs/summarystats_joint_attributable_USmortality2_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
            row.names=FALSE)
  # write.csv(x=sum.stats.byX.joint.attr.mort[[2]], file=paste("Outputs/draws/joint_attributable_USmortality_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
  #           row.names=FALSE)
  write.csv(x=sum.stats.byX.joint.attr.mort[[3]], file=paste("Outputs/draws/RE_joint_PIFs_draws2_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
            row.names=FALSE)
  
}

strata.combos<-"race"


  #########summarize for for each dietary factor###############3
  sum.stats.byX.attr.mort3<-Sum.by.strata(allmort=allmort_t3, totalmort=totalmort_t3, covar=strata.combos  )


  ###joint dietary factors
  PAF.draws3<-data.frame(sum.stats.byX.attr.mort3[[4]])

  strata<-strata.combos
  joint.PAFs.all.draws3<-calc.joint.PAFs(PAF.draws=PAF.draws3, strata=strata)
  #write.csv(x=joint.PAFs.all.draws, file=paste("Outputs/joint_PIFs_all_draws_", paste(strata, collapse="_"), "_", year.vec.string, ".csv", sep=""))

  total.mort3<-sum.stats.byX.attr.mort3[[3]]

  joint.mort.all.draws3<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws3, total.mort.draws=total.mort3,
                                         strata=strata)
  joint.mort.all.draws3$outcome="all"
  joint.mort.all.draws3$riskfactor="overall"
  total.mort3$outcome="all"
  sum.stats.byX.joint.attr.mort3<-Sum.by.strata(allmort=joint.mort.all.draws3, totalmort= total.mort3, covar=strata.combos)

  write.csv(x=sum.stats.byX.joint.attr.mort3[[1]], file=paste("Outputs/summarystats_joint_attributable_USmortality3_", "by_", paste(strata.combos, sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
            row.names=FALSE)
  # write.csv(x=sum.stats.byX.joint.attr.mort[[2]], file=paste("Outputs/draws/joint_attributable_USmortality_draws_", "by_", paste(strata.combos[[i]], sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
  #           row.names=FALSE)
  write.csv(x=sum.stats.byX.joint.attr.mort[[3]], file=paste("Outputs/draws/RE_joint_PIFs_draws3_", "by_", paste(strata.combos, sep="", collapse="_"), "_", year.vec.string, ".csv", sep=""),
            row.names=FALSE)


# 
# ################
# ##4 summarize for all diseases in total
# ################
# # 
# ###Joint PIFs across dietary factors
# PAF.draws<-data.frame(sum.stats.byOverall.attr.mort2[[4]])
# PAF.draws$overall=1
# strata="overall"
# joint.PAFs.all.draws2<-calc.joint.PAFs(PAF.draws=PAF.draws, strata=strata)
# 
# ###Joint mortality across dietary factors
# total.mort<-sum.stats.byOverall.attr.mort2[[3]]
# total.mort$overall=1
# joint.mort.all.draws2<-calc.joint.mort(joint.paf.draws=joint.PAFs.all.draws2, total.mort.draws=total.mort,"overall")
# joint.mort.all.draws2.dt<-data.table(joint.mort.all.draws2)
# # joint disease types to cancer, CVD and diabetes
# allmort_t3<-joint.mort.all.draws2.dt[,lapply(.SD, sum),  .SDcols=cols.to.sum]
# allmort_t3$outcome="overall"
# outcome2<-unique(allmort_t2$outcome2)
# 
# totalmort.dt2<-as.data.table(totalmort_t2)
# cols.to.sum.totalmort<-paste("X", 1:n.sims, sep="")
# totalmort_t3<-totalmort.dt2[,lapply(.SD, sum), .SDcols=cols.to.sum.totalmort]
# totalmort_t3$outcome="overall"
# sum.stats.byOverall.attr.mort3<-Sum.by.overall.joint(allmort=allmort_t3, totalmort=totalmort_t3)
# 
# 


