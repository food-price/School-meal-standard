#only works on specific allmort file created by my code
##use covar="overall" for all

##function of calculating RE PAFs sum stats
get.RE.PAFs.draws<-function(allmort, totalmort) ##for now, only works for age/sex/race
{
  merged<-merge(allmort, totalmort)
  
  allmort.starting.point<-which(names(merged)=="V1")
  allmort.ending.point<-which(names(merged)=="riskfactor")-1
  totalmort.starting.point<-which(names(merged)=="X1")
  totalmort.ending.point<-dim(merged)[2]
  
  RE.PAFs.info<-merged[,which(names(merged) %in% c("age", "female", "race", "outcome", "mean..food.", "se..food", "sd..food.", "riskfactor", "count", "se"))]
  
  RE.PAFs<-cbind(merged[,which(names(merged) %in% c("age", "female", "race", "outcome", "mean..food.", "se..food", "sd..food.", "riskfactor", "count", "se"))], 
                 merged[,allmort.starting.point:allmort.ending.point]/merged[,totalmort.starting.point:totalmort.ending.point])
  RE.PAFs<-RE.PAFs[order(RE.PAFs$riskfactor, RE.PAFs$outcome, RE.PAFs$female, RE.PAFs$age, RE.PAFs$race),]
  
  return(RE.PAFs)
}
  
get.RE.PAFs<-function(allmort, totalmort) ##for now, only works for age/sex/race
{
  merged<-merge(allmort, totalmort)

  allmort.starting.point<-which(names(merged)=="V1")
  allmort.ending.point<-which(names(merged)=="riskfactor")-1
  totalmort.starting.point<-which(names(merged)=="X1")
  totalmort.ending.point<-dim(merged)[2]

  RE.PAFs.info<-merged[,which(names(merged) %in% c("age", "female", "race", "outcome", "mean..food.", "se..food", "sd..food.", "riskfactor", "count", "se"))]

  RE.PAFs<-cbind(merged[,which(names(merged) %in% c("age", "female", "race", "outcome", "mean..food.", "se..food", "sd..food.", "riskfactor", "count", "se"))], 
               merged[,allmort.starting.point:allmort.ending.point]/merged[,totalmort.starting.point:totalmort.ending.point])
  RE.PAFs<-RE.PAFs[order(RE.PAFs$riskfactor, RE.PAFs$outcome, RE.PAFs$female, RE.PAFs$age, RE.PAFs$race),]

  RE.PAFs.sum.stats<-sum.stats.maker.RE.PAFs(RE.PAFs, n.covar)
  return(RE.PAFs.sum.stats)
}

#function to get summary stats##################################
sum.stats.maker<-function(allmort)
{
  allmort.dt<-as.data.table(allmort)
  vars<-paste("V", 1:n.sims, sep="")
  
  
  mort.summary <- allmort.dt[, ":="(means = rowMeans(.SD, na.rm = TRUE),
                                    sd.devs = apply(.SD, 1, sd),
                                    medians = apply(.SD, 1, median),
                                    LB = apply(.SD, 1, function(x) quantile(x, .025, na.rm = T)),
                                    UB = apply(.SD, 1, function(x) quantile(x, .975, na.rm = T))
  ),
  .SDcols = vars
  ]
  
  mort.summary <- mort.summary[, (vars) := NULL]
  
  return(mort.summary)
}

sum.stats.maker.RE.PAFs<-function(RE.PAFs)
{
  RE.PAFs.dt<-as.data.table(RE.PAFs)
  vars<-paste("V", 1:n.sims, sep="")
  
  RE.PAFs.summary <- RE.PAFs.dt[, ":="(RE.PAF.means = rowMeans(.SD, na.rm = TRUE),
                                       RE.PAF.sd.devs = apply(.SD, 1, sd),
                                       RE.PAF.medians = apply(.SD, 1, median),
                                       RE.PAF.LB = apply(.SD, 1, function(x) quantile(x, .025, na.rm = T)),
                                       RE.PAF.UB = apply(.SD, 1, function(x) quantile(x, .975, na.rm = T)),
                                       RE.PAF.medians.as.percent = apply(.SD, 1, median)*100,
                                       percent = apply(.SD, 1, function(x) quantile(x, .025, na.rm = T))*100,
                                       RE.PAF.UB.as.percent = apply(.SD, 1, function(x) quantile(x, .975, na.rm = T))*100
  ),
  .SDcols = vars
  ]
  
  RE.PAFs.summary <- RE.PAFs.summary[, (vars) := NULL]
  
  return(RE.PAFs.summary)
}
#########################################

Sum.by.strata<-function(allmort, totalmort, covar)
{
  allmort.dt<-as.data.table(allmort)
  totalmort.dt<-as.data.table(totalmort)
  strata<-covar
  cols.to.sum.allmort<-paste("V", 1:n.sims, sep="")
  cols.to.sum.totalmort<-paste("X", 1:n.sims, sep="")
    strata.sims.allmort<-allmort.dt[,lapply(.SD, sum), by=c(strata, "outcome", "riskfactor"), .SDcols=cols.to.sum.allmort]
    strata.sims.totalmort<-totalmort.dt[,lapply(.SD, sum), by=c(strata, "outcome"), .SDcols=cols.to.sum.totalmort]
    merged<-merge(strata.sims.allmort, strata.sims.totalmort)
    allmort.starting.point<-which(names(merged)=="V1")
    allmort.ending.point<-which(names(merged)==paste("V", nsim1, sep=""))
    totalmort.starting.point<-which(names(merged)=="X1")
    totalmort.ending.point<-which(names(merged)==paste("X", nsim1, sep=""))
    #RE.PAFs.info<-merged[,c(unlist(strata.combos[j]), "outcome"), with=FALSE]
    RE.PAFs<-cbind(merged[,c(strata, "outcome", "riskfactor"), with=FALSE], 
                   merged[,allmort.starting.point:allmort.ending.point]/merged[,totalmort.starting.point:totalmort.ending.point])
    #replace NA with 0

    setorder(RE.PAFs, riskfactor, outcome)
    
    sum.stats.attr.mort.PAF<-sum.stats.maker(strata.sims.allmort)
    RE.PAFs.sum.stats<-sum.stats.maker.RE.PAFs(RE.PAFs)
    
   # merged.results<-merge(sum.stats.attr.mort.PAF, RE.PAFs.sum.stats)
    merged.results<-merge(data.frame(sum.stats.attr.mort.PAF), data.frame(RE.PAFs.sum.stats))
    
    strata.summary.stats<-merged.results
    
    return(list(strata.summary.stats, strata.sims.allmort, strata.sims.totalmort, RE.PAFs)) 
  
}

Sum.by.overall<-function(allmort, totalmort)
{
  allmort.dt<-as.data.table(allmort)
  totalmort.dt<-as.data.table(totalmort)
  cols.to.sum.allmort<-paste("V", 1:n.sims, sep="")
  cols.to.sum.totalmort<-paste("X", 1:n.sims, sep="")
  strata.sims.allmort<-allmort.dt[,lapply(.SD, sum), by=c("outcome", "riskfactor"), .SDcols=cols.to.sum.allmort]
  strata.sims.totalmort<-totalmort.dt[,lapply(.SD, sum), by=c("outcome"), .SDcols=cols.to.sum.totalmort]
  merged<-merge(strata.sims.allmort, strata.sims.totalmort)
  allmort.starting.point<-which(names(merged)=="V1")
  allmort.ending.point<-which(names(merged)==paste("V", nsim1, sep=""))
  totalmort.starting.point<-which(names(merged)=="X1")
  totalmort.ending.point<-which(names(merged)==paste("X", nsim1, sep=""))
  #RE.PAFs.info<-merged[,c(unlist(strata.combos[j]), "outcome"), with=FALSE]
  RE.PAFs<-cbind(merged[,c("outcome", "riskfactor"), with=FALSE], 
                 merged[,allmort.starting.point:allmort.ending.point]/merged[,totalmort.starting.point:totalmort.ending.point])
  RE.PAFs<-RE.PAFs[order(RE.PAFs$riskfactor, RE.PAFs$outcome),]
  sum.stats.attr.mort.PAF<-sum.stats.maker(strata.sims.allmort)
  RE.PAFs.sum.stats<-sum.stats.maker.RE.PAFs(RE.PAFs)
  merged.results<-merge(sum.stats.attr.mort.PAF, RE.PAFs.sum.stats)
  strata.summary.stats<-merged.results
  return(list(strata.summary.stats, strata.sims.allmort, strata.sims.totalmort, RE.PAFs)) 
}

Sum.by.overall.joint<-function(allmort, totalmort)
{
  allmort.dt<-as.data.table(allmort)
  totalmort.dt<-as.data.table(totalmort)
  cols.to.sum.allmort<-paste("V", 1:n.sims, sep="")
  cols.to.sum.totalmort<-paste("X", 1:n.sims, sep="")
  strata.sims.allmort<-allmort.dt[,lapply(.SD, sum), by=c("outcome"), .SDcols=cols.to.sum.allmort]
  strata.sims.totalmort<-totalmort.dt[,lapply(.SD, sum), by=c("outcome"), .SDcols=cols.to.sum.totalmort]
  merged<-merge(strata.sims.allmort, strata.sims.totalmort)
  allmort.starting.point<-which(names(merged)=="V1")
  allmort.ending.point<-which(names(merged)==paste("V", nsim1, sep=""))
  totalmort.starting.point<-which(names(merged)=="X1")
  totalmort.ending.point<-which(names(merged)==paste("X", nsim1, sep=""))
  
  #RE.PAFs.info<-merged[,c(unlist(strata.combos[j]), "outcome"), with=FALSE]
  RE.PAFs<-cbind(merged[,c("outcome"), with=FALSE], 
                 merged[,allmort.starting.point:allmort.ending.point]/merged[,totalmort.starting.point:totalmort.ending.point])
  setorder(RE.PAFs, outcome)
  
  sum.stats.attr.mort.PAF<-sum.stats.maker(strata.sims.allmort)
  RE.PAFs.sum.stats<-sum.stats.maker.RE.PAFs(RE.PAFs)
  merged.results<-merge(sum.stats.attr.mort.PAF, RE.PAFs.sum.stats, by="outcome")
  strata.summary.stats<-merged.results
  
  return(list(strata.summary.stats, strata.sims.allmort, RE.PAFs)) 
}


Sum.by.strata.joint<-function(allmort, totalmort, covar)
{
  allmort.dt<-as.data.table(allmort)
  totalmort.dt<-as.data.table(totalmort)
  strata<-covar
  cols.to.sum.allmort<-paste("V", 1:n.sims, sep="")
  cols.to.sum.totalmort<-paste("X", 1:n.sims, sep="")
  strata.sims.allmort<-allmort.dt[,lapply(.SD, sum), by=c(strata, "outcome"), .SDcols=cols.to.sum.allmort]
  strata.sims.totalmort<-totalmort.dt[,lapply(.SD, sum), by=c(strata, "outcome"), .SDcols=cols.to.sum.totalmort]
  merged<-merge(strata.sims.allmort, strata.sims.totalmort)
  allmort.starting.point<-which(names(merged)=="V1")
  allmort.ending.point<-which(names(merged)==paste("V", nsim1, sep=""))
  totalmort.starting.point<-which(names(merged)=="X1")
  totalmort.ending.point<-which(names(merged)==paste("X", nsim1, sep=""))
  
  #RE.PAFs.info<-merged[,c(unlist(strata.combos[j]), "outcome"), with=FALSE]
  RE.PAFs<-cbind(merged[,c(strata, "outcome"), with=FALSE], 
                 merged[,allmort.starting.point:allmort.ending.point]/merged[,totalmort.starting.point:totalmort.ending.point])
  
  
  setorder(RE.PAFs, outcome)
  
  sum.stats.attr.mort.PAF<-sum.stats.maker(strata.sims.allmort)
  RE.PAFs.sum.stats<-sum.stats.maker.RE.PAFs(RE.PAFs)
  
  merged.results<-merge(sum.stats.attr.mort.PAF, RE.PAFs.sum.stats)
  
  strata.summary.stats<-merged.results
  
  return(list(strata.summary.stats, strata.sims.allmort, RE.PAFs)) 
}
  

##Capitlizes every first letter of a word
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}



##caclulates joint PAFs by specified strata (but should only choose the fully stratified option)
calc.joint.PAFs<-function(PAF.draws, strata)
{
  n.draws<-length(grep(pattern="V", x=names(PAF.draws)))
  PAF.draws<-PAF.draws[as.character(PAF.draws$riskfactor)!="pufa_c" & as.character(PAF.draws$riskfactor)!="sfat",]
  
  PAF.draws$strata<-interaction(PAF.draws[,strata])
  
  PAF.draws.by.strata.outcome<-split(x=PAF.draws, f=list(PAF.draws$strata, PAF.draws$outcome))
  joint.PAFs.all.draws<-matrix(nrow=length(PAF.draws.by.strata.outcome), ncol=(length(strata)+1+n.draws))
  joint.PAFs.all.draws<-as.data.frame(joint.PAFs.all.draws)
  names(joint.PAFs.all.draws)[1:(1+length(strata)+n.draws)]<-c(strata, "outcome", paste("V", 1:n.draws, sep=""))  
  
  start<-which(names(PAF.draws.by.strata.outcome[[1]])=="V1")
  end<-start+n.draws-1
  
  for(i in 1:length(PAF.draws.by.strata.outcome))
  {
    joint.PAFs.all.draws[i,1:(length(strata))]<-PAF.draws.by.strata.outcome[[i]][1,strata]
    joint.PAFs.all.draws[i,length(strata)+1]<-as.character(PAF.draws.by.strata.outcome[[i]][["outcome"]])[1]
    joint.PAFs.all.draws[i,length(strata)+1+c(1:n.draws)]<-1-apply(1-PAF.draws.by.strata.outcome[[i]][,start:end], 
                                                                   MARGIN=2, FUN=prod)
  }
  return(joint.PAFs.all.draws)
}

##caclulates joint mort, by multilpying joint paf with total mort
calc.joint.mort<-function(joint.paf.draws, total.mort.draws,strata)
{
  merged<-merge(joint.paf.draws, total.mort.draws)
  Joint.PAF.starting.point<-which(names(merged)=="V1")
  Joint.PAF.ending.point<-which(names(merged)==paste("V", nsim1, sep=""))
  totalmort.starting.point<-which(names(merged)=="X1")
  totalmort.ending.point<-which(names(merged)==paste("X", nsim1, sep=""))
  
  joint.mort<-cbind(merged[,c(strata, "outcome")], 
                    merged[,Joint.PAF.starting.point:Joint.PAF.ending.point]*merged[,totalmort.starting.point:totalmort.ending.point])
  return(joint.mort)
}



