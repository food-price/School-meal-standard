#Complete.PAF.Analysis.partOne.r


mort.list<-list()  ##list of morts draws, one element per dietary factor
#observed.mort.list<-list()

PIF.list<-list() # a list of lists, giving the calculated PAFs by riskfactor, disease, strata, and iteration

PIFs<-list() ## above, a list of dataframes
samp_beta<-list()

data<-c()
q<-c()
qsx<-c()
qsx.alt<-c()
mtc<-c()
mtc.alt<-c()
x<-c()
x.alt<-c()
qsy<-c()
tmmtc<-c()
y<-c()
delat<-c()
delat.alt<-c()
p<-c()
p.alt<-c()
p.pp<-c()
p.pp.alt<-c()

delat<-list()
delat.alt<-list()
rr.list<-list()
rr.list.alt<-list()
p.rr<-list()
p.rr.alt<-c()
p<-list()
p.alt<-list()
pif<-replicate(num.diseases, matrix(NA, nsim2, nsim1), simplify=F)
mort<-replicate(num.diseases, matrix(NA, nsim2, nsim1), simplify=F)
observed.mort<-replicate(num.diseases, matrix(NA, nsim2, nsim1), simplify=F)
#observed.mort<-split(x=observed.mort.draws, f=observed.mort.draws$disease)

#paf<-list() ##<-matrix (NA,nsim2, nsim1)     # will need to change if the exposure dataset is longer than 5000 rows
#mort<-list() ##<-matrix (NA,nsim2, nsim1)   #####gms empty matrix for mortality estimations

#pif<-replicate(num.diseases, matrix(NA, nsim2, nsim1), simplify=F)

final_mat_pif<- matrix (NA, nsim2, 12)
final_mat_mort<- matrix (NA, nsim2, 12)      ####gms matrix with final mortality estimates

m<-list()
s<-list()
m_mort<-list()
s_mort<-list()
#*********************************************************************************************************
# First Loop for Each Risk Factor ************************************************************************ 
#*********************************************************************************************************                                                                                                       
# this loop runs for each risk factor in the vector rfvec

for (r in 1:length(rfvec)){  ####gms Start risk factor loop
  
  # vector of age-sex-race specific files to be read in; for dietary risks only one file is in the vector
  
  rr = subset(rrtotal, RF == rfvec[r])      # subset the relative risks dataset to the risk factor of interest

   
  food.to.BMI<-subset(Food.to.BMI.effects, food==rfvec[r])
  food.to.SBP<-subset(Food.to.SBP.effects, food==rfvec[r])
  
  ##From Gita's SSB code
  ################################################################
  ###    STEP 2a: DRAW AN ESTIMATE LINEAR EFFECT OF SSB ON BMI ###
  ################################################################
  
  # We will determine the increase in BMI per serving/day increase in SSB
  # As of 04.04.12 we will use the linear effects from 3 cohorts:
  #    for BMI <= 25 kg/m2 use 0.10
  #    for BMI > 25 kg/m2 use  0.23
  
  #Effect among normalweight
  beta.lin.low = rnorm (n=nsim1, mean=food.to.BMI$effect_normal_mean, sd=food.to.BMI$effect_normal_mean_se)
  #          print(beta.lin.low)
  #          if(is.na(beta.lin.low[1])) stop()
  samp.beta.lin.low = sample (beta.lin.low, nsim1)
  #samp.beta.lin.low[samp.beta.lin.low < 0] = 0
  #Effect among overweight
  beta.lin.high = rnorm (n=nsim1, mean=food.to.BMI$effect_overweight_mean, sd=food.to.BMI$effect_overweight_mean_se)
  samp.beta.lin.high = sample (beta.lin.high, nsim1)
  #samp.beta.lin.high[samp.beta.lin.high < 0] = 0
  
  ###################################################################
  ###    STEP 2a: DRAW AN ESTIMATE LINEAR EFFECT OF SODIUM ON SBP ###
  ###################################################################
  
  ## Sodium effects on blood pressure
  
  ############################################################################
  #################
  ## Main effects##
  #################
  
  #maineffect =  rnorm(n=nsim1, mean=1.62, sd= ((2.25-0.994)/3.92) )
  maineffect =  rnorm (n=nsim1, mean=food.to.SBP$main.effect.mean, sd=food.to.SBP$main.effect.se)
  
  ########################
  ## Interaction by age ##
  ########################
  
  #ageffect = rnorm(n=nsim1, mean = 0.0457, sd = ((0.071-0.0204)/3.92))
  age_effect =  rnorm (n=nsim1, mean=food.to.SBP$age.effect.mean, sd=food.to.SBP$age.effect.se)
  
  #########################
  ## Interaction by race ##
  #########################
  
  #raceffect = rnorm(n=nsim1, mean = 1.08, sd = ((2.11-0.304)/3.92))
  race_effect =  rnorm (n=nsim1, mean=food.to.SBP$race.effect.mean, sd=food.to.SBP$race.effect.se)
  
  ########################################
  ## Interaction by hypertensive status ##
  ########################################
  
  #htneffect = rnorm(n=nsim1, mean = 0.815, sd = ((1.56-0.052)/3.92))
  htneffect = rnorm (n=nsim1, mean=food.to.SBP$htn.effect.mean, sd=food.to.SBP$htn.effect.se)
  
  # theoretical minimums: *** Specific for each rf *************
  ## iterate here to go through all the risk factors
  theominsub = subset(theomin, Risk_factor==rfvec[r])
  mu_tmrd = theominsub$TMRED
  sd_tmrd = theominsub$SD
  
  
  ########################################################
  ### estimating PAF for each risk factor_disease pair ###
  ########################################################
  
  for (l in 1:nsim3){   ####gms Start file loop: loop through exposure datasets (there is usually just 1 exposure file so in that case this is redundant)
    
    #data0 = read.csv(file=file_m[l], header=T)
    data0<-expos
    # data0 will be the exposure file 
    
    data01 = subset(expos, riskfactor==rfvec[r]) 
    # data01 will be the subset of a exposure file for each risk factor  
    ## iterate here to go through all the risk factors.
    
    #data1 = data01[order(data01$year, data01$iso3, data01$sex, data01$age),]
 
    if(identical(covar.vec, c("Age", "Sex", "Race")))
    {
      data1 <- data01[order(data01$female, data01$age, data01$race),] 
    }
    
    # data1 will be the ordered data01 
    # make sure that the rf data is in the same order as the rr data
    
    count.mm.tozero<-0
    count.mm.alt.tozero<-0
    total.count<-0
    
    #for (i in 1:length(data1$exposmean)) {    ####gms Start of loop that goes through each age/sex/country row for a given risk factor
    for (i in 1:nsim2) { 
      # This loop will be repeated for the length of age/sex subgroups for each risk factor
      if(!is.na(data1$sd[i]))
      {
  
        if(identical(covar.vec, c("Age", "Sex", "Race")))
        {
          subset.rr = subset(rr, rr$age == data1$age[i] & rr$female==data1$female[i] & rr$race == data1$race[i])#####gms042414--add race strata here
        }                                                                                                   #<-should add [i] here?
                                                                                                       #<-should add [i] here? probably yes

#        print(i)
      #  print(subset.rr)
        # subset.rr will be the rr spesific to the age/sex group 
        beta<-list()
        mort.samp<-list()
        
       mm1 = rnorm(n=nsim1, mean = data1$Mean[i], sd= data1$se[i])
       mm2 = rnorm(n=nsim1, mean=data1$Mean_alt[i], sd=data1$se.alt[i])
      #mm1 =rep(data1$Mean[i], nsim1)
      #mm2 =rep(data1$Mean_alt[i], nsim1)
        #get prevalence of BMI > 25
        overweight.prev<-rnorm(n=nsim1, mean=data01$overweight_rate[i], sd=data01$overweight_rate_se[i])/100
        overweight.prev[overweight.prev<0]<-0
        overweight.prev[overweight.prev>1]<-1
        food.to.bmi.effect<-beta.lin.high*overweight.prev+beta.lin.low*(1-overweight.prev)
        
        ## define the variables necessary for modifying the linear effect of Na on BP (age, rage, htn..)
        gender = data01$female[i]+1 ##gender = female + 1 in this hypertension model
        agemid<-data01$age.mid[i] ##agemid predefined in input data
        #We now use hyptertension prevalence obtained from nhanes
        htn.prev <- rnorm(n=nsim1, mean=data01$hbp[i], sd=data01$hbp_se[i])
        htn.prev[htn.prev<0]<-0
        htn.prev[htn.prev>1]<-1
        
        #similarly, we use black prevlance obtained from nhanes
        
        #black <- rnorm(n=nsim1, mean=data01$nhb[i], sd=data01$nhb_se[i])
        #black[black<0]<-0
        #black[black>1]<-1
        black<-rep(as.numeric(data01$race[i]==2), nsim1)
        
        ## use proportion of high sbp (over 115) to get overall sodium to sbp effect for each subpopulation
        high.SBP.prev<-rnorm(n=nsim1, mean=data01$highSBP_RATE[i], sd=data01$highSBP_RATE_se[i])/100
        high.SBP.prev[high.SBP.prev<0]<-0
        high.SBP.prev[high.SBP.prev>1]<-1
        
        ## simulating and sample logRRs
        for(j in 1:num.diseases)
        {
          

          beta[[j]]<-rnorm(n=nsim1, mean=subset.rr[[diseases.vec[j]]], sd=subset.rr[[diseases.vec.se[j]]])

          samp_beta[[j]]<-sample(beta[[j]], nsim1)
       ##################################################################
          
          if(agemid>70)
          {
            adjeffect.a[agemid>70] <- (70-50)*age_effect ##for people over age 70, we use the same effect as people at age 70.
            ##In other words, the relationhip between age and linear effect of Na on BP is assumed to be flat after age 70. 
          }else{
            adjeffect.a <- maineffect + (agemid-50)*age_effect
          }

          # race effect 
          adjeffect.a.r = adjeffect.a + race_effect*black
          
          # effect of hypertension (weight by proportion hypertensive)
          adjeffect.a.r.h = htn.prev*(adjeffect.a.r + htneffect) + (1-htn.prev)*(adjeffect.a.r)
          
          food.to.sbp.effect<-adjeffect.a.r.h*high.SBP.prev #+0*(1-high.sbp.prev)
          ##This seems weird but in fact, similar to the original version.
          #in original, we calculate adjusted effect based on race/hyptertension prevelance, etc...
          #But then only use this effect if mean_sbp > 115. If mean_sbp < 115, we have no effect at all.
          #instead of doing this, here, we're going to be a little less crude and multiply effect by proportion above 115.
          #So borderline group will only get half the effect instead of all or none, etc...
          
          
          #mm1 = rnorm(n=nsim1, mean = data1$mean[i], sd= data1$se[i]) ##move this line up, no reason do this within j loop
          
          
          ###Get and save, observed mort draws
          subset.observed.mort.draws<-subset(observed.mort.draws, observed.mort.draws$disease==diseases.vec[j]  & observed.mort.draws$age == data1$age[i] 
                                             & observed.mort.draws$female==data1$female[i] & observed.mort.draws$race == data1$race[i])
          
          observed.mort.draws.start.point<-which(names(subset.observed.mort.draws)=="1")
          observed.mort.draws.end.point<-which(names(subset.observed.mort.draws)==paste(nsim1))
          observed.mort[[j]][i,]<-t(subset.observed.mort.draws[,observed.mort.draws.start.point:observed.mort.draws.end.point])
          
          for (k in 1:nsim1){      ####gms1 Start Simulation Loop
            
            
            mm<-(mm1[k])                ## the mean for each age/sex group
            mm.alt<-(mm2[k])

            sdd = data1$sd[i]    ####gms         ## *** ** Specific for each RF****  data from mean to sd regression based on all global veg. data (see file "mean to sd reg coeff.xls"
            sdd.alt = data1$sd.alt[i]

            total.count<-total.count+1
            if(mm<=0) 
            {
              mm<-0.001   ##can't have negative values or 0 values for shape and rate so in this situatin we set mean to near 0
              count.mm.tozero<-count.mm.tozero+1
            }   
            #count.mm.alt.tozero<-0
            if(mm.alt<=0) 
            {
              mm.alt<-0.001   ##can't have negative values or 0 values for shape and rate so in this situatin we set mean to near 0
              sdd.alt = sdd*mm.alt/mm
              count.mm.alt.tozero<-count.mm.alt.tozero+1
            }  
            

            q<-seq(-6,6,by=.1)                 ## generate the "slices" of standard normal distribution between -6 and 6 by 0.1 increments -- there are 121
            mtc<-rep(mm, times=121)            ## vector with 121 repetitions of mean for the age/sex group of interest (121 = length of q)
            tmmtc<-rep(mu_tmrd, times=121)     ## vector with 121 repetitions of the theoretical minimum for mean
            #qsx<-sdd*q                         ## vector multiplying sd for the age/sex group of interest by each "slice"
            #x=(mtc+(qsx))                      ## vector summing 121 repetitions of mean with the "sliced" sd
            qsy<-sd_tmrd*q                     ## vector multiplying theoretical minimum sd by slices
            y<-(tmmtc+(qsy))                   ## vector summing 121 repetitions of theoretical minimum mean with the "sliced" t.m. sd
            
            #x<-qgamma(pnorm(y, mean=mu_tmrd, sd=sd_tmrd), 
            #       shape=shape, rate=rate)
            
            #x<-qgamma(pnorm(mtc+(sdd*q), mean=mm, sd=sdd), 
            #          shape=shape, rate=rate)     
            shape<-mm^2/sdd^2
            rate<-mm/sdd^2
            shape.alt<-mm.alt^2/sdd.alt^2
            rate.alt<-mm.alt/sdd.alt^2
            
            x<-qgamma(pnorm(q, mean=0, sd=1), 
                      shape=shape, rate=rate)
            x.alt<-qgamma(pnorm(q, mean=0, sd=1), 
                          shape=shape.alt, rate=rate.alt)
            if(rfvec[r]%in% c("BMI","BP")){ 
            x<-qnorm(pnorm(q, mean=0, sd=1), 
                      mean=mm, sd=sdd)       
            x.alt<-qnorm(pnorm(q, mean=0, sd=1), 
                     mean=mm.alt, sd=sdd)   
            }
            if(is.na(sum(x.alt)))
            {
              print(mm)
              print(sdd)
              print(mm.alt)
              print(sdd.alt)
            }
            #print(head(x))
            #print(shape)
            #print(rate)
            # cat("\n")
            #This will get you the same x values as line commented out above using normal tmred distribution or the
            ##incorrect "current" distribution assuming normality.
            ## Unless a proper tmred distribution exists (which doesn't for pmeat and others where
            ##0 sd is assumed), I can't use "y" to define x so I use q instead (it's all the same anyway)
            
            #                                         delat<-(y-x)/subset.rr$RRunit                              
            #                                         print(i)
            #                                         print(k)
            #                                         print(sdd)
            #                                         if(is.na(mm))
            #                                           stop()
            #p.pp<-dnorm(x, mean=mm, sd =sdd, log = FALSE)*(.1*sdd)
            #p.pp<-pgamma(x, shape=shape, rate=rate)
            x.shifted<-c(0, x[-length(x)]) ##same as x but lowest value is 0 and highest value of x is omitted
            x.shifted.alt<-c(0, x.alt[-length(x.alt)]) ##same as x but lowest value is 0 and highest value of x is omitted
            
            ##this is used to get probability between points in x-values
            p.pp<-pgamma(x, shape=shape, rate=rate)-pgamma(x.shifted, shape=shape, rate=rate)
            ## determine the prevalence for each slice of each age-sex group: multiply the density of a normal distribution for each "slice"*sd
            p.pp.alt<-pgamma(x.alt, shape=shape.alt, rate=rate.alt)-pgamma(x.shifted.alt, shape=shape.alt, rate=rate.alt)
            
            
            #x[x<0]<-0
            
            if(j %in% grep(diseases.vec, pattern="medBMI"))
            {
              #defining delat and defining rr.list[[j]] are the two things that are different for mediated effects and direct effects.
              
              ##For delat, the key difference is that  RRunit is not the same, since this is the effect on risk per unit of BMI, in this case 5 kg/m^2
              ##Actually, the way we do it now, the food.to.bmi.effect does not have a fixed unit, so 
              ##we divide by RRunit, as well as 5kg/m^2. e.g: if food.to.BMI effect is 100(g/d)(kg/m^2), and delta is in g,
              ##then you want have (g/d)/(5kg/m^2)*(kg/m^2 / 100 g/d)
              delat[[j]]<-(x-y)/5/subset.rr$RRunit
              delat.alt[[j]]<-(x.alt-y)/5/subset.rr$RRunit
              
              rr.list[[j]]<-exp(delat[[j]]*samp_beta[[j]][k]*food.to.bmi.effect[k]) 
              rr.list.alt[[j]]<-exp(delat.alt[[j]]*samp_beta[[j]][k]*food.to.bmi.effect[k]) 
              
            }else if(j %in% grep(diseases.vec, pattern="medSBP")){
              
              delat[[j]]<-(x-y)/10/subset.rr$RRunit
              delat.alt[[j]]<-(x.alt-y)/10/subset.rr$RRunit
              
              rr.list[[j]]<-exp(delat[[j]]*samp_beta[[j]][k]*food.to.sbp.effect[k]) 
              rr.list.alt[[j]]<-exp(delat.alt[[j]]*samp_beta[[j]][k]*food.to.sbp.effect[k]) 
            }else
            {
             delat[[j]]<-(x-y)/subset.rr$RRunit       ####gms          ## delta is the difference between the actual and theoretical minimum distributions  ######### divide this by RR units if it is not 1.     ## change to y-x for harmful risks; x-y is for protective risks ##Or is it x-y for harmful risks and x-y for protectve risks???
             delat.alt[[j]]<-(x.alt-y)/subset.rr$RRunit
              #cat("delat when we define it: ", delat, "\n\n")
              rr.list[[j]]<-exp(delat[[j]]*samp_beta[[j]][k])
              rr.list.alt[[j]]<-exp(delat.alt[[j]]*samp_beta[[j]][k])
              #cat("delat: ", delat, "\n")
              #                                         rr.ihd [delat>0]<-1     ## set attributable risk to 1 if actual exposure is lower than theoretical minimum 
            }
            #rr.list[[j]][delat[[j]]<0]<-1
            rr.list[[j]][rr.list[[j]]<1]<-1
            rr.list.alt[[j]][rr.list.alt[[j]]<1]<-1
            ##when using rr.list instead of delat to define what to force to 0. The ifelse statement is not necessary,
#            if(diseases.vec[j]=="SC" & rfvec[r]=="sodium")
#            {
#              cat("delat[[j]]: ", delat[[j]], "\n")
#              cat("rr.list[[j]]: ", rr.list[[j]], "\n")  
#            }
            
            #p.rr[[j]]<-p.pp*(rr.list[[j]]-1)                ## numerator of the paf calc for each "slice"
            #p[[j]]<-sum(p.rr[[j]], na.rm=TRUE)         ## sum the numerator vector across all "slices"
            #paf[[j]][i,k]<- p[[j]]/(p[[j]]+1)           ## actual paf calculation, generates a matrix of i rows and k columns
            #                          ihd.mort = rnorm(n=nsim1, mean = data1$ihdmort[i], sd = ((data1$uciihdmort[i]-data1$lciihdmort[i])/3.92))  ####gms create mortality distribution
            #                          ihd.mort.samp = sample(ihd.mort, 1)            ####gms sample from mortality distribution
            #mort.samp[[j]]<-rnorm(n=1, mean=data1[,colnames(data1)==diseases.vec.mn[j]][i], sd=data1[,colnames(data1)==diseases.vec.se[j]][i])       #$IHD[i]     ## We don't need to sample.simulate mortality here since we know the exact number of deaths
            #observed.mort[[j]][i,k]<-rnorm(n=1, mean=data1[,colnames(data1)==diseases.vec.mn[j]][i], sd=data1[,colnames(data1)==diseases.vec.se[j]][i])
            ## Instead we just use mortality count for this particular strata
            #cat("p[[j]][i,k]: ", p.rr[[j]], "\n")
            #cat("paf[[j]][i,k]: ", paf[[j]][i,k], "\n")
            #cat("mort.samp[[j]]: ", mort.samp[[j]], "\n")
            
            p.rr[[j]]<-p.pp*rr.list[[j]]
            p.rr.alt[[j]]<-p.pp.alt*rr.list.alt[[j]]
            p[[j]]<-sum(p.rr[[j]], na.rm=TRUE) 
            p.alt[[j]]<-sum(p.rr.alt[[j]], na.rm=TRUE) 
            pif[[j]][i,k]<-(p[[j]]-p.alt[[j]])/p[[j]]
            
            if(p[[j]]==0)
              pif[[j]][i,k]<-0
            
            ### samp_beta is the effect of food on risk per dose for this particular iteration of the simulation
            ### if it's zero, that means there is no effect in changing food distribution, so the PIF is 0
            ### By jUst just setting it to 0, we can avoid complications
            if(samp_beta[[j]][k]==0) 
              pif[[j]][i,k]<-0
            
            ##If counterfactual is more harmful than current exposure distribution, then we can get negative numberers for PIF. 
            ##Just set it to zero in that case
          ##  if(pif[[j]][i,k]<0 & !rfvec[r] %in% c("nuts","seafood"))
            
          ##  pif[[j]][i,k]<-0
            
            if(is.na(pif[[j]][i,k]))
            {
              print(r)
              print(i)
              print(j)
              print(k)
              cat("pif[[j]][i,k]", pif[[j]][i,k], "\n")
              cat("p[j]", "\n")
              print(p[j])
              cat("p.alt[j]", "\n")
              print(p.alt[j])
              cat("mm", mm, "\n")
              cat("sdd", sdd, "\n")
              cat("mm.alt", mm.alt, "\n")
              cat("sdd.alt", sdd.alt, "\n")
              cat("rr.list[[j]]", rr.list[[j]], "\n")
              cat("rr.list.alt[[j]]", rr.list.alt[[j]], "\n")
              cat("delat[[j]]", delat[[j]], "\n")
              cat("delat.alt[[j]]", delat.alt[[j]], "\n")
              cat("subset.rr$RRunit", subset.rr$RRunit, "\n")
              cat("x", x, "\n")
              cat("x.alt", x.alt, "\n")
              cat("y", y, "\n")
            }
            
            mort[[j]][i,k] = pif[[j]][i,k]*observed.mort[[j]][i,k]     ####gms multiply by the corresponding ith sampled mortality here
            
          }
          #print(count)
        }
        
        
        ####gms1 End Simulation Loop  (nsim1)
      }    ####     End if statement on whether sample size is greater than one for this group.
    }     ####gms  End Age/sex/country loop (nsim2)
    
    #PAF.list[[r]]<-paf
    PIF.list[[r]]<-pif
  }     ####gms End File loop (nsim3)
  ### Here we want to put the paf and mort matrices for each outcome into one giant matrix for each risk factor that can be used for all further calculations(collapsing data, etc)
  ### make each outcome's matrix of mortalities or pafs into a data frame and label the columns with draw_"num"
  ### then merge this with a segment of the exposure/mortality dataset for  identification and further calculations
  ### merge also with population data for further calculations
  ### add a column with the outcome name
  ### then rbind all the individual outcome dataframes together
  
  #ccodes = read.csv(paste(CCDir,"countryregioncodesincome.csv", sep=""))
  #ccodes = ccodes[,c("iso3", "wbincome")]
  #print("error here?")
  a = paste("mortdraw.", seq(1, nsim1, by=1), sep='')

  if(identical(covar.vec, c("Age", "Sex", "Race")))
    id = data1[,c("age", "female", "race", "Mean", "se", "sd")]#,
  #                     "ihdmort", "lciihdmort" "uciihdmort", 
  #                     "istkmort", "lciistkmort", "uciistkmort", 
  #                     "hstkmort", "lcihstkmort", "ucihstkmort", 
  #                     "diabmort", "lcidiabmort", "ucidiabmort")]
  #id = merge(id, ccodes, by = "iso3")
  
  mort.df<-list()
  mort.df2<-list()
  
  PIF.df<-list()
  PIF.df2<-list()
  
#  observed.mort.df<-list()
#  observed.mort.df2<-list()
  #print("error here?2")
  
  for(ii in 1:num.diseases)
  {
    mort.df[[ii]]<-as.data.frame(mort[[ii]])
    mort.df[[ii]]$outcome<-diseases.vec[[ii]]
    mort.df2[[ii]]<-cbind(id, mort.df[[ii]])
    
    PIF.df[[ii]]<-as.data.frame(PIF.list[[r]][[ii]])
    PIF.df[[ii]]$outcome<-diseases.vec[[ii]]
    PIF.df2[[ii]]<-cbind(id, PIF.df[[ii]])
    
 #   observed.mort.df[[ii]]<-as.data.frame(observed.mort[[ii]])
#    observed.mort.df[[ii]]$outcome<-diseases.vec[[ii]]
#    observed.mort.df2[[ii]]<-cbind(id, observed.mort.df[[ii]])
  }
  
  # } # stop here for testing purposes     ############################
  
  
  # this is the dataframe containing all the draws for each outcome, as well as the identifiying info such as iso3, age, sex, etc.)
  #print("error here?3")
  
  allmort<-mort.df2[[1]]
  allPIF<-PIF.df2[[1]]
#  allobservedmort<-observed.mort.df2[[1]]
  for(jj in 2:num.diseases)
  {
    allmort<-rbind(allmort, mort.df2[[jj]])
    allPIF<-rbind(allPIF, PIF.df2[[jj]])
#    allobservedmort<-rbind(allobservedmort, observed.mort.df2[[jj]])
  }
  allmort$riskfactor = rfvec[r]
  allPIF$riskfactor = rfvec[r]
#  allobservedmort$riskfactor = rfvec[r]
  
  allmort<-as.data.frame(allmort)
  
  #mediated<-grep("_med", allmort$outcome)
  n<-dim(allmort)[1]/(n.mediated.effects+1)
  

  ## output this file, so it can be read in again later without re-running the whole simulation
  mort.list[[r]]<-allmort
  PIFs[[r]]<-allPIF
#  observed.mort.list[[r]]<-allobservedmort
  #write.csv(allmort, paste(ResultsDir,file=paste(rfvec[r], "_mortdraws_AgeSex_2010.csv",sep='')), row.names=F)
  
} ####gms End risk factor loop     

all.mortdraws<-mort.list[[1]]
all.PIFs<-PIFs[[1]]
#all.observed.mort<-observed.mort.list[[1]]

if (r>1){
for(r in 2:length(rfvec))
{
  all.mortdraws<-rbind(all.mortdraws, mort.list[[r]])
  all.PIFs<-rbind(all.PIFs, PIFs[[r]])
#  all.observed.mort<-rbind(all.observed.mort, observed.mort.list[[r]])
}
}
names(all.mortdraws)[(1+length(covar.vec)):(3+length(covar.vec))]<-c("mean (food)", "se (food)", "sd (food)")
names(all.PIFs)[(1+length(covar.vec)):(3+length(covar.vec))]<-c("mean (food)", "se (food)", "sd (food)")
#names(all.observed.mort)[(1+length(covar.vec)):(3+length(covar.vec))]<-c("mean (food)", "se (food)", "sd (food)")


write.csv(x=all.mortdraws, file=paste("Outputs/all.mort.draws", year.vec.string, covar.vec.string, nsim1, ".csv", sep="_"), row.names=FALSE)
write.csv(x=all.PIFs, file=paste("Outputs/all.PIFs", year.vec.string, covar.vec.string, nsim1, ".csv", sep="_"), row.names=FALSE)
#write.csv(x=all.observed.mort, file=paste("all.observed.incidence.draws_", year.vec.string, "_", covar.vec.string, ".csv", sep=""), row.names=FALSE)



#rm(all.mortdraws, all.PIFs)
