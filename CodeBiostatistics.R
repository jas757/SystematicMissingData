set.seed(1)
nhanes <- read.csv("nhanes_cleaned.csv", sep=" ")
plco <- read.csv("plco.csv")[, c("plco_id", "arm", "lung_cancer", 
  "entryage_bq", "bmi_curr", "race7", "educat", "cig_stat", "cig_years", 
  "cig_stop", "smokea_f", "cigpd_f", "sex", "marital", "pack_years",  
  "diabetes_f", "emphys_f", "hearta_f", "hyperten_f", "stroke_f", "lung_exitdays")]

nlst <- read.csv("nlst.csv")[, c("proclc", "rndgroup", "can_scr", "canc_rpt_link", "conflc", "candx_days", "age", 
  "height", "weight", "race", "ethnic", "educat", "cigsmok", "smokeyr", "age_quit", "smokeage", 
  "smokeday", "gender", "marital", "pkyr", "smokelive", "diagadas", "diagdiab", "diagemph", 
  "diaghear", "diaghype", "diagstro")]

######### ADD WEIGHTS TO nhanes data
#nhanes$weights = NA
#for(i in 1:nrow(nhanes)){
#col.eq = rep(0, nrow(nhanes.1))
#for(j in 1:nrow(nhanes.1)){
#col.eq[j] = sum(nhanes[i, 1:7] == nhanes.1[j, 1:7]) == 7
#}
#sum(which(col.eq ==1))
#nhanes$weights[i] = nhanes.1$weights[which(col.eq ==1)]
#}

# Calcaulate smokeday 
nhanes$smokeday = nhanes$pkyr * 20/nhanes$smokeyr

##### organize categorical variables #####
##### race: black, white, hispanic, other #####
nhanes$race <- nhanes$race_ethnic

plco$race[which(plco$race7==1)] <- "white"
plco$race[which(plco$race7==2)] <- "black"
plco$race[which(plco$race7==3)] <- "hispanic"
plco$race[which(plco$race7%in%c(4:6))] <- "other"
plco$race[which(plco$race7==7)] <- NA

nlst$race1 <- nlst$race
nlst$race[which(nlst$ethnic > 1 & nlst$race1 == 1)] <- "white"
nlst$race[which(nlst$ethnic > 1 & nlst$race1 == 2)] <- "black"
nlst$race[which(nlst$ethnic == 1 & nlst$race1 <= 7)] <- "hispanic"
nlst$race[which(nlst$ethnic > 1 & nlst$race1 %in% c(3:6))] <- "other"
nlst$race[which((nlst$ethnic == 1 & nlst$race1 > 7) | (nlst$ethnic > 1 & nlst$race1 >=7))] <- NA

##### educat: 1.less than high school, 2.high school graduate, 3.some college, 4.college graduate #####
plco$educat2 <- plco$educat
plco$educat2[which(plco$educat %in% c(1:2))] <- 1
plco$educat2[which(plco$educat %in% c(3:4))] <- 2
plco$educat2[which(plco$educat == 5)] <- 3
plco$educat2[which(plco$educat %in% c(6,7))] <- 3

nlst$educat2 <- nlst$educat
nlst$educat2[which(nlst$educat %in% c(1:2))] <- 1
nlst$educat2[which(nlst$educat %in% c(3:4))] <- 2
nlst$educat2[which(nlst$educat == 5)] <- 3
nlst$educat2[which(nlst$educat %in% c(6:7))] <- 3
nlst$educat2[which(nlst$educat > 7)] <-NA

##### cigsmoke: 0 for former; 1 for current #####
plco$cigsmok <- plco$cig_stat
plco$cigsmok[which(plco$cig_stat == 2)] <- 0
plco$cigsmok[which(plco$cig_stat == 1)] <- 1
plco$cigsmok[which(plco$cig_stat == 0)] <- NA

##### smokeday: 0=0; 1=1-10; 2=11-20; 3=21-30; 4=31-40; 5=41:60; 6=61:80; 7=81+  #####
library(dplyr)
nhanes$cigpd_f <- case_when(
  nhanes$smokeday == 0 ~ 0,
  ((nhanes$smokeday >= 1) & (nhanes$smokeday <= 10)) ~ 1,
  ((nhanes$smokeday >= 11) & (nhanes$smokeday <= 20)) ~ 2,
  ((nhanes$smokeday >= 21) & (nhanes$smokeday <= 30)) ~ 3,
  ((nhanes$smokeday >= 31) & (nhanes$smokeday <= 40)) ~ 4,
  ((nhanes$smokeday >= 41) & (nhanes$smokeday <= 60)) ~ 5,
  ((nhanes$smokeday >= 61) & (nhanes$smokeday <= 80)) ~ 6,
  (nhanes$smokeday >= 81) ~ 7,
)


library(dplyr)
nlst$cigpd_f <- case_when(
  nlst$smokeday == 0 ~ 0,
  ((nlst$smokeday >= 1) & (nlst$smokeday <= 10)) ~ 1,
  ((nlst$smokeday >= 11) & (nlst$smokeday <= 20)) ~ 2,
  ((nlst$smokeday >= 21) & (nlst$smokeday <= 30)) ~ 3,
  ((nlst$smokeday >= 31) & (nlst$smokeday <= 40)) ~ 4,
  ((nlst$smokeday >= 41) & (nlst$smokeday <= 60)) ~ 5,
  ((nlst$smokeday >= 61) & (nlst$smokeday <= 80)) ~ 6,
  (nlst$smokeday >= 81) ~ 7,
)


##### marital: 1 for marrided or living as married; 2 for other #####
plco$marital[which(plco$marital>1)] <- 2

nlst$marital1 <- nlst$marital
nlst$marital[which(nlst$marital1==2)] <- 1
nlst$marital[which(nlst$marital1 %in% c(1, 3:5))] <-2
nlst$marital[which(nlst$marital>5)] <- NA

nhanes$marital = 1- nhanes$marital

##### gender: 1 for male; 2 for female #####
plco$gender <- plco$sex
nhanes$gender = 1 - nhanes$gender

##### lung cancer #####
nlst$lung_cancer <- nlst$can_scr
nlst$lung_cancer[which(nlst$can_scr %in% c(1,2,3))] <- 1
nlst$lung_cancer[which(nlst$can_scr %in% c(4))] <- 1
# Making sure us within six years
nlst$lung_cancer[which(nlst$candx_days > 365 * 6)] = 0
plco$lung_cancer[which(nlst$lung_exitdays > 365 * 6)] = 0

##### organize continuous variable #####
##### bmi #####
plco$bmi <- plco$bmi_curr

nlst$bmi <- 703*(nlst$weight/(nlst$height^2))

##### quit_time #####
plco$quit_time <- plco$cig_stop

nlst$quit_time <- nlst$age-nlst$age_quit

# Hypertension
nhanes$diaghype = 1 - nhanes$diaghype

##### assimilate variable name #####
nlst$arm <- nlst$rndgroup
plco$age <- plco$entryage_bq
plco$smokeyr <- plco$cig_years
plco$smokeage <- plco$smokea_f
plco$pkyr <- plco$pack_years
plco$diagdiab <- plco$diabetes_f
plco$diagemph <- plco$emphys_f
plco$diaghear <- plco$hearta_f
plco$diaghype <- plco$hyperten_f
plco$diagstro <- plco$stroke_f

##### arm #####
# Make X-ray be arm 1 for both trials
plco$arm = as.numeric(plco$arm==1)
nlst$arm = as.numeric(nlst$arm==2)

# Remove ineligible 
plco = plco[plco$age > 54, ]
nlst = nlst[nlst$age > 54, ]

# Make pack years comparable 
plco = plco[plco$pkyr > 29, ]
nhanes = nhanes[nhanes$pkyr > 29, ]
nlst = nlst[nlst$pkyr > 29, ]



##### finalize datasets #####
nhanes.plco <- nhanes[ , c("age", "bmi", "race", "educat2", "smokeyr",
 "gender", "marital", "pkyr", "smokelive", "diagadas", "diagdiab", 
  "diagemph", "diaghear", "diaghype", "cigpd_f", "weights")]

nhanes.nlst <- nhanes[ , c("age", "bmi", "race", "educat2", "smokeyr",
 "gender", "marital", "pkyr", "smokelive", "diagadas", "diagdiab", 
  "diagemph", "diaghear", "diaghype", "smokeday", "weights")]


  plco <- plco[ , c("arm", "lung_cancer", "age", "bmi", "race", "educat2", "smokeyr", 
     "gender", "marital", "pkyr", "diagdiab", "diagemph", "diaghear",
    "diaghype", "cigpd_f")]

  nlst <- nlst[ , c("arm", "lung_cancer", "age", "bmi", "race", "educat2", "smokeyr", 
    "gender", "marital", "pkyr", "smokelive", "diagadas", "diagdiab", "diagemph", 
    "diaghear", "diaghype", "smokeday")]

nhanes.plco <- na.omit(nhanes.plco)
nhanes.nlst <- na.omit(nhanes.nlst)
plco <- na.omit(plco)
nlst <- na.omit(nlst)
nhanes.plco$Sk <- 0
nhanes.nlst$Sk <- 0
plco$Sk <- 1
nlst$Sk <- 1
nlst$weights = 1
plco$weights = 1


nlst_final = nlst
plco_final = plco
nhanes_final.plco = nhanes.plco
nhanes_final.nlst = nhanes.nlst
# Make variables factors that are factors
nlst_final$educat2 = as.factor(nlst_final$educat2)
plco_final$educat2 = as.factor(plco_final$educat2)
nhanes_final.plco$educat2 = as.factor(nhanes_final.plco$educat2)
nhanes_final.nlst$educat2 = as.factor(nhanes_final.nlst$educat2)

# Already removed during data cleaning
#nlst_final$cigpd_f = as.factor(nlst_final$cigpd_f)
#plco_final$cigpd_f = as.factor(plco_final$cigpd_f)
#nhanes_final$cigpd_f = as.factor(nhanes_final$cigpd_f)

nlst_final$gender = as.factor(nlst_final$gender -1 )
plco_final$gender = as.factor(plco_final$gender -1 )
nhanes_final.plco$gender = as.factor(nhanes_final.plco$gender)
nhanes_final.nlst$gender = as.factor(nhanes_final.nlst$gender)

nlst_final$marital = as.factor(nlst_final$marital - 1)
plco_final$marital = as.factor(plco_final$marital - 1)
nhanes_final.plco$marital = as.factor(nhanes_final.plco$marital)
nhanes_final.nlst$marital = as.factor(nhanes_final.nlst$marital)

# Make hispanic and other the same racial variable
nlst_final$race[nlst_final$race  != "white"] = "other" 
nlst_final$race = as.factor(nlst_final$race)
nlst_final$race = droplevels(nlst_final$race)
nhanes_final.plco$race[nhanes_final.plco$race  != "white"] = "other" 
nhanes_final.plco$race = as.factor(nhanes_final.plco$race )
nhanes_final.plco$race = droplevels(nhanes_final.plco$race)
nhanes_final.nlst$race[nhanes_final.nlst$race  != "white"] = "other" 
nhanes_final.nlst$race = as.factor(nhanes_final.nlst$race)
nhanes_final.nlst$race = droplevels(nhanes_final.nlst$race)
plco_final$race[plco_final$race  != "white"] = "other" 
plco_final$race = as.factor(plco_final$race)
plco_final$race = droplevels(plco_final$race)

# Combine education category 3 and 4
nlst_final$educat2[nlst_final$educat2  == "4"] = "3" 
nlst_final$educat2 = droplevels(nlst_final$educat2)
nhanes_final.plco$educat2[nhanes_final.plco$educat2  == "4"] = "3" 
nhanes_final.plco$educat2 = droplevels(nhanes_final.plco$educat2)
nhanes_final.nlst$educat2[nhanes_final.nlst$educat2  == "4"] = "3" 
nhanes_final.nlst$educat2 = droplevels(nhanes_final.nlst$educat2)
plco_final$educat2[plco_final$educat2  == "4"] = "3" 
plco_final$educat2 = droplevels(plco_final$educat2)


# Standardize weights
nlst_final$weights = nlst_final$weights/mean(nlst_final$weights)
plco_final$weights = plco_final$weights/mean(plco_final$weights)
nhanes_final.plco$weights = nhanes_final.plco$weights/mean(nhanes_final.plco$weights)
nhanes_final.nlst$weights = nhanes_final.nlst$weights/mean(nhanes_final.nlst$weights)

# Selecting X-ray arm
plco_arm1 <- plco_final[plco_final$arm==1,]
nlst_arm1 <- nlst_final[nlst_final$arm==1,]

# Combinging datasets
whole_plco <- rbind(plco_arm1[ ,3:17], nhanes_final.plco[ ,c(1:8,11:17)]) 
whole_nlst <- rbind(nlst_arm1[ ,3:19], nhanes_final.nlst[, c(1:15,17,16)]) 



########################################################################################################################
####################   Estimation
########################################################################################################################

########################################################################################################################
####################   Outcome model
########################################################################################################################


# Compue the weights according to sample size
ak <- c(nrow(plco_arm1)/(nrow(plco_arm1)+nrow(nlst_arm1)), nrow(nlst_arm1)/(nrow(plco_arm1)+nrow(nlst_arm1)))

# Fit outcome model E[Y|A=a, X, S =s] for each trial
plco_arm1_1 = plco_arm1[, -1]
ga.fit1 <- glm(lung_cancer ~ .-weights-Sk, family="binomial", data=plco_arm1_1)
# Only fit it using data from the arm under consideration
nlst_arm1_1 = nlst_arm1[, -1]
ga.fit2 <- glm(lung_cancer ~ .-weights-Sk, family="binomial", data=nlst_arm1_1)

ga1 <- predict(ga.fit1, newdata=nhanes_final.plco, type = "response")
ga2 <- predict(ga.fit2, newdata=nhanes_final.nlst, type = "response")


out1 = mean(ga1 * nhanes_final.plco$weights)
out2 = mean(ga2 * nhanes_final.nlst$weights)
out <- sum(ak*c(mean(ga1 * nhanes_final.plco$weights), mean(ga2 * nhanes_final.nlst$weights)))


########################################################################################################################
####################   IOW estimator
########################################################################################################################



hat_p_x.fit1 <- glm(Sk ~ .-gender-weights, family="binomial", data=whole_plco, weights = weights) 

hat_p_x.fit2 <- glm(Sk ~ .-weights, family="binomial", data=whole_nlst, weights = weights) 

hat_p_x1 <- predict(hat_p_x.fit1, newdata=plco_arm1, type="response")

hat_p_x2 <- predict(hat_p_x.fit2, newdata=nlst_arm1, type="response")

ea.fit1 <- glm(arm ~ .-Sk - weights, family="binomial", data=plco_final) 

ea.fit2 <- glm(arm ~ .- Sk - weights, family="binomial", data=nlst_final) 

ea1 <- predict(ea.fit1, newdata=plco_arm1, type="response")

ea2 <- predict(ea.fit2, newdata=nlst_arm1, type="response")

weights.plco = (1-hat_p_x1)/(ea1*hat_p_x1)
weights.nlst = (1-hat_p_x2)/(ea2*hat_p_x2)
IPW1 <- sum((plco_arm1$lung_cancer)*(1-hat_p_x1)/(ea1*hat_p_x1))/sum(weights.plco)
IPW2 <- sum((nlst_arm1$lung_cancer)*(1-hat_p_x2)/(ea2*hat_p_x2))/sum(weights.nlst)
IPW <- sum(ak*c(IPW1, IPW2))


########################################################################################################################
####################   DR estimator
########################################################################################################################

ga11 <- predict(ga.fit1, newdata=plco_arm1, type = "response")
ga22 <- predict(ga.fit2, newdata=nlst_arm1, type = "response")
DR1 <- sum((plco_arm1$lung_cancer-ga11)*(1-hat_p_x1)/(ea1*hat_p_x1))/sum(weights.plco)+mean(ga1 * nhanes_final.plco$weights)
DR2 <- sum((nlst_arm1$lung_cancer-ga22)*(1-hat_p_x2)/(ea2*hat_p_x2))/sum(weights.nlst)+mean(ga2 * nhanes_final.nlst$weights)
DR <- sum(ak*c(DR1, DR2))

pool.est = mean(c(plco_arm1$lung_cancer, nlst_arm1$lung_cancer))

####################################################################################################################################################################################
#########   Boostrapping
####################################################################################################################################################################################

n.boot = 1000
dr.boot = rep(NA, n.boot)
ipw.boot = rep(NA, n.boot)
out.boot = rep(NA, n.boot)
pool.boot = rep(NA, n.boot)

# Create strate for NHANES

for(k in 1:n.boot){

# Sample at the PSU and strate levels
# Start by sampling 
boot.used.nhanes.step.1 = sample(1:2, 2, replace = TRUE)

# For the first PSU
# Indicator if in step one sample
obs.step.one = nhanes$sdmvpsu %in% boot.used.nhanes.step.1[1]
# Which observations in the original data made it into the steo one sample
ind.step.one = which(nhanes$sdmvpsu %in% boot.used.nhanes.step.1[1])
# Restrict only to step 1 observations
strata.step.2 = nhanes$sdmvstra[obs.step.one]
# Do sampling at strata level
obs.used = c()
# Cycle through strata
for(l in 1: length(unique(strata.step.2))){
 # What observations fall in that strata
 ob.sample.st = which(strata.step.2 == unique(strata.step.2)[l])
 # Do the resampling
 obs.used = c(obs.used, sample(ob.sample.st, length(ob.sample.st), replace = TRUE))
}
# Get back to the original observations level
obs.sampled.psu.1 = ind.step.one[obs.used]

# Make sure the sampling worked
#length(unique(nhanes$sdmvpsu[obs.sampled.psu.1]))
#table(nhanes$sdmvstra[obs.sampled.psu.1])
#table(nhanes$sdmvstra[nhanes$sdmvpsu == boot.used.nhanes.step.1[1]])


# For the second PSU
# Indicator if in step one sample
obs.step.one = nhanes$sdmvpsu %in% boot.used.nhanes.step.1[2]
# Which observations in the original data made it into the steo one sample
ind.step.one = which(nhanes$sdmvpsu %in% boot.used.nhanes.step.1[2])
# Restrict only to step 1 observations
strata.step.2 = nhanes$sdmvstra[obs.step.one]
# Do sampling at strata level
obs.used = c()
# Cycle through strata
for(l in 1: length(unique(strata.step.2))){
 # What observations fall in that strata
 ob.sample.st = which(strata.step.2 == unique(strata.step.2)[l])
 # Do the resampling
 obs.used = c(obs.used, sample(ob.sample.st, length(ob.sample.st), replace = TRUE))
}
# Get back to the original observations level
obs.sampled.psu.2 = ind.step.one[obs.used]

# Make sure the sampling worked
#length(unique(nhanes$sdmvpsu[obs.sampled.psu.2]))
#table(nhanes$sdmvstra[obs.sampled.psu.2])
#table(nhanes$sdmvstra[nhanes$sdmvpsu == boot.used.nhanes.step.1[2]])

obs.boot = c(obs.sampled.psu.1, obs.sampled.psu.2)

nhanes.boot = nhanes[obs.boot, ]

nhanes_final.plco.boot = nhanes_final.plco[obs.boot, ]
nhanes_final.nlst.boot = nhanes_final.nlst[obs.boot, ]

# Combinging datasets
whole_plco.boot <- rbind(plco_arm1[ ,3:17], nhanes_final.plco.boot[ ,c(1:8,11:17)]) 
whole_nlst.boot <- rbind(nlst_arm1[ ,3:19], nhanes_final.nlst.boot[, c(1:15,17,16)]) 



########################################################################################################################
####################   Estimation
########################################################################################################################

########################################################################################################################
####################   Outcome model
########################################################################################################################


# Compue the weights according to sample size
ak <- c(nrow(plco_arm1)/(nrow(plco_arm1)+nrow(nlst_arm1)), nrow(nlst_arm1)/(nrow(plco_arm1)+nrow(nlst_arm1)))

# Fit outcome model E[Y|A=a, X, S =s] for each trial
plco_arm1_1 = plco_arm1[, -1]
ga.fit1 <- glm(lung_cancer ~ .-weights-Sk, family="binomial", data=plco_arm1_1)
# Only fit it using data from the arm under consideration
nlst_arm1_1 = nlst_arm1[, -1]
ga.fit2 <- glm(lung_cancer ~ .-weights-Sk, family="binomial", data=nlst_arm1_1)

ga1 <- predict(ga.fit1, newdata=nhanes_final.plco.boot, type = "response")
ga2 <- predict(ga.fit2, newdata=nhanes_final.nlst.boot, type = "response")


out1 = mean(ga1 * nhanes_final.plco.boot$weights)
out2 = mean(ga2 * nhanes_final.nlst.boot$weights)
out.boot[k] <- sum(ak*c(mean(ga1 * nhanes_final.plco.boot$weights), mean(ga2 * nhanes_final.nlst.boot$weights)))


########################################################################################################################
####################   IOW estimator
########################################################################################################################



hat_p_x.fit1 <- glm(Sk ~ .-gender-weights, family="binomial", data=whole_plco.boot, weights = weights) 

hat_p_x.fit2 <- glm(Sk ~ .-weights, family="binomial", data=whole_nlst.boot, weights = weights) 

hat_p_x1 <- predict(hat_p_x.fit1, newdata=plco_arm1, type="response")

hat_p_x2 <- predict(hat_p_x.fit2, newdata=nlst_arm1, type="response")

ea.fit1 <- glm(arm ~ .-Sk - weights, family="binomial", data=plco_final) 

ea.fit2 <- glm(arm ~ .- Sk - weights, family="binomial", data=nlst_final) 

ea1 <- predict(ea.fit1, newdata=plco_arm1, type="response")

ea2 <- predict(ea.fit2, newdata=nlst_arm1, type="response")

weights.plco = (1-hat_p_x1)/(ea1*hat_p_x1)
weights.nlst = (1-hat_p_x2)/(ea2*hat_p_x2)
IPW1 <- sum((plco_arm1$lung_cancer)*(1-hat_p_x1)/(ea1*hat_p_x1))/sum(weights.plco)
IPW2 <- sum((nlst_arm1$lung_cancer)*(1-hat_p_x2)/(ea2*hat_p_x2))/sum(weights.nlst)
ipw.boot[k] <- sum(ak*c(IPW1, IPW2))


########################################################################################################################
####################   DR estimator
########################################################################################################################

ga11 <- predict(ga.fit1, newdata=plco_arm1, type = "response")
ga22 <- predict(ga.fit2, newdata=nlst_arm1, type = "response")
DR1 <- sum((plco_arm1$lung_cancer-ga11)*(1-hat_p_x1)/(ea1*hat_p_x1))/sum(weights.plco)+mean(ga1 * nhanes_final.plco.boot$weights)
DR2 <- sum((nlst_arm1$lung_cancer-ga22)*(1-hat_p_x2)/(ea2*hat_p_x2))/sum(weights.nlst)+mean(ga2 * nhanes_final.nlst.boot$weights)
dr.boot[k] <- sum(ak*c(DR1, DR2))

out.used = c(nlst_arm1$lung_cancer, plco_arm1$lung_cancer)
obs.pool.boot = sample(1:length(out.used), length(out.used), replace = TRUE)

pool.boot[k] = mean(out.used[obs.pool.boot])
}
sd(out.boot)
sd(ipw.boot)
sd(dr.boot)
sd(pool.boot)
c(out - sd(out.boot) * 1.96, out + sd(out.boot) * 1.96)
c(DR - sd(dr.boot) * 1.96, DR + sd(dr.boot) * 1.96)
c(IPW - sd(ipw.boot) * 1.96, IPW + sd(ipw.boot) * 1.96)
c(out - sd(out.boot) * 1.96, out + sd(out.boot) * 1.96)
c(pool.est - sd(pool.boot) * 1.96, pool.est + sd(pool.boot) * 1.96)


####################################################################################################################################################################################
#########   Prevelance estimators
####################################################################################################################################################################################

mean(nlst_final$lung_cancer[nlst_final$diagdiab == 1])
mean(nlst_final$lung_cancer[nlst_final$diagdiab == 0])

mean(plco_final$lung_cancer[plco_final$diagdiab == 1])
mean(plco_final$lung_cancer[plco_final$diagdiab == 0])


mean(nlst_final$lung_cancer[nlst_final$race == "white"])
mean(nlst_final$lung_cancer[nlst_final$race == "other"])

mean(plco_final$lung_cancer[plco_final$race == "white"])
mean(plco_final$lung_cancer[plco_final$race == "other"])


mean(nlst_final$lung_cancer[nlst_final$diaghear == 1])
mean(nlst_final$lung_cancer[nlst_final$diaghear == 0])

mean(plco_final$lung_cancer[plco_final$diaghear == 1])
mean(plco_final$lung_cancer[plco_final$diaghear == 0])

mean(nlst_final$lung_cancer[nlst_final$educat == 3])
mean(nlst_final$lung_cancer[nlst_final$educat == 2])
mean(nlst_final$lung_cancer[nlst_final$educat == 1])

mean(plco_final$lung_cancer[plco_final$educat == 3])
mean(plco_final$lung_cancer[plco_final$educat == 2])
mean(plco_final$lung_cancer[plco_final$educat == 1])

mean(nlst_final$lung_cancer[nlst_final$smokeliv == 1])
mean(nlst_final$lung_cancer[nlst_final$smokeliv == 0])