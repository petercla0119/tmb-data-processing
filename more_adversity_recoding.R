# adversity_processed_data.csv contains all demographic information, raw ages, and scores across RMET and MSPSS tests
      #user responses has been recoded to numeric values
origin.data <-read.csv("adversity_processed_data.csv")

# replace 9999 with NA
origin.data[origin.data==9999] <- NA

#df recoded from "adversity_recoding_function.R"
#26 variables total are yes/no responses 
# c(parentdeath,parentdivorce,foster,institution,alcoholism,drugs,mentalillness,suicide,prison,criminal.d,welfare.d,hunger_poverty.d,verbalabuse.d,fearabuse.d,physabuse1.d,physabuse2.d,unsupervised.d,neglect_clothing,neglect_hunger.d,neglect_medical.d,domesticviolence1.d,domesticviolence2.d)

#WE WILL ONLY USE ADVERSITY VARS WHICH HAVE ASSOCIATED AGES - yes/no
#### exposed/unexposed vars with associated ages - 15 variables
exposed.unexposed <- origin.data[,c("parentdeath","parentdivorce","foster","institution","suicide","prison","verbalabuse.d","fearabuse.d","physabuse1.d","physabuse2.d","physinjury.d","sexabuse1", "sexabuse2", "domesticviolence1.d","domesticviolence2.d")]

    # df for columns with just raw age values - 24 raw AGE columns
      # verbal, fear, physabuse1/2, physinjury, sexabuse1/2, domesticviolence1/2 all have BOTH age 1 and age 2 
      # c("parentdeathAGE","parentdivorceAGE","fosterAGE","institutionAGE","suicideAGE","prisonAGE","verbalAGE1","verbalAGE2","fearabuseAGE1","fearabuseAGE2","physabuse1AGE1","physabuse1AGE2","physabuse2AGE1","physabuse2AGE2","physinjuryAGE1","physinjuryAGE2","sexabuse1AGE1","sexabuse1AGE2","sexabuse2AGE1","sexabuse2AGE2","domesticviolence1AGE1","domesticviolence1AGE2","domesticviolence2AGE1","domesticviolence2AGE2")

#WE WILL ONLY FOCUS ON "AGE1"
#### RAW age 1 exposure vars - 15 variables
age <- origin.data[,c("parentdeathAGE","parentdivorceAGE","fosterAGE","institutionAGE","suicideAGE","prisonAGE","verbalAGE1","fearabuseAGE1","physabuse1AGE1","physabuse2AGE1","physinjuryAGE1","sexabuse1AGE1","sexabuse2AGE1","domesticviolence1AGE1","domesticviolence2AGE1")]

# no associated ages for 11 vars: c("alcoholism","drugs","mentalillness","criminal.d","welfare.d","hunger_poverty.d","dangerouschores.d","unsupervised.d","neglect_clothing.d","neglect_hunger.d","neglect_medical.d") 
    # therefore, there are 24 columns for AGEs
      # of those, 9, do not have yes/no vars (verbalAGE2, fearabuseAGE2, physabuse1AGE2, physabuse2AGE2, physinjuryAGE2 sexabuse1AGE2, sexabuse2AGE2, domesticviolence1AGE2, domesticviolence2AGE2)

#columns with demographic data - 7 variables 
demographics <- origin.data[,c("id","birth_year","age", "gender","handedness","education", "parentaled", "native_language", "race.ethnicity", "relativeSES")]
demographics$age <- as.numeric(as.character(demographics$age))
demographics$birth_year <- as.numeric(as.character(demographics$birth_year))

# columns with score and test duration (only RMET and MSPSS)
scores <- origin.data[,c("duration_rmet", "rmet")]        #subsets RMET scores into df "scores"

age_group <- age
####### GROUPING BY AGE #########
      #all adversity vars broken down into 1 of 4 different age groups based off age partcipant indicated
        #all age group vars end in "_AG#" numbers are 1-4
          # AG1 = ages 0-5, AG2 = ages 6-11, AG3 = ages 11-17, AG4 = ages 18+
#### parentdeathAGE #
age_group$parentdeath_AG1 <- ifelse(age_group$parentdeathAGE >= 0 & age_group$parentdeathAGE <= 5,1,0)
age_group$parentdeath_AG2 <- ifelse(age_group$parentdeathAGE >= 6 & age_group$parentdeathAGE <= 11,1,0)
age_group$parentdeath_AG3 <- ifelse(age_group$parentdeathAGE >= 12 & age_group$parentdeathAGE <= 17,1,0)
age_group$parentdeath_AG4 <- ifelse(age_group$parentdeathAGE >= 18,1,0)

#### parentdivorceAGE #
age_group$parentdivorce_AG1 <- ifelse(age_group$parentdivorceAGE >= 0 & age_group$parentdivorceAGE <= 5,1,0)
age_group$parentdivorce_AG2 <- ifelse(age_group$parentdivorceAGE >= 6 & age_group$parentdivorceAGE <= 11,1,0)
age_group$parentdivorce_AG3 <- ifelse(age_group$parentdivorceAGE >= 12 & age_group$parentdivorceAGE <= 17,1,0)
age_group$parentdivorce_AG4 <- ifelse(age_group$parentdivorceAGE >= 18,1,0)

#### fosterAGE #
age_group$foster_AG1 <- ifelse(age_group$fosterAGE >= 0 & age_group$fosterAGE <= 5,1,0)
age_group$foster_AG2 <- ifelse(age_group$fosterAGE >= 6 & age_group$fosterAGE <= 11,1,0)
age_group$foster_AG3 <- ifelse(age_group$fosterAGE >= 12 & age_group$fosterAGE <= 17,1,0)
age_group$foster_AG4 <- ifelse(age_group$fosterAGE >= 18 ,1,0)

#### institutionAGE #
age_group$institution_AG1 <- ifelse(age_group$institutionAGE >= 0 & age_group$institutionAGE <= 5,1,0)
age_group$institution_AG2 <- ifelse(age_group$institutionAGE >= 6 & age_group$institutionAGE <= 11,1,0)
age_group$institution_AG3 <- ifelse(age_group$institutionAGE >= 12 & age_group$institutionAGE <= 17,1,0)
age_group$institution_AG4 <- ifelse(age_group$institutionAGE >= 18 ,1,0)

#### suicideAGE #
age_group$suicide_AG1 <- ifelse(age_group$suicideAGE >= 0 & age_group$suicideAGE <= 5,1,0)
age_group$suicide_AG2 <- ifelse(age_group$suicideAGE >= 6 & age_group$suicideAGE <= 11,1,0)
age_group$suicide_AG3 <- ifelse(age_group$suicideAGE >= 12 & age_group$suicideAGE <= 17,1,0)
age_group$suicide_AG4 <- ifelse(age_group$suicideAGE >= 18 ,1,0)

#### prisonAGE #
age_group$prison_AG1 <- ifelse(age_group$prisonAGE >= 0 & age_group$prisonAGE <= 5,1,0)
age_group$prison_AG2 <- ifelse(age_group$prisonAGE >= 6 & age_group$prisonAGE <= 11,1,0)
age_group$prison_AG3 <- ifelse(age_group$prisonAGE >= 12 & age_group$prisonAGE <= 17,1,0)
age_group$prison_AG4 <- ifelse(age_group$prisonAGE >= 18 ,1,0)

#### verbalAGE1 #
age_group$verbalAGE1_AG1 <- ifelse(age_group$verbalAGE1 >= 0 & age_group$verbalAGE1 <= 5,1,0)
age_group$verbalAGE1_AG2 <- ifelse(age_group$verbalAGE1 >= 6 & age_group$verbalAGE1 <= 11,1,0)
age_group$verbalAGE1_AG3 <- ifelse(age_group$verbalAGE1 >= 12 & age_group$verbalAGE1 <= 17,1,0)
age_group$verbalAGE1_AG4 <- ifelse(age_group$verbalAGE1 >= 18 ,1,0)

#### fearabuseAGE1 #
age_group$fearabuseAGE1_AG1 <- ifelse(age_group$fearabuseAGE1 >= 0 & age_group$fearabuseAGE1 <= 5,1,0)
age_group$fearabuseAGE1_AG2 <- ifelse(age_group$fearabuseAGE1 >= 6 & age_group$fearabuseAGE1 <= 11,1,0)
age_group$fearabuseAGE1_AG3 <- ifelse(age_group$fearabuseAGE1 >= 12 & age_group$fearabuseAGE1 <= 17,1,0)
age_group$fearabuseAGE1_AG4 <- ifelse(age_group$fearabuseAGE1 >= 18 ,1,0)

#### physabuse1AGE1 #
age_group$physabuse1AGE1_AG1 <- ifelse(age_group$physabuse1AGE1 >= 0 & age_group$physabuse1AGE1 <= 5,1,0)
age_group$physabuse1AGE1_AG2 <- ifelse(age_group$physabuse1AGE1 >= 6 & age_group$physabuse1AGE1 <= 11,1,0)
age_group$physabuse1AGE1_AG3 <- ifelse(age_group$physabuse1AGE1 >= 12 & age_group$physabuse1AGE1 <= 17,1,0)
age_group$physabuse1AGE1_AG4 <- ifelse(age_group$physabuse1AGE1 >= 18 ,1,0)

#### physabuse2AGE1 #
age_group$physabuse2AGE1_AG1 <- ifelse(age_group$physabuse2AGE1 >= 0 & age_group$physabuse2AGE1 <= 5,1,0)
age_group$physabuse2AGE1_AG2 <- ifelse(age_group$physabuse2AGE1 >= 6 & age_group$physabuse2AGE1 <= 11,1,0)
age_group$physabuse2AGE1_AG3 <- ifelse(age_group$physabuse2AGE1 >= 12 & age_group$physabuse2AGE1 <= 17,1,0)
age_group$physabuse2AGE1_AG4 <- ifelse(age_group$physabuse2AGE1 >= 18 ,1,0)

#### physinjuryAGE1 #
age_group$physinjuryAGE1_AG1 <- ifelse(age_group$physinjuryAGE1 >= 0 & age_group$physinjuryAGE1 <= 5,1,0)
age_group$physinjuryAGE1_AG2 <- ifelse(age_group$physinjuryAGE1 >= 6 & age_group$physinjuryAGE1 <= 11,1,0)
age_group$physinjuryAGE1_AG3 <- ifelse(age_group$physinjuryAGE1 >= 12 & age_group$physinjuryAGE1 <= 17,1,0)
age_group$physinjuryAGE1_AG4 <- ifelse(age_group$physinjuryAGE1 >= 18 ,1,0)

#### sexabuse1AGE1 #
age_group$sexabuse1AGE1_AG1 <- ifelse(age_group$sexabuse1AGE1 >= 0 & age_group$sexabuse1AGE1 <= 5,1,0)
age_group$sexabuse1AGE1_AG2 <- ifelse(age_group$sexabuse1AGE1 >= 6 & age_group$sexabuse1AGE1 <= 11,1,0)
age_group$sexabuse1AGE1_AG3 <- ifelse(age_group$sexabuse1AGE1 >= 12 & age_group$sexabuse1AGE1 <= 17,1,0)
age_group$sexabuse1AGE1_AG4 <- ifelse(age_group$sexabuse1AGE1 >= 18 ,1,0)

#### sexabuse2AGE1 #
age_group$sexabuse2AGE1_AG1 <- ifelse(age_group$sexabuse2AGE1 >= 0 & age_group$sexabuse2AGE1 <= 5,1,0)
age_group$sexabuse2AGE1_AG2 <- ifelse(age_group$sexabuse2AGE1 >= 6 & age_group$sexabuse2AGE1 <= 11,1,0)
age_group$sexabuse2AGE1_AG3 <- ifelse(age_group$sexabuse2AGE1 >= 12 & age_group$sexabuse2AGE1 <= 17,1,0)
age_group$sexabuse2AGE1_AG4 <- ifelse(age_group$sexabuse2AGE1 >= 18 ,1,0)

#### domesticviolence1AGE1 #
age_group$domesticviolence1AGE1_AG1 <- ifelse(age_group$domesticviolence1AGE1 >= 0 & age_group$domesticviolence1AGE1 <= 5,1,0)
age_group$domesticviolence1AGE1_AG2 <- ifelse(age_group$domesticviolence1AGE1 >= 6 & age_group$domesticviolence1AGE1 <= 11,1,0)
age_group$domesticviolence1AGE1_AG3 <- ifelse(age_group$domesticviolence1AGE1 >= 12 & age_group$domesticviolence1AGE1 <= 17,1,0)
age_group$domesticviolence1AGE1_AG4 <- ifelse(age_group$domesticviolence1AGE1 >= 18 ,1,0)

#### domesticviolence2AGE1 #
age_group$domesticviolence2AGE1_AG1 <- ifelse(age_group$domesticviolence2AGE1 >= 0 & age_group$domesticviolence2AGE1 <= 5,1,0)
age_group$domesticviolence2AGE1_AG2 <- ifelse(age_group$domesticviolence2AGE1 >= 6 & age_group$domesticviolence2AGE1 <= 11,1,0)
age_group$domesticviolence2AGE1_AG3 <- ifelse(age_group$domesticviolence2AGE1 >= 12 & age_group$domesticviolence2AGE1 <= 17,1,0)
age_group$domesticviolence2AGE1_AG4 <- ifelse(age_group$domesticviolence2AGE1 >= 18 ,1,0)


# age_group has 15 adversities sorted by adversity
    # reorder columns so that AG1-4 are next to respective adversity
age_group <- age_group[c(1, 16:19, 2, 20:23, 3, 24:27, 4, 28:31, 5, 32:35, 6, 36:39, 7, 40:43, 8, 44:47, 9, 48:51, 10, 52:55, 11, 56:59, 12, 60:63, 13, 64:67, 14, 68:71, 15, 72:75)]

# alldata includes demographics, raw ages, recoded groups, and scores for all participants and tests
alldata <- cbind.data.frame(demographics, age_group, scores, exposed.unexposed)

################### ONLY LOOKS AT RMET OUTCOME ######################
# rmet.all df contains RAW ages, grouped ages, demographic info and scores
rmet.all <- subset(alldata,!is.na(alldata$rmet))
    # in rmet.all, col 1-7 = demographic, col 8,13,18,23,28,33,38,43,48,53,58,63,68,73,78
      # AG1 recoded values rmet.all <- [ , seq(9, 82, by = 5)]
      # col 85-99 = exposed/unexposed

##################### RECODING DEMOGRAPHIC INFO #########################
# gender, hispanic, education, maternaled, paternaled, caucasian and native_language are all numeric
# need to recode handedness, race,ethnicity, ethnicity
# handedness - 1=right 0=left
rmet.all$handedness <- gsub("right", 1, rmet.all$handedness)
rmet.all$handedness <- gsub("left", 0, rmet.all$handedness)
rmet.all$handedness <- gsub("both", 1, rmet.all$handedness)
# race.ethnicity
rmet.all$race.ethnicity <- gsub("hispanic", 1, rmet.all$race.ethnicity)
rmet.all$race.ethnicity <- gsub("non1_white", 2, rmet.all$race.ethnicity)
rmet.all$race.ethnicity <- gsub("non1_black", 3, rmet.all$race.ethnicity)
rmet.all$race.ethnicity <- gsub("other", 4, rmet.all$race.ethnicity)

#converts handedness, ethnicity and race.ethnicity to numbers
rmet.all$handedness <- as.numeric(as.character(rmet.all$handedness))
rmet.all$race.ethnicity <- as.numeric(as.character(rmet.all$race.ethnicity))

# converts integers to numerics
indx <- sapply(age, is.integer)
age[indx] <- lapply(age[indx], function(x) as.numeric(as.character(x)))

# CSV file contains: demographics, raw ages, groups recoded, scores, exposed/unexposed
write.csv(rmet.all, "rmet_all_raw.csv")

# should be noted that this data has undergone no transformations and is NOT normally distributed

summary(rmet.all$rmet)
# identifies minimum duration, anything less than 120s is to be excluded
min(rmet.all$duration_rmet)
    # no values were below 120s
# calculates Z scores for all rmet scores 
score_scaled <- scale(rmet.all$rmet)
# identifies any scores greater than 3.5 sd of the mean
apply(score_scaled, 2, function(x) which(x >= 3.5))
# identifies any scores less than -3.5 sd of the mean
apply(score_scaled, 2, function(x) which(x <= -3.5))
  # 8 values identified, --> values are to be recoded so that
rmet.scaled <- rmet.all

#adds score_scaled values to rmet.scaled DF
rmet.scaled$score_scaled <- c(score_scaled)
# sets all scores less than -3.5 to -3.5
rmet.scaled[110,100] = (-3.5)
rmet.scaled[737,100] = (-3.5)
rmet.scaled[1384,100] = (-3.5)
rmet.scaled[2770,100] = (-3.5)
rmet.scaled[2872,100] = (-3.5)
rmet.scaled[2877,100] = (-3.5)
rmet.scaled[3191,100] = (-3.5)
rmet.scaled[3487,100] = (-3.5)

# creates rmet_winzor.csv which contains raw AGE, demographics, scaled scores, raw score, and age groups
#write.csv(rmet.scaled, "rmet_winzor.csv")

