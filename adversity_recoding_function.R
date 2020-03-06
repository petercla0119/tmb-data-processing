##recode data from testmybrain for social cognitive measures
#any "decline to answer" or "dunno" responses are recoded as missing
#"no" coded as 0
#"yes" coded as 1

#find variable names by doing colnames(adversity)
#variables need to all be entered here in the form: adversity$varname 

#you can look at d by doing head(d) or head(adversity)
#head(d$maternaled) or head(adversity$maternaled) willl show first five lines
#d$maternaled[c(10:20)] or adversity$maternaled[c(10:20)]


# create function "d" and assign it "recode_adversity"
recode_adversity <- function (d) {

  #demographic data recoded

# recognizes pattern of specified strings, replaces it with indicated numeric value, location of where substitution should be made
# in this case, anytime middle shows up, replace it with 2 within the d$education column 
d$education <- gsub("middle",2, d$education)
d$education <- gsub("high",3,d$education)
d$education <- gsub("some_college",4,d$education)
d$education <- gsub("college",5,d$education)
d$education <- gsub("grad",6,d$education)
d$education <- gsub("none",NA,d$education)
d$education <- gsub("decline",NA,d$education)

# orginal dataframe being used had values as integers, to make value into number, coerce column into character then coerce character column into numbers and assign it 
d$education <- as.numeric(as.character(d$education))


#adversity data recoded
d$maternaled <- gsub("lessthanHS",2,d$maternaled)
d$maternaled <- gsub("highschool",3,d$maternaled)
d$maternaled <- gsub("BA",4,d$maternaled)
d$maternaled <- gsub("MA",5,d$maternaled)
d$maternaled <- gsub("Prof",6,d$maternaled)
d$maternaled <- gsub("DR",6,d$maternaled)
d$maternaled <- gsub("dunno",NA,d$maternaled)
d$maternaled <- gsub("decline",NA,d$maternaled)
d$maternaled <- as.numeric(as.character(d$maternaled))

d$paternaled <- gsub("lessthanHS",2,d$paternaled)
d$paternaled <- gsub("highschool",3,d$paternaled)
d$paternaled <- gsub("BA",4,d$paternaled)
d$paternaled <- gsub("MA",5,d$paternaled)
d$paternaled <- gsub("Prof",6,d$paternaled)
d$paternaled <- gsub("DR",6,d$paternaled)
d$paternaled <- gsub("dunno",NA,d$paternaled)
d$paternaled <- gsub("decline",NA,d$paternaled)
d$paternaled <- as.numeric(as.character(d$paternaled))

#NEW VARIABLE


make_parental <- function(d) {
  pared = c()      #empty vector
  for (i in 1:length(d$maternaled)) {
    newed <- ifelse(is.na(d$maternaled[i]),d$paternaled[i],ifelse(is.na(d$paternaled[i]),d$maternaled[i],max(d$maternaled[i],d$paternaled[i])))
    pared <- c(pared,newed)
  }
return(pared)
}

# for rows within the maternaled column
  # starting at the first row, 
    # if the maternaled row value is NA, recode it to the paternaled row value, if not, 
#create pared by combining 

d$parentaled <- make_parental(d)

d$relativeSES <- gsub("muchhigher",6,d$relativeSES)
d$relativeSES <- gsub("muchlower",2,d$relativeSES)
d$relativeSES <- gsub("higher",5,d$relativeSES)
d$relativeSES <- gsub("same",4,d$relativeSES)
d$relativeSES <- gsub("lower",3,d$relativeSES)
d$relativeSES <- gsub("decline",NA,d$relativeSES)
d$relativeSES <- gsub("dunno",NA,d$relativeSES)
d$relativeSES <- as.numeric(as.character(d$relativeSES))

# Parental Death ---------------------------------------------------------- #

d$parentdeath <- gsub("dunno",NA,d$parentdeath)                   # substitutes "NA" wherever "dunno" is
d$parentdeath <- gsub("decline",NA,d$parentdeath)                 # substitutes "NA" wherever "decline" is
d$parentdeath <- gsub("no",0,d$parentdeath)                       # substitutes "0" wherever "no" is
d$parentdeath <- gsub("yes",1,d$parentdeath)                      # substitutes "1" wherever "yes" is
d$parentdeath <- as.numeric(as.character(d$parentdeath))          #ensures value is recognized as integer

d$parentdeathAGE <- gsub("none",NA,d$parentdeathAGE)
d$parentdeathAGE <- gsub("dunno",NA,d$parentdeathAGE)
d$parentdeathAGE <- gsub("decline",NA,d$parentdeathAGE)
d$parentdeathAGE <- as.numeric(as.character(d$parentdeathAGE))    #ensures value is recognized as integer 

# Divorce -----------------------------------------------------------------

d$parentdivorce <- gsub("dunno",NA,d$parentdivorce)
d$parentdivorce <- gsub("decline",NA,d$parentdivorce)
d$parentdivorce <- gsub("yes",1,d$parentdivorce)
d$parentdivorce <- gsub("no",0,d$parentdivorce)
d$parentdivorce <- as.numeric(as.character(d$parentdivorce))

d$parentdivorceAGE <- gsub("none",NA,d$parentdivorceAGE)
d$parentdivorceAGE <- gsub("dunno",NA,d$parentdivorceAGE)
d$parentdivorceAGE <- gsub("decline",NA,d$parentdivorceAGE)
d$parentdivorceAGE <- as.numeric(as.character(d$parentdivorceAGE))


# Foster/Institutionalization  --------------------------------------------

d$foster <- gsub("dunno",NA,d$foster)
d$foster <- gsub("decline",NA,d$foster)
d$foster <- gsub("no",0,d$foster)
d$foster <- gsub("yes",1,d$foster)
d$foster <- as.numeric(as.character(d$foster))

d$fosterAGE <- gsub("none",NA,d$fosterAGE)
d$fosterAGE <- gsub("dunno",NA,d$fosterAGE)
d$fosterAGE <- gsub("decline",NA,d$fosterAGE)
d$fosterAGE <- as.numeric(as.character(d$fosterAGE))

d$fosterDUR <- gsub("none",NA,d$fosterDUR)
d$fosterDUR <- gsub("dunno",NA,d$fosterDUR)
d$fosterDUR <- gsub("decline",NA,d$fosterDUR)
d$fosterDUR <- gsub("0-1",0,d$fosterDUR)
d$fosterDUR <- as.numeric(as.character(d$fosterDUR))

d$institution <- gsub("dunno",NA,d$institution)
d$institution <- gsub("decline",NA,d$institution)
d$institution <- gsub("no",0,d$institution)
d$institution <- gsub("yes",1,d$institution)
d$institution <- as.numeric(as.character(d$institution))

d$institutionAGE <- gsub("none",NA,d$institutionAGE)
d$institutionAGE <- gsub("dunno",NA,d$institutionAGE)
d$institutionAGE <- gsub("decline", NA,d$institutionAGE)
d$institutionAGE <- as.numeric(as.character(d$institutionAGE))

d$institutionDUR <- gsub("none",NA,d$institutionDUR)
d$institutionDUR <- gsub("dunno",NA,d$institutionDUR)
d$institutionDUR <- gsub("decline",NA,d$institutionDUR)
d$institutionDUR <- gsub("0-1",0,d$institutionDUR)
d$institutionDUR <- as.numeric(as.character(d$institutionDUR))


# Alcohol/Drugs -----------------------------------------------------------

d$alcoholism <- gsub("dunno",NA,d$alcoholism)
d$alcoholism <- gsub("decline",NA,d$alcoholism)
d$alcoholism <- gsub("no",0,d$alcoholism)
d$alcoholism <- gsub("yes",1,d$alcoholism)
d$alcoholism <- as.numeric(as.character(d$alcoholism))

d$drugs <- gsub("dunno",NA,d$drugs)
d$drugs <- gsub("decline",NA,d$drugs)
d$drugs <- gsub("no",0,d$drugs)
d$drugs <- gsub("yes",1,d$drugs)
d$drugs <- as.numeric(as.character(d$drugs))


# Mental Illness/Suicide ----------------------------------------------------------

d$mentalillness <- gsub("dunno",NA,d$mentalillness)
d$mentalillness <- gsub("decline",NA,d$mentalillness)
d$mentalillness <- gsub("no",0,d$mentalillness)
d$mentalillness <- gsub("yes",1,d$mentalillness)
d$mentalillness <- as.numeric(as.character(d$mentalillness))


d$suicide <- gsub("dunno",NA,d$suicide)
d$suicide <- gsub("decline",NA,d$suicide)
d$suicide <- gsub("no",0,d$suicide)
d$suicide <- gsub("yes",1,d$suicide)
d$suicide <- as.numeric(as.character(d$suicide))

d$suicideAGE <- gsub("none",NA,d$suicideAGE)
d$suicideAGE <- gsub("dunno",NA,d$suicideAGE)
d$suicideAGE <- gsub("decline",NA,d$suicideAGE)
d$suicideAGE <- as.numeric(as.character(d$suicideAGE))

# Prison and Criminal Activity --------------------------------------------

d$prison <- gsub("dunno",NA,d$prison)
d$prison <- gsub("decline",NA,d$prison)
d$prison <- gsub("no",0,d$prison)
d$prison <- gsub("yes",1,d$prison)
d$prison <- as.numeric(as.character(d$prison))

d$prisonAGE <- gsub("none",NA,d$prisonAGE)
d$prisonAGE <- gsub("dunno",NA,d$prisonAGE)
d$prisonAGE <- gsub("decline",NA,d$prisonAGE)
d$prisonAGE <- as.numeric(as.character(d$prisonAGE))

d$criminal <- gsub("never",0,d$criminal)
d$criminal <- gsub("rarely",1,d$criminal)
d$criminal <- gsub("sometimes",2,d$criminal)
d$criminal <- gsub("often",3,d$criminal)
d$criminal <- gsub("dunno",NA,d$criminal)
d$criminal <- gsub("decline",NA,d$criminal)
d$criminal <- as.numeric(as.character(d$criminal))

#NEW VARIABLE
d$criminal.d <- ifelse(d$criminal > 1,1,0)          #creates criminal.d var when frequency was rarely or more

# Welfare/Poverty/Hunger -----------------------------------------------------------------

d$welfare <- gsub("never",0,d$welfare)
d$welfare <- gsub("rarely",1,d$welfare)
d$welfare <- gsub("sometimes",2,d$welfare)
d$welfare <- gsub("often",3,d$welfare)
d$welfare <- gsub("dunno",NA,d$welfare)
d$welfare <- gsub("decline",NA,d$welfare)
d$welfare <- as.numeric(as.character(d$welfare))

#NEW VARIABLE
d$welfare.d <- ifelse(d$welfare > 1,1,0)

d$welfareDUR <- gsub("none",NA,d$welfareDUR)
d$welfareDUR <- gsub("dunno",NA,d$welfareDUR)
d$welfareDUR <- gsub("decline",NA,d$welfareDUR)
d$welfareDUR <- gsub("0-1",0,d$welfareDUR)
d$welfareDUR <- as.numeric(as.character(d$welfareDUR))

d$hunger_poverty <- gsub("never",0,d$hunger_poverty)
d$hunger_poverty <- gsub("rarely",1,d$hunger_poverty)
d$hunger_poverty <- gsub("sometimes",2,d$hunger_poverty)
d$hunger_poverty <- gsub("often",3,d$hunger_poverty)
d$hunger_poverty <- gsub("dunno",NA,d$hunger_poverty)
d$hunger_poverty <- gsub("decline",NA,d$hunger_poverty)
d$hunger_poverty <- as.numeric(as.character(d$hunger_poverty))

#NEW VARIABLE
d$hunger_poverty.d <- ifelse(d$hunger_poverty > 1,1,0)

# Verbal Abuse ------------------------------------------------------------

d$verbalabuse <- gsub("never",0,d$verbalabuse)
d$verbalabuse <- gsub("rarely",1,d$verbalabuse)
d$verbalabuse <- gsub("sometimes",2,d$verbalabuse)
d$verbalabuse <- gsub("often",3,d$verbalabuse)
d$verbalabuse <- gsub("dunno",NA,d$verbalabuse)
d$verbalabuse <- gsub("decline",NA,d$verbalabuse)
d$verbalabuse <- as.numeric(as.character(d$verbalabuse))

#NEW VARIABLE
d$verbalabuse.d <- ifelse(d$verbalabuse >= 1,1,0)           #creates verbalabuse.d var. responses = 1 if adversity was experiences rarely (inclusive) or more often

# Verbal Abuse ------------------------------------------------------------

d$verbalAGE1 <- gsub("none",NA,d$verbalAGE1)
d$verbalAGE1 <- gsub("dunno",NA,d$verbalAGE1)
d$verbalAGE1 <- gsub("decline",NA,d$verbalAGE1)
d$verbalAGE1 <- as.numeric(as.character(d$verbalAGE1))

d$verbalAGE2 <- gsub("none",NA,d$verbalAGE2)
d$verbalAGE2 <- gsub("dunno",NA,d$verbalAGE2)
d$verbalAGE2 <- gsub("decline",NA,d$verbalAGE2)
d$verbalAGE2 <- as.numeric(as.character(d$verbalAGE2))


# Fear of Abuse -----------------------------------------------------------

d$fearabuse <- gsub("never",0,d$fearabuse)
d$fearabuse <- gsub("rarely",1,d$fearabuse)
d$fearabuse <- gsub("sometimes",2,d$fearabuse)
d$fearabuse <- gsub("often",3,d$fearabuse)
d$fearabuse <- gsub("dunno",NA,d$fearabuse)
d$fearabuse <- gsub("decline",NA,d$fearabuse)
d$fearabuse <- as.numeric(as.character(d$fearabuse))

#NEW VARIABLE
d$fearabuse.d <- ifelse(d$fearabuse >= 1,1,0)

d$fearabuseAGE1 <- gsub("none",NA,d$fearabuseAGE1)
d$fearabuseAGE1 <- gsub("dunno",NA,d$fearabuseAGE1)
d$fearabuseAGE1 <- gsub("decline",NA,d$fearabuseAGE1)
d$fearabuseAGE1 <- as.numeric(as.character(d$fearabuseAGE1))

d$fearabuseAGE2 <- gsub("none",NA,d$fearabuseAGE2)
d$fearabuseAGE2 <- gsub("dunno",NA,d$fearabuseAGE2)
d$fearabuseAGE2 <- gsub("decline",NA,d$fearabuseAGE2)
d$fearabuseAGE2 <- as.numeric(as.character(d$fearabuseAGE2))

# Physical Abuse ----------------------------------------------------------

d$physabuse1 <- gsub("never",0,d$physabuse1)
d$physabuse1 <- gsub("rarely",1,d$physabuse1)
d$physabuse1 <- gsub("sometimes",2,d$physabuse1)
d$physabuse1 <- gsub("often",3,d$physabuse1)
d$physabuse1 <- gsub("dunno",NA,d$physabuse1)
d$physabuse1 <- gsub("decline",NA,d$physabuse1)
d$physabuse1 <- as.numeric(as.character(d$physabuse1))

#NEW VARIABLE
d$physabuse1.d <- ifelse(d$physabuse1 >= 1,1,0)


d$physabuse1AGE1 <- gsub("none",NA,d$physabuse1AGE1)
d$physabuse1AGE1 <- gsub("dunno",NA,d$physabuse1AGE1)
d$physabuse1AGE1 <- gsub("decline",NA,d$physabuse1AGE1)
d$physabuse1AGE1 <- as.numeric(as.character(d$physabuse1AGE1))

d$physabuse1AGE2 <- gsub("none",NA,d$physabuse1AGE2)
d$physabuse1AGE2 <- gsub("dunno",NA,d$physabuse1AGE2)
d$physabuse1AGE2 <- gsub("decline",NA,d$physabuse1AGE2)
d$physabuse1AGE2 <- as.numeric(as.character(d$physabuse1AGE2))

d$physabuse2 <- gsub("never",0,d$physabuse2)
d$physabuse2 <- gsub("rarely",1,d$physabuse2)
d$physabuse2 <- gsub("sometimes",2,d$physabuse2)
d$physabuse2 <- gsub("often",3,d$physabuse2)
d$physabuse2 <- gsub("dunno",NA,d$physabuse2)
d$physabuse2 <- gsub("decline",NA,d$physabuse2)
d$physabuse2 <- as.numeric(as.character(d$physabuse2))

#NEW VARIABLE
d$physabuse2.d <- ifelse(d$physabuse2 >= 1,1,0)

d$physabuse2AGE1 <- gsub("none",NA,d$physabuse2AGE1)
d$physabuse2AGE1 <- gsub("dunno",NA,d$physabuse2AGE1)
d$physabuse2AGE1 <- gsub("decline",NA,d$physabuse2AGE1)
d$physabuse2AGE1 <- as.numeric(as.character(d$physabuse2AGE1))

d$physabuse2AGE2 <- gsub("none",NA,d$physabuse2AGE2)
d$physabuse2AGE2 <- gsub("dunno",NA,d$physabuse2AGE2)
d$physabuse2AGE2 <- gsub("decline",NA,d$physabuse2AGE2)
d$physabuse2AGE2 <- as.numeric(as.character(d$physabuse2AGE2))

d$physinjury <- gsub("never",0,d$physinjury)
d$physinjury <- gsub("rarely",1,d$physinjury)
d$physinjury <- gsub("sometimes",2,d$physinjury)
d$physinjury <- gsub("often",3,d$physinjury)
d$physinjury <- gsub("dunno",NA,d$physinjury)
d$physinjury <- gsub("decline",NA,d$physinjury)
d$physinjury <- as.numeric(as.character(d$physinjury))

#NEW VARIABLE
d$physinjury.d <- ifelse(d$physinjury >= 1,1,0)

d$physinjuryAGE1 <- gsub("none",NA,d$physinjuryAGE1)
d$physinjuryAGE1 <- gsub("dunno",NA,d$physinjuryAGE1)
d$physinjuryAGE1 <- gsub("decline",NA,d$physinjuryAGE1)
d$physinjuryAGE1 <- as.numeric(as.character(d$physinjuryAGE1))

d$physinjuryAGE2 <- gsub("none",NA,d$physinjuryAGE2)
d$physinjuryAGE2 <- gsub("dunno",NA,d$physinjuryAGE2)
d$physinjuryAGE2 <- gsub("decline",NA,d$physinjuryAGE2)
d$physinjuryAGE2 <- as.numeric(as.character(d$physinjuryAGE2))

# Sexual Abuse ------------------------------------------------------------

d$sexabuse1 <- gsub("dunno",NA,d$sexabuse1)
d$sexabuse1 <- gsub("decline",NA,d$sexabuse1)
d$sexabuse1 <- gsub("no",0,d$sexabuse1)
d$sexabuse1 <- gsub("yes",1,d$sexabuse1)
d$sexabuse1 <- as.numeric(as.character(d$sexabuse1))

d$sexabuse1AGE1 <- gsub("none",NA,d$sexabuse1AGE1)
d$sexabuse1AGE1 <- gsub("dunno",NA,d$sexabuse1AGE1)
d$sexabuse1AGE1 <- gsub("decline",NA,d$sexabuse1AGE1)
d$sexabuse1AGE1 <- as.numeric(as.character(d$sexabuse1AGE1))

d$sexabuse1AGE2 <- gsub("none",NA,d$sexabuse1AGE2)
d$sexabuse1AGE2 <- gsub("dunno",NA,d$sexabuse1AGE2)
d$sexabuse1AGE2 <- gsub("decline",NA,d$sexabuse1AGE2)
d$sexabuse1AGE2 <- as.numeric(as.character(d$sexabuse1AGE2))

#sexabuse1FREQ is a text response.
d$sexabuse1FREQ <- gsub("^$",NA,d$sexabuse1FREQ)
d$sexabuse1FREQ <- as.factor(d$sexabuse1FREQ)

d$sexabuse1WHO <- gsub("relhome",2,d$sexabuse1WHO)
d$sexabuse1WHO <- gsub("reloutside",3,d$sexabuse1WHO)
d$sexabuse1WHO <- gsub("unrelated",4,d$sexabuse1WHO)
d$sexabuse1WHO <- gsub("dunno",NA,d$sexabuse1WHO)
d$sexabuse1WHO <- gsub("decline",NA,d$sexabuse1WHO)
d$sexabuse1WHO <- as.numeric(as.character(d$sexabuse1WHO))

#d$sexabuse1WHO not to be recodedd$sexabuse2 <- gsub("no",0,d$sexabuse2)

d$sexabuse2 <- gsub("dunno",NA,d$sexabuse2)
d$sexabuse2 <- gsub("decline",NA,d$sexabuse2)
d$sexabuse2 <- gsub("yes",1,d$sexabuse2)
d$sexabuse2 <- gsub("no",0,d$sexabuse2)
d$sexabuse2 <- as.numeric(as.character(d$sexabuse2))

d$sexabuse2AGE1 <- gsub("none",NA,d$sexabuse2AGE1)
d$sexabuse2AGE1 <- gsub("dunno",NA,d$sexabuse2AGE1)
d$sexabuse2AGE1 <- gsub("decline",NA,d$sexabuse2AGE1)
d$sexabuse2AGE1 <- as.numeric(as.character(d$sexabuse2AGE1))

d$sexabuse2AGE2 <- gsub("none",NA,d$sexabuse2AGE2)
d$sexabuse2AGE2 <- gsub("dunno",NA,d$sexabuse2AGE2)
d$sexabuse2AGE2 <- gsub("decline",NA,d$sexabuse2AGE2)
d$sexabuse2AGE2 <- as.numeric(as.character(d$sexabuse2AGE2))

#sexabuse2FREQ is a text response.
d$sexabuse2FREQ <- gsub("^$",NA,d$sexabuse2FREQ)
d$sexabuse2FREQ <- as.factor(d$sexabuse2FREQ)



#d$sexabuse2WHO not to be recoded

d$dangerouschores <- gsub("never",0,d$dangerouschores)
d$dangerouschores <- gsub("rarely",1,d$dangerouschores)
d$dangerouschores <- gsub("sometimes",2,d$dangerouschores)
d$dangerouschores <- gsub("often",3,d$dangerouschores)
d$dangerouschores <- gsub("dunno",NA,d$dangerouschores)
d$dangerouschores <- gsub("decline",NA,d$dangerouschores)
d$dangerouschores <- as.numeric(as.character(d$dangerouschores))

#NEW VARIABLE
d$dangerouschores.d <- ifelse(d$dangerouschores > 1,1,0)


d$unsupervised <- gsub("never",0,d$unsupervised)
d$unsupervised <- gsub("rarely",1,d$unsupervised)
d$unsupervised <- gsub("sometimes",2,d$unsupervised)
d$unsupervised <- gsub("often",3,d$unsupervised)
d$unsupervised <- gsub("dunno",NA,d$unsupervised)
d$unsupervised <- gsub("decline",NA,d$unsupervised)
d$unsupervised <- as.numeric(as.character(d$unsupervised))

#NEW VARIABLE
d$unsupervised.d <- ifelse(d$unsupervised > 1,1,0)

d$neglect_clothing <- gsub("never",0,d$neglect_clothing)
d$neglect_clothing <- gsub("rarely",1,d$neglect_clothing)
d$neglect_clothing <- gsub("sometimes",2,d$neglect_clothing)
d$neglect_clothing <- gsub("often",3,d$neglect_clothing)
d$neglect_clothing <- gsub("dunno",NA,d$neglect_clothing)
d$neglect_clothing <- gsub("decline",NA,d$neglect_clothing)
d$neglect_clothing <- as.numeric(as.character(d$neglect_clothing))

#NEW VARIABLE
d$neglect_clothing.d <- ifelse(d$neglect_clothing > 1,1,0)

d$neglect_hunger <- gsub("never",0,d$neglect_hunger)
d$neglect_hunger <- gsub("rarely",1,d$neglect_hunger)
d$neglect_hunger <- gsub("sometimes",2,d$neglect_hunger)
d$neglect_hunger <- gsub("often",3,d$neglect_hunger)
d$neglect_hunger <- gsub("dunno",NA,d$neglect_hunger)
d$neglect_hunger <- gsub("decline",NA,d$neglect_hunger)
d$neglect_hunger <- as.numeric(as.character(d$neglect_hunger))

#NEW VARIABLE
d$neglect_hunger.d <- ifelse(d$neglect_hunger > 1,1,0)

d$neglect_medical <- gsub("never",0,d$neglect_medical)
d$neglect_medical <- gsub("rarely",1,d$neglect_medical)
d$neglect_medical <- gsub("sometimes",2,d$neglect_medical)
d$neglect_medical <- gsub("often",3,d$neglect_medical)
d$neglect_medical <- gsub("dunno",NA,d$neglect_medical)
d$neglect_medical <- gsub("decline",NA,d$neglect_medical)
d$neglect_medical <- as.numeric(as.character(d$neglect_medical))

#NEW VARIABLE
d$neglect_medical.d <- ifelse(d$neglect_medical > 1,1,0)

# Domestic Violence -------------------------------------------------------

d$domesticviolence1 <- gsub("never",0,d$domesticviolence1)
d$domesticviolence1 <- gsub("rarely",1,d$domesticviolence1)
d$domesticviolence1 <- gsub("sometimes",2,d$domesticviolence1)
d$domesticviolence1 <- gsub("often",3,d$domesticviolence1)
d$domesticviolence1 <- gsub("dunno",NA,d$domesticviolence1)
d$domesticviolence1 <- gsub("decline",NA,d$domesticviolence1)
d$domesticviolence1 <- as.numeric(as.character(d$domesticviolence1))

#NEW VARIABLE
d$domesticviolence1.d <- ifelse(d$domesticviolence1 >= 1,1,0)


d$domesticviolence1AGE1 <- gsub("none",NA,d$domesticviolence1AGE1)
d$domesticviolence1AGE1 <- gsub("dunno",NA,d$domesticviolence1AGE1)
d$domesticviolence1AGE1 <- gsub("decline",NA,d$domesticviolence1AGE1)
d$domesticviolence1AGE1 <- as.numeric(as.character(d$domesticviolence1AGE1))

d$domesticviolence1AGE2 <- gsub("none",NA,d$domesticviolence1AGE2)
d$domesticviolence1AGE2 <- gsub("dunno",NA,d$domesticviolence1AGE2)
d$domesticviolence1AGE2 <- gsub("decline",NA,d$domesticviolence1AGE2)
d$domesticviolence1AGE2 <- as.numeric(as.character(d$domesticviolence1AGE2))

d$domesticviolence2 <- gsub("never",0,d$domesticviolence2)
d$domesticviolence2 <- gsub("rarely",1,d$domesticviolence2)
d$domesticviolence2 <- gsub("sometimes",2,d$domesticviolence2)
d$domesticviolence2 <- gsub("often",3,d$domesticviolence2)
d$domesticviolence2 <- gsub("dunno",NA,d$domesticviolence2)
d$domesticviolence2 <- gsub("decline",NA,d$domesticviolence2)
d$domesticviolence2 <- as.numeric(as.character(d$domesticviolence2))

#NEW VARIABLE
d$domesticviolence2.d <- ifelse(d$domesticviolence2 >= 1,1,0)

d$domesticviolence2AGE1 <- gsub("none",NA,d$domesticviolence2AGE1)
d$domesticviolence2AGE1 <- gsub("dunno",NA,d$domesticviolence2AGE1)
d$domesticviolence2AGE1 <- gsub("decline",NA,d$domesticviolence2AGE1)
d$domesticviolence2AGE1 <- as.numeric(as.character(d$domesticviolence2AGE1))

d$domesticviolence2AGE2 <- gsub("none",NA,d$domesticviolence2AGE2)
d$domesticviolence2AGE2 <- gsub("dunno",NA,d$domesticviolence2AGE2)
d$domesticviolence2AGE2 <- gsub("decline",NA,d$domesticviolence2AGE2)
d$domesticviolence2AGE2 <- as.numeric(as.character(d$domesticviolence2AGE2))

# Demographic Info --------------------------------------------------------

d$additionalinfo <- as.character(d$additionalInfo)

d <- d[d$age >= 18 & d$age <= 70,]
d$age <- as.numeric(as.character(d$age))

d$hispanic <- gsub(3,NA,d$hispanic)
d$hispanic <- gsub(2,NA,d$hispanic)
d$hispanic <- as.numeric(d$hispanic)

d$gender <- gsub("female",0,d$gender)
d$gender <- gsub("male",1,d$gender)
d$gender <- as.numeric(d$gender)

d$ethnicity <- gsub("decline",NA,d$ethnicity)
d$ethnicity <- gsub("none",NA,d$ethnicity)
d$ethnicity<- factor(d$ethnicity, levels = c("europe","africa","east_asia","west_asia","pacific_asia","americas","two","more_than_two"))
# factors d$ethnicity column and seperates it into with 8 specified levels

# new variable - handedness
d$handedness <- gsub("right",1,d$handedness)
d$handedness <- gsub("left",0,d$handedness)

# create new variable
d$race.ethnicity[d$hispanic == 1] <- "hispanic"
d$race.ethnicity[d$hispanic == 0 & d$ethnicity == "europe"] <- "nonhispanic_white"                        #non-hispanic/european descent = non-hispanic white
d$race.ethnicity[d$hispanic == 0 & d$ethnicity == "africa"] <- "nonhispanic_black"                        #non-hispanic/african descent = non-hispanic black
d$race.ethnicity[d$hispanic == 0 & d$ethnicity != "europe" & d$ethnicity != "africa"] <- "other"          #non-hispanic, not european or african descent = other
d$race.ethnicity <- factor(d$race.ethnicity, levels=c("nonhispanic_white","nonhispanic_black","hispanic","other"))
d$caucasian[d$hispanic == 0 & d$ethnicity == "europe"] <- 1
d$caucasian[d$hispanic == 1 | d$ethnicity != "europe"] <- 0
d$caucasian <- as.numeric(d$caucasian)

# native_language recoding ####
#languages were recoded into numeric values by region of the world
#language responses were typed in by user --> variations of different spelling for same language are coded as same respective numeric value
        #responses which had more than one language listed (i.e. english and russian), were coded into whatever group the first language typed would fit in
        #languages are seperated by continent and sub-separated by region 
            #next to direction (i.e. south) lists countries considered to be in region
  # english
d$native_language <- gsub("english", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("english.", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("american", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("american english", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("australian", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("asl", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("british", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("british english", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("eng", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("endlish", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("englis", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("england", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("english and russian", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("english-french", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("english.", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("englsh", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("engrish", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("enlish", 1, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("enlgish", 1, d$native_language, ignore.case = TRUE)

#### EUROPEAN LANGUAGES ####
    # north - iceland, ireland, UK, denmark, norway, sweden, finand, estonia, latvia, lithuania
d$native_language <- gsub("danish", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("danish.", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("deutsch", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("dutch", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("estonian", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("finnish", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("icelandic", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("irish", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("latvian", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("lithuanian", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("norwegian", 2, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("swedish", 2, d$native_language, ignore.case = TRUE)

      #east - poland, belarus, ukraine, moldova, bulgaria, romania, hungary, slovakia, poland, czech republic
d$native_language <- gsub("bulgarian", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("czech", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("hungarian", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("polish", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("romanian", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("roumanian", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("slovak", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("slovenian", 3,d$native_language, ignore.case = TRUE)
d$native_language <- gsub("slovene", 3, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("ukranian", 3, d$native_language, ignore.case = TRUE)

      # south - portugal, spain, italy, albania, greece, macedonia, serbia, kosovo, montenergro, croatia, slovenia, bosnia
d$native_language <- gsub("albanian", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("albania", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("bosnian", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("catalan", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("croatian", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("croatian.", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("greek", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("italian", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("italiano", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("macedonian", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("maltese", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("spanish", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("espanol", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("portugese", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("serbian", 4, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("portuguese", 4, d$native_language, ignore.case = TRUE)

      # west - neatherlands, germany, belgium, france, luxembourg, austria, 
d$native_language <- gsub("france", 5, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("french", 5, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("german", 5, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("amharic", 5, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("luxembourish", 5, d$native_language, ignore.case = TRUE)

#### ASIAN LANGUAGES #### 
      # north - russian
d$native_language <- gsub("russian", 6, d$native_language, ignore.case = TRUE)

      # east - china, japan, north korea, mongolia, taiwan
d$native_language <- gsub("cantonese", 7, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("chinese", 7, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("japanese", 7, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("korean", 7, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("mandarin", 7, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("hangul", 7, d$native_language, ignore.case = TRUE)

      # southeastern - brunei, cambodia, indonesia, laos, malaysia, myanmar, philippines, singapore, thailand, vietnam
d$native_language <- gsub("filipino", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("filipino.", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("ilonggo", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("indonesian", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("indonesia", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("malay", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("tagalog", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("thai", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("vietnamese", 8, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("visaya", 8, d$native_language, ignore.case = TRUE)

      # central - kazakhstan, kyrgyzstan, tajikistan, turkmenistan, uzbekistan
d$native_language <- gsub("kazakh", 9, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("kyrgyz", 9, d$native_language, ignore.case = TRUE)

      # south - afghanistan, bagladesh, bhutan, india, pakistan
d$native_language <- gsub("badaga", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("bahasa", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("bahasa indonesia", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("bangla", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("bahasa melayu", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("bangali", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("dzongkha", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("gujarati", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("hindi", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("indian", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("kannada", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("khasi", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("konkani", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("malayalam", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("malaysian", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("malyalam", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("marathi", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("mara", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("nepali", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("nepal", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("pashto", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("punjabi", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("sindhi", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("sinhala", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("sinhalese", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("tamil", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("telugu", 10, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("urdu", 10, d$native_language, ignore.case = TRUE)

      # west - turkey, bahrain, kuwait, qatar, saudi, UAE, armenia, azerbaijan, iraq, israel, jordan, palestine, lebanon, syria, iran, egypt, cyprus
d$native_language <- gsub("azari", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("azerbaijani", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("azeri-turkish", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("assamese", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("arab",11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("arabic",11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("ar", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("armenian", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("farsi", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("hebrew", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("kurdish", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("lebanese", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("persian", 11, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("turkish", 11, d$native_language, ignore.case = TRUE)

#### AFRICAN LANGUAGES ####
d$native_language <- gsub("africaans", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("creole", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("dholuo", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("isizulu", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("kikuyu", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("malagasy", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("somali", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("swahili", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("tswana", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("uhrobo", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("venda", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("oshiwambo", 12, d$native_language, ignore.case = TRUE)
d$native_language <- gsub("afrikaans", 12, d$native_language, ignore.case = TRUE)

d$native_language <- ifelse(d$native_language >= 1 | d$native_language <= 12, d$native_language, "NA" )       #recodes any other languages not above as missing
d$native_language <- as.numeric(as.character(d$native_language))

return(d)
}

head(recode_adversity)  # name of function to be called to apply to dataframes
     
