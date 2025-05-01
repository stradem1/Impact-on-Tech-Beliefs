
## Project:  SOC 302 Final Multivariate Project
# Located:   ----
# File Name: Strader-tech_beliefs.R
# Date:      3/31/2025
# Who:       Marissa Strader


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
setwd("/courses/SOC302/stradem1")

library(dplyr)
library(psych)

##Fix Max Print Function##
options(max.print = 10000)

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
table(GSS$nextgen)
# Step 2: Create dummy variables for each category of agreement with nextgen
GSS <- mutate(GSS, strgagree_nextgen = ifelse(nextgen==1, 1, 0))
GSS <- mutate(GSS, agree_nextgen = ifelse(nextgen==2, 1, 0))
GSS <- mutate(GSS, disagree_nextgen = ifelse(nextgen==3, 1, 0))
GSS <- mutate(GSS, strgdisagree_nextgen = ifelse(nextgen==4, 1, 0))

# Step 3: Confirm creation by looking at nextgen and each dummy variable
table(GSS$nextgen, GSS$strgagree_nextgen)
table(GSS$nextgen, GSS$agree_nextgen)
table(GSS$nextgen, GSS$disagree_nextgen)
table(GSS$nextgen, GSS$strgdisagree_nextgen)




############                     DEPENDENT VARIABLE                     ############
############                   Beliefs About Technology                ############

# STEP 1: Examine variable and coding schema 
table(GSS$nextgen)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, strgagree_nextgen = ifelse(nextgen==1, 1, 0))
GSS <- mutate(GSS, agree_nextgen = ifelse(nextgen==2, 1, 0))
GSS <- mutate(GSS, disagree_nextgen = ifelse(nextgen==3, 1, 0))
GSS <- mutate(GSS, strgdisagree_nextgen = ifelse(nextgen==4, 1, 0))

# variable for overall agreement with statement for nextgen
GSS <- mutate(GSS, agree = ifelse(nextgen<=2, 1, 0))

#variable for overall disagreement with statement for nextgen
GSS <- mutate (GSS, disagree = ifelse(nextgen> 2, 1, 0))


# STEP 3: Confirm creation (if necessary)
table(GSS$nextgen, GSS$strgagree_nextgen)
table(GSS$nextgen, GSS$agree_nextgen)
table(GSS$nextgen, GSS$disagree_nextgen)
table(GSS$nextgen, GSS$strgdisagree_nextgen)

table(GSS$nextgen, GSS$agree)
table(GSS$nextgen, GSS$disagree)



############                  INDEPENDENT VARIABLE                    ############
############       Consent to Being Linked to Data by Respondent      ############

# STEP 1: Examine variable and coding schema 
table(GSS$adminconsent)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, consent_adminconsent = ifelse(adminconsent==1, 1, 0))
GSS <- mutate(GSS, noconsent_adminconsent = ifelse(adminconsent==2, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$adminconsent, GSS$consent_adminconsent)
table(GSS$adminconsent, GSS$noconsent_adminconsent)


############                  INDEPENDENT VARIABLE                    ############
############               Time Spent on Technology                   ############

# STEP 1: Examine variable and coding schema 
hist(GSS$usetech)

# STEP 2: no recoding, treat it as a continuous variable 

# STEP 3: n/a


############                  INDEPENDENT VARIABLE                    ############
############                    Education Level                    ############

# STEP 1: Examine variable and coding schema 
table(GSS$degree)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, no_hs = ifelse(degree==0, 1, 0))
GSS <- mutate(GSS, hs = ifelse(degree==1, 1, 0))
GSS <- mutate(GSS, associates = ifelse(degree==2, 1, 0))
GSS <- mutate(GSS, bachelors = ifelse(degree==3, 1, 0))
GSS <- mutate(GSS, grad = ifelse(degree==4, 1, 0))


# STEP 3: Confirm creation (if necessary)
table(GSS$degree, GSS$no_hs)
table(GSS$degree, GSS$hs)
table(GSS$degree, GSS$associates)
table(GSS$degree, GSS$bachelors)
table(GSS$degree, GSS$grad)



############                  INDEPENDENT VARIABLE                    ############
############                        Gender                            ############

# STEP 1: Examine variable and coding schema 
table(GSS$sex)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, man = ifelse(sex==1, 1, 0))
GSS <- mutate(GSS, woman = ifelse(sex==2, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$sex, GSS$man)
table(GSS$sex, GSS$woman)


############                  INDEPENDENT VARIABLE                    ############
############  Beliefs about Government tracking sick individuals      ############

# STEP 1: Examine variable and coding schema 
table(GSS$mobilsurv)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, strg_agreems = ifelse(mobilsurv==1, 1, 0))
GSS <- mutate(GSS, agreems = ifelse(mobilsurv==2, 1, 0))
GSS <- mutate(GSS, disagreems = ifelse(mobilsurv==3, 1, 0))
GSS <- mutate(GSS, strg_disagreems = ifelse(mobilsurv==4, 1, 0))



# STEP 3: Confirm creation (if necessary)
table(GSS$mobilsurv, GSS$strg_agreems)
table(GSS$mobilsurv, GSS$agreems)
table(GSS$mobilsurv, GSS$disagreems)
table(GSS$mobilsurv, GSS$strg_disagreems)



############                  INDEPENDENT VARIABLE                    ############
############                  GSS Mode of Interview                   ############

# STEP 1: Examine variable and coding schema 
table(GSS$mode)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, in_person = ifelse(mode==1, 1, 0))
GSS <- mutate(GSS, by_phone = ifelse(mode==2, 1, 0))
GSS <- mutate(GSS, combo_personphone = ifelse(mode==3, 1, 0))
GSS <- mutate(GSS, web = ifelse(mode==4, 1, 0))



# STEP 3: Confirm creation (if necessary)
table(GSS$mode, GSS$in_person)
table(GSS$mode, GSS$by_phone)
table(GSS$mode, GSS$combo_personphone)
table(GSS$mode, GSS$web)




############                  INDEPENDENT VARIABLE                    ############
############                  Owning a Telephone                      ############

# STEP 1: Examine variable and coding schema 
table(GSS$phone)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, no_phone = ifelse(phone==1, 1, 0))
GSS <- mutate(GSS, refused_phone = ifelse(phone==2, 1, 0))
GSS <- mutate(GSS, home_phone = ifelse(phone==3, 1, 0))
GSS <- mutate(GSS, phone_elsewhere = ifelse(phone==4, 1, 0))
GSS <- mutate(GSS, no_phonelocation = ifelse(phone==5, 1, 0))
GSS <- mutate(GSS, cellphone = ifelse(phone==6, 1, 0))


# STEP 3: Confirm creation (if necessary)
table(GSS$phone, GSS$no_phone)
table(GSS$phone, GSS$refused_phone)
table(GSS$phone, GSS$home_phone)
table(GSS$phone, GSS$phone_elsewhere)
table(GSS$phone, GSS$no_phonelocation)
table(GSS$phone, GSS$cellphone)



####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("nextgen", "adminconsent", "strgagree_nextgen", "agree_nextgen", "disagree_nextgen", "strgdisagree_nextgen", "agree", "consent_adminconsent", "noconsent_adminconsent",
                "disagree", "usetech","degree", "no_hs", "hs", "associates", "bachelors", "grad", "sex", "man", "woman", "mode", "in_person", "by_phone", "combo_personphone", "web", "phone", 
                "no_phone", "refused_phone", "home_phone", "phone_elsewhere", "no_phonelocation", "cellphone")

##Step 1 variation- do this for correlation table to look better ##
my_varlist <- c("agree", "consent_adminconsent", "noconsent_adminconsent","usetech", "no_hs", "hs", "associates", "bachelors", "grad", 
                "man", "woman", "in_person", "by_phone", "combo_personphone", "web", "no_phone", "refused_phone", "cellphone")

### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE (for marginal probabilities for table 1)
describe(my_dataset)

####################################################################################
############              PHASE 4: Correlation Matrix                  ############
####################################################################################

cor(my_dataset)
##correlation between key IV and Dv##
cor(my_dataset$nextgen, my_dataset$adminconsent)



####################################################################################
############      PHASE 5: Logistic Regression Analysis                  ############
####################################################################################

##Model 1a 
model1a <- glm(agree ~ noconsent_adminconsent, data = my_dataset, family = binomial)
summary(model1a)

##relative to those who do consent to be linked to their data, those who do not consent
##to be linked to their data are 6% less likely to agree that technology creates opportunities- NOT SS 
exp(-0.059512)

##Model 2a 
model2a <- glm(agree ~ noconsent_adminconsent + usetech, data = my_dataset, family = binomial)
summary(model2a)

## as time spent on technology at work increases, respondents are 0.1% less likely to agree that technology creates opportunities holding consent to be linked to one's data constant - NOT SS
exp(-0.001492)

##Model 3a 
model3a <- glm(agree ~ noconsent_adminconsent + usetech + no_hs + associates + bachelors + grad, data = my_dataset, family = binomial)
summary(model3a)

##relative to those who have a high school degree, those with less than a high school education are 89% more likely to 
##agree that technology creates opportunities holding not consenting to be linked to one's data and time spent on technology constant - NOT SS

##those with an associates degree are 35% more likely than those with a high school degree to agree that technology creates opportunities holding all else constant - NOT SS

##those with a bachelors degree are 29% more likely than those with a hs degree to agree that tech creates opportunities holding all else constant - NOT SS

##those with a grad degree are 47% more likely than those with a hs degree to agree that tech creates opportunities holding all else constant - NOT SS

##Model 4a 
model4a <- glm(agree ~ noconsent_adminconsent + usetech + no_hs + associates + bachelors + grad + man, data = my_dataset, family = binomial)
summary(model4a)

#### respondents who identified as male are 2% less likely than women to agree that technology creates opportunities holding all else constant - NOT SS

##Model 5a 
model5a <- glm(agree ~ noconsent_adminconsent + usetech + no_hs + associates + bachelors + grad + man + in_person + by_phone + combo_personphone, data = my_dataset, family = binomial)
summary(model5a)

####respondents who had their GSS interview take place in person are 19% more likely than those who had their interview take place online to agree that tech creates opportunities holding all else constant - NOT SS
### respondents who had their interview take place by phone are 34% less likely than those who had their interview take place online to agree that tech creates opportunities holding all else constant - NOT SS
### respondents who had their interview partially in person and over the phone are 59% less likely than those who had their interview take place online to agree that tech creates opportunities holding all else constant - NOT SS

##Model 6a 
model6a <- glm(agree ~ noconsent_adminconsent + usetech + no_hs + associates + bachelors + grad + man + in_person + by_phone + combo_personphone + no_phone + refused_phone, data = my_dataset, family = binomial)
summary(model6a)

####respondents that had no phone number to provide were 51% less likely than those that provided a phone number to agree that tech creates opportunities holding all else constant - STATISTICALLY SIGNIFICANT!!!!
### respondents that refused to provide their phone number were 40% less likely than those that provided a phone number to agree that tech creates opportunities holding all else constant - NOT SS



