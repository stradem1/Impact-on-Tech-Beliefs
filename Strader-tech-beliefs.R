
## Project:  SOC 302 Final Multivariate Project
# Located:   ----
# File Name: Strader-tech_beliefs.R
# Date:      5/13/2025
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

##Step 1 
my_varlist <- c("agree", "no_phone", "refused_phone","cellphone", "no_hs", "hs", "associates", "bachelors", "grad", 
                "in_person", "by_phone", "combo_personphone", "web", "usetech", "consent_adminconsent", "noconsent_adminconsent", "man", "woman")

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

##Model 1
model1 <- glm(agree ~ no_phone + refused_phone + no_hs + associates + bachelors + grad, data = my_dataset, family = binomial)
summary(model1)

##Model 2
model2 <- glm(agree ~ no_phone + refused_phone + no_hs + associates + bachelors + grad + in_person + by_phone + combo_personphone + usetech, data = my_dataset, family = binomial)
summary(model2)

##Model 3
model3 <- glm(agree ~ no_phone + refused_phone + no_hs + associates + bachelors + grad + in_person + by_phone + combo_personphone + usetech + noconsent_adminconsent, data = my_dataset, family = binomial)
summary(model3)

##Model 4
model4 <- glm(agree ~ no_phone + refused_phone + no_hs + associates + bachelors + grad + in_person + by_phone + combo_personphone + usetech + noconsent_adminconsent + man, data = my_dataset, family = binomial)
summary(model4)

