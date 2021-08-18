# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# PROJECT:    How Terrorism Does (and Does Not) Affect Citizens' Attitudes: 
#             A Meta-Analysis. 
# AUTHOR:     Amélie Godefroidt
# CONTACT:    amelie.godefroidt@ntnu.no
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# This R file contains the code necessary to clean the data for the Shiny App

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
rm(list=ls())

#set your own directory
#setwd("../C:/Users/ameliego/Box Sync/Articles/Article5_MetaAnalysis-ACCEPT/Article5_Papers/Meta-Analysis/4. AJPS/Godefroidt_2021_ReplicationFiles/") 
getwd()

#get necessary packages
ipak <- function(pkg){  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies=TRUE)
sapply(pkg, require, character.only=TRUE) }
packages <- c("dplyr", "readxl") 
ipak(packages)


###### A. GET THE DATA ######
#Full Data-Set of Meta-Analysis
mydata <- read_excel("Data/DatafileS2-MetaAnalysis_Data.xlsx", sheet = "Data", col_names = TRUE, skip = 3)
myotherdata <- read_excel("Data/DatafileS1-Sampling_Information.xlsx", sheet = "Final sample (N=238)", col_names = TRUE)

#Sub-Set Used for Shiny App
shinydata <- mydata %>%
  select(Fisher, Variance_F, SE_F, ID_R, ID_ES_Unique,
    country = Country, 
    design = TypeStudy,
    terrortype = Type,
    outcome = PA_Category)


###### B. RECODE THE DATA: Make Cleaner Selection Variables ######
#### 1. Re-categorize country of study #### 
summary(as.factor(shinydata$country))
shinydata$country[shinydata$country=="US"] <- "United States"
shinydata$country[shinydata$country=="Israel" |
             shinydata$country=="Israel + NI"] <- "Israel"
shinydata$country[shinydata$country=="Australia" |
             shinydata$country=="Austria" |
             shinydata$country=="Belgium" |
             shinydata$country=="Bosni?" |
             shinydata$country=="Canada" |
             shinydata$country=="Czech Republic" | 
             shinydata$country=="Denmark" |
             shinydata$country=="EU" |
             shinydata$country=="Estonia" |
             shinydata$country=="Finland" |
             shinydata$country=="France" |
             shinydata$country=="Germany" |
             shinydata$country=="Hungary" |
             shinydata$country=="Ireland" |
             shinydata$country=="Italy" |
             shinydata$country=="Luxembourg" | 
             shinydata$country=="Netherlands" |
             shinydata$country=="New Zealand" |
             shinydata$country=="Northern Ireland" |
             shinydata$country=="Norway" |
             shinydata$country=="Poland" |
             shinydata$country=="Portugal" |
             shinydata$country=="Romania" |
             shinydata$country=="Slovakia" |
             shinydata$country=="Slovenia" |
             shinydata$country=="Spain" |
             shinydata$country=="Sweden" |
             shinydata$country=="Switzerland" |
             shinydata$country=="US, UK, and Australia"|
             shinydata$country=="UK" ] <- "Other: Western"
shinydata$country[shinydata$country=="Colombia" |
                    shinydata$country=="Egypt" |
                    shinydata$country=="Morocco" |
                    shinydata$country=="Nigeria" |
                    shinydata$country=="Pakistan" |
                    shinydata$country=="Crimea" |
                    shinydata$country=="Turkey" |
                    shinydata$country=="South-Africa" ] <- "Other: Non-Western"
shinydata$country <- factor(shinydata$country,                 # Relevel group factor
                     levels = c("United States", "Israel", 
                                "Other: Western", "Other: Non-Western"))
table(shinydata$country)


x[is.na(x)]

#### 2. Re-categorize type of terror #### 
summary(as.factor(shinydata$terrortype))
shinydata$terrortype[shinydata$terrortype==0] <- "No Ideology"
shinydata$terrortype[shinydata$terrortype==1] <- "Islamist"
shinydata$terrortype[shinydata$terrortype==2] <- "Extreme Right"
shinydata$terrortype[shinydata$terrortype==3] <- "Other"
shinydata$terrortype[shinydata$terrortype==5] <- "Other"
shinydata$terrortype[shinydata$terrortype==6] <- "Other"
shinydata$terrortype <- factor(shinydata$terrortype,                 # Relevel group factor
                               levels = c("Islamist", 
                                "Extreme Right", 
                                "No Ideology", 
                                "Other"))
summary(shinydata$terrortype)

#### 3. Re-categorize type of outcome #### 
summary(as.factor(shinydata$outcome))
shinydata$outcome[shinydata$outcome == 10 |
              shinydata$outcome == 99 ] <- "Outgroup Hostility"
shinydata$outcome[shinydata$outcome == 5 |
              shinydata$outcome == 6 |
              shinydata$outcome == 7 |
              shinydata$outcome == 8 |
              shinydata$outcome == 9 |
              shinydata$outcome == 11 ] <- "Conservatism"
shinydata$outcome[shinydata$outcome == 1 |
              shinydata$outcome == 2 |
              shinydata$outcome == 3 |
              shinydata$outcome == 4 ] <- "Rally Effects"
shinydata$outcome <- factor(shinydata$outcome,                # Relevel group factor
                     levels = c("Outgroup Hostility", "Conservatism", 
                                "Rally Effects"))
table(shinydata$outcome)

#### 4. Re-categorize methodology #### 
summary(as.factor(shinydata$design))
shinydata$design[shinydata$design==1] <- "(Quasi)Experiment"
shinydata$design[shinydata$design==2] <- "(Quasi)Experiment"
shinydata$design[shinydata$design==3] <- "Correlation"
shinydata$design[shinydata$design==4] <- "Longitudinal"
shinydata$design <- factor(shinydata$design,                # Relevel group factor
                     levels = c("Correlation", 
                                "(Quasi)Experiment",
                                "Longitudinal"))
table(shinydata$design)


#### 5. Save clean datasets
write.csv(shinydata,'Code/ShinyApp/main_data.csv')
write.csv(myotherdata,'Code/ShinyApp/reports_data.csv')
