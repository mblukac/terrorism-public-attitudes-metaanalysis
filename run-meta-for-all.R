library(DescTools)
library(metafor)

# You'll need to change this to access the source file!
# or just drop it into the same folder if you are using RStudio Projects
data <- readxl::read_excel("DatafileS2-MetaAnalysis_Data.xlsx", 
                           sheet = "Data", col_names = TRUE, skip = 3)

# Subset
shinydata <- data %>%
  select(ID_R, ID_ES_Unique,
         Fisher, Variance_F, SE_F, 
         Year,
         country = Country, 
         design = TypeStudy,
         terrortype = Type,
         outcome = PA_Category)



###### B. RECODE THE DATA ######
#### 1. Re-categorize country of study #### 
summary(as.factor(shinydata$country))
shinydata$country[shinydata$country=="US"] <- "United States"
shinydata$country[shinydata$country=="Israel" |
                    shinydata$country=="Israel + NI"] <- "Israel"
shinydata$country[shinydata$country=="Australia" |
                    shinydata$country=="Austria" |
                    shinydata$country=="Belgium" |
                    shinydata$country=="Canada" |
                    shinydata$country=="Denmark" |
                    shinydata$country=="EU" |
                    shinydata$country=="Finland" |
                    shinydata$country=="France" |
                    shinydata$country=="Germany" |
                    shinydata$country=="Italy" |
                    shinydata$country=="Netherlands" |
                    shinydata$country=="New Zealand" |
                    shinydata$country=="Northern Ireland" |
                    shinydata$country=="Norway" |
                    shinydata$country=="Spain" |
                    shinydata$country=="Sweden" |
                    shinydata$country=="Switserland" |
                    shinydata$country=="US, UK, and Australia "|
                    shinydata$country=="UK" ] <- "Other: Western"
shinydata$country[is.na(shinydata$country)] <- "Other: Non-Western"
shinydata$country <- factor(shinydata$country,                 # Relevel group factor
                            levels = c("United States", "Israel", 
                                       "Other: Western", "Other: Non-Western"))
table(shinydata$country)

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



#### Meta-Analysis ------------------------------------------------------------
# Calculate for all combinations
# Get all combinations
all <- c(names(table(shinydata$country)), 
         names(table(shinydata$design)),
         names(table(shinydata$terrortype)),
         names(table(shinydata$outcome)))
set <- map(1:14, function(x) CombSet(all, x, repl = F))

# Set up empty containers and iterator
result <- list()
i <- 1
length <- sum(unlist(map(1:14, function(x) nrow(set[[x]]))))

# Calculate
for(lst in 1:length(set)){
  for(vct in 1:nrow(set[[lst]])){
    temp_vector <- set[[lst]][vct,]
    temp_data <- shinydata %>%
      filter(country %in% temp_vector,
             design %in% temp_vector,
             terrortype %in% temp_vector,
             outcome %in% temp_vector)
    
    if(nrow(temp_data) < 2){
      estimate <- NA
    } else {
      meta_fast <- rma.mv(y=Fisher, V=Variance_F,
                          random = ~ 1 | ID_R/ID_ES_Unique, 
                          data = temp_data,
                          method = "ML")
      estimate <- meta_fast$b
    }
    
    result[[i]] <- list(input = temp_vector,
                        b = estimate)
    message("Calculating round:", i, ".")
    i <- i + 1
  }
}

# Extract only viable estimates
listmis <- map(1:length(result), function(x) is.na(result[[x]]$b))
res_clean <- result[!unlist(listmis)]

saveRDS(res_clean, "all-estimates.rds")