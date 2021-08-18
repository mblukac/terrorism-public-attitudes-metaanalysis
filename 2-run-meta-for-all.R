# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# PROJECT:    How Terrorism Does (and Does Not) Affect Citizens' Attitudes: 
#             A Meta-Analysis. 
#             
# Project and paper by Amélie Godefroidt (amelie.godefroidt@ntnu.no)
# R code by Martin Lukac (m.b.lukac@gmail.com)
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# This R code contains the code necessary to compute the common effect sizes

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

library(DescTools)
library(metafor)
library(readr)
library(tidyverse)

# You'll need to change this to access the source file!
# or just drop it into the same folder if you are using RStudio Projects

shinydata <- read_csv("main_data.csv")


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
      set.seed(1234)
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
