library(DescTools)
library(metaSEM)
library(readr)
library(tidyverse)

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
      meta_fast <- meta3(y=Fisher, v=Variance_F, cluster=ID_R,
                         data = temp_data)
      estimate <- coef(meta_fast)[1]
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


# OLD rma meta_fast
# meta_fast <- rma.mv(y=Fisher, V=Variance_F,
# random = ~ 1 | ID_R/ID_ES_Unique, 
# data = temp_data,
# method = "ML")
# estimate <- meta_fast$b