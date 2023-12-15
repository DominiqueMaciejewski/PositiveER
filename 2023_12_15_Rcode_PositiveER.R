#' ---
#' title: "Analyses Positive ER in daily life"
#' author: "Dominique Maciejewski"
#' output:
#'    html_document:
#'       toc: true
#' ---


# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
# 

# I want to acknowledge following ressources, which helped me coding
# Paper&Code by Weermeijer et al. (2022) https://osf.io/rcy8t?view_only=0d70ea4e0b8d4241901516131cc38cad 

# Note that the ER strategy "reflection" is reported as "savoring" in the paper!

# Options ----------------------------------------------
options(scipen = 999) 
here::i_am("2023_7_18_Rcode_PositiveER.R") #Set location of script
sessionInfo()



# Library ----------------------------------------------

library(haven) #for reading SPSS files
library(tidyverse) #data wrangling
library(multilevelTools) #for reliability
library(esmpack) #for handling ESM data
library(MplusAutomation) #for running Mplus remotely
library(multiplex) #for writing datfiles
library(rhdf5) #for writing datfiles
library(plyr)  #data wrangling
library(here) #for folder structure
library(rempsyc) #for nice word tables
library(flextable) #for nice word tables
library(lme4) #for ICCS
library(performance) #for ICCS
library(misty)  #for within/between correlation
library(readxl) #for reading excel files
library(officer) #for making wordtables
library(papaja) #for converting numbers for table
library(ggpubr) #for figures
library(ggplot2) #for figures


# Read in data ----------------------------------------------
PER_data <- haven::read_spss(here("2022 07 15 ESM data complete Mplus.sav"))

# Datapreparation ----------------------------------------------

## Selection of participants -----------------------------------

#### Count number of participants < 30 years old ----------------------------------------
PER_data %>% 
  dplyr::filter(age_yr < 30) %>% 
  dplyr::summarise(n=n_distinct(Eth_ID)) #N=201

# Exclude participants below 30 years and observations beyond 80 days 
PER_data <- PER_data %>% 
  filter(age_yr < 30 & Time < 81) %>% 
  select(-c(25,26)) #exclude in SPSS calculated NA and PA scale (will be calculated new in R)

## Calculate mean affect -----------------------------------
PER_data$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm",
                                "PA_daadk","PA_vrol","PA_dankb"), 
                              PER_data, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

PER_data$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr",
                                "NA_boos","NA_somb"), 
                              PER_data, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## Construct day variable  -----------------------------------

# Define the boundaries of the time intervals
time_intervals <- seq(1, 80, by = 5)

# Create corresponding labels for the intervals
labels <- 1:length(time_intervals)

# Categorize the 'Time' values into intervals using 'cut'
# Set right = FALSE to make the intervals left-closed and right-open
# Assign the resulting categorical values to 'PER_data$day'
PER_data$day <- cut(PER_data$Time, breaks = c(time_intervals, Inf), labels = labels, right = FALSE)

## Anonymize dataset ------------------------------------------------
# While the dataset does not contain any identifying information, we took two steps
# in order to further ensure anonymity, namely replacing the ID number with other numbers
# and reporting the age in years without decimals.

### Make new ID variable ------------------------------------------------
PER_data <- 
  PER_data %>%
  dplyr::group_by(Eth_ID) %>%
  dplyr::mutate(Eth_ID = cur_group_id()) 

### Round age to years without decimals ------------------------------------------------
PER_data$age_yr <- round(PER_data$age_yr, digits = 0)

# Generate different datasets ----------------------------------------

## Compliance  ----------------------------------------

# Calculate percentage compliance
PER_data$compliance <- PER_data$Measures_valid/ 70 * 100 

### Main dataset ----------------------------------------
## data0: 33% valid assessments as inclusion criteria (pre-registered) & NA and PA scales with all items
data0 <- PER_data %>% filter(compliance >= 33) 

### Compliance criterion abandoned ----------------------------------------
## data1: no inclusion criterion based on compliance & NA and PA scales with all items
data1 <- PER_data  

### Different NA and PA operationalizations ----------------------------------------

## data2: "PA_enth" excluded
data2 <- data0
data2$PA_mean <- combitems(c("PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data2, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data2$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data2, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data3: "PA_tevr" excluded
data3 <- data0
data3$PA_mean <- combitems(c("PA_enth","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data3, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data3$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data3, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data4: "PA_ener" excluded
data4 <- data0
data4$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data4, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data4$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data4, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data5: "PA_kalm" excluded
data5 <- data0
data5$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_daadk","PA_vrol","PA_dankb"), 
                           data5, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data5$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data5, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data6: "PA_daadk" excluded
data6 <- data0
data6$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_vrol","PA_dankb"), 
                           data6, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data6$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data6, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data7: "PA_vrol" excluded
data7 <- data0
data7$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_dankb"), 
                           data7, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data7$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data7, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data8: "PA_dankb" excluded
data8 <- data0
data8$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol"), 
                           data8, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data8$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data8, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data9: "NA_irri" excluded
data9 <- data0
data9$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data9, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data9$NA_mean <- combitems(c("NA_verv","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data9, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data10: "NA_verv" excluded
data10 <- data0
data10$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data10, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data10$NA_mean <- combitems(c("NA_irri","NA_nerv","NA_verdr","NA_boos","NA_somb"), 
                           data10, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data11: "NA_nerv" excluded
data11 <- data0
data11$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data11, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data11$NA_mean <- combitems(c("NA_irri","NA_verv","NA_verdr","NA_boos","NA_somb"), 
                           data11, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data12: "NA_verdr" excluded
data12 <- data0
data12$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data12, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data12$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_boos","NA_somb"), 
                           data12, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data13: "NA_boos" excluded
data13 <- data0
data13$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data13, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data13$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_somb"), 
                           data13, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)

## data14: "NA_somb" excluded
data14 <- data0
data14$PA_mean <- combitems(c("PA_enth","PA_tevr","PA_ener","PA_kalm","PA_daadk","PA_vrol","PA_dankb"), 
                           data14, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)
data14$NA_mean <- combitems(c("NA_irri","NA_verv","NA_nerv","NA_verdr","NA_boos"), 
                           data14, grep=FALSE, fun=mean, na.rm=TRUE, verbose=TRUE)


# Descriptive Statistics ----------------------------------------------

## Within-person means & variance of ER strategies ----------------------------------------------
ER_variables <- c("PER_att", "PER_refl", "PER_self", "PER_expr", "PER_damp", "PER_codamp", "PER_capit")

# Create an empty data frame to store the results
data0_MSD <- data.frame(Eth_ID = character(), stringsAsFactors = FALSE)

# Loop over the variables
for (var in ER_variables) {
  # Apply ddply and summarize for each variable
  temp <- ddply(data0, "Eth_ID", plyr::summarize,
                imean = mean(get(var), na.rm = TRUE),
                isd = sd(get(var), na.rm = TRUE)
  )
  
  # Rename the columns to include the variable name
  colnames(temp)[2:3] <- paste0(c("imean", "isd"), "_", var)
  
  # Merge the temporary results with the overall result
  data0_MSD <- merge(data0_MSD, temp, by = "Eth_ID", all = TRUE)
}

# Extract coefficients

descriptives <- list()

# Loop over the variables
for (var in ER_variables) {
  mean_var <- mean(data0_MSD[[paste0("imean_", var)]], na.rm = TRUE)
  min_var <- min(data0_MSD[[paste0("imean_", var)]], na.rm = TRUE)
  max_var <- max(data0_MSD[[paste0("imean_", var)]], na.rm = TRUE)
  sd_bet_var <- sd(data0_MSD[[paste0("imean_", var)]], na.rm = TRUE)
  sd_wit_var <- mean(data0_MSD[[paste0("isd_", var)]], na.rm = TRUE)
  
  # Create a data frame with the results for each variable
  result <- data.frame(
    Variable = paste0("imean_", var),
    Mean = mean_var,
    Min = min_var,
    Max = max_var,
    SD_Between = sd_bet_var,
    SD_Within = sd_wit_var,
    stringsAsFactors = FALSE
  )
  
  # Append the result to the results list
  descriptives[[var]] <- result
}

# Combine the results into a single data frame
results_desc <- do.call(rbind, descriptives)


## % zero & < 10 responses ----------------------------------------------

# Loop over variables and calculate the percentage of zeros and below 10
results_per <- data.frame(Variable = character(), Percentage_Zeros = numeric(), Percentage_Below_10 = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(ER_variables)) {
  variable <- ER_variables[i]
  
  # Calculate the percentage of zeros
  zeros <- (sum(data0[[variable]] == 0, na.rm = TRUE) / sum(!is.na(data0[[variable]]))) * 100
  
  # Calculate the percentage below 10
  below_10 <- (sum(data0[[variable]] < 10, na.rm = TRUE) / sum(!is.na(data0[[variable]]))) * 100
  
  # Create a row for the variable with percentages
  row <- data.frame(Variable = variable, Percentage_Zeros = zeros, Percentage_Below_10 = below_10)
  
  # Append the row to the results data frame
  results_per <- rbind(results_per, row)
}

# Print the results
print(results_per)

## ICC ----------------------------------------------

# Create an empty data frame to store the results
results_ICC <- data.frame(Variable = character(), ICC_adjusted = numeric())

# Loop over the variables
for (variable in ER_variables) {
  # Fit the linear mixed-effects model
  ICC_model <- lmer(as.formula(paste(variable, "~ 1 + (1 | Eth_ID)")), data = data0)
  
  # Calculate the ICC values using the performance::icc() function
  ICC <- performance::icc(ICC_model)
  
  # Create a row for the variable with ICC values
  row <- data.frame(Variable = variable, ICC_adjusted = ICC$ICC_adjusted)
  
  # Append the row to the results data frame
  results_ICC <- rbind(results_ICC, row)
}

# Print the results
print(results_ICC)


## paired t-tests ----------------------------------------------
# Subset the dataframe to include only the "imean" variables
imean_vars <- subset(data0_MSD, select = startsWith(names(data0_MSD), "imean"))
num_vars <- ncol(imean_vars)

# Create an empty dataframe to store the results of the paired t-test
paired_t_test <- data.frame(Variable1 = character(),
                            Variable2 = character(),
                            t_value = numeric(),
                            p_value = numeric(),
                            stringsAsFactors = FALSE)

# Perform pairwise t-tests and store the results (loop through all combinations)
for (i in 1:(num_vars - 1)) {
  for (j in (i + 1):num_vars) {
    # Perform paired t-test
    t_test <- t.test(imean_vars[, i], imean_vars[, j], paired = TRUE)
    
    # Store the results in the dataframe
    paired_t_test <- rbind(paired_t_test, data.frame(Variable1 = names(imean_vars)[i],
                                                     Variable2 = names(imean_vars)[j],
                                                     t_value = t_test$statistic,
                                                     p_value = t_test$p.value,
                                                     stringsAsFactors = FALSE))
  }
}

print(paired_t_test)

# # DSEM Analyses -----------------------------------------
# # Commented out now, because Mplus files were run on the supercomputer
# 
# ## Prepare files as Mplus files (dat.files) ----------------------------------------
# # Following instructions from:
# # https://garberadamc.github.io/project-site/explore-prepare-mplus-auto#prepare-datasets-remove-spss-labeling
# 
# # Save as datfile - Loop over 11 datasets
# for (i in 0:14) {
#   # Generate the filenames
#   csv_filename <- paste0("data", i, ".csv")
#   dat_filename <- paste0("data", i, ".dat")
# 
#   # Write CSV data file
#   write_csv(get(paste0("data", i)), here("data", csv_filename))
# 
#   # Read the unlabeled data back into R
#   nolabel_data <- read_csv(here("data", csv_filename))
# 
#   # Write DAT data file
#   # I save all datfiles in each of the Mplus-analysis folders, because the
#   # pathfile-name in Mplus is too long if I put in the complete path (you do not
#   # need the complete path if the datfile is in the same folder).
#   prepareMplusData(nolabel_data, here("data", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ1b", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ1c", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ2a","Anhedonia", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ2a","Anhedonia_contdep", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ2b","Anhedonia", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ2b","Anhedonia_contdep", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ2c","Anhedonia", dat_filename))
#   prepareMplusData(nolabel_data, here("Scripts","RQ2c","Anhedonia_contdep", dat_filename))
# 
# }
# 
# ## Create Mplus input files ----------------------------------------
# 
# # These are the DSEM models, but then you loop certain parts of the model.
# # Here, for instance, I will loop the datasets as well as the different ER strategies.
# # To use the createModels function, you need to create txt files, in which you specify which part of the
# # code needs to be looped.
# 
# # We made one change to the model specifications:
# # The models 1b and 2b for dampening and co-dampening did not converge with fbiterations
# # = .005 for some of the datasets.
# # For all dampening and codampening models I used the Mplus default, for which the models
# # did converge (fbiterations = .05)
# # To dtay consistent, we did it for all datasets involved in the ER strategy (e.g., when
# # data0 converged, but data5 did not, we still changed the criterion for all datasets)
# 
# # Models are created separately per hypothesis
# 
# #Hypothesis 1b: Affect t-1 --> ER t (no lag of ER)
# createModels(here("Scripts","Templates","Template_1b.txt"))
# createModels(here("Scripts","Templates","Template_1b_damp-codamp.txt"))
# 
# #Hypothesis 1c: ER t --> Affect t (+ lag of Affect (t-1))
# createModels(here("Scripts","Templates","Template_1c.txt"))
# 
# #Hypothesis 2a: Main effect of Anhedonia on ER and Affect
# createModels(here("Scripts","Templates","Template_2a.txt"))
# createModels(here("Scripts","Templates","Template_2a_contdep.txt"))
# 
# #Hypothesis 2b: Moderation of Anhedonia of Affect t-1 --> ER t (no lag of ER)
# createModels(here("Scripts","Templates","Template_2b.txt"))
# createModels(here("Scripts","Templates","Template_2b_contdep.txt"))
# createModels(here("Scripts","Templates","Template_2b_damp-codamp.txt"))
# createModels(here("Scripts","Templates","Template_2b_contdep_damp-codamp.txt"))
# 
# #Hypothesis 2c: Moderation of Anhedonia of ER t --> Affect t (+ lag of Affect (t-1))
# createModels(here("Scripts","Templates","Template_2c.txt"))
# createModels(here("Scripts","Templates","Template_2c_contdep.txt"))
# 
# 
# ## Running batches of Mplusfiles ----------------------------------------
# runModels(here("Scripts","RQ1b"))
# runModels(here("Scripts","RQ1c"))
# runModels(here("Scripts","RQ2a","Anhedonia"))
# runModels(here("Scripts","RQ2a","Anhedonia_contdep"))
# runModels(here("Scripts","RQ2b","Anhedonia"))
# runModels(here("Scripts","RQ2b","Anhedonia_contdep"))
# runModels(here("Scripts","RQ2c","Anhedonia"))
# runModels(here("Scripts","RQ2c","Anhedonia_contdep"))

## Extracting results ----------------------------------------

### Names & Functions ----------------------------------------
# Here I have names & functions of several commands that I use more often

#### Names
# Names ER strategy for tables (ER strategies in order as in analyses; alphabetically)
ER_strategy <- c("Attention", "Capitalization", "Co_dampening", "Dampening", "Expression", "Savoring", "Self_focus")

# Names variables & columns for word tables (ER strategies in order as in paper)
variables <- c("Attention", "Savoring", "Self_focus", "Expression", "Capitalization", "Dampening", "Co_dampening")
columns <- c("est", "SE", "p")

#### Function that shortens the outputnames from Mplus for easier handling
# Example usage: Output_RQ2a <- shorten_column_names(Output_RQ2a, ".*RQ2a\\.")
shorten_column_names <- function(dataframe, pattern) {
  original_names <- names(dataframe)
  shortened_names <- sub(pattern, "", original_names)
  names(dataframe) <- shortened_names
  assign("shortened_names", shortened_names, envir = parent.frame())
  return(dataframe)
}

#### Function to filter out results from main-model, drop model "ER" column (dataset data0)

# Example usage: filtered_df_rq2a <- filter_add_rearrange(estimates_df_RQ2a)
filter_data0 <- function(estimates_df) {
  # Filter rows based on ER containing "data0"
  filtered_df <- subset(estimates_df, grepl("data0", ER)) %>%
    # Drop ER column, since is the same across models & that gives a problem with binding
    select(-ER) 
  return(filtered_df)
}

#### Function to round estimate and SE column to 2 decimals
round_columns <- function(df) {
  # Select columns ending with .est or .SE
  columns_to_round <- grep("\\.est$|\\.SE$", colnames(df), value = TRUE)
  
  # Round the selected columns to 2 decimal places
  df_transformed <- df %>%
    mutate_at(all_of(vars(columns_to_round)), ~ round(., 2))
  
  return(df_transformed)
}

#### Function to restructure tables where the estimate and SE are in one column, i.e. est (SE)
restructure_table <- function(df) {
  # Rename column names with hyphens to underscores
  colnames(df) <- gsub("-", "_", colnames(df))
  
  # Create an empty data frame with the same number of rows as the original table
  transformed_df <- data.frame(ER = df$ER)
  
  # Loop through the variable names and transform the columns
  variable_names <- c("Attention", "Savoring", "Self_focus", "Expression", "Capitalization", "Dampening", "Co_dampening")
  for (variable in variable_names) {
    est_column <- paste0(variable, ".est")
    se_column <- paste0(variable, ".SE")
    p_column <- paste0(variable, ".p")
    
    est_se_values <- paste0(df[[est_column]], " (", df[[se_column]], ")")
    
    transformed_df[[paste0(variable, ".estSE")]] <- est_se_values
    transformed_df[[paste0(variable, ".p")]] <- df[[p_column]]
  }
  
  return(transformed_df)
}


### Hypothesis RQ1b ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ1b <- readModels(
  here("Scripts","RQ1b"),
  recursive=TRUE, what="parameters")

# shorten name
Output_RQ1b <- shorten_column_names(Output_RQ1b, ".*RQ1b\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ1b <- data.frame(
  ER = character(),
  PA.est = numeric(),
  PA.SE = numeric(),
  PA.p = numeric(),
  NA.est = numeric(),
  NA.SE = numeric(),
  NA.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ1b[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    PA.est = list_data[1, 3],
    PA.SE = list_data[1, 4],
    PA.p = list_data[1, 5],
    NA.est = list_data[2, 3],
    NA.SE = list_data[2, 4],
    NA.p = list_data[2, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ1b <- rbind(estimates_df_RQ1b, row)
}

# show all estimates
estimates_df_RQ1b

### Hypothesis RQ1c ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ1c <- readModels(
  here("Scripts","RQ1c"),
  recursive=TRUE, what="parameters")

# shorten name
Output_RQ1c <- shorten_column_names(Output_RQ1c, ".*RQ1c\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ1c <- data.frame(
  ER = character(),
  PA.est = numeric(),
  PA.SE = numeric(),
  PA.p = numeric(),
  NA.est = numeric(),
  NA.SE = numeric(),
  NA.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ1c[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    PA.est = list_data[1, 3],
    PA.SE = list_data[1, 4],
    PA.p = list_data[1, 5],
    NA.est = list_data[3, 3],
    NA.SE = list_data[3, 4],
    NA.p = list_data[3, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ1c <- rbind(estimates_df_RQ1c, row)
}

# show all estimates
estimates_df_RQ1c

### Hypothesis RQ2a - not controlling for depression ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ2a <- readModels(
  here("Scripts","RQ2a","Anhedonia"),
  recursive=TRUE, what="parameters")

# shorten names
Output_RQ2a <- shorten_column_names(Output_RQ2a, ".*RQ2a\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ2a <- data.frame(
  ER = character(),
  An.est = numeric(),
  An.SE = numeric(),
  An.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ2a[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    An.est = list_data[2, 3],
    An.SE = list_data[2, 4],
    An.p = list_data[2, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ2a <- rbind(estimates_df_RQ2a, row)
}

# show all estimates
estimates_df_RQ2a

### Hypothesis RQ2a - controlling for depression ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ2a_contdep <- readModels(
  here("Scripts","RQ2a","Anhedonia_contdep"),
  recursive=TRUE, what="parameters")

# shorten names
Output_RQ2a_contdep <- shorten_column_names(Output_RQ2a_contdep, ".*RQ2a\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ2a_contdep <- data.frame(
  ER = character(),
  An.est = numeric(),
  An.SE = numeric(),
  An.p = numeric(),
  Dep.est = numeric(),
  Dep.SE = numeric(),
  Dep.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ2a_contdep[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    An.est = list_data[2, 3],
    An.SE = list_data[2, 4],
    An.p = list_data[2, 5],
    Dep.est = list_data[3, 3],
    Dep.SE = list_data[3, 4],
    Dep.p = list_data[3, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ2a_contdep <- rbind(estimates_df_RQ2a_contdep, row)
}

# show all estimates
estimates_df_RQ2a_contdep

### Hypothesis RQ2b - not controlling for depression ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ2b <- readModels(
  here("Scripts","RQ2b","Anhedonia"),
  recursive=TRUE, what="parameters")

# shorten names
Output_RQ2b <- shorten_column_names(Output_RQ2b, ".*RQ2b\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ2b <- data.frame(
  ER = character(),
  PA.est = numeric(),
  PA.SE = numeric(),
  PA.p = numeric(),
  NA.est = numeric(),
  NA.SE = numeric(),
  NA.p = numeric(),
  An.est = numeric(),
  An.SE = numeric(),
  An.p = numeric(),
  PAModAn.est = numeric(),
  PAModAn.SE = numeric(),
  PAModAn.p = numeric(),
  NAModAn.est = numeric(),
  NAModAn.SE = numeric(),
  NAModAn.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ2b[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    PA.est = list_data[1, 3],
    PA.SE = list_data[1, 4],
    PA.p = list_data[1, 5],
    NA.est = list_data[2, 3],
    NA.SE = list_data[2, 4],
    NA.p = list_data[2, 5],
    An.est = list_data[13, 3],
    An.SE = list_data[13, 4],
    An.p = list_data[13, 5],
    PAModAn.est = list_data[11, 3],
    PAModAn.SE = list_data[11, 4],
    PAModAn.p = list_data[11, 5],
    NAModAn.est = list_data[12, 3],
    NAModAn.SE = list_data[12, 4],
    NAModAn.p = list_data[12, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ2b <- rbind(estimates_df_RQ2b, row)
}

# show all estimates
estimates_df_RQ2b

### Hypothesis RQ2b - controlling for depression ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ2b_contdep <- readModels(
  here("Scripts","RQ2b","Anhedonia_contdep"),
  recursive=TRUE, what="parameters")

# shorten names
Output_RQ2b_contdep <- shorten_column_names(Output_RQ2b_contdep, ".*RQ2b\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ2b_contdep <- data.frame(
  ER = character(),
  PA.est = numeric(),
  PA.SE = numeric(),
  PA.p = numeric(),
  NA.est = numeric(),
  NA.SE = numeric(),
  NA.p = numeric(),
  An.est = numeric(),
  An.SE = numeric(),
  An.p = numeric(),
  Dep.est = numeric(),
  Dep.SE = numeric(),
  Dep.p = numeric(),
  PAModAn.est = numeric(),
  PAModAn.SE = numeric(),
  PAModAn.p = numeric(),
  NAModAn.est = numeric(),
  NAModAn.SE = numeric(),
  NAModAn.p = numeric(),
  PAModDep.est = numeric(),
  PAModDep.SE = numeric(),
  PAModDep.p = numeric(),
  NAModDep.est = numeric(),
  NAModDep.SE = numeric(),
  NAModDep.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ2b_contdep[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    PA.est = list_data[1, 3],
    PA.SE = list_data[1, 4],
    PA.p = list_data[1, 5],
    NA.est = list_data[2, 3],
    NA.SE = list_data[2, 4],
    NA.p = list_data[2, 5],
    An.est = list_data[15, 3],
    An.SE = list_data[15, 4],
    An.p = list_data[15, 5],
    Dep.est = list_data[16, 3],
    Dep.SE = list_data[16, 4],
    Dep.p = list_data[16, 5],
    PAModAn.est = list_data[11, 3],
    PAModAn.SE = list_data[11, 4],
    PAModAn.p = list_data[11, 5],
    NAModAn.est = list_data[13, 3],
    NAModAn.SE = list_data[13, 4],
    NAModAn.p = list_data[13, 5],
    PAModDep.est = list_data[12, 3],
    PAModDep.SE = list_data[12, 4],
    PAModDep.p = list_data[12, 5],
    NAModDep.est = list_data[14, 3],
    NAModDep.SE = list_data[14, 4],
    NAModDep.p = list_data[14, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ2b_contdep <- rbind(estimates_df_RQ2b_contdep, row)
}

# show all estimates
estimates_df_RQ2b_contdep


### Hypothesis RQ2c - not controlling for depression ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ2c <- readModels(
  here("Scripts","RQ2c","Anhedonia"),
  recursive=TRUE, what="parameters")

# shorten names
Output_RQ2c <- shorten_column_names(Output_RQ2c, ".*RQ2c\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ2c <- data.frame(
  ER = character(),
  ERPA.est = numeric(),
  ERPA.SE = numeric(),
  ERPA.p = numeric(),
  ERNA.est = numeric(),
  ERNA.SE = numeric(),
  ERNA.p = numeric(),
  AnPA.est = numeric(),
  AnPA.SE = numeric(),
  AnPA.p = numeric(),
  AnNA.est = numeric(),
  AnNA.SE = numeric(),
  AnNA.p = numeric(),
  PAModAn.est = numeric(),
  PAModAn.SE = numeric(),
  PAModAn.p = numeric(),
  NAModAn.est = numeric(),
  NAModAn.SE = numeric(),
  NAModAn.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ2c[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    ERPA.est = list_data[1, 3],
    ERPA.SE = list_data[1, 4],
    ERPA.p = list_data[1, 5],
    ERNA.est = list_data[3, 3],
    ERNA.SE = list_data[3, 4],
    ERNA.p = list_data[3, 5],
    AnPA.est = list_data[12, 3],
    AnPA.SE = list_data[12, 4],
    AnPA.p = list_data[12, 5],
    AnNA.est = list_data[13, 3],
    AnNA.SE = list_data[13, 4],
    AnNA.p = list_data[13, 5],
    PAModAn.est = list_data[10, 3],
    PAModAn.SE = list_data[10, 4],
    PAModAn.p = list_data[10, 5],
    NAModAn.est = list_data[11, 3],
    NAModAn.SE = list_data[11, 4],
    NAModAn.p = list_data[11, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ2c <- rbind(estimates_df_RQ2c, row)
}

# show all estimates
estimates_df_RQ2c

### Hypothesis RQ2c - controlling for depression ---------------------

#### Read models --------------------- 
# Extract parameters
Output_RQ2c_contdep <- readModels(
  here("Scripts","RQ2c","Anhedonia_contdep"),
  recursive=TRUE, what="parameters")

# shorten names
Output_RQ2c_contdep <- shorten_column_names(Output_RQ2c_contdep, ".*RQ2c\\.")

#### Loop extraction of parameters over all models --------------------- 

# Define the model names (these are just the shortened model names)
models <- shortened_names

# Create an empty data frame to store the estimates
estimates_df_RQ2c_contdep <- data.frame(
  ER = character(),
  ERPA.est = numeric(),
  ERPA.SE = numeric(),
  ERPA.p = numeric(),
  ERNA.est = numeric(),
  ERNA.SE = numeric(),
  ERNA.p = numeric(),
  AnPA.est = numeric(),
  AnPA.SE = numeric(),
  AnPA.p = numeric(),
  AnNA.est = numeric(),
  AnNA.SE = numeric(),
  AnNA.p = numeric(),
  DepPA.est = numeric(),
  DepPA.SE = numeric(),
  DepPA.p = numeric(),
  DepNA.est = numeric(),
  DepNA.SE = numeric(),
  DepNA.p = numeric(),
  PAModAn.est = numeric(),
  PAModAn.SE = numeric(),
  PAModAn.p = numeric(),
  NAModAn.est = numeric(),
  NAModAn.SE = numeric(),
  NAModAn.p = numeric(),
  PAModDep.est = numeric(),
  PAModDep.SE = numeric(),
  PAModDep.p = numeric(),
  NAModDep.est = numeric(),
  NAModDep.SE = numeric(),
  NAModDep.p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all models
for (model in models) {
  # Extract standardized results
  list_data <- Output_RQ2c_contdep[[model]]$parameters$stdyx.standardized
  
  # Create a row for the current list
  row <- data.frame(
    ER = model,
    ERPA.est = list_data[1, 3],
    ERPA.SE = list_data[1, 4],
    ERPA.p = list_data[1, 5],
    ERNA.est = list_data[3, 3],
    ERNA.SE = list_data[3, 4],
    ERNA.p = list_data[3, 5],
    AnPA.est = list_data[14, 3],
    AnPA.SE = list_data[14, 4],
    AnPA.p = list_data[14, 5],
    AnNA.est = list_data[16, 3],
    AnNA.SE = list_data[16, 4],
    AnNA.p = list_data[16, 5],
    DepPA.est = list_data[15, 3],
    DepPA.SE = list_data[15, 4],
    DepPA.p = list_data[15, 5],
    DepNA.est = list_data[17, 3],
    DepNA.SE = list_data[17, 4],
    DepNA.p = list_data[17, 5],
    PAModAn.est = list_data[10, 3],
    PAModAn.SE = list_data[10, 4],
    PAModAn.p = list_data[10, 5],
    NAModAn.est = list_data[12, 3],
    NAModAn.SE = list_data[12, 4],
    NAModAn.p = list_data[12, 5],
    PAModDep.est = list_data[11, 3],
    PAModDep.SE = list_data[11, 4],
    PAModDep.p = list_data[11, 5],
    NAModDep.est = list_data[13, 3],
    NAModDep.SE = list_data[13, 4],
    NAModDep.p = list_data[13, 5],
    stringsAsFactors = FALSE
  )
  
  # Add the row to the estimates data frame (new row for every looped model)
  estimates_df_RQ2c_contdep <- rbind(estimates_df_RQ2c_contdep, row)
}

# show all estimates
estimates_df_RQ2c_contdep

# Results reported in the paper ----------------------------------

## Estimates reported in paper ----------------------------------

### Participants section ----------------------------------

#### Comparing included and excluded participants ----------------------------------------
# Calculate within-person aggregates of study variables

# define variables 
stud_variables <- c("compliance",  "age_yr", "gesl", "Anhedonia", "BDI_sum", "NA_mean", "PA_mean", 
                    "PER_att", "PER_refl", "PER_self", "PER_expr", "PER_damp", "PER_codamp", "PER_capit")

# Create an empty data frame to store the results
data_M <- data.frame(Eth_ID = character(), stringsAsFactors = FALSE)

# Loop over the variables
for (var in stud_variables) {
  # Apply ddply and summarize for each variable
  temp <- ddply(PER_data, "Eth_ID", plyr::summarize,
                imean = mean(get(var), na.rm = TRUE)
  )
  
  # Rename the columns to include the variable name
  colnames(temp)[2] <- paste0(c("imean"), "_", var)
  
  # Merge the temporary results with the overall result
  data_M <- merge(data_M, temp, by = "Eth_ID", all = TRUE)
}

# Calculate inclusion variable
data_M<-data_M %>% 
  dplyr::mutate(inclusion = ifelse(imean_compliance >= 33,
                                   1,0))

# No. participants
table(data_M$inclusion)

# Compare included and excluded participants on all continuous study variables
# group 0 = excluded, group 1 = included

# Create an empty dataframe to store the results of the paired t-test
t_test_results <- data.frame(Variable = character(),
                             mean_exc = numeric(),
                             mean_inc = numeric(),
                             t_value = numeric(),
                             p_value = numeric(),
                             stringsAsFactors = FALSE)

# Perform pairwise t-tests and store the results (loop through all combinations)
for (i in 3:15) {
  # Perform independent t-test
  t_test <- t.test(data_M[, i] ~ data_M$inclusion) 
  
  # Store the results in the dataframe
  t_test_results <- rbind(t_test_results, data.frame(Variable = names(data_M)[i],
                                                     mean_exc = t_test$estimate[["mean in group 0"]],
                                                     mean_inc = t_test$estimate[["mean in group 1"]],
                                                     t_value = t_test$statistic,
                                                     p_value = t_test$p.value,
                                                     stringsAsFactors = FALSE))
}

print(t_test_results)

# Compare included and excluded participants on gender (1=male, 2=female)
gen_inc<-table(data_M$imean_gesl, data_M$inclusion)
prop.table(gen_inc,2)
chisq.test(gen_inc) 

## Identify rows where p_value > 0.05/ >.05
t_test_results[t_test_results$p_value > 0.05, ]
t_test_results[t_test_results$p_value < 0.05, ]

# Identify min and max of t's and p's for (in)significant comparisons
## Subset rows with p_value < 0.05/ >.05
significant_rows <- subset(t_test_results, p_value < 0.05)
nonsignificant_rows <- subset(t_test_results, p_value > 0.05)
## Calculate absolute values of t_value
significant_rows$abs_t_value <- abs(significant_rows$t_value)
nonsignificant_rows$abs_t_value <- abs(nonsignificant_rows$t_value)

## Find minimum and maximum values
min(significant_rows$abs_t_value)
max(significant_rows$abs_t_value)
max(significant_rows$p_value)
min(nonsignificant_rows$p_value)

#### Demographics ----------------------------------------
# Person-level dataset for demographics
demo <- ddply(data0, "Eth_ID", plyr::summarize,
              age = mean(age_yr), na.rm = TRUE,
              gender = mean(gesl), na.rm = TRUE)

# age
psych::describe(demo$age)

# gender
gen<-table(demo$gender)
prop.table(gen)

### Procedure section ----------------------------------

#### Compliance ----------------------------------
# Individual
comp <- ddply(data0, "Eth_ID", plyr::summarize,
              compliance = mean(compliance), na.rm = TRUE,
              Measures_valid = mean(Measures_valid), na.rm = TRUE)

mean(comp$compliance)
sd(comp$compliance)

mean(comp$compliance)
sd(comp$compliance)
mean(comp$Measures_valid)
sd(comp$Measures_valid)
min(comp$Measures_valid)
max(comp$Measures_valid)

# Valid measures
sum(comp$Measures_valid)

### Measures Section ----------------------------------------

#### Reliability-analyses PA/NA ----------------------------------------------
omega_PA <- omegaSEM(
  items = c("PA_enth","PA_tevr","PA_ener","PA_kalm",
            "PA_daadk","PA_vrol","PA_dankb"),
  id = "Eth_ID",
  data = data0,
  savemodel = FALSE)

omega_PA

omega_NA <- omegaSEM(
  items = c("NA_irri","NA_verv","NA_nerv","NA_verdr",
            "NA_boos","NA_somb"),
  id = "Eth_ID",
  data = data0,
  savemodel = FALSE)

omega_NA

#### Histograms ESM variables ----------------------------------------------
# to see whether moving the slider will lead to a bimodal distribution (see work by Hasslbeck & Ryan)

ggplot_hist <- function(variable, axis_title) {
  ggplot(data0, aes(x=variable)) + geom_histogram(bins=101,color="black", fill="steelblue") +
    labs(x = axis_title, y = "Frequency") +
    theme_minimal()
}

hist_PA_enth <- ggplot_hist(data0$PA_enth, "PA item enthusiastic")
hist_PA_tevr <- ggplot_hist(data0$PA_tevr, "PA item satisfied")
hist_PA_ener <- ggplot_hist(data0$PA_ener, "PA item energetic")
hist_PA_kalm <- ggplot_hist(data0$PA_kalm, "PA item calm")
hist_PA_daadk <- ggplot_hist(data0$PA_daadk, "PA item determined")
hist_PA_vrol <- ggplot_hist(data0$PA_vrol, "PA item cheerful")
hist_PA_dankb <- ggplot_hist(data0$PA_dankb, "PA item grateful")
hist_NA_irri <- ggplot_hist(data0$NA_irri, "NA item irritated")
hist_NA_verv <- ggplot_hist(data0$NA_verv, "NA item bored")
hist_NA_nerv <- ggplot_hist(data0$NA_nerv, "NA item nervous")
hist_NA_verdr <- ggplot_hist(data0$NA_verdr, "NA item sad")
hist_NA_boos <- ggplot_hist(data0$NA_boos, "NA item angry")
hist_NA_somb <- ggplot_hist(data0$NA_somb, "NA item low")
hist_PER_att <- ggplot_hist(data0$PER_att, "ER item attention")
hist_PER_refl <- ggplot_hist(data0$PER_refl, "ER item savoring")
hist_PER_self <- ggplot_hist(data0$PER_self, "ER item self_focus")
hist_PER_expr <- ggplot_hist(data0$PER_expr, "ER item expression")
hist_PER_damp <- ggplot_hist(data0$PER_damp, "ER item dampening")
hist_PER_codamp <- ggplot_hist(data0$PER_codamp, "ER item co_dampening")
hist_PER_capit <- ggplot_hist(data0$PER_capit, "ER item capitalization")

# Arrange plots using ggarrange
pa_plot <- ggarrange(hist_PA_enth, hist_PA_tevr, hist_PA_ener, hist_PA_kalm,
                     hist_PA_daadk, hist_PA_vrol, hist_PA_dankb,
                     ncol = 3, nrow=3)
na_plot <- ggarrange(hist_NA_irri, hist_NA_verv, hist_NA_nerv, hist_NA_verdr,
                     hist_NA_boos,hist_NA_somb,
                     ncol = 3, nrow=2)
per_plot <- ggarrange(hist_PER_att, hist_PER_refl, hist_PER_self, hist_PER_expr,
                      hist_PER_damp, hist_PER_codamp, hist_PER_capit,
                      ncol = 3, nrow=3)

# Save plots
ggsave("Figures/pa_plot.png", pa_plot, width = 10, height = 7)
ggsave("Figures/na_plot.png", na_plot, width = 10, height = 7)
ggsave("Figures/per_plot.png", per_plot, width = 10, height = 7)

## Table 1: Descriptives ----------------------------------

# Add item formulations
item_form_ER <- c("I consciously noticed and paid attention to my feelings", #Attention
                  "I shared or celebrated my feelings with others", #Capitalization
                  "I talked with others in a negative way about my positive feelings, for example reminding each other that these positive feelings won't last long", #Co-dampening
                  "I had negative thoughts about the positive feelings and the situation she caused", #Dampening
                  "I expressed my positive feelings, for example by laughing",  #Expression
                  "I thought about how happy/vigorous/satisfied/ cheerful/energetic/calm etc. I feel", #Savoring
                  "I patted myself on the back") #Self-focus

# bind all results, sort by Variable and add ER strategy names, 
# drop variable column, sort by ER strategy as presented in paper
res_table1 <- arrange(cbind(results_desc, results_per, results_ICC), Variable) %>%
  cbind(ER_strategy, item_form_ER, .) %>%
  select(-contains("Variable")) %>%
  arrange(factor(ER_strategy, levels = c('Attention', 'Savoring', 'Self_focus', 
                                         'Expression', 'Capitalization', 'Dampening', 'Co_dampening'))) 





print(res_table1)

# Information for note table
## Identify rows where p_value > 0.05
paired_t_test[paired_t_test$p_value > 0.05, ]

# Identify min and max of t's and p's for significant comparisons
## Subset rows with p_value < 0.05
significant_rows <- subset(paired_t_test, p_value < 0.05)
## Calculate absolute values of t_value
significant_rows$abs_t_value <- abs(significant_rows$t_value)

## Find minimum and maximum values
min(significant_rows$abs_t_value)
max(significant_rows$abs_t_value)
max(significant_rows$p_value)

## make table 
table_1 <- nice_table(res_table1,
                      italics = seq(res_table1),
                      stars = FALSE,
                      title = c("Table 1", "Descriptive Statistics for the Positive ER Strategies"),
                      note = c("ER = Emotion regulation, M = Mean, SD = Standard Deviation, Min = Minimum, Max = Maximum, ICC = Intra-Class Correlation.",
                               "Mean, SD between, minimum and maximum represent the descriptives of the person-aggregated scores for each ER strategy. The within-person SD for each ER strategy represents the within-strategy variability. Paired-samplt t-tests indicated that mean levels of self-focus and capitalization did not significantly differ from each other (t = 1.30, p = .19), all other mean levels were significantly different (t’s ranging between 3.01 and 20.17, all ps < .003)).",
                               "Note that in the pre-registration, we accidentally said we would conduct one-sample t-tests for comparing the different ER strategies, although this should have been paired sample t-tests."
                      )
)

table_1 <- table_1 %>%
  set_header_labels(
    ER_strategy = "ER strategy",
    item_form_ER = "Wording",
    SD_Between = "SD between",
    SD_Within = "SD within",
    Percentage_Zeros = "% 0 responses",
    Percentage_Below_10 = "% <10 responses",
    ICC_adjusted = "ICC")

print(table_1)

## save table as word file
flextable::save_as_docx(table_1, path = here("Tables", "table_1.docx"))


## Table 2: Correlations ----------------------------------

# define variables for cor-matrix
studyvars<-c("PER_att", "PER_refl", "PER_self",
             "PER_expr","PER_capit","PER_damp", "PER_codamp")

# calculate within-person and between-person correlation and save results as excelfile
multilevel.cor(data0[, studyvars],
               cluster = data0$Eth_ID, 
               split = FALSE, sig = TRUE, 
               tri.lower = FALSE,
               print = c("cor", "p"),
               write = "correlation")

# Read data from Excel sheets (results of multilevel correlations)
tibble_a <- readxl::read_excel("correlation.xlsx", sheet = 2)
tibble_b <- readxl::read_excel("correlation.xlsx", sheet = 3)

# Create a copy of the original dataframe
p_stars <- as.data.frame(tibble_b)
colnames(p_stars)[1] <- "variables"

# Loop through the dataframe and assign star ratings based on the criteria
for (i in 1:nrow(p_stars)) {
  for (j in 1:ncol(p_stars)) {
    if (!is.na(p_stars[i, j])) {
      if (p_stars[i, j] < 0.001) {
        p_stars[i, j] <- "***"
      } else if (p_stars[i, j] < 0.01) {
        p_stars[i, j] <- "**"
      } else if (p_stars[i, j] < 0.05) {
        p_stars[i, j] <- "*"
      } else {
        p_stars[i, j] <- ""
      }
    }
  }
}



# Remove the 'variables' column from the new dataframe
p_stars<-cbind(studyvars,p_stars)
p_stars$variables <- NULL

# Create a copy of the original dataframe
res_table2 <- as.data.frame(tibble_a)
colnames(res_table2)[1] <- "variables"

# Format numbers to delete leading zero
res_table2 <- apa_num(res_table2, gt1=FALSE)

# convert diagnoale to empty
res_table2 <- res_table2 %>% mutate_at(vars(-1), ~ ifelse(. == "> .99", " ", .))

# Loop through the dataframe and add stars from p_stars based on the criteria
for (i in 1:nrow(res_table2)) {
  for (j in 1:ncol(res_table2)) {
    if (!is.na(p_stars[i, j])) {
      if (p_stars[i, j] %in% c("***", "**", "*")) {
        res_table2[i, j] <- paste(res_table2[i, j], p_stars[i, j], sep = "")
      }
    }
  }
}

# Sort studyvariables for merging
res_table2<-arrange(res_table2, studyvars)

# bind all results, sort by Variable and add ER strategy names, 
# drop variable column, sort by ER strategy as presented in paper
res_table2 <- cbind(ER_strategy,res_table2) %>%
  select(-contains("variables")) %>%
  arrange(factor(ER_strategy, levels = c('Attention', 'Savoring', 'Self_focus', 
                                         'Expression', 'Capitalization', 'Dampening', 'Co_dampening')))

## make table 
table_2 <- nice_table(res_table2,
                      italics = seq(res_table2),
                      stars = FALSE,
                      title = c("Table 2", "Within-person and between-person correlation of ER strategies"),
                      note = c("ER = Emotion regulation. Above the diagonal represent the within-person correlations, below the diagonal between-person correlations.",
                               "* p < .05, ** p < .01, *** p < .001"
                      )
)



table_2 <- table_2 %>%
  set_header_labels(
    ER_strategy = "ER strategy",
    PER_att = "1.",
    PER_refl = "2.",
    PER_self = "3.",
    PER_expr = "4.",
    PER_capit = "5." ,
    PER_damp = "6.",
    PER_codamp = "7.")

print(table_2)

## save table as word file
flextable::save_as_docx(table_2, path = here("Tables", "table_2.docx"))

## Table 3: Influence of Emotions t-1 -> ER t ----------------------------------
## Main effects of Emotions t-1 -> ER t (Hypothesis 1b)
## Main effects of Anhedonia -> ER t (Hypothesis 2a)
## Moderation effects of Emotions t-1*Anhedonia -> ER t (Hypothesis 2b)

# Bind all results
Results1b<-filter_data0(estimates_df_RQ1b)
Results2a<-filter_data0(estimates_df_RQ2a)
Results2b<-filter_data0(estimates_df_RQ2b)

res_table3<-cbind(Results1b,Results2a,Results2b)

# Add ER_strategy column
res_table3<-cbind(ER_strategy,res_table3)

# Re-arrange table
res_table3 <- res_table3 %>%
  arrange(factor(ER_strategy, levels = c('Attention', 'Savoring', 'Self_focus', 'Expression', 'Capitalization', 'Dampening', 'Co_dampening')))

# Transpose and restructure the estimates dataframe so that ER strategies are the columns
# Create an empty dataframe
res_table3_t <- data.frame(stringsAsFactors = FALSE, row.names = c(1L:5L))

# Add the ER column
res_table3_t$ER <- c("Positive emotions t-1 on ER t","Negative emotions t-1 on ER t",
                     "Anhedonia on ER t",
                     "Positive emotions t-1*Anhedonia on ER t", "Negative emotions t-1*Anhedonia on ER t")



# Loop through the variables and add the columns to the dataframe
for (i in 1:length(variables)) {
  for (j in 1:length(columns)) {
    column_name <- paste0(variables[i], ".", columns[j])
    res_table3_t[, column_name] <- c(res_table3[i, paste0("PA.", columns[j])],
                                     res_table3[i, paste0("NA.", columns[j])],
                                     res_table3[i, paste0("An.", columns[j])],
                                     res_table3[i, paste0("PAModAn.", columns[j])],
                                     res_table3[i, paste0("NAModAn.", columns[j])])
  }
}

# Print the transformed dataframe
print(res_table3_t)

# Rounding columns with .est and .SE
res_table3_t <- round_columns(res_table3_t)

# Restructure table so that est(SE) are in the same column (for the table)
res_table3_transformed <- restructure_table(res_table3_t)
print(res_table3_transformed)

## make table with multilevel headers
## see https://rempsyc.remi-theriault.com/articles/table#special-situation-multilevel-headers

table_3 <- nice_table(res_table3_transformed,
                      separate.header = TRUE,
                      italics = seq(res_table3_transformed),
                      stars = FALSE,
                      title = c("Table 3", "Emotions at t-1 and the Use of Positive ER Strategies at t - Within-Person Effects and Moderation by Anhedonia"),
                      note = c("ER = Emotion regulation. Estimates are within-level standardized.",
                               "Effects that were significant after applying the FDR correction (p < .023016) are displayed in bold."))

print(table_3)

## save table as word file
flextable::save_as_docx(table_3, path = here("Tables", "table_3.docx"))




## Table 4: Influence of ER t -> Emotions t (controlled for depression) ----------------------------------
## Main effects of ER t -> Emotions t (Hypothesis 1b)
## Main effects of Anhedonia/Depression -> Emotions t (not hypothesized, but included for moderation)
## Moderation effects of ER t*Anhedonia/Depression -> Emotions t (Hypothesis 2b)

# Bind all results
# Note that the main effects from anhedonia on emotions was tested in model 2c
Results1c<-filter_data0(estimates_df_RQ1c)
Results2c<-filter_data0(estimates_df_RQ2c)

res_table4<-cbind(Results1c,Results2c)

# Add ER_strategy column
res_table4<-cbind(ER_strategy,res_table4)

# Re-arrange table
res_table4 <- res_table4 %>%
  arrange(factor(ER_strategy, levels = c('Attention', 'Savoring', 'Self_focus', 'Expression', 'Capitalization', 'Dampening', 'Co_dampening')))

# Transpose and restructure the estimates dataframe so that ER strategies are the columns
# Create an empty dataframe
res_table4_t <- data.frame(stringsAsFactors = FALSE, row.names = c(1L:6L))

# Add the ER column
res_table4_t$ER <- c("ER t on positive emotions t","ER t on negative emotions t",
                     "Anhedonia on positive emotions", "Anhedonia on negative emotions",
                     "ER t*Anhedonia on positive emotions t", "ER t*Anhedonia on negative emotions t")



# Loop through the variables and add the columns to the dataframe
for (i in 1:length(variables)) {
  for (j in 1:length(columns)) {
    column_name <- paste0(variables[i], ".", columns[j])
    res_table4_t[, column_name] <- c(res_table4[i, paste0("PA.", columns[j])],
                                     res_table4[i, paste0("NA.", columns[j])],
                                     res_table4[i, paste0("AnPA.", columns[j])],
                                     res_table4[i, paste0("AnNA.", columns[j])],
                                     res_table4[i, paste0("PAModAn.", columns[j])],
                                     res_table4[i, paste0("NAModAn.", columns[j])])
  }
}

# Print the transformed dataframe
print(res_table4_t)

# Rounding columns with .est and .SE
res_table4_t <- round_columns(res_table4_t)

# Restructure table so that est(SE) are in the same column (for the table)
res_table4_transformed <- restructure_table(res_table4_t)
print(res_table4_transformed)

## make table with multilevel headers
## see https://rempsyc.remi-theriault.com/articles/table#special-situation-multilevel-headers

table_4 <- nice_table(res_table4_transformed,
                      separate.header = TRUE,
                      italics = seq(res_table4_transformed),
                      stars = FALSE,
                      title = c("Table 4", "Use of Positive ER strategies at t and Emotions at t - Within-Person Effects and Moderation by Anhedonia"),
                      note = c("ER = Emotion regulation. Estimates are within-level standardized.",
                               "Effects that were significant after applying the FDR correction (p < .023016) are displayed in bold."))

print(table_4)

## save table as word file
flextable::save_as_docx(table_4, path = here("Tables", "table_4.docx"))

## Supplementary Table 1: Influence of Emotions t-1 -> ER t (controlled for depression) ----------------------------------
## Main effects of Emotions t-1 -> ER t (Hypothesis 1b)
## Main effects of Anhedonia/Depression -> ER t (Hypothesis 2a)
## Moderation effects of Emotions t-1*Anhedonia/Depression -> ER t (Hypothesis 2b)

# Bind all results
Results1b<-filter_data0(estimates_df_RQ1b)
Results2a_contdep<-filter_data0(estimates_df_RQ2a_contdep)
Results2b_contdep<-filter_data0(estimates_df_RQ2b_contdep)

res_tablesup1<-cbind(Results1b,Results2a_contdep,Results2b_contdep)

# Add ER_strategy column
res_tablesup1<-cbind(ER_strategy,res_tablesup1)

# Re-arrange table
res_tablesup1 <- res_tablesup1 %>%
  arrange(factor(ER_strategy, levels = c('Attention', 'Savoring', 'Self_focus', 'Expression', 'Capitalization', 'Dampening', 'Co_dampening')))

# Transpose and restructure the estimates dataframe so that ER strategies are the columns
# Create an empty dataframe
res_tablesup1_t <- data.frame(stringsAsFactors = FALSE, row.names = c(1L:8L))

# Add the ER column
res_tablesup1_t$ER <- c("Positive emotions t-1 on ER t","Negative emotions t-1 on ER t",
                        "Anhedonia on ER t", "Depression on ER t",
                        "Positive emotions t-1*Anhedonia on ER t", "Negative emotions t-1*Anhedonia on ER t",
                        "Positive emotions t-1*Depression on ER t", "Negative emotions t-1*Depression on ER t")



# Loop through the variables and add the columns to the dataframe
for (i in 1:length(variables)) {
  for (j in 1:length(columns)) {
    column_name <- paste0(variables[i], ".", columns[j])
    res_tablesup1_t[, column_name] <- c(res_tablesup1[i, paste0("PA.", columns[j])],
                                        res_tablesup1[i, paste0("NA.", columns[j])],
                                        res_tablesup1[i, paste0("An.", columns[j])],
                                        res_tablesup1[i, paste0("Dep.", columns[j])],
                                        res_tablesup1[i, paste0("PAModAn.", columns[j])],
                                        res_tablesup1[i, paste0("NAModAn.", columns[j])],
                                        res_tablesup1[i, paste0("PAModDep.", columns[j])],
                                        res_tablesup1[i, paste0("NAModDep.", columns[j])])
  }
}

# Print the transformed dataframe
print(res_tablesup1_t)

# Rounding columns with .est and .SE
res_tablesup1_t <- round_columns(res_tablesup1_t)

# Restructure table so that est(SE) are in the same column (for the table)
res_tablesup1_transformed <- restructure_table(res_tablesup1_t)
print(res_tablesup1_transformed)

## make table with multilevel headers
## see https://rempsyc.remi-theriault.com/articles/table#special-situation-multilevel-headers

table_sup1 <- nice_table(res_tablesup1_transformed,
                         separate.header = TRUE,
                         italics = seq(res_tablesup1_transformed),
                         stars = FALSE,
                         title = c("Supplementary Table 1", "Emotions at t-1 and the Use of Positive ER Strategies at t - Within-Person Effects and Moderation by Anhedonia and Depression"),
                         note = c("ER = Emotion regulation. Estimates are within-level standardized.",
                                  "Effects that were significant after applying the FDR correction (p < .023016) are displayed in bold."))

print(table_sup1)

## save table as word file
flextable::save_as_docx(table_sup1, path = here("Tables", "table_sup1.docx"))


## Supplementary Table 2: Influence of Emotions t-1 -> Emotions t (controlled for depression) ----------------------------------
## Main effects of Emotions t-1 -> Emotions t (Hypothesis 1b)
## Main effects of Anhedonia/Depression -> Emotions t (Hypothesis 2a)
## Moderation effects of ER t*Anhedonia/Depression -> Emotions t (Hypothesis 2b)

# Bind all results
# Note that the main effects from anhedonia on emotions was tested in model 2c
Results1c<-filter_data0(estimates_df_RQ1c)
Results2c_contdep<-filter_data0(estimates_df_RQ2c_contdep)

res_tablesup2<-cbind(Results1c,Results2c_contdep)

# Add ER_strategy column
res_tablesup2<-cbind(ER_strategy,res_tablesup2)

# Re-arrange table
res_tablesup2 <- res_tablesup2 %>%
  arrange(factor(ER_strategy, levels = c('Attention', 'Savoring', 'Self_focus', 'Expression', 'Capitalization', 'Dampening', 'Co_dampening')))

# Transpose and restructure the estimates dataframe so that ER strategies are the columns
# Create an empty dataframe
res_tablesup2_t <- data.frame(stringsAsFactors = FALSE, row.names = c(1L:10L))

# Add the ER column
res_tablesup2_t$ER <- c("ER t on positive emotions t","ER t on negative emotions t",
                     "Anhedonia on positive emotions", "Anhedonia on negative emotions",
                     "Depression on positive emotions", "Depression on negative emotions",
                     "ER t*Anhedonia on positive emotions t", "ER t*Anhedonia on negative emotions t",
                     "ER t*Depression on positive emotions t", "ER t*Depression on negative emotions t")



# Loop through the variables and add the columns to the dataframe
for (i in 1:length(variables)) {
  for (j in 1:length(columns)) {
    column_name <- paste0(variables[i], ".", columns[j])
    res_tablesup2_t[, column_name] <- c(res_tablesup2[i, paste0("PA.", columns[j])],
                                     res_tablesup2[i, paste0("NA.", columns[j])],
                                     res_tablesup2[i, paste0("AnPA.", columns[j])],
                                     res_tablesup2[i, paste0("AnNA.", columns[j])],
                                     res_tablesup2[i, paste0("DepPA.", columns[j])],
                                     res_tablesup2[i, paste0("DepNA.", columns[j])],
                                     res_tablesup2[i, paste0("PAModAn.", columns[j])],
                                     res_tablesup2[i, paste0("NAModAn.", columns[j])],
                                     res_tablesup2[i, paste0("PAModDep.", columns[j])],
                                     res_tablesup2[i, paste0("NAModDep.", columns[j])])
  }
}

# Print the transformed dataframe
print(res_tablesup2_t)

# Rounding columns with .est and .SE
res_tablesup2_t <- round_columns(res_tablesup2_t)

# Restructure table so that est(SE) are in the same column (for the table)
res_tablesup2_transformed <- restructure_table(res_tablesup2_t)
print(res_tablesup2_transformed)

## make table with multilevel headers
## see https://rempsyc.remi-theriault.com/articles/table#special-situation-multilevel-headers

table_sup2 <- nice_table(res_tablesup2_transformed,
                      separate.header = TRUE,
                      italics = seq(res_tablesup2_transformed),
                      stars = FALSE,
                      title = c("Supplementary Table 2", "Use of Positive ER strategies at t and Emotions at t - Within-Person Effects and Moderation by Anhedonia and Depression"),
                      note = c("ER = Emotion regulation. Estimates are within-level standardized.",
                               "Effects that were significant after applying the FDR correction (p < .023016) are displayed in bold."))

print(table_sup2)

## save table as word file
flextable::save_as_docx(table_sup2, path = here("Tables", "table_sup2.docx"))

## Sensitivity Analyses ----------------------------------

# Here, I am checking whether analyses are robust against
# 1. abandoning the compliance inclusion criterion
# 2. different NA and PA operationalizations by leaving one PA/NA item out

# Here, I compare whether significant effects stayed significant

# Overview dataset numbers
## Data0: 33% compliance inclusion criteria (pre-registered) & NA and PA scales with all items
## Data1: no inclusion criterion based on compliance & NA and PA scales with all items
## Data2: 33% compliance inclusion criteria (pre-registered) & PA enthusiast (enthusiastic) item removed
## Data3: 33% compliance inclusion criteria (pre-registered) & PA tevreden (satisfied) item removed
## Data4: 33% compliance inclusion criteria (pre-registered) & PA energiek (energetic) item removed
## Data5: 33% compliance inclusion criteria (pre-registered) & PA kalm (calm) item removed
## Data6: 33% compliance inclusion criteria (pre-registered) & PA daadkrachtig (determined) item removed
## Data7: 33% compliance inclusion criteria (pre-registered) & PA vrolijk (cheerful) item removed
## Data8: 33% compliance inclusion criteria (pre-registered) & PA dankbaar (grateful) item removed
## Data9: 33% compliance inclusion criteria (pre-registered) & NA geirritieerd (irritated) item removed
## Data10: 33% compliance inclusion criteria (pre-registered) & NA Verveeld (bored) item removed
## Data11: 33% compliance inclusion criteria (pre-registered) & NA nerveus (nervous) item removed
## Data12: 33% compliance inclusion criteria (pre-registered) & NA verdrietig (sad) item removed
## Data13: 33% compliance inclusion criteria (pre-registered) & NA boss (angry) item removed
## Data14: 33% compliance inclusion criteria (pre-registered) & NA somber (low) item removed

# Significance threshold after FDR correction
threshold <- .023016

### Estimates_df_RQ1b -----------------------------------

# Add column indicating whether scores were significant
estimates_df_RQ1b <- estimates_df_RQ1b %>%
  mutate(PA.p.sig = PA.p < threshold,
         NA.p.sig = NA.p < threshold)

# Add predictor column (extract predictor from model name)
estimates_df_RQ1b <- estimates_df_RQ1b %>%
  mutate(predictor = sub(".*\\.(per_[^\\.]+)\\.data\\d+\\.out", "\\1", ER))

# Compare TRUE/FALSE significance values within predictor group
estimates_df_RQ1b %>%
  dplyr::group_by(predictor) %>%
  dplyr::summarise(
    PA.p.consistency = n_distinct(PA.p.sig) == 1,
    NA.p.consistency = n_distinct(NA.p.sig) == 1
  ) %>%
  print()

# Follow-up cases, where it was not consistent
damp1b<-estimates_df_RQ1b %>%
  filter(predictor=="per_damp") %>%
  select(c("ER", "PA.est", "PA.p", "PA.p.sig")) 

expr1b<-estimates_df_RQ1b %>%
  filter(predictor=="per_expr") %>%
  select(c("ER", "NA.est", "NA.p", "NA.p.sig"))

print(expr1b)
psych::describe(expr1b$NA.p)

### Estimates_df_RQ1c -----------------------------------

# Add column indicating whether scores were significant
estimates_df_RQ1c <- estimates_df_RQ1c %>%
  mutate(PA.p.sig = PA.p < threshold,
         NA.p.sig = NA.p < threshold)

# Add predictor column (extract predictor from model name)
estimates_df_RQ1c <- estimates_df_RQ1c %>%
  mutate(predictor = sub(".*\\.(per_[^\\.]+)\\.data\\d+\\.out", "\\1", ER))

# Compare TRUE/FALSE significance values within predictor group
estimates_df_RQ1c %>%
  dplyr::group_by(predictor) %>%
  dplyr::summarise(
    PA.p.consistency = n_distinct(PA.p.sig) == 1,
    NA.p.consistency = n_distinct(NA.p.sig) == 1
  ) %>%
  print()

# Follow-up cases, where it was not consistent

att1c<-estimates_df_RQ1c %>%
  filter(predictor=="per_att") %>%
  select(c("ER", "NA.est", "NA.p", "NA.p.sig"))

capit1c<-estimates_df_RQ1c %>%
  filter(predictor=="per_capit") %>%
  select(c("ER", "NA.est", "NA.p", "NA.p.sig"))

print(capit1c)
psych::describe(capit1c$NA.p)

### Estimates_df_RQ2a -----------------------------------

# Add column indicating whether scores were significant
estimates_df_RQ2a <- estimates_df_RQ2a %>%
  mutate(An.p.sig = An.p < threshold)

# Add predictor column (extract predictor from model name)
estimates_df_RQ2a <- estimates_df_RQ2a %>%
  mutate(predictor = sub(".*\\.(per_[^\\.]+)\\.data\\d+\\.out", "\\1", ER))

# Compare TRUE/FALSE significance values within predictor group
estimates_df_RQ2a %>%
  dplyr::group_by(predictor) %>%
  dplyr::summarise(
    An.p.consistency = n_distinct(An.p.sig) == 1
  ) %>%
  print()

# Follow-up cases, where it was not consistent
estimates_df_RQ2a %>%
  filter(predictor=="per_capit")

### Estimates_df_RQ2b -----------------------------------

# Add column indicating whether scores were significant
estimates_df_RQ2b <- estimates_df_RQ2b %>%
  mutate(PAModAn.p.sig = PAModAn.p < threshold,
         NAModAn.p.sig = NAModAn.p < threshold)

# Add predictor column (extract predictor from model name)
estimates_df_RQ2b <- estimates_df_RQ2b %>%
  mutate(predictor = sub(".*\\.(per_[^\\.]+)\\.data\\d+\\.out", "\\1", ER))

# Compare TRUE/FALSE significance values within predictor group
estimates_df_RQ2b %>%
  dplyr::group_by(predictor) %>%
  dplyr::summarise(
    PAModAn.p.consistency = n_distinct(PAModAn.p.sig) == 1,
    NAModAn.p.consistency = n_distinct(NAModAn.p.sig) == 1
  ) %>%
  print()

# Follow-up cases, where it was not consistent
estimates_df_RQ2b %>%
  filter(predictor=="per_capit") %>%
  select(c("ER", "PAModAn.est", "PAModAn.p", "PAModAn.p.sig"))

estimates_df_RQ2b %>%
  filter(predictor=="per_capit") %>%
  select(c("ER", "NAModAn.est", "NAModAn.p", "NAModAn.p.sig"))

damp2b<-estimates_df_RQ2b %>%
  filter(predictor=="per_damp") %>%
  select(c("ER", "NAModAn.est", "NAModAn.p", "NAModAn.p.sig")) 

print(damp2b)
psych::describe(damp2b$NAModAn.p)

### Estimates_df_RQ2c -----------------------------------
# Here I am only looking at moderation effects

# Add column indicating whether scores were significant
estimates_df_RQ2c <- estimates_df_RQ2c %>%
  mutate(PAModAn.p.sig = PAModAn.p < threshold,
         NAModAn.p.sig = NAModAn.p < threshold)

# Add predictor column (extract predictor from model name)
estimates_df_RQ2c <- estimates_df_RQ2c %>%
  mutate(predictor = sub(".*\\.(per_[^\\.]+)\\.data\\d+\\.out", "\\1", ER))

# Compare TRUE/FALSE significance values within predictor group
estimates_df_RQ2c %>%
  dplyr::group_by(predictor) %>%
  dplyr::summarise(
    PAModAn.p.consistency = n_distinct(PAModAn.p.sig) == 1,
    NAModAn.p.consistency = n_distinct(NAModAn.p.sig) == 1
  ) %>%
  print()

# Follow-up cases, where it was not consistent
damp2c<-estimates_df_RQ2c %>%
  filter(predictor=="per_damp") %>%
  select(c("ER", "PAModAn.est", "PAModAn.p", "PAModAn.p.sig"))

print(damp2c)
psych::describe(damp2c$PAModAn.p)

codamp2c<-estimates_df_RQ2c %>%
  filter(predictor=="per_codamp") %>%
  select(c("ER", "NAModAn.est", "NAModAn.p", "NAModAn.p.sig"))

print(codamp2c)
psych::describe(codamp2c$NAModAn.p)

estimates_df_RQ2c %>%
  filter(predictor=="per_refl") %>%
  select(c("ER", "NAModAn.est", "NAModAn.p", "NAModAn.p.sig"))

## Overview of all models where effects turned non-significant
print(expr1b)
psych::describe(expr1b$NA.p)

print(capit1c)
psych::describe(capit1c$NA.p)

print(damp2b)
psych::describe(damp2b$NAModAn.p)

print(damp2c)
psych::describe(damp2c$PAModAn.p)

print(codamp2c)
psych::describe(codamp2c$NAModAn.p)


# Difference between our (33% inclusion criterion) and whole sample
diff1<-capit1c$ERNA.p[1]-capit1c$ERNA.p[2]
diff2<-damp2b$NAModAn.p[1]-damp2b$NAModAn.p[2] 
diff3<-damp2c$PAModAn.p[1]-damp2c$PAModAn.p[2] 

diff<-rbind(diff1,diff2,diff3)

mean(diff)
range(diff)
