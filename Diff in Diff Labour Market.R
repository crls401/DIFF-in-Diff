install.packages("ggfixest")
install.packages("sandwich")
install.packages("lmtest")
library(data.table) ## For some minor data wrangling
library(fixest) 
library(haven)
library(dplyr)
library(fixest)
library(ggplot2)
#library(ggfixest)
library(sandwich)
library(lmtest)
library(zoo)
library(broom)
library(tidyverse)

png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\permanent_OVERALL.png", width = 12, height = 6, units = "in", res = 300)

data <- read_dta("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\data1.dta")

data <- data %>%
  mutate(
    NFORMA_num = recode(NFORMA,
                        AN = 1,
                        P1 = 2,
                        P2 = 3,
                        S1 = 4,
                        SG = 5,
                        SP = 6,
                        SU = 7,
                        .default = NA_real_),
    NFORMA_label = case_when(
      NFORMA == "AN" ~ "Analfabetos",
      NFORMA == "P1" ~ "Primaria Incompleta",
      NFORMA == "P2" ~ "Primaria Completa",
      NFORMA == "S1" ~ "Secundaria 1ª Etapa",
      NFORMA == "SG" ~ "Secundaria General",
      NFORMA == "SP" ~ "Secundaria Profesional",
      NFORMA == "SU" ~ "Educación Superior",
      TRUE ~ "No Clasificado"  # For unmatched cases
    )
  )

# Convert relevant columns to factors
data <- data %>%
  mutate(
    DUCON1 = as.numeric(DUCON1),
    DUCON2 = as.numeric(DUCON2),
    DUCON3 = as.numeric(DUCON3),
    PARCO1 = as.numeric(PARCO1),
    PARCO2 = as.numeric(PARCO2),
    CICLO =  as.numeric(as.character(CICLO)),
    CCAA = as.factor(CCAA),
    EDAD1 = as.factor(EDAD1),
    SEXO1 = as.factor(SEXO1),
    NAC1 = as.factor(NAC1),
    OCUP1 = as.factor(OCUP1)
  )
data <- data %>%
  filter(!is.na(OCUP1))
# Filter the data for 2019 to calculate the share of temporary contracts
data_2019 <- data %>%
  filter(as.numeric(as.character(CICLO)) >= 174 & as.numeric(as.character(CICLO)) <= 189
         
  )

# Calculate the share of temporary contracts in 2019 for each sector
temp_share_2019 <- data %>%
  group_by(OCUP1) %>%
  summarize(temp_share = mean(DUCON1 == 1, na.rm = TRUE))

# Merge the temporary contract share back to the main data
data <- data %>%
  left_join(temp_share_2019, by = "OCUP1")

# Create treatment indicator (Treat) based on the share of temporary contracts in 2019
data <- data %>%
  mutate(Treat = ifelse(temp_share > median(temp_share, na.rm = TRUE), 1, 0))

# Create Post indicator (Post)
data <- data %>%
  mutate(Post = ifelse(as.numeric(as.character(CICLO)) >= 197, 1, 0))

# Interaction term for Difference-in-Differences
data <- data %>%
  mutate(Treat_Post = Treat * Post)

baseline_period <- 197

# Calculate time_to_treat
data <- data %>%
  mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))

# Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
data <- data %>%
  mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
         temporary = ifelse(DUCON1 == 6, 1, 0))




mod_perm = feols(permanent ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                 cluster = ~OCUP1, data = data)


iplot(mod_perm, 
      xlab = 'Time to treatment',
      main = 'Event study: Permananet contracts overall')

dev.off()

png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\TEMPORARY_OVERALL.png", width = 12, height = 6, units = "in", res = 300)
data <- read_dta("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\data1.dta")
# Convert relevant columns to factors
data <- data %>%
  mutate(
    DUCON1 = as.numeric(DUCON1),
    CICLO =  as.numeric(as.character(CICLO)),
    CCAA = as.factor(CCAA),
    EDAD1 = as.factor(EDAD1),
    SEXO1 = as.factor(SEXO1),
    ECIV1 = as.factor(ECIV1),
    NAC1 = as.factor(NAC1),
    OCUP1 = as.factor(OCUP1)
  )
data <- data %>%
  filter(!is.na(OCUP1))
# Filter the data for 2019 to calculate the share of temporary contracts
data_2019 <- data %>%
  filter(as.numeric(as.character(CICLO)) >= 174 & as.numeric(as.character(CICLO)) <= 189
         
  )

# Calculate the share of temporary contracts in 2019 for each sector
temp_share_2019 <- data %>%
  group_by(OCUP1) %>%
  summarize(temp_share = mean(DUCON1 == 6, na.rm = TRUE))

# Merge the temporary contract share back to the main data
data <- data %>%
  left_join(temp_share_2019, by = "OCUP1")

# Create treatment indicator (Treat) based on the share of temporary contracts in 2019
data <- data %>%
  mutate(Treat = ifelse(temp_share > median(temp_share, na.rm = TRUE), 1, 0))

# Create Post indicator (Post)
data <- data %>%
  mutate(Post = ifelse(as.numeric(as.character(CICLO)) >= 197, 1, 0))

# Interaction term for Difference-in-Differences
data <- data %>%
  mutate(Treat_Post = Treat * Post)

baseline_period <- 197

# Calculate time_to_treat
data <- data %>%
  mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))

# Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
data <- data %>%
  mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
         temporary = ifelse(DUCON1 == 6, 1, 0))

mod_temp = feols(temporary ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                 cluster = ~OCUP1, data = data)
iplot(mod_temp, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')



dev.off()

summary(mod_temp)
# Load required libraries
library(broom)
library(dplyr)
library(writexl)

# Function to extract and format model coefficients
extract_coefficients <- function(model, model_name) {
  
  # Extract fixed effects from model
  fixed_effects <- broom::tidy(model)  
  
  # Initialize empty data frame to store results
  model_coefficients <- data.frame()
  
  # Iterate over each coefficient in the model
  for (i in seq_len(nrow(fixed_effects))) {
    
    # Extract values
    effect <- fixed_effects$term[i]
    coefficient <- fixed_effects$estimate[i]
    std_error <- fixed_effects$std.error[i]
    t_value <- fixed_effects$statistic[i]
    p_value <- fixed_effects$p.value[i]
    
    # Convert p-value to readable format
    p_value_formatted <- ifelse(p_value < 2e-16, "< 2e-16", format.pval(p_value, digits = 3))
    
    # Assign significance level
    significance <- case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
    
    # Append results to data frame
    model_coefficients <- rbind(model_coefficients, data.frame(
      Model = model_name,
      Variable = effect,
      Coefficient = coefficient,
      Std_Error = std_error,
      t_Value = t_value,
      p_Value = p_value_formatted,
      Significance = significance
    ))
  }
  
  return(model_coefficients)
}

# Extract coefficients for each model
results_temp <- extract_coefficients(mod_temp, "Temporary")
results_perm <- extract_coefficients(mod_perm, "Permanent")

# Combine both models into one table
regression_table <- bind_rows(results_temp, results_perm)

# Helper function to safely extract summary statistics
safe_extract <- function(summary_model, field) {
  if (!is.null(summary_model[[field]])) {
    return(summary_model[[field]])
  } else {
    return(NA)
  }
}

# Model summaries
summary_temp <- summary(mod_temp)
summary_perm <- summary(mod_perm)

# Create table for model summary statistics (robust version)
model_summary_table <- data.frame(
  Statistic = c("Observations", "R-squared", "Adj. R-squared", "AIC", "BIC", "Log-Likelihood"),
  Temporary = c(
    nobs(mod_temp),
    safe_extract(summary_temp, "r.squared"),
    safe_extract(summary_temp, "adj.r.squared"),
    AIC(mod_temp),
    BIC(mod_temp),
    as.numeric(logLik(mod_temp))
  ),
  Permanent = c(
    nobs(mod_perm),
    safe_extract(summary_perm, "r.squared"),
    safe_extract(summary_perm, "adj.r.squared"),
    AIC(mod_perm),
    BIC(mod_perm),
    as.numeric(logLik(mod_perm))
  )
)

# Export both tables to Excel
file_path <- "G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Temp-Perm_Overall.xlsx"

write_xlsx(list(
  "Regression Results" = regression_table,
  "Model Summary" = model_summary_table
), file_path)

# Display tables
print(regression_table)
print(model_summary_table)







##############################Tipology##########################################
############################Temporary#######################################
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\TEMPORARY_Tipology.png", width = 12, height = 6, units = "in", res = 300)

# Define the function to process data and create plots
process_data_for_gender <- function(data, Tipology) {
  # Filter the data for the specified gender
  data_gender <- data %>% filter(PARCO1 == Tipology)
  
  data <- data %>%
    mutate(
      DUCON1 = as.numeric(DUCON1),
      DUCON2 = as.numeric(DUCON2),
      DUCON3 = as.numeric(DUCON3),
      PARCO1 = as.numeric(PARCO1),
      PARCO2 = as.numeric(PARCO2),
      CICLO =  as.numeric(as.character(CICLO)),
      CCAA = as.factor(CCAA),
      EDAD1 = as.factor(EDAD1),
      SEXO1 = as.factor(SEXO1),
      NAC1 = as.factor(NAC1),
      OCUP1 = as.factor(OCUP1)
    )
  data <- data %>%
    filter(!is.na(OCUP1))
  
  # Filter the data for 2019 to calculate the share of temporary contracts
  data_2019 <- data_gender %>%
    filter(CICLO >= 174 & CICLO <= 189)
  
  # Calculate the share of temporary contracts in 2019 for each sector
  temp_share_2019 <- data_2019 %>%
    group_by(OCUP1) %>%
    summarize(temp_share = mean(DUCON1 == 6, na.rm = TRUE))
  
  # Merge the temporary contract share back to the main data
  data_gender <- data_gender %>%
    left_join(temp_share_2019, by = "OCUP1")
  
  # Create treatment indicator (Treat) based on the share of temporary contracts in 2019
  data_gender <- data_gender %>%
    mutate(Treat = ifelse(temp_share > median(temp_share, na.rm = TRUE), 1, 0))
  
  # Create Post indicator (Post)
  data_gender <- data_gender %>%
    mutate(Post = ifelse(CICLO >= 197, 1, 0))
  
  # Interaction term for Difference-in-Differences
  data_gender <- data_gender %>%
    mutate(Treat_Post = Treat * Post)
  
  baseline_period <- 197
  
  # Calculate time_to_treat
  data_gender <- data_gender %>%
    mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))
  
  # Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
  data_gender <- data_gender %>%
    mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
           temporary = ifelse(DUCON1 == 6, 1, 0))
  
  # Model for temporary contracts
  gend_mod_temp <- feols(temporary ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                         cluster = ~OCUP1, data = data_gender)
  
  return(gend_mod_temp)
}

# Load the data
data <- read_dta("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\data1.dta")

# Process and get models for male (SEXO1 == 1) and female (SEXO1 == 2)
mod_temp_Full <- process_data_for_gender(data, 1)
mod_temp_Part <- process_data_for_gender(data, 6)


# Plot the results for both genders in the same graph
ggiplot(
  list('Full-Time' = mod_temp_Full, 'Part-Time' = mod_temp_Part),
  ref.line = 0,
  main = 'Event study: Staggered treatment by Tipology'
) +
  xlab('Time to treatment') +
  theme_minimal()
dev.off()

############################Permanent#######################################
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\PERMANENT_Typology.png", width = 12, height = 6, units = "in", res = 300)

# Define the function to process data and create plots
process_data_for_gender <- function(data, Tipology) {
  # Filter the data for the specified gender
  data_gender <- data %>% filter(PARCO1 == Tipology)
  
  data <- data %>%
    mutate(
      DUCON1 = as.numeric(DUCON1),
      DUCON2 = as.numeric(DUCON2),
      DUCON3 = as.numeric(DUCON3),
      PARCO1 = as.numeric(PARCO1),
      PARCO2 = as.numeric(PARCO2),
      CICLO =  as.numeric(as.character(CICLO)),
      CCAA = as.factor(CCAA),
      EDAD1 = as.factor(EDAD1),
      SEXO1 = as.factor(SEXO1),
      NAC1 = as.factor(NAC1),
      OCUP1 = as.factor(OCUP1)
    )
  data <- data %>%
    filter(!is.na(OCUP1))
  
  # Filter the data for 2019 to calculate the share of temporary contracts
  data_2019 <- data_gender %>%
    filter(CICLO >= 174 & CICLO <= 189)
  
  # Calculate the share of Permanent contracts in 2019 for each sector
  perm_share_2019 <- data_2019 %>%
    group_by(OCUP1) %>%
    summarize(perm_share = mean(DUCON1 == 1, na.rm = TRUE))
  
  # Merge the temporary contract share back to the main data
  data_gender <- data_gender %>%
    left_join(perm_share_2019, by = "OCUP1")
  
  # Create treatment indicator (Treat) based on the share of temporary contracts in 2019
  data_gender <- data_gender %>%
    mutate(Treat = ifelse(perm_share > median(perm_share, na.rm = TRUE), 1, 0))
  
  # Create Post indicator (Post)
  data_gender <- data_gender %>%
    mutate(Post = ifelse(CICLO >= 197, 1, 0))
  
  # Interaction term for Difference-in-Differences
  data_gender <- data_gender %>%
    mutate(Treat_Post = Treat * Post)
  
  baseline_period <- 197
  
  # Calculate time_to_treat
  data_gender <- data_gender %>%
    mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))
  
  # Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
  data_gender <- data_gender %>%
    mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
           temporary = ifelse(DUCON1 == 6, 1, 0))
  
  # Model for temporary contracts
  gend_mod_perm <- feols(permanent ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                         cluster = ~OCUP1, data = data_gender)
  
  return(gend_mod_perm)
}

# Load the data
data <- read_dta("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\data1.dta")

# Process and get models for male (SEXO1 == 1) and female (SEXO1 == 2)
mod_perm_Full <- process_data_for_gender(data, 1)
mod_perm_Part <- process_data_for_gender(data, 6)


# Plot the results for both genders in the same graph
ggiplot(
  list('Full-Time' = mod_perm_Full, 'Part-Time' = mod_perm_Part),
  ref.line = 0,
  main = 'Event study: Staggered treatment by Typology'
) +
  xlab('Time to treatment') +
  theme_minimal()
dev.off()

#-------------------------------------------------------------------------------
                        # Table Temporal_Part-Time_Full-Time
#-------------------------------------------------------------------------------
# Load required libraries
library(broom)
library(dplyr)
library(writexl)

# Function to extract and format model coefficients
extract_coefficients <- function(model, model_name) {
  
  # Extract fixed effects from model
  fixed_effects <- broom::tidy(model)  
  
  # Initialize empty data frame to store results
  model_coefficients <- data.frame()
  
  # Iterate over each coefficient in the model
  for (i in seq_len(nrow(fixed_effects))) {
    
    # Extract values
    effect <- fixed_effects$term[i]
    coefficient <- fixed_effects$estimate[i]
    std_error <- fixed_effects$std.error[i]
    t_value <- fixed_effects$statistic[i]
    p_value <- fixed_effects$p.value[i]
    
    # Convert p-value to readable format
    p_value_formatted <- ifelse(p_value < 2e-16, "< 2e-16", format.pval(p_value, digits = 3))
    
    # Assign significance level
    significance <- case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
    
    # Append results to data frame
    model_coefficients <- rbind(model_coefficients, data.frame(
      Model = model_name,
      Variable = effect,
      Coefficient = coefficient,
      Std_Error = std_error,
      t_Value = t_value,
      p_Value = p_value_formatted,
      Significance = significance
    ))
  }
  
  return(model_coefficients)
}

# Extract coefficients for each model
results_temp <- extract_coefficients(mod_temp_Full, "Temporal_Full")
results_perm <- extract_coefficients(mod_temp_Part, "Temporal_Part")

# Combine both models into one table
regression_table <- bind_rows(results_temp, results_perm)

# Helper function to safely extract summary statistics
safe_extract <- function(summary_model, field) {
  if (!is.null(summary_model[[field]])) {
    return(summary_model[[field]])
  } else {
    return(NA)
  }
}

# Model summaries
summary_temp <- summary(mod_temp_Full)
summary_perm <- summary(mod_temp_Part)

# Create table for model summary statistics (robust version)
model_summary_table <- data.frame(
  Statistic = c("Observations", "R-squared", "Adj. R-squared", "AIC", "BIC", "Log-Likelihood"),
  Temporal_Full = c(
    nobs(mod_temp),
    safe_extract(summary_temp, "r.squared"),
    safe_extract(summary_temp, "adj.r.squared"),
    AIC(mod_temp),
    BIC(mod_temp),
    as.numeric(logLik(mod_temp))
  ),
  Temporal_Part = c(
    nobs(mod_perm),
    safe_extract(summary_perm, "r.squared"),
    safe_extract(summary_perm, "adj.r.squared"),
    AIC(mod_perm),
    BIC(mod_perm),
    as.numeric(logLik(mod_perm))
  )
)

# Export both tables to Excel
file_path <- "G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Temporal_Part-Time_Full-Time_results.xlsx"

write_xlsx(list(
  "Regression Results" = regression_table,
  "Model Summary" = model_summary_table
), file_path)

# Display tables
print(regression_table)
print(model_summary_table)


#-------------------------------------------------------------------------------
  # Table Permanent_Part-Time_Full-Time
#-------------------------------------------------------------------------------
  # Load required libraries
  library(broom)
library(dplyr)
library(writexl)

# Function to extract and format model coefficients
extract_coefficients <- function(model, model_name) {
  
  # Extract fixed effects from model
  fixed_effects <- broom::tidy(model)  
  
  # Initialize empty data frame to store results
  model_coefficients <- data.frame()
  
  # Iterate over each coefficient in the model
  for (i in seq_len(nrow(fixed_effects))) {
    
    # Extract values
    effect <- fixed_effects$term[i]
    coefficient <- fixed_effects$estimate[i]
    std_error <- fixed_effects$std.error[i]
    t_value <- fixed_effects$statistic[i]
    p_value <- fixed_effects$p.value[i]
    
    # Convert p-value to readable format
    p_value_formatted <- ifelse(p_value < 2e-16, "< 2e-16", format.pval(p_value, digits = 3))
    
    # Assign significance level
    significance <- case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
    
    # Append results to data frame
    model_coefficients <- rbind(model_coefficients, data.frame(
      Model = model_name,
      Variable = effect,
      Coefficient = coefficient,
      Std_Error = std_error,
      t_Value = t_value,
      p_Value = p_value_formatted,
      Significance = significance
    ))
  }
  
  return(model_coefficients)
}

# Extract coefficients for each model
results_temp <- extract_coefficients(mod_perm_Full, "Permanent_Full")
results_perm <- extract_coefficients(mod_perm_Part, "Permanent_Part")

# Combine both models into one table
regression_table <- bind_rows(results_temp, results_perm)

# Helper function to safely extract summary statistics
safe_extract <- function(summary_model, field) {
  if (!is.null(summary_model[[field]])) {
    return(summary_model[[field]])
  } else {
    return(NA)
  }
}

# Model summaries
summary_temp <- summary(mod_temp_Full)
summary_perm <- summary(mod_temp_Part)

# Create table for model summary statistics (robust version)
model_summary_table <- data.frame(
  Statistic = c("Observations", "R-squared", "Adj. R-squared", "AIC", "BIC", "Log-Likelihood"),
  Permanent_Full = c(
    nobs(mod_temp),
    safe_extract(summary_temp, "r.squared"),
    safe_extract(summary_temp, "adj.r.squared"),
    AIC(mod_temp),
    BIC(mod_temp),
    as.numeric(logLik(mod_temp))
  ),
  Permanent_Part = c(
    nobs(mod_perm),
    safe_extract(summary_perm, "r.squared"),
    safe_extract(summary_perm, "adj.r.squared"),
    AIC(mod_perm),
    BIC(mod_perm),
    as.numeric(logLik(mod_perm))
  )
)

# Export both tables to Excel
file_path <- "G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permanent_Part-Time_Full-Time_results.xlsx"

write_xlsx(list(
  "Regression Results" = regression_table,
  "Model Summary" = model_summary_table
), file_path)

# Display tables
print(regression_table)
print(model_summary_table)






################################################################################
################################################################################
##################################Gender###########################################

png("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\tEMPORARY_gender_Typology.png", width = 12, height = 6, units = "in", res = 300)

# Define the function to process data and create plots
process_data_for_gender_age <- function(data, gender, Tipology) {
  # Filter the data for the specified gender and age range
  data_filtered <- data %>% filter(SEXO1 == gender & PARCO1 == Tipology)
  
  # Convert relevant columns to factors
  data <- data %>%
    mutate(
      DUCON1 = as.numeric(DUCON1),
      DUCON2 = as.numeric(DUCON2),
      DUCON3 = as.numeric(DUCON3),
      PARCO1 = as.numeric(PARCO1),
      PARCO2 = as.numeric(PARCO2),
      CICLO =  as.numeric(as.character(CICLO)),
      CCAA = as.factor(CCAA),
      EDAD1 = as.factor(EDAD1),
      SEXO1 = as.factor(SEXO1),
      NAC1 = as.factor(NAC1),
      OCUP1 = as.factor(OCUP1)
    )
  data <- data %>%
    filter(!is.na(OCUP1))
  
  
  # Filter the data for 2019 to calculate the share of temporary contracts
  data_2019 <- data_filtered %>%
    filter(CICLO >= 174 & CICLO <= 189)

  # Calculate the share of temporary contracts in 2019 for each sector
  temp_share_2019 <- data_2019 %>%
    group_by(OCUP1) %>%
    summarize(temp_share = mean(DUCON1 == 6, na.rm = TRUE))
  
  # Merge the temporary contract share back to the main data
  data_filtered <- data_filtered %>%
    left_join(temp_share_2019, by = "OCUP1")
  
  # Create treatment indicator (Treat) based on the share of temporary contracts in 2019
  data_filtered <- data_filtered %>%
    mutate(Treat = ifelse(temp_share > median(temp_share, na.rm = TRUE), 1, 0))
  
  # Create Post indicator (Post)
  data_filtered <- data_filtered %>%
    mutate(Post = ifelse(CICLO >= 197, 1, 0))
  
  # Interaction term for Difference-in-Differences
  data_filtered <- data_filtered %>%
    mutate(Treat_Post = Treat * Post)
  
  baseline_period <- 197
  
  # Calculate time_to_treat
  data_filtered <- data_filtered %>%
    mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))
  
  # Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
  data_filtered <- data_filtered %>%
    mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
           temporary = ifelse(DUCON1 == 6, 1, 0))
  
  # Model for temporary contracts
  mod_temp <- feols(temporary ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                    cluster = ~OCUP1, data = data_filtered)
  
  return(mod_temp)
}

# Load the data
data <- read_dta("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\data1.dta")

# Process and get models for each gender and PARCO1 category
models <- list()

# Define the PARCO1 categories
parco_categories <- list(
  "Full-Time" = 1,
  "Part-Time" = 6
)

# Loop through genders and PARCO1 categories
for (gender in c(1, 6)) { # Assuming 1 = Male, 6 = Female
  for (category_name in names(parco_categories)) {
    Tipology <- parco_categories[[category_name]]
    
    # Call the updated function
    model <- process_data_for_gender_age(data, gender, Tipology)
    
    # Add the model to the list with a descriptive key
    models[[paste(ifelse(gender == 1, "Male", "Female"), category_name, sep = "_")]] <- model
  }
}


# Plot the results for both genders and age groups in the same graph
ggiplot(
  models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender and Age Group'
) +
  xlab('Time to treatment') +
  theme_minimal()
dev.off()
#############################Permanent##########################################


png("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\PERMANENT_gender_Typology.png", width = 12, height = 6, units = "in", res = 300)

# Define the function to process data and create plots
process_data_for_gender_age <- function(data, gender, Tipology) {
  # Filter the data for the specified gender and age range
  data_filtered <- data %>% filter(SEXO1 == gender & PARCO1 == Tipology)
  
  # Convert relevant columns to factors
  data_filtered <- data_filtered %>%
    mutate(
      DUCON1 = as.numeric(DUCON1),
      DUCON2 = as.numeric(DUCON2),
      DUCON3 = as.numeric(DUCON3),
      PARCO1 = as.numeric(PARCO1),
      PARCO2 = as.numeric(PARCO2),
      CICLO =  as.numeric(as.character(CICLO)),
      CCAA = as.factor(CCAA),
      EDAD1 = as.factor(EDAD1),
      SEXO1 = as.factor(SEXO1),
      NAC1 = as.factor(NAC1),
      OCUP1 = as.factor(OCUP1)
    )
  data <- data %>%
    filter(!is.na(OCUP1))
  
  
  # Filter the data for 2019 to calculate the share of temporary contracts
  data_2019 <- data_filtered %>%
    filter(CICLO >= 174 & CICLO <= 189)
  
  # Calculate the share of temporary contracts in 2019 for each sector
  temp_share_2019 <- data_2019 %>%
    group_by(OCUP1) %>%
    summarize(temp_share = mean(DUCON1 == 6, na.rm = TRUE))
  
  # Merge the temporary contract share back to the main data
  data_filtered <- data_filtered %>%
    left_join(temp_share_2019, by = "OCUP1")
  
  # Create treatment indicator (Treat) based on the share of temporary contracts in 2019
  data_filtered <- data_filtered %>%
    mutate(Treat = ifelse(temp_share > median(temp_share, na.rm = TRUE), 1, 0))
  
  # Create Post indicator (Post)
  data_filtered <- data_filtered %>%
    mutate(Post = ifelse(CICLO >= 197, 1, 0))
  
  # Interaction term for Difference-in-Differences
  data_filtered <- data_filtered %>%
    mutate(Treat_Post = Treat * Post)
  
  baseline_period <- 197
  
  # Calculate time_to_treat
  data_filtered <- data_filtered %>%
    mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))
  
  # Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
  data_filtered <- data_filtered %>%
    mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
           temporary = ifelse(DUCON1 == 6, 1, 0))
  
  # Model for temporary contracts
  mod_perm <- feols(permanent ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                    cluster = ~OCUP1, data = data_filtered)
  
  return(mod_perm)
}

# Load the data
data <- read_dta("G:\\Mi unidad\\Tilburg\\Block 2\\Policy\\Assignments\\Treball\\data1.dta")

# Process and get models for each gender and PARCO1 category
models <- list()

# Define the PARCO1 categories
parco_categories <- list(
  "Full-Time" = 1,
  "Part-Time" = 6
)

# Loop through genders and PARCO1 categories
for (gender in c(1, 6)) { # Assuming 1 = Male, 6 = Female
  for (category_name in names(parco_categories)) {
    Tipology <- parco_categories[[category_name]]
    
    # Call the updated function
    model <- process_data_for_gender_age(data, gender, Tipology)
    
    # Add the model to the list with a descriptive key
    models[[paste(ifelse(gender == 1, "Male", "Female"), category_name, sep = "_")]] <- model
  }
}


# Plot the results for both genders and age groups in the same graph
ggiplot(
  models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender and Age Group'
) +
  xlab('Time to treatment') +
  theme_minimal()
dev.off()

################################################################################
###################################AGE##########################################
################################################################################

# Define the function to process data and create plots
# Define the function to process data for gender, age group, and PARCO1
process_data_for_group <- function(data, gender, Tipology, age_group) {
  # Filter the data for the specified gender, age group, and PARCO1 category
  data_filtered <- data %>%
    filter(
      SEXO1 == gender & 
        EDAD1 %in% age_group & 
        PARCO1 == Tipology
    )
  
  
  # Convert relevant columns to factors
  data_filtered <- data_filtered %>%
    mutate(
      DUCON1 = as.numeric(DUCON1),
      CICLO =  as.numeric(as.character(CICLO)),
      CCAA = as.factor(CCAA),
      EDAD1 = as.factor(EDAD1),
      SEXO1 = as.factor(SEXO1),
      ECIV1 = as.factor(ECIV1),
      NAC1 = as.factor(NAC1),
      OCUP1 = as.factor(OCUP1)
    ) %>%
    filter(!is.na(OCUP1))
  
  # Filter the data for 2019 to calculate the share of temporary contracts
  data_2019 <- data_filtered %>%
    filter(CICLO >= 174 & CICLO <= 189)
  
  # Calculate the share of temporary contracts in 2019 for each sector
  temp_share_2019 <- data_2019 %>%
    group_by(OCUP1) %>%
    summarize(temp_share = mean(DUCON1 == 6, na.rm = TRUE))
  
  # Merge the temporary contract share back to the main data
  data_filtered <- data_filtered %>%
    left_join(temp_share_2019, by = "OCUP1")
  
  # Create treatment indicator (Treat) based on the share of temporary contracts in 2019
  data_filtered <- data_filtered %>%
    mutate(Treat = ifelse(temp_share > median(temp_share, na.rm = TRUE), 1, 0))
  
  # Create Post indicator (Post)
  data_filtered <- data_filtered %>%
    mutate(Post = ifelse(CICLO >= 197, 1, 0))
  
  # Interaction term for Difference-in-Differences
  data_filtered <- data_filtered %>%
    mutate(Treat_Post = Treat * Post)
  
  baseline_period <- 197
  
  # Calculate time_to_treat
  data_filtered <- data_filtered %>%
    mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))
  
  # Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
  data_filtered <- data_filtered %>%
    mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
           temporary = ifelse(DUCON1 == 6, 1, 0))
  
  # Model for temporary contracts
  mod_temp <- feols(temporary ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                    cluster = ~OCUP1, data = data_filtered)
  
  return(mod_temp)
}

# Define age groups based on the ranges provided
age_groups <- list(
  "16-24" = 16:24,
  "25-35" = 25:35,
  "36-46" = 36:45,
  "46-65" = 45:65
)

# Define the PARCO1 categories
parco_categories <- list(
  "Full-Time" = 1,
  "Part-Time" = 6
)

# Process and get models for each combination of gender, PARCO1, and age group
models <- list()

for (gender in c(1, 6)) { # Assuming 1 = Male, 6 = Female
  for (category_name in names(parco_categories)) {
    Tipology <- parco_categories[[category_name]]
    
    for (age_group_name in names(age_groups)) {
      age_group <- age_groups[[age_group_name]]
      
      # Call the updated function
      data_group <- process_data_for_group(data, gender, Tipology, age_group)
      
      # Create a key for the models list
      key <- paste(
        ifelse(gender == 1, "Male", "Female"),
        category_name,
        age_group_name,
        sep = "_"
      )
      
      # Store the result in the models list
      models[[key]] <- data_group
    }
  }
}

# Prepare the models for plotting dynamically
plot_male_models <- list(
  'Male_Full-Time_16-24' = models[["Male_Full-Time_16-24"]],
  'Male_Full-Time_25-35' = models[["Male_Full-Time_25-35"]],
  'Male_Part-Time_16-19' = models[["Male_Part-Time_16-24"]],
  'Male_Part-Time_25-35' = models[["Male_Part-Time_25-35"]],
  'Male_Full-Time_36-45' = models[["Male_Full-Time_36-45"]],
  'Male_Full-Time_46-65' = models[["Male_Full-Time_46-65"]],
  'Male_Part-Time_36-45' = models[["Male_Part-Time_36-45"]],
  'Male_Part-Time_46-65' = models[["Male_Part-Time_46-65"]]
)

plot_female_models <- list(
  'Female_Full-Time_16-24' = models[["Female_Full-Time_16-24"]],
  'Female_Full-Time_25-35' = models[["Female_Full-Time_25-35"]],
  'Female_Part-Time_16-19' = models[["Female_Part-Time_16-24"]],
  'Female_Part-Time_25-35' = models[["Female_Part-Time_25-35"]],
  'Female_Full-Time_36-45' = models[["Female_Full-Time_36-45"]],
  'Female_Full-Time_46-65' = models[["Female_Full-Time_46-65"]],
  'Female_Part-Time_36-45' = models[["Female_Part-Time_36-45"]],
  'Female_Part-Time_46-65' = models[["Female_Part-Time_46-65"]]
)

# Plot the results for males
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Temporary_Male_AgeGroups_Typology.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_male_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Males) (Temporary)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()

# Plot the results for females
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\temporary_Female_AgeGroups_Typology.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_female_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Females) (Temporary)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()

# Load required libraries
library(broom)
library(dplyr)
library(fixest)
library(writexl)

# Function to extract regression coefficients and key statistics
extract_model_results <- function(model, model_name) {
  
  # Extract coefficients
  coef_table <- tidy(model, conf.int = TRUE)
  
  results_df <- coef_table %>%
    mutate(
      Model = model_name,
      Coefficient = estimate,
      Std_Error = std.error,
      t_value = statistic,
      p_value = p.value,
      p_value_formatted = ifelse(p_value < 2e-16, "< 2e-16", format.pval(p_value, digits = 3)),
      Significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    ) %>%
    select(Model, term, Coefficient, Std_Error, t_value, p_value_formatted, Significance) %>%
    rename(Variable = term, t_Value = t_value, p_Value = p_value_formatted)
  
  # Extract key summary statistics from fixest model
  summary_stats <- glance(model) %>%
    mutate(
      Observations = nobs(model),
      Model = model_name
    ) %>%
    select(Model, Observations, r.squared, adj.r.squared, AIC, BIC, logLik)
  
  return(list(coefficients = results_df, summary_stats = summary_stats))
}

# Create a combined table from the models list
all_coefficients <- data.frame()
all_summary_stats <- data.frame()

# Iterate through all stored models
for (model_name in names(models)) {
  
  model <- models[[model_name]]
  
  # Extract results
  model_results <- extract_model_results(model, model_name)
  
  # Append results
  all_coefficients <- bind_rows(all_coefficients, model_results$coefficients)
  all_summary_stats <- bind_rows(all_summary_stats, model_results$summary_stats)
}

# Define Excel file path
file_path <- "G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Temporary_detailed_Model_Results.xlsx"

# Export results to Excel (two sheets: Coefficients & Summary Stats)
write_xlsx(
  list(
    "Regression_Coefficients" = all_coefficients,
    "Model_Summary_Statistics" = all_summary_stats
  ),
  file_path
)

# Display results briefly
print(all_coefficients)
print(all_summary_stats)

##################################################################################
##################################################################################
# Export both tables to Excel
file_path <- "G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permanent_Part-Time_Full-Time_results.xlsx"

# Define the function to process data and create plots
# Define the function to process data for gender, age group, and PARCO1
process_data_for_group <- function(data, gender, Tipology, age_group) {
  # Filter the data for the specified gender, age group, and PARCO1 category
  data_filtered <- data %>%
    filter(
      SEXO1 == gender & 
        EDAD1 %in% age_group & 
        PARCO1 == Tipology
    )
  
  
  # Convert relevant columns to factors
  data_filtered <- data_filtered %>%
    mutate(
      DUCON1 = as.numeric(DUCON1),
      CICLO =  as.numeric(as.character(CICLO)),
      CCAA = as.factor(CCAA),
      EDAD1 = as.factor(EDAD1),
      SEXO1 = as.factor(SEXO1),
      ECIV1 = as.factor(ECIV1),
      NAC1 = as.factor(NAC1),
      OCUP1 = as.factor(OCUP1)
    ) %>%
    filter(!is.na(OCUP1))
  
  # Filter the data for 2019 to calculate the share of temporary contracts
  data_2019 <- data_filtered %>%
    filter(CICLO >= 174 & CICLO <= 189)
  
  # Calculate the share of temporary contracts in 2019 for each sector
  perm_share_2019 <- data_2019 %>%
    group_by(OCUP1) %>%
    summarize(perm_share = mean(DUCON1 == 1, na.rm = TRUE))
  
  # Merge the temporary contract share back to the main data
  data_filtered <- data_filtered %>%
    left_join(perm_share_2019, by = "OCUP1")
  
  # Create treatment indicator (Treat) based on the share of temporary contracts in 2019
  data_filtered <- data_filtered %>%
    mutate(Treat = ifelse(perm_share > median(perm_share, na.rm = TRUE), 1, 0))
  
  # Create Post indicator (Post)
  data_filtered <- data_filtered %>%
    mutate(Post = ifelse(CICLO >= 197, 1, 0))
  
  # Interaction term for Difference-in-Differences
  data_filtered <- data_filtered %>%
    mutate(Treat_Post = Treat * Post)
  
  baseline_period <- 197
  
  # Calculate time_to_treat
  data_filtered <- data_filtered %>%
    mutate(time_to_treat = ifelse(Treat == 1, CICLO - baseline_period, 0))
  
  # Create dummy variables for permanent (DUCON1 == 1) and temporary (DUCON1 == 6) contracts
  data_filtered <- data_filtered %>%
    mutate(permanent = ifelse(DUCON1 == 1, 1, 0),
           temporary = ifelse(DUCON1 == 6, 1, 0))
  
  # Model for temporary contracts
  mod_perm <- feols(permanent ~ i(time_to_treat, Treat, ref = 0) + factor(OCUP1) | OCUP1, 
                    cluster = ~OCUP1, data = data_filtered)
  
  return(mod_perm)
}

# Define age groups based on the ranges provided
age_groups <- list(
  "16-24" = 16:24,
  "25-35" = 25:35,
  "36-45" = 36:45,
  "46-65" = 46:65
)

# Define the PARCO1 categories
parco_categories <- list(
  "Full-Time" = 1,
  "Part-Time" = 6
)

# Process and get models for each combination of gender, PARCO1, and age group
models <- list()

for (gender in c(1, 6)) { # Assuming 1 = Male, 6 = Female
  for (category_name in names(parco_categories)) {
    Tipology <- parco_categories[[category_name]]
    
    for (age_group_name in names(age_groups)) {
      age_group <- age_groups[[age_group_name]]
      
      # Call the updated function
      data_group <- process_data_for_group(data, gender, Tipology, age_group)
      
      # Create a key for the models list
      key <- paste(
        ifelse(gender == 1, "Male", "Female"),
        category_name,
        age_group_name,
        sep = "_"
      )
      
      # Store the result in the models list
      models[[key]] <- data_group
    }
  }
}

# Prepare the models for plotting dynamically
plot_male_models <- list(
  'Male_Full-Time_16-24' = models[["Male_Full-Time_16-24"]],
  'Male_Full-Time_25-35' = models[["Male_Full-Time_25-35"]],
  'Male_Part-Time_16-19' = models[["Male_Part-Time_16-24"]],
  'Male_Part-Time_25-35' = models[["Male_Part-Time_25-35"]],
  'Male_Full-Time_36-45' = models[["Male_Full-Time_36-45"]],
  'Male_Full-Time_46-65' = models[["Male_Full-Time_46-65"]],
  'Male_Part-Time_36-45' = models[["Male_Part-Time_36-45"]],
  'Male_Part-Time_46-65' = models[["Male_Part-Time_46-65"]]
)

plot_female_models <- list(
  'Female_Full-Time_16-24' = models[["Female_Full-Time_16-24"]],
  'Female_Full-Time_25-35' = models[["Female_Full-Time_25-35"]],
  'Female_Part-Time_16-19' = models[["Female_Part-Time_16-24"]],
  'Female_Part-Time_25-35' = models[["Female_Part-Time_25-35"]],
  'Female_Full-Time_36-45' = models[["Female_Full-Time_36-45"]],
  'Female_Full-Time_46-65' = models[["Female_Full-Time_46-65"]],
  'Female_Part-Time_36-45' = models[["Female_Part-Time_36-45"]],
  'Female_Part-Time_46-65' = models[["Female_Part-Time_46-65"]]
)

# Plot the results for males
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permananet_Male_AgeGroups_Typology8.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_male_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Males) (Permanent)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()

# Plot the results for females
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permanent_Female_AgeGroups_Typology.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_female_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment of by Gender, Age Group, and Typology (Females) (Permanent)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()







# Load required libraries
library(broom)
library(dplyr)
library(fixest)
library(writexl)

# Function to extract regression coefficients and key statistics
extract_model_results <- function(model, model_name) {
  
  # Extract coefficients
  coef_table <- tidy(model, conf.int = TRUE)
  
  results_df <- coef_table %>%
    mutate(
      Model = model_name,
      Coefficient = estimate,
      Std_Error = std.error,
      t_value = statistic,
      p_value = p.value,
      p_value_formatted = ifelse(p_value < 2e-16, "< 2e-16", format.pval(p_value, digits = 3)),
      Significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    ) %>%
    select(Model, term, Coefficient, Std_Error, t_value, p_value_formatted, Significance) %>%
    rename(Variable = term, t_Value = t_value, p_Value = p_value_formatted)
  
  # Extract key summary statistics from fixest model
  summary_stats <- glance(model) %>%
    mutate(
      Observations = nobs(model),
      Model = model_name
    ) %>%
    select(Model, Observations, r.squared, adj.r.squared, AIC, BIC, logLik)
  
  return(list(coefficients = results_df, summary_stats = summary_stats))
}

# Create a combined table from the models list
all_coefficients <- data.frame()
all_summary_stats <- data.frame()

# Iterate through all stored models
for (model_name in names(models)) {
  
  model <- models[[model_name]]
  
  # Extract results
  model_results <- extract_model_results(model, model_name)
  
  # Append results
  all_coefficients <- bind_rows(all_coefficients, model_results$coefficients)
  all_summary_stats <- bind_rows(all_summary_stats, model_results$summary_stats)
}

# Define Excel file path
file_path <- "G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permanent_detailed_Model_Results.xlsx"

# Export results to Excel (two sheets: Coefficients & Summary Stats)
write_xlsx(
  list(
    "Regression_Coefficients" = all_coefficients,
    "Model_Summary_Statistics" = all_summary_stats
  ),
  file_path
)

# Display results briefly
print(all_coefficients)
print(all_summary_stats)

# Prepare the models for plotting dynamically
plot_male_models <- list(
  'Male_Full-Time_16-24' = models[["Male_Full-Time_16-24"]],
  'Male_Full-Time_25-35' = models[["Male_Full-Time_25-35"]],
  'Female_Full-Time_16-24' = models[["Female_Full-Time_16-24"]],
  'Female_Full-Time_25-35' = models[["Female_Full-Time_25-35"]],
  'Male_Full-Time_36-45' = models[["Male_Full-Time_36-45"]],
  'Female_Full-Time_36-45' = models[["Female_Full-Time_36-45"]]
)

# Plot the results for males
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permananet_Maleand Female_AgeGroups_Typology16-45.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_male_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Males) (Permanent)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()




# Prepare the models for plotting dynamically
plot_male_models <- list(
  'Male_Full-Time_36-45' = models[["Male_Full-Time_36-45"]],
  'Male_Full-Time_46-65' = models[["Male_Full-Time_46-65"]],
  'Female_Full-Time_36-45' = models[["Female_Full-Time_36-45"]],
  'Female_Full-Time_46-65' = models[["Female_Full-Time_46-65"]]
  
)

# Plot the results for males
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permananet_Maleand Female_AgeGroups_Typology8.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_male_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Males) (Permanent)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()




# Prepare the models for plotting dynamically
plot_male_models <- list(
  'Male_Part-Time_16-24' = models[["Male_Part-Time_16-24"]],
  'Male_Part-Time_25-35' = models[["Male_Part-Time_25-35"]],
  'Female_Part-Time_16-24' = models[["Female_Part-Time_16-24"]],
  'Female_Part-Time_25-35' = models[["Female_Part-Time_25-35"]],
  'Male_Part-Time_36-45' = models[["Male_Part-Time_36-45"]],
  'Female_Part-Time_36-45' = models[["Female_Part-Time_36-45"]]
)

# Plot the results for males
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permananet_Maleand_Female_AgeGroups_Typology16-45.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_male_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Males) (Permanent)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()




# Prepare the models for plotting dynamically
plot_male_models <- list(
  'Male_Part-Time_36-45' = models[["Male_Part-Time_36-45"]],
  'Male_Part-Time_46-65' = models[["Male_Part-Time_46-65"]],
  'Female_Part-Time_36-45' = models[["Female_Part-Time_36-45"]],
  'Female_Part-Time_46-65' = models[["Female_Part-Time_46-65"]]
  
)

# Plot the results for males
png("G:\\Mi unidad\\Meus papers\\Spanish Labour Market\\Figure\\Permananet_Maleand Female_AgeGroups_Typology36-65.png", width = 12, height = 6, units = "in", res = 300)
ggiplot(
  plot_male_models,
  ref.line = 0,
  main = 'Event study: Staggered treatment by Gender, Age Group, and Typology (Males) (Permanent)'
) +
  xlab('Time to treatment') +
  ylab('Estimate and 95% Conf. Int.') +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
dev.off()