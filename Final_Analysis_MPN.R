#Final_Analysis for MPN_data: Relationship between BMI nad MPN risk
# Wed Jul 31 14:14:24 2024 ------------------------------

#To read the database from excel into R;
#using the readr package.

install.packages("dplyr", lib="C:\\R\\R-4.4.1\\library")
library(dplyr)
install.packages("tidyverse",lib="C:\\R\\R-4.4.1\\library")
library(tidyverse)
install.packages("janitor", lib="C:\\R\\R-4.4.1\\library")
library(janitor)
install.packages("Hmisc",lib="C:\\R\\R-4.4.1\\library" )
library(Hmisc)
install.packages("readr",lib="C:\\R\\R-4.4.1\\library")
library(readr)
view

mpn_data <- read_csv(file.choose())
View(mpn_data)

#To view the structure of the data
str(mpn_data)

#To clean the column names of the variables; using the janitor package

mpn_data_clean <- janitor::clean_names(mpn_data)
mpn_data_clean
view(mpn_data_clean)

#To convert the character variables into numeric variables

mpn_data_clean <- transform(mpn_data_clean,
                            current_bmi = as.numeric(current_bmi))

#To convert the character variables into numeric variables

mpn_data_clean <- transform(mpn_data_clean,
                            previous_bmi = as.numeric(previous_bmi))

#To convert the character variables into numeric variables

mpn_data_clean <- transform(mpn_data_clean,
                            pmf = as.numeric(pmf))

#To convert the character variables into numeric variables

mpn_data_clean <- transform(mpn_data_clean,
                            age_of_smoking = as.numeric(age_of_smoking))


#To rename specific columns

mpn_data_clean <- dplyr::rename(mpn_data_clean, Category = category, Height_metres = height_m, Education_level = education_level_no, PV_ET_PMF = pv_et_pmf, PV = pv, ET = et, PMF = pmf, Mosaicc_id = mosaicc_id, Case_control = case_control, Age = age, Current_bmi= current_bmi, Previous_bmi = previous_bmi, Current_weight_kg = current_weight_kg, Previous_weight_kg = previous_weight_ten_years_ago_kg, Age_of_smoking = age_of_smoking)

#To keep the column needed for actual analysis of the data
data_mpn <- dplyr::select(mpn_data_clean, -height_feet,-height_inches,-height_final_inch_conversion,-height_cm,-height_m_2,-current_weight_stone,-current_weight_pounds,-current_weight_final_conversion_pounds,-previous_weight_tem_years_ago_stone,-previous_weight_ten_years_ago_pounds,-previous_weight_ten_years_ago_conversion_in_pounds,-PV_ET_PMF, -Age_of_smoking)


#To rename specific column
data_mpn <- dplyr::rename(data_mpn, Sex = gender, Physical_activity = physical_activity, Rheumatoid_arthritis = rheumatoid_arthritis, Diabetes = diabetes, Crohns_disease = crohns_disease, Hyperthyroidism= hyperthyroidism, Hypothyroidism = hypothyroidism, Blood_conditions = blood_conditions, Clinical_self_report = clinical_self_report, MPN_type = mpn_type)

#To know the proportion of self-reported and clincal reported patients with cases
#The reason for this is that some patients gave a self report for whether they have
#either of the MPN sub-types(PV,ET,PMF) as opposed to those obtained from
#clinical notes of the participants especially cases.

data_mpn %>% 
  group_by(Clinical_self_report) %>% 
  summarise(nr_observation_per_Clinical_self_report = n()) %>% 
  ungroup()

#To obtain the percentage of clinical to self reported patients:
df_clinical_report <- data_mpn %>%
  group_by (Clinical_self_report) %>%
  summarise (n=n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the data in tabular form:

df_clinical_report %>% gt()


#To categorize the current BMI

# categorize BMI values
# Underweight: BMI less than 18.5
# Normal weight: BMI 18.5 to 24.9
# Overweight: BMI 25 to 29.9
# Obese: BMI 30 or greater
data_mpn <- data_mpn %>% mutate(Current_BMI_Class = case_when(Current_bmi >= 0  & Current_bmi <= 18.5 ~ 'Underweight',
                                                              Current_bmi >= 18.5 & Current_bmi <= 24.9 ~ 'Normal weight',
                                                              Current_bmi >= 25  & Current_bmi <= 29.9 ~ 'Overweight',
                                                              Current_bmi >= 30 ~'Obese'))
view(data_mpn)

#To categorize the Previous BMI

# categorize BMI values
# Underweight: BMI less than 18.5
# Normal weight: BMI 18.5 to 24.9
# Overweight: BMI 25 to 29.9
# Obese: BMI 30 or greater
data_mpn <- data_mpn %>% mutate(Previous_BMI_Class = case_when(Previous_bmi >= 0  & Previous_bmi <= 18.5 ~ 'Underweight',
                                                               Previous_bmi >= 18.5 & Previous_bmi <= 24.9 ~ 'Normal weight',
                                                               Previous_bmi >= 25  & Previous_bmi <= 29.9 ~ 'Overweight',
                                                               Previous_bmi >= 30 ~'Obese'))

view(data_mpn)


#To rearrange the column Current_BMI_Class 

data_mpn %>% relocate(Current_BMI_Class, .before = Previous_weight_kg)

#To rearrange the column Previous_BMI_Class 

data_mpn %>% relocate(Previous_BMI_Class, .before = Education_level)

view(data_mpn)

#To count the number of the different category of BMI_Class
data_mpn %>% 
  group_by(Current_BMI_Class) %>% 
  summarise(nr_observation_per_Current_BMI_Class = n()) %>% 
  ungroup()

#To obtain the percentage of Current_BMI_Class:
df_Current_BMI_Class <- data_mpn %>%
  group_by (Current_BMI_Class) %>%
  summarise (n=n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the above data in tabular form:
df_Current_BMI_Class %>% gt()

#To count the number of the different category of Previous_BMI_Class
data_mpn %>% 
  group_by(Previous_BMI_Class) %>% 
  summarise(nr_observation_per_Previous_BMI_Class = n()) %>% 
  ungroup()

#To obtain the percentage of Previous_BMI_Class:
df_Previous_BMI_Class <- data_mpn %>%
  group_by (Previous_BMI_Class) %>%
  summarise (n=n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the above data in tabular form:
df_Previous_BMI_Class %>% gt()

#To group Current_BMI_Class into cases and control in percentage:
df_Case_control <- data_mpn %>% 
  group_by(Current_BMI_Class, Case_control) %>% 
  summarise (n=n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))


#To present the above data in tabular form:
df_Case_control %>% gt()


#To group Previous_BMI_Class into cases and control in percentage:
df_Previous_Case_control <- data_mpn %>% 
  group_by(Previous_BMI_Class, Case_control) %>% 
  summarise (n=n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL)) 

#To present the above data in tabular form:
df_Previous_Case_control %>% gt()

#To group MPN_subtype into percentages:
df_Sub_type <- data_mpn %>%
  group_by (MPN_type) %>%
  summarise (n=n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the above data in tabular form:
df_Sub_type %>% gt()



#To group the Case_control and Previous_BMI_Class into proportion:
data_no<- data_mpn %>%
  group_by(Case_control, Previous_BMI_Class) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))


#To present the data in a tabular form:
data_no%>% gt()


#To group the MPN_type and Medical condition, Diabetes into proportion:
data_cases<- data_mpn %>%
  group_by(MPN_type, Diabetes) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#To present the data in a tabular form:
data_cases%>% gt()

#To group the MPN_type and Medical condition,Crohns_disease into proportion:
data_cases<- data_mpn %>%
  group_by(MPN_type, Crohns_disease) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#To present the data of the MPN_type and crohn's disease in a tabular form:
data_cases%>% gt()

#To group the MPN_type and Medical condition,Hyperthyroidism into proportion:
data_cases<- data_mpn %>%
  group_by(MPN_type, Hyperthyroidism) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#To present the data of the MPN_type and Hyperthyroidism in a tabular form:
data_cases%>% gt()

#To group the MPN_type and Medical condition,Hypothyroidism into proportion:
data_cases<- data_mpn %>%
  group_by(MPN_type, Hypothyroidism) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#To present the data of the MPN_type and Hypothyroidism in a tabular form:
data_cases%>% gt()


#To present the data of the MPN_type and Previous_BMI_Class 
data_class<- data_mpn %>%
  group_by(MPN_type, Previous_BMI_Class) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the data of the MPN_type and Previous_BMI_Class in a tabular form:
data_class%>% gt()


#To present the data of the MPN_type and Sex
data_age <- data_mpn %>%
  group_by(MPN_type, Sex) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the data of the MPN_type and Sex in a tabular form:
data_age%>% gt()

#To present the data of the MPN_type and Sex
data_education <- data_mpn %>%
  group_by(MPN_type, Education_level) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the data of the MPN_type and Sex in a tabular form:
data_education %>% gt()

#To present the data of the MPN_type and Sex
data_smoke <- data_mpn %>%
  group_by(MPN_type, smoker_non_smoker) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the data of the MPN_type and Sex in a tabular form:
data_smoke %>% gt()

#To present the data of the MPN_type and Sex
data_report <- data_mpn %>%
  group_by(MPN_type, Clinical_self_report) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 1), "%", collapse = NULL))

#To present the data of the MPN_type and Sex in a tabular form:
data_report %>% gt()


#To calculate the mean Current_bmi

mean_bmi <- mean(data_mpn$Current_bmi, na.rm = TRUE)
mean_bmi

#To calculate the mean Previous_bmi

mean_bmi <- mean(data_mpn$Previous_bmi, na.rm = TRUE)
mean_bmi


#To categorize gender:
#Male: represented as "0"
#Female : represented as "1"

data_mpn <- data_mpn %>% mutate(Sex_Class = case_when(Sex == 0   ~ 'Male',
                                                      Sex == 1  ~ 'Female'))

#To count the number of the different category of Gender_Class
data_mpn %>% 
  group_by(Sex_Class) %>% 
  summarise(nr_observation_per_Sex_Class = n()) %>% 
  ungroup()


#find total NA values by column
sapply(data_mpn, function(x) sum(is.na(x)))



#To convert BMI_class to factor variable
data_mpn <- transform(data_mpn,
                           Previous_BMI_Class = as.factor(Previous_BMI_Class))

#To convert BMI_class to factor variable
data_mpn <- transform(data_mpn,
                      Current_BMI_Class = as.factor(Current_BMI_Class))


#To convert Case_control to factor variable
data_mpn_ <- transform(data_mpn,
                            Case_control = as.factor(Case_control))


#replace missing values with 'Female' in Gender_Class column
data_mpn$Gender_Class <- data_mpn$Gender_Class %>% replace_na('Female')


#replace missing values with 'Normal weight' in BMI_Class column
data_mpn$BMI_Class <- data_mpn$BMI_Class %>% replace_na('Normal weight')

#identify positions of NA values in column PV_Class
which(is.na(data_mpn$PV_Class))


#To keep the column needed for actual analysis of the data
#and  model building(backward regression)
data_mpn_set <- dplyr::select(data_mpn,-Clinical_self_report,-Category)

#To convert Case_control to numeric variable
data_mpn_set <- transform(data_mpn,
                          Case_control = as.numeric(Case_control))



#To compute summary statistics of data-set
#Descriptive statistics
describe(data_mpn)



#Backwardmodel
install.packages("stats", lib="C:\\R\\R-4.4.1\\library")
library(stats)

install.packages("gtsummary", lib="C:\\R\\R-4.4.1\\library")
library(gtsummary)

install.packages("gt", lib="C:\\R\\R-4.4.1\\library")
library(gt)

full_modelbmi<-glm(data_mpn$Case_control ~ data_mpn$Current_bmi + Age + Sex + Physical_activity, data = data_mpn, family = binomial)
full_modelbmi


#backward regression using 
backward_modelbmi <- step(full_modelbmi, direction = "backward")
tbl_regression (backward_modelbmi)

backward_modelbmi <- step(full_modelbmi, direction = "backward")
tbl_regression(backward_modelbmi)

#Ids to remove with two MPN_subtypes
ids_to_remove <- c("M00005", "M00009", "M00201", "M00217", "M00298", "M00495", "MIRE021")

#To reorder the mpn_type as a factor
data_mpn$MPN_type <- as.factor(data_mpn$MPN_type) 

#To re-level the data
data_mpn$MPN_type <- relevel(data_mpn$MPN_type, ref = "None")


library(nnet)
full_model_mpn <- multinom(MPN_type ~ Previous_bmi + Age + Education_level + Sex  + Physical_activity+ smoker_non_smoker + Diabetes + Hypothyroidism + Rheumatoid_arthritis,
                           data = data_mpn,
                           family = binomial)


# backward elimination function
backward_elimination <- function(model, data_mpn, threshold = 0.2) {
  # Get the summary of the model
  model_summary <- summary(model)
  
  
  
  
  # Extract the p-values
  
  p_values <- coef(model_summary)[, 4]
  
  # iterate until all p-values are below the threshold
  
  while (any(p_values > threshold)) {
    
    # find the predictor with the highest p-value
    
    worst_p_value <- max(p_values)
    
    if (worst_p_value < threshold) break
    
    # get the name of the predictor to remove
    
    worst_predictor <- names(which.max(p_values))
    
    # update the formula by removing the worst predictor
    
    formula <- as.formula(update(model, paste(". ~ . -", worst_predictor)))
    
    # fit the updated model
    
    model <- glm(formula, data = data_mpn, family = binomial)
    
    # Update the summary and p-values
    
    model_summary <- summary(model)
    
    p_values <- coef(model_summary)[, 4]
    
  }
  
  return(model)
  
}

# Apply the custom backward elimination

final_model_mpn_bmi <- backward_elimination(full_model_mpn, data_mpn, threshold = 0.2)


# Produce the regression table

tbl_regression(final_model_mpn_bmi, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 4),
               
               pvalue_fun = purrr::partial(style_sigfig, digits = 4))

#Unadjusted MPN and previousbmi
mpn_unadjusted <- glm(MPN_type ~ Previous_bmi, data= data_mpn, family = binomial)

tbl_regression(mpn_unadjusted, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 4),
               
               pvalue_fun = purrr::partial(style_sigfig, digits = 4))


#MPN_class and Previous_BMi
mpn_unadj <- multinom(MPN_type ~  Previous_bmi,
                      data = data_mpn,
                      family = binomial)  
tbl_regression(mpn_unadj, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 4),
               
               pvalue_fun = purrr::partial(style_sigfig, digits = 4))


#Previous_BMI_Class by MPN_type
mpn_unadj_class <- multinom(MPN_type ~  Previous_BMI_Class,
                      data = data_mpn,
                      family = binomial)  
tbl_regression(mpn_unadj_class, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 4),
               
               pvalue_fun = purrr::partial(style_sigfig, digits = 4))




summary_stats_mpntypebmi <- data_mpn_set %>%
  
  group_by(MPN_type) %>%
  
  summarise(
    
    mean_Previous_BMI= mean(Previous_bmi, na.rm = TRUE),
    
    sd_Previous_BMI = sd(Previous_bmi, na.rm = TRUE),
    
    min_Previous_BMI = min(Previous_bmi, na.rm = TRUE),
    
    max_Previous_BMI = max(Previous_bmi, na.rm = TRUE)
    
  )

# Print the summary statistics 

print(summary_stats_mpntypebmi)


#Case_control by Previous_BMI summary
mpn_case_control <- glm(Case_control ~ Previous_bmi, data = data_mpn, family = binomial)

tbl_regression(mpn_case_control, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 4),
               
               pvalue_fun = purrr::partial(style_sigfig, digits = 4))


# full model with all predictors
full_model_casecontol <- glm(Case_control ~ Previous_bmi+ Age + Education_level+ smoker_non_smoker + Sex + Physical_activity,
                             data = data_mpn,
                             family = binomial)


final_model_casecontrol <- backward_elimination(full_model_casecontol, data_mpn, threshold = 0.2)

# Produce the regression table
tbl_regression(full_model_casecontol, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 4),
               pvalue_fun = purrr::partial(style_sigfig, digits = 4))


citation()




full_model_mpn_set <- multinom(MPN_type ~ Current_bmi + Age + Education_level + Sex  + Physical_activity+ smoker_non_smoker + Diabetes + Hypothyroidism + Rheumatoid_arthritis,
                               data = data_mpn,
                               family = binomial)


# backward elimination function
backward_elimination <- function(model, data, threshold = 0.2) {
  # Get the summary of the model
  model_summary <- summary(model)
  
  
  
  
  # Extract the p-values
  
  p_values <- coef(model_summary)[, 4]
  
  # iterate until all p-values are below the threshold
  
  while (any(p_values > threshold)) {
    
    # find the predictor with the highest p-value
    
    worst_p_value <- max(p_values)
    
    if (worst_p_value < threshold) break
    
    # get the name of the predictor to remove
    
    worst_predictor <- names(which.max(p_values))
    
    # update the formula by removing the worst predictor
    
    formula <- as.formula(update(model, paste(". ~ . -", worst_predictor)))
    
    # fit the updated model
    
    model <- glm(formula, data = data, family = binomial)
    
    # Update the summary and p-values
    
    model_summary <- summary(model)
    
    p_values <- coef(model_summary)[, 4]
    
  }
  
  return(model)
  
}









  
  
  
 
 

