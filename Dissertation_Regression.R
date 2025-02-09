##DISSERTATION##
#Regression#

#PREPARATION#
#Loading relevant packages
library(dplyr)
library(car)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggpattern)

#Load datasets into R
#1. UCDP Dyadic Dataset
ucdp_dyadic <- read.csv("~/Studium/Dissertation/Datasets/ucdp-dyadic-181.csv")

#2. UCDP ESD Dataset
# Read the first sheet from the Excel file
ucdp_esd <- read_excel("~/Studium/Dissertation/Datasets/ucdp-esd-dy-181.xlsx")

#COMBINATION OF DATSETS#
#Step 1. Merge UCDP ESD and UCDP dyadic datasets based on conflict dyad ID
#Check whether rows are unique in ESD dataset
distinct(ucdp_esd) #Yes, all unique

#check number of unique combinations
#checking for a "primary key"
nrow(ucdp_dyadic %>% distinct(dyad_id, year))
nrow(ucdp_esd %>% distinct(dyad_id,year))

ucdp_esd %>% distinct(dyad_id, year) #all unique -> can merge the two datasets based on these two variables

# Merge UCDP dyadic and UCDP ESD datasets on 'dyad_id' and 'year'
merged_ucdp <- ucdp_dyadic %>%
  inner_join(ucdp_esd, by = c("dyad_id", "year"))

#Check whether rows are unique
nrow(distinct(merged_ucdp)) #Yes

#before removing duplicate columns, check with summary statistics if the 
#values are the same in the columns for example the code below gives you the
#name of the columns that are duplicates the code give all those that have 
#.y at the end which means there is another column with the same name but 
#with .x at the end

merged_ucdp %>% 
  dplyr::select(ends_with(".y")) %>%
  names()

#Check which variables are the same in both datasets
intersect(names(ucdp_dyadic), names (ucdp_esd))

#check which columns have matching names and values in both datasets
common_columns <-intersect(colnames(ucdp_dyadic), colnames(ucdp_esd))

for (col in common_columns) {
  if (length(intersect(ucdp_dyadic[[col]], ucdp_esd[[col]])) > 0) {
    print(paste("Column", col, "has matching values"))
  } else {
    print(paste("Column", col, "no matching values"))
  }
}

#Compare values for common columns in both datasets
# Define the common columns you want to compare
common_columns <- c("conflict_id", "location", "side_a", "side_a_id", 
                    "side_b", "side_b_id", "gwno_a", "gwno_b")

# Loop through each common column to compare pairs
for (col_name in common_columns) {
  col_x <- paste0(col_name, ".x")
  col_y <- paste0(col_name, ".y")
  
  # Check if the columns are identical
  if (all(merged_ucdp[[col_x]] == merged_ucdp[[col_y]], na.rm = TRUE)) {
    # If identical, remove one of the columns
    merged_ucdp <- merged_ucdp %>% 
      dplyr::select(-all_of(col_y))
    cat("Columns", col_x, "and", col_y, "are identical. Keeping", col_x, "and removing", col_y, "\n")
  } else {
    cat("Columns", col_x, "and", col_y, "are not identical. Keeping both.\n")
  }
}

  #conflict_id is identical -> removing conflict_id.y 
  #location is not identical -> Keeping both.
  #side_a is identical -> removing side_a.y 
  #side_a_id is identical -> removing side_a_id.y 
  #side_b is not identical -> Keeping both.
  #side_b_id is not identical -> Keeping both.
  #gwno_a is not identical -> Keeping both.
  #gwno_b is not identical -> Keeping both.

# Check the final dataset to confirm the changes
glimpse(merged_ucdp)

#Checking for missing values
sum(is.na(merged_ucdp)) #5325

#COMBINATION OF FORMS OF SUPPORT#
#Combining the separate forms of support variables into one
  #categorical variable ranking different forms of support (from the lowest to the most extreme form of support/non-direct to direct support)
# Add a new column for the categorical variable
merged_ucdp <- merged_ucdp %>%
  mutate(
    ext_category = case_when(
      ext_sum == 0 ~ "no support",                                    # No support provided
      ext_sum > 1 ~ "several forms of support",                       # More than one type of support provided
      ext_u == 1 ~ "unknown support",                                 # Unknown support
      ext_o == 1 ~ "other support",                                   # Other support
      ext_l == 1 ~ "access to territory",                             # Access to territory
      ext_i == 1 ~ "intelligence",                                    # Intelligence
      ext_f == 1 ~ "funding",                                         # Funding
      ext_t == 1 ~ "training and expertise",                          # Training and expertise
      ext_m == 1 ~ "materiel and statistics",                         # Materiel and statistics
      ext_w == 1 ~ "weapons",                                         # Weapons
      ext_y == 1 ~ "access to infrastructure/joint operations",       # Access to infrastructure/joint operations
      ext_p == 1 ~ "foreign troop presence",                          # Foreign troop presence
      ext_x == 1 ~ "troop support",                                   # Troop support
      TRUE ~ "no support"                                             # Default to "no support"
    ),
    ext_category = factor(
      ext_category,
      levels = c(
        "no support",
        "unknown support",
        "other support",
        "access to territory",
        "intelligence",
        "funding",
        "training and expertise",
        "materiel and statistics",
        "weapons",
        "access to infrastructure/joint operations",
        "foreign troop presence",
        "troop support",
        "several forms of support"
      ),
    )
  )
#We now have 117 variables so seems to have worked
  ###add further tests to see whether it worked

  ###Think about how to operationalise this variable
    #Which forms of support are you specifically interested in: funding, weapons, troop

#Creation of a variable to differentiate between indirect and direct support
merged_ucdp <- merged_ucdp %>%
  mutate(
    ext_type = case_when(
      ext_sum == 0 ~ "no support",                               # No support provided
      ext_sum > 1 & (ext_x == 1 | ext_p == 1) &                  # Mixed direct and indirect
        (ext_l == 1 | ext_i == 1 | ext_f == 1 | 
           ext_t == 1 | ext_m == 1 | ext_w == 1 | ext_y == 1) ~ "direct and indirect",
      ext_sum > 1 & (ext_x + ext_p == ext_sum) ~ "direct",       # All support is direct
      ext_sum > 1 & (ext_l + ext_i + ext_f + ext_t + 
                       ext_m + ext_w + ext_y == ext_sum) ~ "indirect", # All support is indirect
      ext_x == 1 | ext_p == 1 ~ "direct",                        # Single direct support
      ext_l == 1 | ext_i == 1 | ext_f == 1 | ext_t == 1 | 
        ext_m == 1 | ext_w == 1 | ext_y == 1 ~ "indirect",       # Single indirect support
      ext_u == 1 ~ "unknown"                                     # Unknown support
    ),
    ext_type = factor(
      ext_type,
      levels = c("no support", "indirect", "direct", "direct and indirect", "unknown") # Set the order
    )
  )
  ##118 variables now so seems to have worked
    ###add further checks

#Creation of a duration variable#
# Ensure the dataset is sorted by dyad_id and year
merged_ucdp <- merged_ucdp %>% arrange(dyad_id, year)

# Calculate the new variables
merged_ucdp <- merged_ucdp %>%
  group_by(dyad_id) %>%
  mutate(
    # Calculate the first and last observed years for the dyad
    start_year = min(year, na.rm = TRUE),  # First year in the dataset for the dyad
    end_year = max(year, na.rm = TRUE),    # Last year in the dataset for the dyad
    
    # Calculate the duration including missing years
    duration_cease = end_year - start_year + 1,
    
    # Calculate the duration excluding missing years
    duration_peace = n()  # Count the actual number of rows (years) for the dyad
  ) %>%
  ungroup()

# Reorder columns to place the new ones after 'year'
merged_ucdp <- merged_ucdp %>%
  relocate(start_year, end_year, duration_cease, duration_peace, .after = year)

#Creation of a cumulative measure of duration
merged_ucdp <- merged_ucdp %>%
  group_by(dyad_id) %>%  # Group by dyad_id to calculate for each conflict-dyad
  mutate(
    start_year = min(year, na.rm = TRUE),  # Find the first year for each dyad_id
    cumulative_duration = year - start_year + 1  # Calculate duration as the difference from the start year
  ) %>%
  ungroup()  # Ungroup to return the dataset to its original structure

# Reorder columns to place the new ones after 'year'
merged_ucdp <- merged_ucdp %>%
  relocate(cumulative_duration, .after = year)

# Creation of a variable indicating whether the observation is before or after 9/11#
merged_ucdp <- merged_ucdp %>%
  mutate(
    nine_eleven = ifelse(year > 2001, "After 9/11", "Before 9/11")
  )

#Creation of a variable indicating whether the observation is during or after the Cold War#
merged_ucdp <- merged_ucdp %>%
  mutate(
    cold_war = case_when(
      year >= 1947 & year <= 1991 ~ "Cold War",
      year > 1991 ~ "Post-Cold War",
      TRUE ~ NA_character_ # Handles missing or out-of-range values
    )
  )

# Ensure the new variables are factors for better usability
merged_ucdp <- merged_ucdp %>%
  mutate(
    nine_eleven = factor(nine_eleven, levels = c("Before 9/11", "After 9/11")),
    cold_war = factor(cold_war, levels = c("Cold War", "Post-Cold War"))
  )

#Factorising variables
#intensity
merged_ucdp$intensity <- factor(merged_ucdp$intensity,
                                      levels = c(1, 2),
                                      labels = c("Minor armed conflict", "War"))
#ext_coalition
merged_ucdp$ext_coalition <- factor(merged_ucdp$ext_coalition,
                                 levels = c(0, 1),
                                 labels = c("Bilateral Support", "Coalition Support"))
#incompatibility
merged_ucdp$incompatibility <- factor(merged_ucdp$incompatibility,
                                      levels = c(1, 2, 3),
                                      labels = c("territory", "government", "territory and government"))

#type of conflict
merged_ucdp$type <- factor(merged_ucdp$type,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("extrasystemic", "interstate", "intrastate", "internationalised intrastate"))
#ext_sup
merged_ucdp$ext_sup <- factor(merged_ucdp$ext_sup,
                           levels = c(0, 1),
                           labels = c("No external support", "External support"))

#region variable
merged_ucdp$region <- factor(merged_ucdp$region,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Europe", "Middle East", "Asia", "Africa", "Americas"))

#REGRESSION ANALYSIS#
#Preparation
library(arm)
library(gmodels)
library(effects)
library(sjPlot)
library(lme4)
library(plm)

#suppress scientific notation
options (scipen = 999)

#As the observations are part of a broader cluster (conflict), random effects logistic regression is appropriate
  #calculates both within-cluster-variation and between-cluster-variation

# if a linear regression models looks like this: model <- lm(y ~ x, data = data)
# the lme4 model will look like this model_randomintercepts <- lmer(y ~ x + (1 | CLUSTER), data = data)
# the random effects logistic regression model will look like this: model_randomintercepts <- glmer(y ~ x + (1 | CLUSTER), data = data, family = "binomial")

#Form of support provided (DV) ~ Reason for conflict/incompatibility (IV), Form of conflict (IV) and conflict intensity (IV), support as a coalition (IV)
#Running robust effects logistic and linear regression for all forms of support seperately

#1. Troop support
#1.1. random effects linear regression
rlinregress_x <- lmer(ext_x ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_x)
  #if the t-value is above 2 or below -2 it is statistically significant
  ##the average intercept (3.231), type (intrastate) (-3.691) and type (internationalised intrastate) (26.501) are statistically significant

summary(is.na(merged_ucdp$cumulative_duration))

#Checking for multicollinearity
vif(rlinregress_x)
  ##no multicollinearity

#1.2. random effects logistic regression
rlogregress_x <- glmer(ext_x ~  incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_x)
  ##ext_coalition (coalition support) (**) is statistically significant

##the differences in statistical significance suggest that the logistic regression model might be better fit for interpretation

#Exponentiate results
exp(fixef(rlogregress_x))

#Check multicollinearity
#Variance Inflation Factor (VIF) quantifies the extent of multicollinearity in a regression model. 
#It measures how much the variance of a regression coefficient increases due to collinearity with other predictors.
  #VIF < 5: Low to moderate multicollinearity (acceptable).
  #VIF = 5–10: Moderate multicollinearity (investigate further; might require adjustments).
  #VIF > 10: High multicollinearity (likely problematic; consider corrective actions).
vif(logregress_x)
  ##no multicollinearity

#1.3. Fixed effects linear regression
flinregress_x <- plm(ext_x ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_x)
  ###intercept, incompatibility and region are not portrayed in output, why?

#1.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_x <- glmer(ext_x ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_x)
  ##Coalition support is statistically significant

#2. Foreign troop presence
#2.1. random effects linear regression
rlinregress_p <- lmer(ext_p ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_p)
  ##intensity (war) (2.697) and ext_coalition (Coalition support) (3.114) are statistically significant

#Check multicollinearity
vif(rlinregress_p)
  ##no multicollinearity

#2.2 random effects logistic regression
rlogregress_p <- glmer(ext_p ~  incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_p)
  ##intensity (war) (**) is statistically significant

#Exponentiate results
exp(fixef(rlogregress_p))

#Check multicollinearity
vif(rlogregress_p)
  ##no multicollinearity

#2.3. Fixed effects linear regression
flinregress_p <- plm(ext_p ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_p)
  ###intercept, incompatibility and region are not portrayed in output, why?

#2.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_p <- glmer(ext_p ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_p)
  ##intensity (war) (**) is statistically significant

#3. Access to infrastructure/joint operations
#3.1. random effects linear regression
rlinregress_y <- lmer(ext_y ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_y)
  ##cold_war (Post-Cold War) (3.073) and ext_coalition (Coalition support) (4.229) are statistically significant

#Check multicollinearity
vif(rlinregress_y)
  ##no multicollinearity

#3.2. random effects logistic regression
rlogregress_y <- glmer(ext_y ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_y)
  ##cumulative_duration (.), cold war (Post-Cold war) (***), ext_coalition (Coalition support) (***) are statistically significant

#Exponentiate results
exp(fixef(rlogregress_y))

#Check multicollinearity
vif(rlogregress_y)
  ##no multicollinearity

#3.3. Fixed effects linear regression
flinregress_y <- plm(ext_y ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_y)

#3.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_y <- glmer(ext_y ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_y)
  ##cumulative_duration (.), cold war (Post-Cold war) (***), ext_coalition (Coalition support) (***) are statistically significant
  
#4. weapons
#4.1. random effects linear regression
rlinregress_w <- lmer(ext_w ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_w)
  ##average Intercept (4.028), intensity (war) (3.996), nine_eleven (after 9/11) (2.625), and cold_war (Post-Cold war) (-5.280) are statistically significant

#Check multicollinearity
vif(rlinregress_w)
  ##no multicollinearity

#4.2. random effects logistic regression
rlogregress_w <- glmer(ext_w ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_w)
  ##everything but incompatibility (territory and government) is statistically significant (***)

##the differences in statistical significance suggest that the logistic regression model might be better fit for interpretation

#Exponentiate results
exp(fixef(rlogregress_w))

#Check multicollinearity
vif(rlogregress_w)
  ##no multicollinearity

#4.3. Fixed effects linear regression
flinregress_w <- plm(ext_w ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_w)

#4.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_w <- glmer(ext_w ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_w)
  ##all but incompatibility (territory and government) are statistically significant (***)

#5. materiel and statistics
#5.1. random effects linear regression
rlinregress_m <- lmer(ext_m ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_m)
  ##average intercept (3.075), type (internationalised intrastate) (2.029), and intensity (war) (2.899) statistically significant

#Check multicollinearity
vif(rlinregress_m)
  ##no multicollinearity

#5.2. random effects logistic regression
rlogregress_m <- glmer(ext_m ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_m)
  ##type (internationalised intrastate) (*) and intensity (war) (***) are significant

#Exponentiate results
exp(fixef(rlogregress_m))

#Check multicollinearity
vif(rlogregress_m)
  ##no multicollinearity

#5.3. Fixed effects linear regression
flinregress_m <- plm(ext_m ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_m)

#5.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_m <- glmer(ext_m ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_m)
  ##type (internationalised intrastate) (*) and intensity (war) (***) are significant

#6. training and expertise
#6.1. random effects linear regression
rlinregress_t <- lmer(ext_t ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_t)
  ##average intercept (3.222), type (internationalised intrastate) (2.251) and cold_war (Post-Cold War) (-3.517) are statistically significant

#Check multicollinearity
vif(rlinregress_t)
  ##no multicollinearity

#6.2. random effects logistic regression
rlogregress_t <- glmer(ext_t ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_t)
  ##type (intrastate) (.), type (internationalised intrastate) (*) and cold_war (Post-Cold War) (**) are statistically significant

##the differences in statistical significance suggest that the logistic regression model might be better fit for interpretation

#Exponentiate results
exp(fixef(rlogregress_t))

#Check multicollinearity
vif(rlogregress_t)
  ##no multicollinearity

#6.3. Fixed effects linear regression
flinregress_t <- plm(ext_t ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_t)

#6.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_t <- glmer(ext_t ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_t)
  ##type (intrastate) (.), type (internationalised intrastate) (*) and cold_war (Post-Cold War) (**) are statistically significant

#7. funding
#7.1. random effects linear regression
rlinregress_f <- lmer(ext_f ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_f)
  ##average intercept (2.212), intensity (war) (4.441), cumulative_duration (-2.612), nine_eleven (after 9/11) (2.072), cold_war (Post-Cold war) (-2.516), and ext_coalition (Coalition support) (-2.187) are statistically significant

#Check multicollinearity
vif(rlinregress_f)
  ##no multicollinearity

#7.2. random effects logistic regression
rlogregress_f <- glmer(ext_f ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_f)
  ##type (internationalised intrastate) (.), intensity (war) (***), cumulative_duration (*) and cold_war (Post-Cold war) are significant

##the differences in statistical significance suggest that the logistic regression model might be better fit for interpretation

#Exponentiate results
exp(fixef(rlogregress_f))

#Check multicollinearity
vif(rlogregress_f)
  ##no multicollinearity

#7.3. Fixed effects linear regression
flinregress_f <- plm(ext_f ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_f)

#7.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_f <- glmer(ext_f ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_f)
  ##type (internationalised intrastate) (.), intensity (war) (***), cumulative_duration (*) and cold_war (Post-Cold war) are significant

#8. intelligence
#8.1. random effects linear regression
rlinregress_i <- lmer(ext_i ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_i)
  ##incompatibility (government) (-2.986), region (Americas) (2.936), nine_eleven (After 9/11) (7.519), cold_war (Post-Cold war) (2.022), and ext_coalition (Coalition support) (2.797) are statistically significant

#Check multicollinearity
vif(rlinregress_i)
  ##no multicollinearity

#8.2. random effects logistic regression
rlogregress_i <- glmer(ext_i ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_i)
  ##Intercept (*), incompatibility (government) (**), intensity (war) (.), region (Americas) (*), nine_eleven (after 9/11) (***), cold_war (Post-Cold war) (**), and ext_coalition (Coalition support) (*) are statistically significant

#Exponentiate results
exp(fixef(rlogregress_i))

#Check multicollinearity
vif(rlogregress_i)
  ##no multicollinearity

#8.3. Fixed effects linear regression
flinregress_i <- plm(ext_i ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_i)

#8.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_i <- glmer(ext_i ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_i)
  ##Intercept (*), incompatibility (government) (**), intensity (war) (.), region (Americas) (*), nine_eleven (after 9/11) (***), cold_war (Post-Cold war) (**), and ext_coalition (Coalition support) (*) are statistically significant

#9. access to territory
#9.1. random effects linear regression
rlinregress_l <- lmer(ext_l ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp)
summary(rlinregress_l)
  ##average intercept (-2.033), type (intrastate) (3.118), type (internationalised intrastate) (3.316), region (Middle East) (2.214), region (Africa) (3.787), region (Americas) (2.063), nine_eleven (After 9/11) (-2.071), and ext_coalition (Coalition support) (2.165) are statistically significant

#Check multicollinearity
vif(rlinregress_l)
  ##no multicollinearity

#9.2. random effects logistic regression
rlogregress_l <- glmer(ext_l ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = "binomial")
summary(rlogregress_l)
  ##incompatibility (government) (.), region (Middle East) (.), region (Africa) (***), nine_eleven (After 9/11) (.), and ext_coalition (Coalition support) (*) are statistically significant

##the differences in statistical significance suggest that the logistic regression model might be better fit for interpretation

#Exponentiate results
exp(fixef(rlogregress_l))

#Check multicollinearity
vif(rlogregress_l)
  ##no multicollinearity

#9.3. Fixed effects linear regression
flinregress_l <- plm(ext_l ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp, index = c("dyad_id", "year"), model = "within")
summary(flinregress_l)

#9.4. Fixed effects logistic regression (Random Intercepts for dyad_id)
flogregress_l <- glmer(ext_l ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition + (1 | dyad_id), data = merged_ucdp, family = binomial(link = "logit"))
summary(flogregress_l)
  ##incompatibility (government) (.), region (Middle East) (.), region (Africa) (***), nine_eleven (After 9/11) (.), and ext_coalition (Coalition support) (*) are statistically significant

##Visulaizing random effects logistic regression as a table
tab_model(rlogregress_p, rlogregress_y, rlogregress_w, rlogregress_m, rlogregress_t, rlogregress_f, rlogregress_i, rlogregress_l, pred.labels = c("(Intercept)", "Incompatibility (government)", "Incompatibility (territory and government)", "Type (intrastate)", "Type (internationalised intratstate)", "Intensity (war)", "Region (Middle East)", "Region (Asia)", "Region (Africa)", "Region (Americas)", "Duration", "9/11", "Cold War", "Coalition support"), dv.labels = c("Foreign troop presence", "Access to infrastructure/joint operations", "Weapons", "Materiel and statistics", "Training and expertise", "Funding", "Intelligence", "Access to territory"))

#Running a combined logistic regression
#Preparation
library(nnet) # For multinomial logistic regression
library(sandwich) # For robust standard errors
library(lmtest) # For hypothesis testing with robust SEs
library(brglm2) # For bias-reduced logistic regression

# Fit the multinomial logistic regression model
multinom_1 <- brmultinom(ext_category ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp)
summary(multinom_1)

# Extract coefficients
coef(multinom_1)

#Logistic regression on type of external support provided
# Fit the multinomial logistic regression model
multinom_2 <- multinom(ext_type ~ incompatibility + type + intensity + region + cumulative_duration + nine_eleven + cold_war + ext_coalition, data = merged_ucdp)
summary(multinom_2)

# Calculate z-values and p-values for the coefficients
z_values_2 <- summary(multinom_2)$coefficients / summary(multinom_2)$standard.errors
p_values_2 <- (1 - pnorm(abs(z_values_2), 0, 1)) * 2  # Two-tailed test

z_values_2
p_values_2

# Predict probabilities for each observation
predicted_probs_2 <- predict(multinom_2, type = "probs")

# Predict the category for each observation
predicted_categories_2 <- predict(multinom_2)

###DRAFT: CONTINUATION OF REGRESSION INTERPRETATION##
#Rescaling the input variables to tell which predictors have a stronger effect
standardize(regress_1)
  #If the predictors stay the same, that indicates that the scales are already so close to each other that it doesn't change anything

#Obtaining confident intervals for the estimated coefficients
confint(regress_1)

###DRAFT: Visualizing the results
#As a plot
plot_model(regress_1, title = "Correlation between the forms of external support provided and conflict characteristics")
  #If a given confidence interval crosses 0 it is not statistically significant
  #Negative association: below 0, positive association: above 0

#Effect plots
plot(allEffects(regress_1), ask=FALSE)

#Interaction effect
regress_2 <- lm(ext_category ~ incompatibility + type + intensity + incompatibility*type*intensity, data = merged_ucdp)
#Summarize the model
summary(regress_2)
  #Is interaction effect significant?
  #If R-squared stays the same for all regression models, continue the interpretation with original model