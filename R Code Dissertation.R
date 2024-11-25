##DISSERTATION##

#PREPARATION#
#Loading relevant packages
library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)

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

#COMBINATION OF FORMS OF SUPPORT#
#Combining the separate forms of support variables into one
  #ordinal categorical variable ranking different forms of support (from the lowest to the most extreme form of support/non-direct to direct support)
# Add a new column for the ordinal categorical variable
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
      ordered = TRUE
    )
  )
#We now have 117 variables so seems to have worked
  ###add further tests to see whether it worked

#DESCRIPTIVE ANALYSIS#
#1. Conflict intensity
#1.1.Distribution of conflict intensity
#Convert intensity_level to a factor for better labeling
merged_ucdp$intensity <- factor(merged_ucdp$intensity,
                                      levels = c(1, 2),
                                      labels = c("Minor armed conflicts", "War"))
#Bar chart
ggplot(merged_ucdp, aes(x=intensity)) +
  geom_bar(width = 0.2, fill = "#8c510a", color = "black")+
  labs(title = "Distribution of conflict intensity levels",
       x = "Intensity level",
       y = "Count of conflicts") +
  theme_minimal()

#1.2. Trend analysis conflict intensity
#Create a summary dataset by year and intensity_level
intensity_summary <- merged_ucdp %>%
  filter(!is.na(intensity)) %>%  #Remove any NA values in intensity_level
  group_by(year, intensity) %>%
  summarise(conflict_count = n()) %>%
  ungroup()

#Create a stacked bar plot
ggplot(intensity_summary, aes(x = year, y = conflict_count, fill = intensity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "World armed conflicts by intensity level",
       x = "Year",
       y = "Number of conflicts",
       fill = "Conflict intensity") +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_continuous(breaks = seq(min(intensity_summary$year), max(intensity_summary$year), by = 5)) + #Label years every 5 years
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate x-axis labels for better readability

#2. Descriptive statistics for ext_coalition
ext_coalition_stats <- merged_ucdp |>
  group_by(ext_coalition) |>
  summarise(count = n()) |>
  mutate(percentage = (count / sum(count)) * 100)

#2.1. Distribution of ext_coalition
#Convert ext_coalition to a factor for better labeling in the plot
merged_ucdp$ext_coalition <- factor(merged_ucdp$ext_coalition,
                                 levels = c(0, 1),
                                 labels = c("Bilateral Support", "Coalition Support"))
#Create a bar plot
ggplot(merged_ucdp, aes(x = ext_coalition, fill = ext_coalition)) +
  geom_bar() +
  labs(title = "Distribution of external support: bilateral vs coalition",
       x = "Type of external support",
       y = "Count",
       fill = "Support type") +
  scale_fill_manual(
    values = c(
      "Bilateral Support" = "#8c510a",   
      "Coalition Support" = "#01665e"
    )
  ) +
  theme_minimal()

#2.2. Trend analysis coalition support
# Summarize the data by year and ext_coalition
coalition_summary <- merged_ucdp %>%
  group_by(year, ext_coalition) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Create the line chart
ggplot(coalition_summary, aes(x = year, y = conflict_count, color = ext_coalition, group = ext_coalition)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of support types over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Support type"
  ) +
  scale_color_manual(
    values = c(
      "Bilateral Support" = "#8c510a",
      "Coalition Support" = "#01665e"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#3. Incompatibility
#3.1. Distribution of incompatibility
#Convert incompatibility to a factor for better labeling in the plot
merged_ucdp$incompatibility <- factor(merged_ucdp$incompatibility,
                                      levels = c(1, 2, 3),
                                      labels = c("territory", "government", "territory and government"))

# Create a bar plot
ggplot(merged_ucdp, aes(x = incompatibility, fill = incompatibility)) +
  geom_bar() +
  labs(
    title = "Distribution of incompatibility",
    x = "Reason for conflict",
    y = "Count",
    fill = "Reason for conflict"
  ) +
  scale_fill_manual(
    values = c(
      "territory" = "#8c510a",   
      "government" = "#01665e",
      "territory and government" = "#80cdc1"
    )
  ) +
  theme_minimal()

#3.2. Trend analysis incompatibility
# Summarize the data by year and incompatibility
incompatibility_summary <- merged_ucdp %>%
  group_by(year, incompatibility) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Create the line chart
ggplot(incompatibility_summary, aes(x = year, y = conflict_count, color = incompatibility, group = incompatibility)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of incompatibility over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Incompatibility"
  ) +
  scale_color_manual(
    values = c(
      "territory" = "#8c510a",   
      "government" = "#01665e",
      "territory and government" = "#80cdc1"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


#4. Conflict type
#4.1. Distribution of conflict type
#Convert type_of_conflict to a factor for better labeling in the plot
merged_ucdp$type <- factor(merged_ucdp$type,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("extrasystemic", "interstate", "intrastate", "internationalised intrastate"))
#Create a bar plot
ggplot(merged_ucdp, aes(x = type, fill = type)) +
  geom_bar() +
  labs(title = "Distribution of conflict types",
       x = "Type of conflict",
       y = "Count",
       fill = "Conflict type") +
  scale_fill_manual(
    values = c(
      "extrasystemic" = "#8c510a",
      "interstate" = "#01665e",
      "intrastate" = "#dfc27d",
      "internationalised intrastate" = "#80cdc1"
    )
  ) +
  theme_minimal()

#4.2. Evolution of conflict type
# Prepare the data for visualization
type_summary <- merged_ucdp %>%
  group_by(year, type) %>%  # Group by year and conflict type
  summarise(conflict_count = n(), .groups = "drop")  # Count conflicts for each type and year

# Create the grouped line chart
ggplot(type_summary, aes(x = year, y = conflict_count, color = type)) +
  geom_line(size = 1) +  # Add lines for each conflict type
  labs(
    title = "Evolution of conflict types over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Conflict type"
  ) +
  scale_color_manual(
    values = c(
      "extrasystemic" = "#8c510a",
      "interstate" = "#01665e",
      "intrastate" = "#dfc27d",
      "internationalised intrastate" = "#80cdc1"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

#5. External support offered
#5.1. Distribution of external support offered
#Convert ext_sup to a factor for better labeling in the plot
merged_ucdp$ext_sup <- factor(merged_ucdp$ext_sup,
                           levels = c(0, 1),
                           labels = c("No external support", "External support"))

#Create a bar plot
ggplot(merged_ucdp, aes(x = ext_sup, fill = ext_sup)) +
  geom_bar() +
  labs(title = "Distribution of external support",
       x = "External support",
       y = "Count",
       fill = "Support offered") +
  scale_fill_manual(
    values = c(
      "No external support" = "#8c510a",
      "External support" = "#01665e"
    )
  ) +
  theme_minimal()

#5.2. Trend analysis external support
# Summarize the data by year and ext_sup
support_summary <- merged_ucdp %>%
  group_by(year, ext_sup) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Create the line chart
ggplot(support_summary, aes(x = year, y = conflict_count, color = ext_sup, group = ext_sup)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of external support over time",
    x = "Year",
    y = "Number of conflicts",
    color = "External support"
  ) +
  scale_color_manual(
    values = c(
      "No external support" = "#8c510a",
      "External support" = "#01665e"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#6. Forms of support offered
#6.1. Distribution of forms of support
# Bar plot for the distribution of support categories
ggplot(merged_ucdp, aes(x = ext_category, fill = ext_category)) +
  geom_bar() +
  labs(
    title = "Distribution of forms of external support",
    x = "Form of support",
    y = "Count",
    fill = "Form of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#d9d9d9",
    "unknown support" = "#bdbdbd",
    "other support" = "#969696",
    "access to territory" = "#737373",
    "intelligence" = "#525252",
    "funding" = "#252525",
    "training and expertise" = "#084594",
    "materiel and statistics" = "#2171b5",
    "weapons" = "#4292c6",
    "access to infrastructure/joint operations" = "#6baed6",
    "foreign troop presence" = "#9ecae1",
    "troop support" = "#c6dbef",
    "several forms of support" = "#deebf7"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

#6.2. Evolution of forms of support
# Grouped summary data for trend analysis
support_trend <- merged_ucdp %>%
  group_by(year, ext_category) %>%
  summarise(count = n(), .groups = "drop")

# Line chart for evolution of support categories over time
ggplot(support_trend, aes(x = year, y = count, color = ext_category, group = ext_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolution of forms of external support over time",
    x = "Year",
    y = "Count",
    color = "Form of support"
  ) +
  scale_color_manual(values = c(
    "no support" = "#d9d9d9",
    "unknown support" = "#bdbdbd",
    "other support" = "#969696",
    "access to territory" = "#737373",
    "intelligence" = "#525252",
    "funding" = "#252525",
    "training and expertise" = "#084594",
    "materiel and statistics" = "#2171b5",
    "weapons" = "#4292c6",
    "access to infrastructure/joint operations" = "#6baed6",
    "foreign troop presence" = "#9ecae1",
    "troop support" = "#c6dbef",
    "several forms of support" = "#deebf7"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


#BIVARIATE ANALYSIS#
#Global relationship between military expenditure and conflict intensity level

#Global relationship between arms imports and conflict intensity level

#Global relationship between military expenditure and arms exports

#REGRESSION ANALYSIS#
#Preparation
library(arm)
library(gmodels)
library(effects)
library(sjPlot)

#Form of support provided (DV) ~ Reason for conflict/incompatibility (IV), Form of conflict (IV) and conflict intensity (IV), support as a coalition (IV)
#Running logistic(?) regression for all forms of support seperately
#Troop support
regress_x <- glm(ext_x ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_x)
  #incompatibility (government) and incompatibility (government + territory) are non-significant
exp(coef(regress_x))

#OR
library(lessR, quietly= TRUE)
regress_xi <- Logit(ext_x ~ incompatibility + type + intensity, data = merged_ucdp, brief=TRUE)
  ###Error message:Variable redundant with a prior predictor in the model: typeinternationalised.intrastate

#Foreign troop presence
regress_p <- glm(ext_p ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_p)
  #only intercept and intensity (war) are significant (*** and **)
exp(coef(regress_p))

#Access to infrastructure/joint operations
regress_y <- glm(ext_y ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_y)
  #intercept and incompatibility territory + government are significant (***) 
  #all other are not or only slightly (*) or non-significant
exp(coef(regress_y))

#weapons
regress_w <- glm(ext_w ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_w)
  #internationalised intrastate (type) + intensity (war) are significant (***)
exp(coef(regress_w))

#materiel and statistics
regress_m <- glm(ext_m ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_m)
  #all except for incompatibility (government) are significant (* or ***)
exp(coef(regress_m))

#training and expertise
regress_t <- glm(ext_t ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_t)
  #only type (internationalised intrastate) and intensity (war) are significant (***)
exp(coef(regress_t))

#funding
regress_f <- glm(ext_f ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_f)
  #all except incompatibility (government) are significant (* or ***)
exp(coef(regress_f))

#intelligence
regress_i <- glm(ext_i ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_i)
  #all except intensity (war) are significant (ranging from * to ***)
exp(coef(regress_i))

#access to territory
regress_l <- glm(ext_l ~ incompatibility + type + intensity, data = merged_ucdp, family = "binomial")
summary(regress_l)
  #intercept, type (intrastate) and type (internationalised intrastate) are significant (***)
exp(coef(regress_l))

#Running a combined ordinal logistic regression
#1. Preparation
library(MASS)
library(nnet)
library(pscl)

#2. Relevelling the variable
merged_ucdp$ext_category <- factor(merged_ucdp$ext_category, 
                                   levels = c("no support", "unknown support", "other support", "access to territory", 
                                              "intelligence", "funding", "training and expertise", 
                                              "materiel and statistics", "weapons", "access to infrastructure/joint operations", 
                                              "foreign troop presence", "troop support", "several forms of support"), 
                                   ordered = TRUE)

#3. Run the ordinal logistic regression
# Fit the ordinal logistic regression model
ordinal_model <- polr(ext_category ~ incompatibility + type + intensity, data = merged_ucdp, method = "logistic")

# Summary of the model
summary(ordinal_model)

  ###Error in polr(ext_category ~ incompatibility + type + intensity, data = merged_ucdp_clean,: attempt to find suitable starting values failed
    #Checked structure of the data (whether all are factors) and NAs, 
    #both didn't seem to be the issue

# Simplified model with one predictor
ordinal_model_simple <- polr(ext_category ~ incompatibility, data = merged_ucdp, method = "logistic")
summary(ordinal_model_simple)

#Check for perfect separation#
  #Perfect separation occurs when one or more predictor variables perfectly predict the outcome variable, making it impossible to estimate coefficients. 

#Check for perfect separation by inspecting cross-tabulations:
  #Rows (or columns) in the cross-tabulation that contain only zeros except for a single non-zero count. 
  #This indicates that a particular level of the predictor is exclusively associated with a single level of the outcome variable.
table(merged_ucdp_clean$ext_category, merged_ucdp_clean$incompatibility)
  #Only non-zero count between no support + territory and government (1) & several forms of support + territory and government (10)
  #This is close to perfect separation because territory and government is almost exclusively associated with several forms of support, with one exception.
table(merged_ucdp_clean$ext_category, merged_ucdp_clean$type)
  #For extrasystemic and internationalised intrastate, certain levels of ext_category are either absent or only associated with several forms of support.
  #This suggests perfect or near-perfect separation, as some combinations of type and ext_category have no observations.
  #There is evidence of perfect separation in the predictor type, especially for extrasystemic.
table(merged_ucdp_clean$ext_category, merged_ucdp_clean$intensity)
  #ext_category levels like foreign troop presence, materiel and statistics, intelligence, and others show very low counts overall, particularly for War.
  #This indicates that some categories of intensity and ext_category may suffer from sparse data, but there isn’t clear perfect separation.
  
#If perfect separation exists, consider merging or re-coding levels of the problematic variable.
  ##Do this???

#4. Interpreting model output
  #The coefficients of the ordinal logistic regression are interpreted as the log-odds of being in a higher category of ext_category relative to the baseline category (the first level, unless specified otherwise).

# Calculate z-values and p-values
z <- summary(ordinal_model)$coefficients / summary(ordinal_model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2  # Two-tailed test
p

#5. Model diagnostics
# Pseudo R-squared
pR2(ordinal_model)

# Predictions: probabilities for each category
predicted_probs <- predict(ordinal_model, type = "probs")

# Predicted categories
predicted_categories <- predict(ordinal_model, type = "class")


##DRAFT: CONTINUATION OF REGRESSION INTERPRETATION##
#Rescaling the input variables to tell which predictors have a stronger effect
standardize(regress_1)
  #If the predictors stay the same, that indicates that the scales are already so close to each other that it doesn't change anything

#Obtaining confident intervals for the estimated coefficients
confint(regress_1)

#DRAFT: Visualizing the results
#As a plot
plot_model(regress_1, title = "Correlation between the forms of external support provided and conflict characteristics")
  #If a given confidence interval crosses 0 it is not statistically significant
  #Negative association: below 0, positive association: above 0

#OR as a table
tab_model(regress_1, pred.labels = c("(Intercept)", "Incompatibility", "Type of conflict", "Conflict intensity"), dv.labels = "Provided external support")

#Effect plots
plot(allEffects(regress_1), ask=FALSE)

#Interaction effect
regress_2 <- lm(ext_category ~ incompatibility + type + intensity + incompatibility*type*intensity, data = merged_ucdp)
#Summarize the model
summary(regress_2)
  #Is interaction effect significant?
  #If R-squared stays the same for all regression models, continue the interpretation with original model