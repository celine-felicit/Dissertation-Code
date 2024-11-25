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

#DESCRIPTIVE ANALYSIS#
#1. Distribution of conflict intensity
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

#1.1. Trend analysis conflict intensity
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

#Visualization of ext_coalition distribution
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

#2.1. Trend analysis coalition support
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

#3. Distribution of incompatibility
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

#3.1. Trend analysis incompatibility
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


#4. Distribution of conflict type
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

#4.1. Evolution of conflict type
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

#5. Distribution of external support offered
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

#5.1. Trend analysis external support
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

#Combining the separate forms of support variables into one
  #ordinal categorical variable ranking different forms of support (from the lowest to the most extreme form of support/non-direct to direct support)
#Descriptive statistics
#DRAFT: Running a combined ordinal logistic regression
regress_1 <- glm("forms of support" ~ incompatibility + type + intensity, data = merged_ucdp, family = "ordinal")
summary(regress_1)

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
regress_2 <- lm("forms of support" ~ incompatibility + type + intensity + incompatibility*type*intensity, data = merged_ucdp)
#Summarize the model
summary(regress_2)
  #Is interaction effect significant?
  #If R-squared stays the same for all regression models, continue the interpretation with original model