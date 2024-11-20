#PREPARATION#
#Loading relevant packages
library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)

#3. Load SIPRI Milex data
file_path <- "~/Studium/Dissertation/Datasets/SIPRI-Milex-data-1948-2023.xlsx"

milex_constantUS <- read_excel(file_path, sheet = 5) #military expenditure in constant (2022) US$
milex_shareGDP <- read_excel(file_path, sheet = 7) #military expenditure as share of GDP
milex_percapita <- read_excel(file_path, sheet = 8) #military expenditure per capita
milex_share_govspending <- read_excel(file_path, sheet = 9) #military burden = military expenditure as share of government spending
  ##Which of these datasets do I want to work with??

#DRAFT: 4. SIPRI Arms Trade (?)
#Read in only the columns you specify
read_csv(
  dataset,
  col_types = cols_only(specified_column = col_character())
)

#TIDY DATA#
#DRAFT
# Reshape SIPRI Milex data from wide to long format
# Gather year columns into key-value pairs
sipri_milex_long <- sipri_milex %>% #specify which dataset you want to work with
  pivot_longer(cols = starts_with("19"):starts_with("20"),
               names_to = "year",
               values_to = "military_expenditure") %>%
  mutate(year = as.integer(year))  # Convert year to integer

#COMBINATION OF DATASETS#
#DRAFT: Step 2: Merge with military expenditure data 
# Merge based on external supporter (ext_name) and year
final_dataset <- merged_ucdp %>%
  left_join(sipri_milex_long, by = c("ext_name" = "country", "year" = "year"))

#DRAFT: Step 3: View and analyze the final dataset
summary(final_dataset)

#DESCRIPTIVE ANALYSIS#
#DRAFT: 6. Military trade (imports and exports)
ggplot("sipri_trade", aes(x= "variable for time", y= "exports")) +
  geom_line( color="#69b3a2", linewidth=2, alpha=0.9, linetype=2) +
  theme_ipsum() +
  ggtitle("Global arms exports")

#DRAFT: 7. Global military expenditure
class("milex_sheet"$"expenditure") 

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

#Regression 2: military expenditure of supporting country (DV) ~ Reason for conflict/incompatibility (IV), Form of conflict (IV) and conflict intensity (IV), support as a coalition (IV)

#DRAFT: Checking the assumptions#
#1: Validity. The data should be appropriate for the question that you are trying to answer.

#2: Additivity and linearity. If the relationship is non linear (e.g, it is curvilinear) predicted values will be wrong in a biased manner, meaning that predicted values will systematically miss the true pattern of the mean of y (as related to the x-variables).
#Checking linearity.
par(mfrow = c(2, 2))
plot(regress_1) 
  #Residuals vs Fitted: Used to check the linear relationship assumptions
  #Linearity assumption is met if the line is horizontal, without distinct patterns. This is an indication for a linear relationship.

#Checking additivity
#Does Reading suggest that the IV are dependent/there is a connection?
  #If not, additivity is met

residualPlots(regress_1)
  #If P-value is not significant, this indicates fit of the predictors

#Checking multicollinearity
#between incompatibility and intensity
with(merged_ucdp, CrossTable(incompatibility, intensity, prop.chisq = FALSE, format = c("SPSS")))

#between incompatibility and type of conflict
with(merged_ucdp, CrossTable(incompatibility, type, prop.chisq = FALSE, format = c("SPSS")))

#between type of conflict and intensity
with(merged_ucdp, CrossTable(type, intensity, prop.chisq = FALSE, format = c("SPSS")))


#3: Independence of errors. Regression assumes that the errors from the prediction line (or hyperplane for multiple regression) are independent. If there is dependency between the observations (you are assessing change across the same units, working with spatial units, or with units that are somehow grouped such as students from the same class), you may have to use models that are more appropriate (e.g., multilevel models, spatial regression, etc.).
#Some rows represent the continuation of a conflict and might therefore not be independent of other rows

#4: Equal variances of errors. When the variance of the residuals is unequal, you may need different estimation methods. This is, nonetheless, considered a minor issue. There is a small effect on the validity of t-test and F-test results, but generally regression inferences are robust with regard to the variance issue.
par(mfrow = c(2, 2))
plot(regress_1)
  #Scale-Location (or Spread-Location): Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity.

#OR ALTERNATIVELY
marginalModelPlots(regress_1, sd = TRUE)
  #Not constant across the x-axis, suggests heteroscedasticity

#Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis.
  #Horizontal line indicates that there are no extreme values that might influence the regression results when included or excluded from the analysis

lmtest::bptest(regress_1)  #Breusch-Pagan test
car::ncvTest(regress_1) #NCV test
  #If both tests have a p-value lower than the significance level of 0.05, we can reject the null hypothesis that the variance of the residuals is constant and infer that heteroscedasticity is indeed present.

#5: Normality of errors. The residuals should be normally distributed. 
#If the errors do not have a normal distribution, it usually is not particularly serious. Regression inferences tend to be robust with respect to normality (or nonnormality of the errors). In practice, the residuals may appear to be nonnormal when the wrong regression equation has been used.
par(mfrow = c(2, 2))
plot(regress_1)
  #If the residual points follow the straight dashed line in the normal Q-Q plot, which indicates a normal distribution.

nortest::ad.test(regress_1$residuals)
  #If the p-value is less than the significance level of 0.05, we can reject the null hypothesis that the residuals are normally distributed. 

#Test the model for outliers
outlierTest(regress_1)