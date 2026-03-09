#PREPARATION#
#Loading relevant packages
library(readxl) # For reading Excel files
library(tidyverse)
library(panelr) # For panel data analysis

#Load datasets into R
#1. UCDP Dyadic Dataset
#ucdp_dyadic <- read.csv("~/Studium/Dissertation/Dissertation Code/Datasets/ucdp-dyadic-181.csv")
ucdp_dyadic <- read.csv("Datasets/ucdp-dyadic-181.csv")

#2. UCDP ESD Dataset
# Read the first sheet from the Excel file
#ucdp_esd <- read_excel("~/Studium/Dissertation/Dissertation Code/Datasets/ucdp-esd-dy-181.xlsx")
ucdp_esd <- read_excel("Datasets/ucdp-esd-dy-181.xlsx")


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
sum(is.na(merged_ucdp)) #5321

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

#Creation of a duration variable#
# Ensure the dataset is sorted by dyad_id and year
merged_ucdp <- merged_ucdp %>% arrange(dyad_id, year)

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

# Create a variable indicating whether the observation is during or after the Cold War#
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

#Creation of a binary variable based on 'incompatibility' that indicates whether a conflict is about territory (1) or not (0)
merged_ucdp <- merged_ucdp %>%
  mutate(territorial = ifelse(incompatibility == 1 | incompatibility == 3, 1, 0))

#Factorising variables
#intensity
merged_ucdp$intensity <- factor(merged_ucdp$intensity,
                                levels = c(1, 2),
                                labels = c("Minor armed conflict", "War"))

#ext_coalition
merged_ucdp$ext_coalition <- factor(merged_ucdp$ext_coalition,
                                    levels = c(0, 1),
                                    labels = c("Bilateral Support", "Coalition Support"),
                                    exclude = NULL)

#incompatibility
merged_ucdp$incompatibility <- factor(merged_ucdp$incompatibility,
                                      levels = c(1, 2, 3),
                                      labels = c("territory", "government", "territory and government"))

#type of conflict
merged_ucdp$type <- factor(merged_ucdp$type,
                           levels = c(1, 2, 3, 4),
                           labels = c("extrasystemic", "interstate", "intrastate", "internationalised intrastate"))

#Relevel type so that intrastate becomes the reference group in the regression analysis
merged_ucdp$type <- factor(merged_ucdp$type,
                           levels = c("intrastate", "extrasystemic", "interstate", "internationalised intrastate"))

#ext_nonstate
merged_ucdp$ext_nonstate <- factor(merged_ucdp$ext_nonstate,
                                   levels = c(0, 1),
                                   labels = c("state supporter", "non-state supporter"),
                                   exclude = NULL)

#region
merged_ucdp$region <- factor(merged_ucdp$region,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Europe", "Middle East", "Asia", "Africa", "Americas"))

#Remove unnecessary objects
#Remove datasets that are no longer needed
rm(ucdp_dyadic, ucdp_esd)
#Remove objects that are no longer needed
rm(common_columns, col_name, col, col_x, col_y)


#Number of dyads
nrow(merged_ucdp %>% distinct(dyad_id)) #472
#Number of conflicts
nrow(merged_ucdp%>% distinct(conflict_id.x)) #212

#Number of NAs
sum(is.na(merged_ucdp)) #5489
#Find out which columns have NAs
colSums(is.na(merged_ucdp))
#region: 4
#Can be disregarded as a very minor number for 2234 observations in total
#ext_id, ext_name, ext_bothsides: 411
#not of relevance as not working with this variable
#ext_nonstate: 411
#quite a big number, include in visualisations and limitations
#ext_coalition: 783
#quite a big number, include in visualisations and limitations
#ext_coalition_name: 2091
#not of relevance as not working with this variable
#ext_elements: 803
#not of relevance as not working with this variable
#ext_combination: 160
#should not include any NAs!! Fix this issue
#ext_type: 4
##Can be disregarded as a very minor number for 2234 observations in total


#Calculate proportions for variables
#Proportion of conflict types
type_proportion <- merged_ucdp %>%
  group_by(type) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of conflict intensities
intensity_proportion <- merged_ucdp %>%
  group_by(intensity) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of conflict incompatibilities
incompatibility_proportion <- merged_ucdp %>%
  group_by(incompatibility) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of territorial disputes
territorial_proportion <- merged_ucdp %>%
  group_by(territorial) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of regions
region_proportion <- merged_ucdp %>%
  group_by(region) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of cold war status
cold_war_proportion <- merged_ucdp %>%
  group_by(cold_war) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of 9/11 status
nine_eleven_proportion <- merged_ucdp %>%
  group_by(nine_eleven) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Mean/Median of cumulative duration
summary(merged_ucdp$cumulative_duration)
#Mean: 7.892
#Median: 5.000

#Proportion of external support coalitions
ext_coalition_proportion <- merged_ucdp %>%
  group_by(ext_coalition) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of external support category
ext_category_proportion <- merged_ucdp %>%
  group_by(ext_category) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of external support types
ext_type_proportion <- merged_ucdp %>%
  group_by(ext_type) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of external support (ext_sup)
ext_sup_proportion <- merged_ucdp %>%
  group_by(ext_sup) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of troop support (ext_x)
ext_x_proportion <- merged_ucdp %>%
  group_by(ext_x) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of troop presence (ext_p)
ext_p_proportion <- merged_ucdp %>%
  group_by(ext_p) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of access to infrastructure (ext_y)
ext_y_proportion <- merged_ucdp %>%
  group_by(ext_y) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of weapon support (ext_w) 
ext_w_proportion <- merged_ucdp %>%
  group_by(ext_w) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of materiel and statistics (ext_m)
ext_m_proportion <- merged_ucdp %>%
  group_by(ext_m) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of training and expertise (ext_t)
ext_t_proportion <- merged_ucdp %>%
  group_by(ext_t) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of funding (ext_f)
ext_f_proportion <- merged_ucdp %>%
  group_by(ext_f) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of intelligence (ext_i)
ext_i_proportion <- merged_ucdp %>%
  group_by(ext_i) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of access to territory (ext_l)
ext_l_proportion <- merged_ucdp %>%
  group_by(ext_l) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of other support (ext_o)
ext_o_proportion <- merged_ucdp %>%
  group_by(ext_o) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

#Proportion of unknown support (ext_u)
ext_u_proportion <- merged_ucdp %>%
  group_by(ext_u) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 2))

save(merged_ucdp, file = "Paper/merged_ucdp.RData")

#####################################################

# panel data
ucdp_panel <- merged_ucdp %>%
  mutate(wave = year - 1975) %>%
  panel_data(id = dyad_id, wave = wave)

# Overall provision (PPML — logistic did not converge)
wbm_0 <- wbm(ext_sup ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "poisson")

# Troop support (PPML — logistic did not converge)
wbm_x <- wbm(ext_x ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "poisson")

# Foreign troop presence (PPML — logistic did not converge)
wbm_p <- wbm(ext_p ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "poisson")

# Access to infrastructure (logistic)
wbm_y <- wbm(ext_y ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "binomial")

# Weapons (logistic)
wbm_w <- wbm(ext_w ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "binomial")

# Materiel and logistics (logistic)
wbm_m <- wbm(ext_m ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "binomial")

# Training and expertise (PPML — logistic did not converge)
wbm_t <- wbm(ext_t ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "poisson")

# Funding (logistic)
wbm_f <- wbm(ext_f ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "binomial")

# Intelligence (logistic)
wbm_i <- wbm(ext_i ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "binomial")

# Access to territory (logistic)
wbm_l <- wbm(ext_l ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition
             | type + territorial
             | (1 | dyad_id),
             data = ucdp_panel, family = "binomial")

model_list <- list(
  "Overall provision"    = wbm_0,
  "Troop support"        = wbm_x,
  "Troop presence"       = wbm_p,
  "Infrastructure"       = wbm_y,
  "Weapons"              = wbm_w,
  "Materiel & logistics" = wbm_m,
  "Training & expertise" = wbm_t,
  "Funding"              = wbm_f,
  "Intelligence"         = wbm_i,
  "Access to territory"  = wbm_l
)

extract_wbm_coefs <- function(mod, mod_name) {
  s <- summary(mod)
  
  within_coefs <- as.data.frame(s$within_table)
  within_coefs$term <- rownames(within_coefs)
  within_coefs$component <- "Within"
  
  between_coefs <- as.data.frame(s$between_table)
  between_coefs$term <- rownames(between_coefs)
  between_coefs$component <- "Between"
  
  bind_rows(within_coefs, between_coefs) %>%
    mutate(outcome = mod_name)
}

coef_df <- purrr::map_dfr(names(model_list), function(nm) {
  extract_wbm_coefs(model_list[[nm]], nm)
})

coef_df <- coef_df %>%
  tibble::as_tibble() %>%
  rename(estimate = `Est.`, se = `S.E.`, z = `z val.`) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # Strip imean() prefix for labelling
    term_clean = gsub("^imean\\((.+)\\)$", "\\1", term),
    or      = exp(estimate),
    ci_low  = exp(estimate - 1.96 * se),
    ci_high = exp(estimate + 1.96 * se),
    sig = p < 0.05,
    term_label = case_when(
      term_clean == "intensityWar"                          ~ "Intensity (war)",
      term_clean == "cumulative_duration"                   ~ "Cumulative duration",
      term_clean == "nine_elevenAfter 9/11"                 ~ "Post-9/11",
      term_clean == "cold_warPost-Cold War"                 ~ "Post-Cold War",
      term_clean == "ext_coalitionCoalition Support"        ~ "Coalition support",
      term_clean == "typeinterstate"                        ~ "Type (interstate)",
      term_clean == "typeinternationalised intrastate"      ~ "Type (int. intrastate)",
      term_clean == "territorial"                           ~ "Territorial incompatibility",
      TRUE ~ term_clean
    ),
    term_label = factor(term_label, levels = rev(c(
      "Intensity (war)",
      "Cumulative duration",
      "Post-9/11",
      "Post-Cold War",
      "Coalition support",
      "Type (interstate)",
      "Type (int. intrastate)",
      "Territorial incompatibility"
    ))),
    outcome = factor(outcome, levels = c(
      "Overall provision",
      "Troop support", "Troop presence", "Infrastructure",
      "Weapons", "Materiel & logistics", "Training & expertise",
      "Funding", "Intelligence", "Access to territory"
    )),
    support_type = case_when(
      outcome %in% c("Troop support", "Troop presence") ~ "Direct",
      outcome == "Overall provision" ~ "Overall",
      TRUE ~ "Indirect"
    )
  )

# vif results
vif_results <- purrr::map_dfr(names(model_list), function(nm) {
  v <- car::vif(model_list[[nm]])
  if (is.matrix(v)) {
    tibble::tibble(model = nm, term = rownames(v), vif = v[, "GVIF"])
  } else {
    tibble::tibble(model = nm, term = names(v), vif = v)
  }
})

# save all results in a tibble
save(
  wbm_0, wbm_x, wbm_p, wbm_y, wbm_w, wbm_m, wbm_t, wbm_f, wbm_i, wbm_l,
  coef_df,
  vif_results,
  model_list,
  file = "Paper/model_results.RData"
)
