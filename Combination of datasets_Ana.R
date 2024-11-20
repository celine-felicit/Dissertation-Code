library(readxl)
library(dplyr)

dyad <- read_excel("ucdp-dyadic-181.xlsx")

dyad_esd <- read_excel("ucdp-esd-dy-181.xlsx")


#dyad data merging####
#check which columns have matching names and values in both datasets
common_columns <-intersect(colnames(dyad), colnames(dyad_esd))

for (col in common_columns) {
  if (length(intersect(dyad[[col]], dyad_esd[[col]])) > 0) {
    print(paste("Column", col, "has matching values"))
  } else {
    print(paste("Column", col, "no matching values"))
  }
}

#check number of unique combinations
#checking for a "primary key"
nrow(dyad %>% distinct(dyad_id, year))
nrow(dyad_esd %>% distinct(dyad_id,year))

#two ways of merging
#merge keeps some NAs, inner_join does not
merged_d <- dyad %>%
  inner_join(dyad_esd, by= c("dyad_id", "year")) #%>%
#use the following to remove duplicate columns
#select(-contains(".y"))

#before removing the duplicate columns
#maybe you can check with summary statistics if
#the values are the same in the columns
#for example the code below gives you the
#name of the columns that are duplicates
#the code give all those that have .y at the end
#which means there is another column with the same name
#but with .x at the end

merged_d %>% select(ends_with(".y")) %>%
  names()

#the summary for these two is the same so we can
#definetely delete one of them
#check for the rest just to be sure,
#but they can probably be deleted
summary(merged_d$conflict_id.x)
summary(merged_d$conflict_id.y)

merged_dyad<- merge(dyad,dyad_esd, by= c("dyad_id", "year"), all.y=TRUE)

#triad data merging####
tryad <- read_excel("ucdp-esd-ty-181.xlsx")

common_columns <- intersect(colnames(dyad), colnames(tryad))
for (col in common_columns) {
  overlap <- length(intersect(dyad[[col]], tryad[[col]]))
  cat("Column:", col, "- Matching values:", overlap, "\n")
}

for (col in common_columns) {
  if (length(intersect(dyad[[col]], tryad[[col]])) > 0) {
    print(paste("Column", col, "has matching values"))
  } else {
    print(paste("Column", col, "no matching values"))
  }
}

#check number of unique combinations
#checking for a "primary key"
nrow(dyad %>% distinct(dyad_id, year, conflict_id))
nrow(tryad %>% distinct(dyad_id,year,conflict_id))

merged_ty <- dyad %>%
  inner_join(tryad, by= c("dyad_id", "year","conflict_id")) # %>%
#select(-contains(".y"))

merged_ty %>% select(ends_with(".y")) %>%
  names()

#check how many rows have unique values
nrow(unique(merged_ty))