## ----message=FALSE, warning=FALSE---------------------------------------------
# Load packages
library(NHANES)         # NHANES dataset
library(dplyr)          # Data manipulation
library(ExclusionTable)

# Attach data
data("NHANES")

## -----------------------------------------------------------------------------
# Subset NHANES data using dplyr::filter
NHANES_subset <- NHANES %>% 
  filter(Gender == "female",
         Age    >= 65,
         !is.na(BMI))

# Print number of observations
nrow(NHANES_subset)

## -----------------------------------------------------------------------------
exclusion_table(NHANES,
                inclusion_criteria = c("Gender == 'female'",
                                       "Age    >= 65"),
                exclusion_criteria = "is.na(BMI)",
                keep_data = FALSE)

## -----------------------------------------------------------------------------
exclusion_table(NHANES,
                inclusion_criteria = c("Gender == 'female'",
                                       "Age    >= 65"),
                exclusion_criteria = "is.na(BMI)",
                labels_inclusion   = c("Get females",
                                       "Age is >= 65"),
                labels_exclusion   = "Missing BMI",
                keep_data = FALSE)

## ----message=FALSE------------------------------------------------------------
NHANES_ex_tab <- 
  exclusion_table(NHANES,
                  inclusion_criteria = c("Gender == 'female'",
                                         "Age    >= 65"),
                  exclusion_criteria = "is.na(BMI)",
                  labels_inclusion   = c("Get females",
                                         "Age is >= 65"),
                  labels_exclusion   = "Missing BMI",
                  keep_data = TRUE)

# Print structure
str(NHANES_ex_tab, 1)

## -----------------------------------------------------------------------------
NAHANES_cleaned <- NHANES_ex_tab[["dataset"]]

nrow(NAHANES_cleaned)

## -----------------------------------------------------------------------------
room_selection <- c(2, 4, 9)

exclusion_table(NHANES,
                inclusion_criteria = c("HomeRooms %in% obj$room_selection"),
                labels_inclusion   = c("2, 4, 9 rooms"),
                obj = list(room_selection = room_selection))

## ----MESSAGE=FALSE------------------------------------------------------------
NHANES_ex_id <- 
  exclusion_table(NHANES,
                  exclusion_criteria = "is.na(BMI)",
                  labels_exclusion   = "Missing BMI",
                  id                 = "ID",
                  keep_data = FALSE)

NHANES_ex_id$table_ex

