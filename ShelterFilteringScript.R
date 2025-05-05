rm(list = ls())
library(tidyverse)
library(dplyr)

# PROJECT TYPE DESCRIPTIONS
{
# Emergency Shelter (ES): Provides short-term or nightly accommodation for 
# individuals or families experiencing homelessness.

# Transitional Housing (TH): Offers time-limited housing (up to 24 months) 
# with supportive services to help transition to permanent housing.

# Safe Haven (SH): A low-barrier facility for hard-to-serve individuals (often 
# with mental health issues), providing 24-hour residence with minimal demands.

# Permanent Supportive Housing (PSH): Long-term housing with supportive services,
# typically reserved for people with disabilities and histories of homelessness.

# Rapid Re-housing (RRH): Provides short-term rental assistance and services to 
# quickly move individuals/families from homelessness to permanent housing.

# Other Permanent Housing (OPH): Permanent housing for formerly homeless people 
# that does not fall under PSH or RRH (may or may not include services).
}

# BED TYPE DESCRIPTIONS
{
# Facility-Based Beds (F): Beds located in a dedicated homeless assistance facility.
  
# Voucher Beds (V): Beds provided through hotel or motel vouchers funded by 
# homeless assistance programs.
  
# Other Beds (O): Beds located in non-dedicated settings, such as churches or 
# community centers that temporarily offer shelter.
}

# Read in any of the HIC.csv's 
all_shelters <- read.csv("/Users/tristanpoul/Desktop/ECON 192/Winter 2025/Datasets192/2023-HIC-Counts-by-State.csv")

# filter for CA and bay area shelters
# output: df_ca with CA shelters and df_bay with bay area shelters
{
# filter for only CA shelters
df_ca <- all_shelters %>%
  filter(CocState == "CA")

# filter for only bay area shelters
  bay_area_cocs <- c(
    "San Jose/Santa Clara City & County CoC",
    "San Francisco CoC",
    "Oakland, Berkeley/Alameda County CoC",
    "Richmond/Contra Costa County CoC",
    "Daly/San Mateo County CoC",
    "Santa Rosa, Petaluma/Sonoma County CoC",
    "Napa City & County CoC",
    "Vallejo/Solano County CoC"
  )
  
  # Filter dataset for only Bay Area CoCs
  df_bay <- df_ca %>% filter(CoC %in% bay_area_cocs)
  }

# filter so target population isn't for domestic violence or HIV
{
  df_no_DV_or_HIV <- df_bay %>%
    filter(is.na(Target.Population))
}

# filter for Project Type and Bed Type that are relevant to our project
{
  # We exclude Rapid Re-housing (RRH) and Other Permanent Housing (OPH) because 
  # they provide more stable, private housing rather than concentrated shelter 
  # environments. RRH focuses on quickly placing individuals and families into 
  # regular rental housing, reducing their interaction with high-crime areas. 
  # OPH, unless it includes supportive services, functions similarly to regular 
  # housing and does not create the same community-level impact as shelters. 
  # Voucher Beds (V) are also removed because they are spread across hotels and 
  # motels rather than centralized in dedicated facilities, making their impact 
  # on crime more diffuse and harder to analyze. By focusing on shelters with 
  # concentrated populations and higher turnover, we better capture the 
  # potential relationship between housing instability and crime.
  
  # keep all project and bed types that shouldn't be dropped
  df_without_V_and_RRH_OPH <- df_no_DV_or_HIV %>%
    filter(!(Bed.Type %in% c("V")) & !(Project.Type %in% c("RRH", "OPH")))

}

# make a final df that filters out any shelters that were missed with
# Project.Type and Bed.Type filtering:
# Fuzzy search Project.Name for keywords
{
  # Define keywords to search for
  keywords <- c("Women", "Children", "Family", "Families", "Youth", "Mother", "Infant", "Child", "Parent", "Woman")
  
  # Create a regex pattern
  pattern <- paste(keywords, collapse = "|")
  
  # Filter out rows that match the pattern
  almost_final_shelters <- df_without_V_and_RRH_OPH %>%
    filter(!grepl(pattern, Project.Name, ignore.case = TRUE))
  
}

# Optional: keep only relevant columns to not have ~80 unused columns
{
  columns_to_keep <- c(
    names(almost_final_shelters)[1:10], 
    "Project.ID",
    "Project.Name",
    "HMIS.Project.ID",
    "HIC.Date",
    "Project.Type",
    "Bed.Type",
    "Geo.Code",
    "Target.Population", 
    "city",
    "state",
    "zip",
    "Total.Beds",
    "PIT.Count",
    "housingType",
    "Total.Beds"
  )
  
  # Filter dataset to only keep selected columns
  final_shelters <- almost_final_shelters %>%
    select(all_of(columns_to_keep))
}


# write.csv(final_shelters, "/Users/tristanpoul/Desktop/ECON 192/Winter 2025/Datasets192/final_shelters.csv", row.names = FALSE)

# Comments:
# the final_shelters df is highly dependant on which CoC's you include in the
# bay area filter, as well as which project types you keep in the analysis. 
# I included all bay area CoC's the dataset had, as I didn't know exactly which
# counties we have the data for.




