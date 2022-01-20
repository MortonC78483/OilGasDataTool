# This script formats the wells data to be placed in the tool. It MUST be run each time the wells data is updated.

library(tidyverse)
library(readxl)
library(sf)
library(data.table)

## ----- CLEAN WELLS DATA -----
# Source: https://www.conservation.ca.gov/calgem/maps
all_wells <- fread("RawData/AllWells_table_10_4_2021/AllWells_20211004.csv",
                   select = c("WellStatus", "WellTypeLa", "OperatorNa", "SpudDate", "Latitude", "Longitude", "API")) %>%
  filter(WellStatus != "Canceled")

# Source: David Gonzalez, archived DOGGR data (retrieved 11/06/2021)
doggr_wells <- read_rds("RawData/doggr_wells_full_april2018.rds") %>%
  select(AbdDate, API) %>%
  filter(!is.na(AbdDate)) %>%
  mutate(AbdDate = as.Date(AbdDate, format = "%m/%d/%Y")) %>%
  mutate(AbdDate = as.numeric(substr(AbdDate, 1, 4))) %>%
  filter(API != "02943101") %>%
  mutate(API = paste0("4", API)) %>%
  mutate(API = as.numeric(API))

# Note well with API: 02943101 has an abandoned date of 3016, 
# which is impossible, so it is excluded
merged <- left_join(all_wells, doggr_wells, by = "API")

# Get wells with abandoned date prior to spud date, which is impossible, 
# change abandoned date to NA
impossible_dates <- merged %>%
  filter(!(as.numeric(substr(SpudDate, 7, 10)) <= AbdDate)) %>%
  mutate(AbdDate = NA)

# Add back wells with abandoned date prior to spud date and NA abandoned date
output <- merged %>%
  filter(!(API %in% impossible_dates$API)) %>%
  rbind(impossible_dates)

# Write output file
write_rds(output, "input_data.rds")

## ----- CLEAN SENSITIVE RECEPTORS DATA -----
# Source https://www.cde.ca.gov/ds/si/ds/pubschls.asp
schools <- read_excel("RawData/pubschls.xlsx", skip = 5, col_names = TRUE) %>%
  filter(StatusType == "Active") %>%
  select(c(School, Latitude, Longitude)) %>%
  mutate(type = "school") %>%
  rename("Name" = "School")

# Source https://data.chhs.ca.gov/dataset/hospital-building-data/resource/d97adf28-ebaf-4204-a29e-bb6bdb7f96b9
hospitals <- read_csv("https://data.chhs.ca.gov/dataset/dab37323-5b23-492e-9328-3bcc93bd1335/resource/d97adf28-ebaf-4204-a29e-bb6bdb7f96b9/download/ca-oshpd-gachospital-building-10212021.csv") %>%
  select(c("Facility Name", Latitude, Longitude)) %>%
  mutate(type = "hospital") %>%
  rename("Name" = "Facility Name")

# Put schools and hospitals into one dataset
sens_receptors <- rbind(schools, hospitals)

# Get rid of repeats
sens_unique <- sens_receptors %>%
  unique()

# Write sensitive receptors data
write_rds(sens_unique, "sensitive_receptors.rds")

## ---- CLEAN OIL FIELDS DATA -----
crs_nad83 <- st_crs("+init=epsg:4269 +proj=longlat +ellps=GRS80
                        +datum=NAD83 +no_defs +towgs84=0,0,0")  

# Data from: https://www.conservation.ca.gov/calgem/maps "Field Boundaries"
field_boundaries <- read_sf("RawData/Field_Boundary") %>%
  select(geometry) %>%
  st_transform(crs_nad83)

# Write oil field boundaries data
write_sf(field_boundaries, "Field_Boundary_Cleaned.shp")
  
## ---- SPANISH TRANSLATED INPUT DATA ----
output_spanish <- output
output_spanish$WellStatus <- as.factor(output_spanish$WellStatus)
# translate relevant well statuses
levels(output_spanish$WellStatus) <- c("Abeyance", "Activa", "Fuera de servicio",
                                       "Nuevo", "Tapado", "PluggedOnly", "Unknown")
output_spanish$WellTypeLa <- as.factor(output_spanish$WellTypeLa)  
# translate well types
levels(output_spanish$WellTypeLa) <- c("Inyección de aire",
                                       "Core agujero",
                                       "Ciclo de vapor",
                                       "Gas seco",
                                       "Pozo seco",
                                       "Gas",
                                       "Eliminación de Gas",
                                       "Almacenamiento de Gas", 
                                       "Inyección",
                                       "Gas licuado",
                                       "Multiuso",
                                       "Observación",
                                       "Aceite y gas",
                                       "Presión de Mantenimiento",
                                       "Inundación con vapor",
                                       "Desconocida",
                                       "Eliminación de agua",
                                       "Fuente de agua",
                                       "Inundación con agua")
output_spanish$WellStatus <- as.character(output_spanish$WellStatus)
output_spanish$WellTypeLa <- as.character(output_spanish$WellTypeLa)  
# Write output file
write_rds(output_spanish, "input_data_spanish.rds")

