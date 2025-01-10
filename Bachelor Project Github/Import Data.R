library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

# Loader luftforureningsdata
load("Instituttet/HCAB_street_ALLto2022.Rdata")
luft_hcab <- as.data.frame(station_data)
colnames(luft_hcab)[1] <- "datehour"
load("Instituttet/HCOE_urb_ALLto2022.Rdata")
luft_hcoe <- as.data.frame(station_data)
colnames(luft_hcoe)[1] <- "datehour"
load("Instituttet/JGTV_street_ALLto2022.Rdata")
luft_jgtv <- as.data.frame(data_XY_hourly)
colnames(luft_jgtv)[1] <- "datehour"
load("Instituttet/RISOE_rur_2010to2022 (1).Rdata")
luft_risoe <- as.data.frame(station_data)
colnames(luft_risoe)[1] <- "datehour"

# Loader trafikdata
#trafikdata_2022 <- read.csv("Data/trafikdata_2022_csv(in).csv", fileEncoding = "latin1", sep = ";")
trafikdata_2022 <- read.csv("Vejdirektoratet/samlet_trafik_2015_2022.csv", fileEncoding = "latin1", sep = ";")
colnames(trafikdata_2022)[1] <- "tid"

# Beholder relevante kolonner, de andre er tomme
trafikdata_2022 <- trafikdata_2022[,1:6]

# Retter tiden
trafikdata_2022 <- trafikdata_2022 %>%
  mutate(tid = case_when(
    tid == "01-feb" ~ "01 - 02",
    tid == "02-mar" ~ "02 - 03",
    tid == "03-apr" ~ "03 - 04",
    tid == "04-maj" ~ "04 - 05",
    tid == "05-jun" ~ "05 - 06",
    tid == "06-jul" ~ "06 - 07",
    tid == "07-aug" ~ "07 - 08",
    tid == "08-sep" ~ "08 - 09",
    tid == "09-okt" ~ "09 - 10",
    tid == "10-nov" ~ "10 - 11",
    tid == "11-dec" ~ "11 - 12",
    tid == "dec-13" ~ "12 - 13",
    TRUE ~ tid  # Keep other values unchanged
  ))

# Tilføjer kategori som ny kolonne
trafikdata_2022 <- trafikdata_2022 %>%
  mutate(retningspor = ifelse(tid == "RetningSpor", X.2, NA)) %>%
  fill(retningspor, .direction = "down")

# Tilføjer dato
trafikdata_2022 <- trafikdata_2022 %>%
  mutate(dato = ifelse(tid == "Periode", X, NA)) %>%
  fill(dato, .direction = "down") %>% 
  mutate(dato = str_sub(dato, -10))

# Fjerner uønskede rækker
keep_values <- c(
  "00 - 01", "01 - 02", "02 - 03", "03 - 04", 
  "04 - 05", "05 - 06", "06 - 07", "07 - 08", 
  "08 - 09", "09 - 10", "10 - 11", "11 - 12", 
  "12 - 13", "13 - 14", "14 - 15", "15 - 16", 
  "16 - 17", "17 - 18", "18 - 19", "19 - 20", 
  "20 - 21", "21 - 22", "22 - 23", "23 - 24"
)

trafikdata_2022 <- trafikdata_2022 %>%
  filter(tid %in% keep_values)

# Tilføjer dato med timer
trafikdata_2022 <- trafikdata_2022 %>%
  mutate(
    hour = as.numeric(strsplit(tid, " - ") %>% sapply(function(x) x[2])),
    datehour = paste(dato, sprintf("%02d:00:00", hour)),
    datehour = dmy_hms(datehour)
  )


trafikdata_2022 <- trafikdata_2022 %>%
  select(-dato, -hour, -tid)

# Merger trafikdata og luftforurening
samlet_hcab <- inner_join(x = trafikdata_2022, y = luft_hcab, by = "datehour")
# Merger med luft_hcoe
samlet_hcab <- inner_join(x = samlet_hcab, y = luft_hcoe, by = "datehour")

# Fjerner punktum fra tal og laver numerisk
samlet_hcab <- samlet_hcab %>%
  mutate(
    X.4 = na_if(X.4, ""),
    X.4 = gsub("\\.", "", X.4),
    X.4 = as.numeric(X.4),
    X = gsub("\\.", "", X),
    X = as.numeric(X)
  )

# Samler traffikdata i en ny kolonne
samlet_hcab <- samlet_hcab %>%
  mutate(
    trafik = ifelse(!is.na(X.4), X.4, X),
    trafik = as.numeric(trafik)
  ) %>%
  select(-c(X, X.1, X.2, X.3, X.4))


# Fixer navne
#samlet_hcab$retningspor <- samlet_hcab$retningspor %>%
#  gsub("\u009d", "ø", .) %>%
#  gsub("\u009b", "ø", .)

# Fjerner duplikerede rækker
samlet_hcab <- samlet_hcab %>%
  distinct(datehour, .keep_all = TRUE)

#### Indlæser data fra DMI
api_key <- "&api-key=168a5a3d-61a1-43ab-9c01-4a758c3d65e6"
base_url <- "https://dmigw.govcloud.dk/v2/metObs"
ressource <- "/collections/observation/items"
københavns_lufthavn_id <- "06180"
limit = "300000"
parameter_name <- "wind_speed_past1h"
parameter_name2 <- "wind_dir_past1h"
parameter_name3 <- "humidity_past1h"
parameter_name4 <- "temp_mean_past1h"
#parameter_name5 <- "pressure_at_sea"	

# Få dato fra data
min_date <- min(samlet_hcab$datehour)
max_date <- max(samlet_hcab$datehour)
min_date_utc <- format(min_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
max_date_utc <- format(max_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
date <- paste0("&datetime=", min_date_utc, "/", max_date_utc)


query_1 <- paste0("?limit=",limit ,"&stationId=", københavns_lufthavn_id, date,"&parameterId=",parameter_name, api_key)
query_2 <- paste0("?limit=",limit ,"&stationId=", københavns_lufthavn_id, date,"&parameterId=",parameter_name2, api_key)
query_3 <- paste0("?limit=",limit ,"&stationId=", københavns_lufthavn_id, date,"&parameterId=",parameter_name3, api_key)
query_4 <- paste0("?limit=",limit ,"&stationId=", københavns_lufthavn_id, date,"&parameterId=",parameter_name4, api_key)
#query_5 <- paste0("?limit=",limit ,"&stationId=", københavns_lufthavn_id, date,"&parameterId=",parameter_name5, api_key)

total_url_kbh_wind_speed <- paste0(base_url, ressource, query_1)
total_url_kbh_wind_dir <- paste0(base_url, ressource, query_2)
total_url_kbh_wind_hum <- paste0(base_url, ressource, query_3)
total_url_kbh_wind_temp <- paste0(base_url, ressource, query_4)
#total_url_kbh_atm_pres <- paste0(base_url, ressource, query_5)

# Henter data
kbh_vind_speed <- GET(total_url_kbh_wind_speed)
Sys.sleep(1)
kbh_vind_speed$status_code
kbh_vind_speed <- fromJSON(content(kbh_vind_speed, as = "text"))
kbh_vind_speed <- data.frame(kbh_vind_speed[["features"]][["properties"]])
colnames(kbh_vind_speed)[5] <- "speed"

kbh_vind_dir <- GET(total_url_kbh_wind_dir)
Sys.sleep(1)
kbh_vind_dir$status_code
kbh_vind_dir <- fromJSON(content(kbh_vind_dir, as = "text"))
kbh_vind_dir <- data.frame(kbh_vind_dir[["features"]][["properties"]])
colnames(kbh_vind_dir)[5] <- "dir"

kbh_vind_hum <- GET(total_url_kbh_wind_hum)
Sys.sleep(1)
kbh_vind_hum$status_code
kbh_vind_hum <- fromJSON(content(kbh_vind_hum, as = "text"))
kbh_vind_hum <- data.frame(kbh_vind_hum[["features"]][["properties"]])
colnames(kbh_vind_hum)[5] <- "hum"

kbh_vind_temp <- GET(total_url_kbh_wind_temp)
Sys.sleep(1)
kbh_vind_temp$status_code
kbh_vind_temp <- fromJSON(content(kbh_vind_temp, as = "text"))
kbh_vind_temp <- data.frame(kbh_vind_temp[["features"]][["properties"]])
colnames(kbh_vind_temp)[5] <- "temp"

#kbh_atm_pres <- GET(total_url_kbh_atm_pres)
#Sys.sleep(1)
#kbh_atm_pres$status_code
#kbh_atm_pres <- fromJSON(content(kbh_atm_pres, as = "text"))
#kbh_atm_pres <- data.frame(kbh_atm_pres[["features"]][["properties"]])
#colnames(kbh_atm_pres)[5] <- "pres"



# Merger vind-data
kbh_vind <- merge(kbh_vind_dir, kbh_vind_speed, by = "observed")
kbh_vind <- merge(kbh_vind, kbh_vind_hum, by = "observed")
kbh_vind <- merge(kbh_vind, kbh_vind_temp, by = "observed")
#kbh_vind <- merge(kbh_vind, kbh_atm_pres, by = "observed")

# Ændrer dato kolonnen til at mache de andre
kbh_vind$observed <- as.POSIXct(kbh_vind$observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
kbh_vind$observed <- as.POSIXct(kbh_vind$observed)

# Fjerner uønskede kolonner og ændre kolonnenavne
kbh_vind <- kbh_vind %>%
  select(observed, speed, dir, hum, temp)

colnames(kbh_vind)[1] <- "datehour"
kbh_vind <- kbh_vind %>%
  distinct(datehour, .keep_all = TRUE)

# Ændrer tidszonen til den samme på begge kolonner
kbh_vind$datehour <- with_tz(kbh_vind$datehour, tzone = "Europe/Paris")
samlet_hcab$datehour <- with_tz(samlet_hcab$datehour, tzone = "Europe/Paris")
# Korrigerer tiden
samlet_hcab <- samlet_hcab %>%
  mutate(datehour = datehour - hours(1))
# Merger på data
samlet_hcab <- inner_join(samlet_hcab, kbh_vind, by = "datehour")
samlet_hcab <- samlet_hcab[!is.na(samlet_hcab$c_NOx_HCAB), ]

# Sorterer på samlet trafik
data_2022 <- samlet_hcab %>% 
  filter(retningspor == "Samlet trafik")

# Tager gennemsnit og laver det dagligt
data_2022_dag <- data_2022 %>%
  mutate(datehour = as.Date(datehour)) %>%
  group_by(datehour) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# Laver det dagligt og tager summen af trafik
data_2022_sumtrafik <- data_2022 %>%
  mutate(datehour = as.Date(datehour)) %>% 
  group_by(datehour) %>% 
  summarise(
    daily_traffic = sum(trafik, na.rm = TRUE)
  )

# Tilføjer summon af traffik på main data
data_2022_daglig <- merge(data_2022_dag, data_2022_sumtrafik, by = "datehour")

# Fjerne tomme/irrelevante kolonner
data_2022_daglig <- data_2022_daglig %>%
  select(-c("trafik", "c_SM200-PM25_HCAB", "c_SM200-PM10_HCAB", "c_SM200-PM25_HCOE", "c_SM200-PM10_HCAB"))

data_2022_time <- data_2022 %>%
  select(-c("c_SM200-PM25_HCAB", "c_SM200-PM10_HCAB", "c_SM200-PM25_HCOE", "c_SM200-PM10_HCAB"))

# Tilføjer wind dir i factor
data_2022_time$dir_factor <- cut(
  data_2022_time$dir,
  breaks = c(-22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),
  labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
  include.lowest = TRUE
)

data_2022_daglig$dir_factor <- cut(
  data_2022_daglig$dir,
  breaks = c(-22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),
  labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
  include.lowest = TRUE
)

