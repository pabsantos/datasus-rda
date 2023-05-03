library(microdatasus)
library(lubridate)
library(tidyverse)
library(readODS)
library(janitor)

source("utils.R")

datasus_causas_externas <- fetch_datasus(
    year_start = 1996,
    year_end = 2021,
    information_system = "SIM-DOEXT", 
    vars = c(
        "CAUSABAS", "CODMUNOCOR", "DTOBITO", "IDADE", "SEXO", "RACACOR", "ESC",
        "OCUP"
    )
)

# datasus_url_2021 <- 
#     "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO21OPEN.csv"

datasus_url_2022 <- 
    "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO22OPEN.csv"

datasus_previa <- read_datasus_preliminar(datasus_url_2022)

datasus_total <- join_datasus_db()

ibge_municipios <- read_ods("data/ibge_lista_municipios.ods", skip = 5)

datasus_transito_sim <- arrange_datasus_sim()

lista_municipios <- create_lista_municipios()

datasus_sim <- join_datasus_municipios()
    
save(datasus_sim, file = "rda/datasus-sim-2022.rda")
