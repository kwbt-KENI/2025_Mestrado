rm(list = ls(all = T))

remotes::install_github("rfsaldanha/microdatasus")

library(tidyverse)
library(duckplyr) # pato salva vidas!
# library(fpp3)
library(microdatasus)


# Infodengue --------------------------------------------------------------

infodengue_tab <- read_csv(
  paste0(
    "https://info.dengue.mat.br/api/alertcity?",
    "geocode=",
    4126504,
    "&disease=",
    "dengue",
    "&format=",
    "csv",
    "&ew_start=",
    21,
    "&ew_end=",
    26,
    "&ey_start=",
    2014,
    "&ey_end=",
    2026
  ),
  show_col_types = FALSE
) |>
  arrange(data_iniSE)

infodengue <- infodengue_tab |>
  mutate(Data = data_iniSE, epi_data = yearweek(data_iniSE, week_start = 7)) |>
  select(epi_data, Data, everything()) |>
  select(-data_iniSE, -SE) |>
  select(epi_data, Data, casos, nivel)

View(infodengue)

rm(infodengue_tab)

# MicroDataSUS ------------------------------------------------------------

# dengue_all <- fetch_datasus(year_start = 2016, year_end = 2026, uf = "PR",
#                        information_system = "SINAN-DENGUE",
#                        timeout = 800)
#
# saveRDS(dados, "data/sus.RDS")

# Dengue - tudo - 2015 - 2026 --------------------------------------------
{
  dengue = fetch_datasus(
    year_start = 2015,
    year_end = 2026,
    information_system = "SINAN-DENGUE",
    timeout = 800
  )

  saveRDS(dengue, "data/SINAN_den_2015_2026.RDS")
}

dengue <- readRDS("data/SINAN_den_2015_2026.RDS")

{
  dengue <- readRDS("data/SINAN_den_2015_2026.RDS")
  dengue_fil <- process_sinan_dengue(dengue, municipality_data = TRUE)

  rm(dengue)

  saveRDS(dengue_fil, "data/SINAN_den_2015_2026_FILL.RDS")

  DENV_PR <- dengue_fil |>
    filter(SG_UF_NOT == "Paraná")

  saveRDS(DENV_PR, "data/SINAN_den_2015_2026_fill_PR.RDS")

  rm(DENV_PR)

  DENV_serta <- dengue_fil |>
    filter(ID_MUNICIP == 412650)

  saveRDS(DENV_serta, "data/SINAN_den_2015_2026_fill_PR_Serta.RDS")

  rm(DENV_PR)
  rm(DENV_PR)
}

## 2015 - 2018 -------------------------------------------------------------
### MicroDataSUS - DENV ---------------------------------------------------

{
  dengue = fetch_datasus(
    year_start = 2015,
    year_end = 2018,
    uf = "PR",
    information_system = "SINAN-DENGUE",
    timeout = 800
  )

  dengue_fil <- process_sinan_dengue(dengue, municipality_data = TRUE)

  saveRDS(dengue, "data/den_2015_2018.RDS")
  saveRDS(dengue_fil, "data/den_2015_2018_FILL.RDS")

  DENV_PR <- dengue_fil |>
    filter(SG_UF_NOT == "Paraná")

  DENV_serta <- DENV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(DENV_PR, "data/den_2015_2018_fil_PR.RDS")
  saveRDS(DENV_serta, "data/den_2015_2018_fil_PR_Serta.RDS")
}

### MicroDataSUS - CHIKV --------------------------------------------------
{
  chikungunya = fetch_datasus(
    year_start = 2015,
    year_end = 2018,
    uf = "PR",
    information_system = "SINAN-CHIKUNGUNYA",
    timeout = 800
  )

  chikungunya_fil <- process_sinan_chikungunya(
    chikungunya,
    municipality_data = TRUE
  )

  saveRDS(chikungunya, "data/chik_2015_2018.RDS")
  saveRDS(chikungunya_fil, "data/chik_2015_2018_FILL.RDS")

  CHIKV_PR <- chikungunya_fil |>
    filter(SG_UF_NOT == "Paraná")

  CHIKV_serta <- CHIKV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(CHIKV_PR, "data/chi_2015_2018_fil_PR.RDS")
  saveRDS(CHIKV_serta, "data/chi_2015_2018_fil_PR_Serta.RDS")
}
### MicroDataSUS - ZIKAV ---------------------------------------------------

{
  zika = fetch_datasus(
    year_start = 2015,
    year_end = 2018,
    uf = "PR",
    information_system = "SINAN-ZIKA",
    timeout = 800
  )

  zika_fil <- process_sinan_zika(zika, municipality_data = TRUE)

  saveRDS(zika, "data/zika_2015_2018.RDS")
  saveRDS(zika_fil, "data/zika_2015_2018_FILL.RDS")

  ZIKAV_PR <- zika_fil |>
    filter(SG_UF_NOT == "Paraná")

  ZIKAV_serta <- ZIKAV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(ZIKAV_PR, "data/zik_2015_2018_fil_PR.RDS")
  saveRDS(ZIKAV_serta, "data/zik_2015_2018_fil_PR_Serta.RDS")
  rm(list = ls())
}


## 2019 - 2022 -------------------------------------------------------------
### MicroDataSUS - DENV ---------------------------------------------------

{
  dengue = fetch_datasus(
    year_start = 2019,
    year_end = 2022,
    uf = "PR",
    information_system = "SINAN-DENGUE",
    timeout = 800
  )

  dengue_fil <- process_sinan_dengue(dengue, municipality_data = TRUE)

  saveRDS(dengue, "data/den_2019_2022.RDS")
  saveRDS(dengue_fil, "data/den_2019_2022_FILL.RDS")

  DENV_PR <- dengue_fil |>
    filter(SG_UF_NOT == "Paraná")

  DENV_serta <- DENV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(DENV_PR, "data/den_2019_2022_fil_PR.RDS")
  saveRDS(DENV_serta, "data/den_2019_2022_fil_PR_Serta.RDS")
  rm(list = ls())
}

### MicroDataSUS - CHIKV --------------------------------------------------
{
  chikungunya = fetch_datasus(
    year_start = 2019,
    year_end = 2022,
    uf = "PR",
    information_system = "SINAN-CHIKUNGUNYA",
    timeout = 800
  )

  chikungunya_fil <- process_sinan_chikungunya(
    chikungunya,
    municipality_data = TRUE
  )

  saveRDS(chikungunya, "data/chik_2019_2022.RDS")
  saveRDS(chikungunya_fil, "data/chik_2019_2022_FILL.RDS")

  CHIKV_PR <- chikungunya_fil |>
    filter(SG_UF_NOT == "Paraná")

  CHIKV_serta <- CHIKV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(CHIKV_PR, "data/chi_2019_2022_fil_PR.RDS")
  saveRDS(CHIKV_serta, "data/chi_2019_2022_fil_PR_Serta.RDS")
  rm(list = ls())
}

### MicroDataSUS - ZIKAV ---------------------------------------------------

{
  zika = fetch_datasus(
    year_start = 2019,
    year_end = 2022,
    uf = "PR",
    information_system = "SINAN-ZIKA",
    timeout = 800
  )

  zika_fil <- process_sinan_zika(zika, municipality_data = TRUE)

  saveRDS(zika, "data/zika_2019_2022.RDS")
  saveRDS(zika_fil, "data/zika_2019_2022_FILL.RDS")

  ZIKAV_PR <- zika_fil |>
    filter(SG_UF_NOT == "Paraná")

  ZIKAV_serta <- ZIKAV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(ZIKAV_PR, "data/zik_2019_2022_fil_PR.RDS")
  saveRDS(ZIKAV_serta, "data/zik_2019_2022_fil_PR_Serta.RDS")
  rm(list = ls())
}


## 2023 - 2026 -------------------------------------------------------------
### MicroDataSUS - DENV ---------------------------------------------------
{
  dengue = fetch_datasus(
    year_start = 2023,
    year_end = 2026,
    uf = "PR",
    information_system = "SINAN-DENGUE",
    timeout = 800
  )

  dengue_fil <- process_sinan_dengue(dengue, municipality_data = TRUE)

  saveRDS(dengue, "data/den_2023_2026.RDS")
  saveRDS(dengue_fil, "data/den_2023_2026_FILL.RDS")

  DENV_PR <- dengue_fil |>
    filter(SG_UF_NOT == "Paraná")

  DENV_serta <- DENV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(DENV_PR, "data/den_2023_2026_fil_PR.RDS")
  saveRDS(DENV_serta, "data/den_2023_2026_fil_PR_Serta.RDS")
  rm(list = ls())
}

### MicroDataSUS - CHIKV --------------------------------------------------
{
  chikungunya = fetch_datasus(
    year_start = 2023,
    year_end = 2026,
    uf = "PR",
    information_system = "SINAN-CHIKUNGUNYA",
    timeout = 800
  )

  chikungunya_fil <- process_sinan_chikungunya(
    chikungunya,
    municipality_data = TRUE
  )

  saveRDS(chikungunya, "data/chik_2023_2026.RDS")
  saveRDS(chikungunya_fil, "data/chik_2023_2026_FILL.RDS")

  CHIKV_PR <- chikungunya_fil |>
    filter(SG_UF_NOT == "Paraná")

  CHIKV_serta <- CHIKV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(CHIKV_PR, "data/chi_2023_2026_fil_PR.RDS")
  saveRDS(CHIKV_serta, "data/chi_2023_2026_fil_PR_Serta.RDS")
  rm(list = ls())
}


### MicroDataSUS - ZIKAV ---------------------------------------------------

{
  zika = fetch_datasus(
    year_start = 2023,
    year_end = 2026,
    uf = "PR",
    information_system = "SINAN-ZIKA",
    timeout = 800
  )

  zika_fil <- process_sinan_zika(zika, municipality_data = TRUE)

  saveRDS(zika, "data/zika_2023_2026.RDS")
  saveRDS(zika_fil, "data/zika_2023_2026_FILL.RDS")

  ZIKAV_PR <- zika_fil |>
    filter(SG_UF_NOT == "Paraná")

  ZIKAV_serta <- ZIKAV_PR |>
    filter(ID_MUNICIP == 412650)

  saveRDS(ZIKAV_PR, "data/zik_2023_2026_fil_PR.RDS")
  saveRDS(ZIKAV_serta, "data/zik_2023_2026_fil_PR_Serta.RDS")
  rm(list = ls())
}
