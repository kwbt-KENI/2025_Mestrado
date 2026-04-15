rm(list = ls(all = T))

library(tidyverse)
library(fpp3)
library(janitor)
library(duckplyr)


# Dados - Paraná ----------------------------------------------------------

denv_2015 <- readRDS("data/den_2015_2018_fil_PR.RDS") |>
  remove_empty(which = "cols", cutoff = 1) |>
  select(-DOENCA_TRA)

denv_2015$SOROTIPO <- NA
denv_2015$RESUL_PCR_ <- NA
denv_2015$DT_PCR <- NA
denv_2015$DT_OBITO <- NA
denv_2015$DT_DIGITA <- NA
denv_2015$ANO_NASC <- NA

denv_2019 <- readRDS("data/den_2019_2022_fil_PR.RDS") |>
  remove_empty(which = "cols", cutoff = 1)

denv_2023 <- readRDS("data/den_2023_2026_fil_PR.RDS") |>
  filter(!SEM_PRI > 202553) |>
  remove_empty(which = "cols", cutoff = 1)

denv_2019$COMPLICA <- NA
denv_2019$DT_CHIK_S2 <- NA
denv_2019$EPISTAXE <- NA
denv_2019$EVIDENCIA <- NA
denv_2019$GENGIVO <- NA
denv_2019$HEMATURA <- NA
denv_2019$LACO_N <- NA
denv_2019$MANI_HEMOR <- NA
denv_2019$METRO <- NA
denv_2019$PETEQUIAS <- NA
denv_2019$PLAQ_MENOR <- NA
denv_2019$PLASMATICO <- NA
denv_2019$SANGRAM <- NA

denv_2023$DT_NASC <- NA
denv_2023$COMPLICA <- NA
denv_2023$DT_CHIK_S2 <- NA
denv_2023$EPISTAXE <- NA
denv_2023$EVIDENCIA <- NA
denv_2023$GENGIVO <- NA
denv_2023$HEMATURA <- NA
denv_2023$LACO_N <- NA
denv_2023$MANI_HEMOR <- NA
denv_2023$METRO <- NA
denv_2023$PETEQUIAS <- NA
denv_2023$PLAQ_MENOR <- NA
denv_2023$PLASMATICO <- NA
denv_2023$SANGRAM <- NA

# compare_df_cols(denv_2015, denv_2019, denv_2023)

denv_15_sem <- denv_2015 |>
  mutate(SEM_PRI = as.numeric(SEM_PRI)) |>
  filter(SEM_PRI > 201450) |>
  mutate(
    ano_sem = yearweek(
      str_c(
        str_sub(
          SEM_PRI,
          start = 1,
          end = 4
        ),
        str_c(
          str_sub(
            SEM_PRI,
            start = 5,
            end = 6
          ),
          "W"
        ),
        sep = " "
      ),
      week_start = getOption("lubridate.week.start", 7)
    )
  ) |>
  arrange(ano_sem)

saveRDS(denv_15_sem, "data/den_2015_2018_fil_PR_sem.RDS")

denv_19_sem <- denv_2019 |>
  mutate(SEM_PRI = as.numeric(SEM_PRI)) |>
  filter(SEM_PRI > 201450) |>
  mutate(
    ano_sem = yearweek(
      str_c(
        str_sub(
          SEM_PRI,
          start = 1,
          end = 4
        ),
        str_c(
          str_sub(
            SEM_PRI,
            start = 5,
            end = 6
          ),
          "W"
        ),
        sep = " "
      ),
      week_start = getOption("lubridate.week.start", 7)
    )
  )

saveRDS(denv_19_sem, "data/den_2019_2022_fil_PR_sem.RDS")

denv_23_sem <- denv_2023 |>
  mutate(SEM_PRI = as.numeric(SEM_PRI)) |>
  filter(SEM_PRI > 201450) |>
  mutate(
    ano_sem = yearweek(
      str_c(
        str_sub(
          SEM_PRI,
          start = 1,
          end = 4
        ),
        str_c(
          str_sub(
            SEM_PRI,
            start = 5,
            end = 6
          ),
          "W"
        ),
        sep = " "
      ),
      week_start = getOption("lubridate.week.start", 7)
    )
  )

saveRDS(denv_23_sem, "data/den_2023_2026_fil_PR_sem.RDS")

denV_PR_15_25_sem <- rbind(denv_15_sem, denv_19_sem, denv_23_sem)
saveRDS(denV_PR_15_25_sem, "data/den_2015_2026_fil_PR_sem.RDS")

casos_denV_PR_15_25_semanal <- denV_PR_15_25_sem |>
  group_by(ano_sem) |>
  summarise(total_casos = n()) |>
  mutate(
    pop = 11890517,
    casos_hab = total_casos / 11890517,
    porcent = casos_hab * 100,
    porcent_round = round(porcent, digits = 6)
  )

saveRDS(casos_denV_PR_15_25_semanal, "data/casos_denV_2015_2026_PR_semanal.RDS")


# Dados - Sertanópolis ----------------------------------------------------

#### (comentário)
#### Não teve casos de Zika em Sertanópolis, no período de 2015 à 2025
# zikaV_serta_15_25 <- rbind(
#   readRDS("data/zik_2015_2018_fil_PR_Serta.RDS"),
#   readRDS("data/zik_2019_2022_fil_PR_Serta.RDS"),
#   readRDS("data/zik_2023_2026_fil_PR_Serta.RDS")
# )

chikV_serta_15_25 <- rbind(
  readRDS("data/chi_2015_2018_fil_PR_Serta.RDS"),
  readRDS("data/chi_2019_2022_fil_PR_Serta.RDS"),
  readRDS("data/chi_2023_2026_fil_PR_Serta.RDS")
)

View(chikV_serta_15_25)

chikV_serta_15_25 |>
  filter(CLASSI_FIN == c("Descartado"))

# denV_serta_15_25 <- rbind(
#   readRDS("data/den_2015_2018_fil_PR_Serta.RDS"),
#   readRDS("data/den_2019_2022_fil_PR_Serta.RDS"),
#   readRDS("data/den_2023_2026_fil_PR_Serta.RDS")
# )

denv_2015 <- readRDS("data/den_2015_2018_fil_PR_Serta.RDS") |>
  remove_empty(which = "cols", cutoff = 1) |>
  select(-DOENCA_TRA)

denv_2015$SOROTIPO <- NA
denv_2015$RESUL_PCR_ <- NA
denv_2015$DT_PCR <- NA
denv_2015$DT_OBITO <- NA
denv_2015$DT_DIGITA <- NA
denv_2015$ANO_NASC <- NA

denv_2019 <- readRDS("data/den_2019_2022_fil_PR_Serta.RDS") |>
  remove_empty(which = "cols", cutoff = 1)

denv_2023 <- readRDS("data/den_2023_2026_fil_PR_Serta.RDS") |>
  filter(!SEM_PRI > 202553) |>
  remove_empty(which = "cols", cutoff = 1)

denv_2023$DT_NASC <- NA

compare_df_cols(denv_2015, denv_2019, denv_2023)

denV_Serta_15_25 <- rbind(denv_2015, denv_2019, denv_2023)


# Obitos em Sertanopolis --------------------------------------------------

denV_Serta_15_25 |>
  filter(EVOLUCAO == "Óbito por dengue") |>
  select(NU_IDADE_N, DT_SIN_PRI, DT_OBITO) |>
  mutate(
    NU_IDADE_N = as.integer(NU_IDADE_N) - 4000,
    DT_SIN_PRI = as_date(DT_SIN_PRI),
    DT_OBITO = as_date(DT_OBITO),
    Pri_ate_obito = DT_OBITO - DT_SIN_PRI
  )


# Detecção de Sorotipo de Dengue ------------------------------------------

## Sem imunohistoquímica para controlar:
denV_Serta_15_25 |>
  select(DT_SIN_PRI, SOROTIPO, IMUNOH_N, ID_OCUPA_N, TPAUTOCTO) |>
  remove_empty(which = "rows", cutoff = 1) |>
  filter(!is.na(SOROTIPO)) |>
  mutate(DT_SIN_PRI = as_date(DT_SIN_PRI)) |>
  View()

## Com imunohistoquímica para controlar:
denV_Serta_15_25 |>
  select(DT_SIN_PRI, SOROTIPO, IMUNOH_N) |>
  remove_empty(which = "rows", cutoff = 1) |>
  filter(!is.na(SOROTIPO)) |>
  filter(IMUNOH_N != "Não") |>
  filter(!is.na(IMUNOH_N)) |>
  mutate(DT_SIN_PRI = as_date(DT_SIN_PRI)) |>
  View()


# calculo - semanal -------------------------------------------------------

denV_Serta_15_25 <- denV_Serta_15_25 |>
  arrange(SEM_PRI) |>
  mutate(
    ano = str_sub(SEM_PRI, start = 1, end = 4),
    sem = str_sub(SEM_PRI, start = 5, end = 6),
    W = "W",
    ano_sem = yearweek(
      str_c(ano, str_c(sem, W), sep = " "),
      week_start = getOption("lubridate.week.start", 7)
    )
  )

denV_Serta_15_25_semanal <- denV_Serta_15_25 |>
  group_by(ano_sem) |>
  summarise(total_casos = n())

den_casos_sem_2020_2025 <- denV_Serta_15_25_semanal |>
  filter(
    ano_sem >
      yearweek("2019 W52", week_start = getOption("lubridate.week.start", 7))
  )

saveRDS(den_casos_sem_2020_2025, "data/den_casos_sem_2020_2025.RDS")
