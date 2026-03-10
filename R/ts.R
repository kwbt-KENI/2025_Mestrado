rm(list = ls(all = T))

library(readxl)
library(fpp3)
library(sf)
library(igraph)


dados <- read_excel("./data/dados.xlsx", col_names = TRUE, na = c("-", ""))

colnames(dados) <- janitor::excel_numeric_to_date(
  as.numeric(colnames(dados)),
  date_system = "modern"
)
colnames(dados)[1] <- "armadilha"

df_ovos <- pivot_longer(
  dados,
  col = !armadilha,
  names_to = "data",
  values_to = "ovos"
) |>
  mutate(
    armadilha = armadilha,
    data = ymd(data),
    sem = yearweek(data),
    ovos = as.integer(ovos)
  ) |>
  filter(!is.na(ovos))

View(df_ovos)

## sem padrão aparente
## possivelmente por armadilhas estarem sendo incluídas
df_ovos |>
  group_by(sem) |>
  reframe(
    IDO = sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE),
    IPO = (sum(ovos > 0, na.rm = TRUE) / sum(ovos >= 0, na.rm = TRUE)) *
      100,
    tot_ovos = sum(ovos, na.rm = TRUE),
    media = mean(ovos, na.rm = TRUE),
    desvio_padrao = sd(ovos, na.rm = TRUE),
    data = data
  ) |>
  unique() |>
  as_tsibble(index = sem) |>
  autoplot(desvio_padrao)

## padrão?
## separar as armadilhas mais estáveis, desdo início do projeto
### ai são: A1-A12, A14-A18, A20-A22, A24, A26, A28-A29, A31-36

# View(df_ovos)
df_ovos |>
  filter(armadilha %in% c(1:12, 14:18, 20:22, 24, 26, 28, 29, 31:36)) |>
  group_by(sem) |>
  reframe(
    IDO = sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE),
    IPO = (sum(ovos > 0, na.rm = TRUE) / sum(ovos >= 0, na.rm = TRUE)) *
      100,
    tot_ovos = sum(ovos, na.rm = TRUE),
    media = mean(ovos, na.rm = TRUE),
    desvio_padrao = sd(ovos, na.rm = TRUE),
    data = data
  ) |>
  unique() |>
  as_tsibble(index = sem) |>
  autoplot(IDO)
