rm(list = ls())

library(tidyverse)
library(fpp3)
library(janitor)
library(duckplyr)
library(readxl)
library(fpp3)
library(flextable)
library(officer)

df_adultos <- read_excel(
  "./data/Org_aspiração.xlsx",
  col_names = TRUE,
  na = c("-", "")
)

colnames(df_adultos) <- janitor::excel_numeric_to_date(
  as.numeric(colnames(df_adultos)),
  date_system = "modern"
)

colnames(df_adultos)[1] <- "local"
colnames(df_adultos)[2] <- "spp"

df_ad_long <- pivot_longer(
  df_adultos,
  col = -c(local, spp),
  names_to = "data",
  values_to = "n"
) |>
  filter(!is.na(n)) |>
  mutate(data = ymd(data), sem = yearweek(data)) |>
  mutate(spp = str_replace(spp, "Cules", "Culex")) |>
  mutate(spp = str_replace(spp, "Culex", "Cx. quinquefasciatus"))

df_ad_long_mes <- df_ad_long |>
  left_join(data.frame(
    mes = rep(seq(yearmonth("2024-03"), yearmonth("2025-12"), 1), 2),
    data = unique(df_ad_long$data)
  ))


df_ad_long_mes |>
  saveRDS("./data/df_adultos.RDS")


tabela <- df_ad_long_mes |>
  select(mes, local, spp, n) |>
  group_by(mes, local, spp) |>
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = spp,
    values_from = n,
    values_fill = 0
  ) |>
  arrange(mes, local)

linhas_quebra_mes <- which(tabela$mes[-1] != tabela$mes[-nrow(tabela)])

fex_tabela <- tabela |>
  flextable() |>
  set_header_labels(
    mes = "Mês",
    local = "Local",
    `Cx. quinquefasciatus F` = "Fe.",
    `Cx. quinquefasciatus M` = "Ma.",
    `Ae. aegypti F` = "Fe.",
    `Ae. aegypti M` = "Ma.",
    `Ae. albopictus F` = "Fe.",
    `Ae. albopictus M` = "Ma."
  ) |>
  add_header_row(
    values = c(
      "Mês",
      "Local",
      "Cx. quinquefasciatus",
      "Cx. quinquefasciatus",
      "Ae. aegypti",
      "Ae. aegypti",
      "Ae. albopictus",
      "Ae. albopictus"
    )
  ) |>
  add_header_row(
    values = c(
      "Mês",
      "Local",
      "Espécie capturada",
      "Espécie capturada",
      "Espécie capturada",
      "Espécie capturada",
      "Espécie capturada",
      "Espécie capturada"
    )
  ) |>
  merge_h(part = "header") |>
  merge_v(part = "header") |>
  merge_v(j = "mes", part = "body") |>
  rotate(j = "mes", rotation = "btlr", part = "body") |>
  theme_booktabs() |>
  hline(
    i = linhas_quebra_mes,
    j = 2:8,
    border = fp_border(color = "black", width = 0.5)
  ) |>
  align(align = "center", part = "all") |>
  valign(valign = "center", part = "all") |>
  fontsize(size = 10, part = "all") |>
  bold(part = "header") |>
  italic(i = 2, part = "header") |>
  autofit()
fex_tabela
