rm(list = ls(all = T))

library(tidyverse)
library(janitor)
library(readxl)
library(fpp3)
library(ggridges)
library(viridis)
library(showtext)
library(sf)
library(igraph)
library(ggtext)

# Calc métrico -----------------------------------------------------------

urb_area <- st_read("./data/shp/area_urbana_sertanopolis_250m_31982.shp")
cord_sf <- st_read("./data/shp/pontos_a50_2025-ago-19.shp")

proximidade <- st_is_within_distance(cord_sf, dist = 250)

cord_sf$grupos <- igraph::components(graph_from_adj_list(
  proximidade,
  mode = "all"
))$membership

cord_sf_metrico <- cord_sf |>
  add_count(grupos) |>
  mutate(grupo_cor = ifelse(n > 1, grupos, NA)) |>
  select(-n) |>
  mutate(
    grupo_id = case_when(
      is.na(grupo_cor) ~ "Isolados (>250m)",
      grupo_cor == 1 ~ "G01 - A01 e A06", # desdo inicio
      grupo_cor == 11 ~ "G02 - A12, A48 e A34", # adição A48 em 07/08/2025
      grupo_cor == 13 ~ "G03 - A14 e A15", # desdo inicio
      grupo_cor == 12 ~ "G04 - A13 e A43", # adição da A43 em 04/12/2023
      grupo_cor == 15 ~ "G05 - A17, A18 e A38", # adição da A38 em 04/12/2023
      grupo_cor == 17 ~ "G06 - A20 e A28", # desdo inicio
      grupo_cor == 19 ~ "G07 - A22 e A50", # adição da A50 em 19/08/2025
      grupo_cor == 20 ~ "G08 - A23 e A24", # alteração da A23 em 09/12/2024
      grupo_cor == 23 ~ "G09 - A27 e A40", # alteração da A27 em 04/12/2023
      grupo_cor == 28 ~ "G10 - A33 e A42", # adição da A42 em 04/12/2023
      grupo_cor == 33 ~ "G11 - A41 e A49" # adição da A41 em 04/12/2023 e A49 em 19/08/2025
    )
  ) |>
  rename(armadilha = ï..Pont) |>
  arrange(grupo_id) |>
  select(-Class)

# Dados ovitrampas -------------------------------------------------------

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
  filter(!is.na(ovos)) |>
  arrange(data)

df_ovos_cord <- df_ovos |>
  left_join(cord_sf_metrico, by = "armadilha") |>
  left_join(
    data.frame(
      ano_mes = rep(
        seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
        each = 2
      ),
      data = unique(df_ovos$data)
    ),
    by = "data"
  ) |>
  group_by(ano_mes, geometry) |>
  mutate(
    IDO_mes = sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE),
    IDO_mes = ifelse(is.nan(IDO_mes), 0, IDO_mes)
  ) |>
  ungroup() |>
  group_by(ano_mes, grupos) |> # só podem ser considerados os grupos a partir das datas corretas
  mutate(
    IDO_grupo = sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE),
    IDO_grupo = ifelse(is.nan(IDO_grupo), 0, IDO_grupo)
  ) |>
  ungroup() |>
  select(-data, -ovos, -sem, -Longitude, -Latitude, -grupo_cor) |>
  distinct()

agrupados <- df_ovos_cord |>
  filter(grupo_id != "Isolados (>250m)") |>
  mutate(difere = IDO_mes - IDO_grupo) |>
  glimpse()

boxplot(agrupados$difere)


## percebe-se que existem IDOs de locais agrupados que não seguem um padrão...
### vamos separar por grupos e ver cada uma das obs então

# G01 ---------------------------------------------------------------------
## G01 - Armadilha 1 e 6 - dados desdo início
A01 <- df_ovos_cord |>
  filter(armadilha == 1) |>
  mutate(armadilha = as.character(armadilha)) |>
  glimpse()

A06 <- df_ovos_cord |>
  filter(armadilha == 6) |>
  mutate(armadilha = as.character(armadilha)) |>
  glimpse()

Plot_G01 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 200, 300),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A01,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A06,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas no Municípios de Sertanópolis/PR, <br> entre Outubro/2023 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas com proximidade de até 250 metros de distância",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black",
      angle = 30
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

Plot_G01

# G02 ---------------------------------------------------------------------
## G02 - A12, A48 e A34 - adição A48 em 07/08/2025

A12 <- df_ovos_cord |>
  filter(armadilha == 12) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

A48 <- df_ovos_cord |>
  filter(armadilha == 48) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

A34 <- df_ovos_cord |>
  filter(armadilha == 34) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

Plot_G02 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 150, 200, 250, 300),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A12,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A48,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A34,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Setembro/2025 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G03 ---------------------------------------------------------------------
## G03 - A14 e A15 - desdo inicio

A14 <- df_ovos_cord |>
  filter(armadilha == 14) |>
  mutate(armadilha = as.character(armadilha)) |>
  glimpse()

A15 <- df_ovos_cord |>
  filter(armadilha == 15) |>
  mutate(armadilha = as.character(armadilha)) |>
  glimpse()


Plot_G03 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(
      0,
      50,
      100,
      150,
      200,
      250,
      300,
      350,
      400,
      450,
      500,
      550,
      600
    ),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A14,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A15,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Outubro/2023 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )


# G04 ---------------------------------------------------------------------
## G04 - A13 e A43 - adição da A43 em 04/12/2023

A13 <- df_ovos_cord |>
  filter(armadilha == 13) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

A43 <- df_ovos_cord |>
  filter(armadilha == 43) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()


Plot_G04 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(
      0,
      50,
      100,
      150,
      200,
      250,
      300,
      350,
      400,
      450,
      500,
      550,
      600
    ),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A13,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A43,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Dezembro/2023 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G05 ---------------------------------------------------------------------
## G05 - A17, A18 e A38 - adição da A38 em 04/12/2023

A17 <- df_ovos_cord |>
  filter(armadilha == 17) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

A18 <- df_ovos_cord |>
  filter(armadilha == 18) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

A38 <- df_ovos_cord |>
  filter(armadilha == 38) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()


Plot_G05 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A17,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A18,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A38,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Novembro/2025 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G06 ---------------------------------------------------------------------
## G06 - A20 e A28 - desdo inicio

A20 <- df_ovos_cord |>
  filter(armadilha == 20) |>
  mutate(armadilha = as.character(armadilha)) |>
  glimpse()

A28 <- df_ovos_cord |>
  filter(armadilha == 28) |>
  mutate(armadilha = as.character(armadilha)) |>
  glimpse()


Plot_G06 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 150, 200, 250, 300),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A20,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A28,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Outubro/2023 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G07 ---------------------------------------------------------------------
## G07 - A22 e A50 - adição da A50 em 19/08/2025

A22 <- df_ovos_cord |>
  filter(armadilha == 22) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

A50 <- df_ovos_cord |>
  filter(armadilha == 50) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

Plot_G07 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 150, 200, 250, 300),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A22,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A50,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Outubro/2025 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G08 ---------------------------------------------------------------------
## G08 - A23 e A24 - alteração da A23 em 09/12/2024

A23 <- df_ovos_cord |>
  filter(armadilha == 23) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2024-12")) |>
  glimpse()

A24 <- df_ovos_cord |>
  filter(armadilha == 24) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2024-12")) |>
  glimpse()

Plot_G08 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 150, 200, 250, 300),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A23,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A24,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Janeiro/2025 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G09 ---------------------------------------------------------------------
## G09 - A27 e A40 - alteração da A27 em 04/12/2023
A27 <- df_ovos_cord |>
  filter(armadilha == 27) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

A40 <- df_ovos_cord |>
  filter(armadilha == 40) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

Plot_G09 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A27,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A40,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Dezembro/2025 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G10 ---------------------------------------------------------------------
## G10 - A33 e A42 - adição da A42 em 04/12/2023
A33 <- df_ovos_cord |>
  filter(armadilha == 33) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

A42 <- df_ovos_cord |>
  filter(armadilha == 42) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2023-11")) |>
  glimpse()

Plot_G10 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(
      0,
      50,
      100,
      150,
      200,
      250,
      300,
      350,
      400,
      450,
      500,
      550,
      600,
      650,
      700
    ),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A33,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A42,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Dezembro/2023 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# G11 ---------------------------------------------------------------------
## G11 - A41 e A49 - adição da A41 em 04/12/2023 e A49 em 19/08/2025
A41 <- df_ovos_cord |>
  filter(armadilha == 41) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

A49 <- df_ovos_cord |>
  filter(armadilha == 49) |>
  mutate(armadilha = as.character(armadilha)) |>
  filter(ano_mes > yearmonth("2025-09")) |>
  glimpse()

Plot_G11 <- df_ovos_cord |>
  ggplot(aes(x = ano_mes, colour = armadilha)) +
  geom_hline(
    yintercept = c(
      0,
      50,
      100,
      150,
      200,
      250,
      300,
      350,
      400,
      450,
      500,
      550,
      600,
      650,
      700
    ),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = A41,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  geom_line(
    data = A49,
    aes(x = ano_mes, y = IDO_mes),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_yearmonth(
    breaks = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
    labels = seq(yearmonth("2023-10"), yearmonth("2026-01"), 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparativo do Índice de Densidade de Ovos (IDO) calculado mensalmente por Ovitrampas instaladas <br> em até 250 metros de distância no Municípios de Sertanópolis/PR, entre Outubro/2025 até Janeiro/2026",
    subtitle = "Comparativo entre Ovitrampas",
    colour = "Armadilha"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(0, 0, 0, 0),
      hjust = 0
    ),
    plot.margin = margin(5, 20, 10, 15),
    axis.text.x = element_text(
      size = 8.5,
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

# Plots -------------------------------------------------------------------

dist_pontos
Plot_G01
Plot_G02
Plot_G03
Plot_G04
Plot_G05
Plot_G06
Plot_G07
Plot_G08
Plot_G09
Plot_G10
Plot_G11
