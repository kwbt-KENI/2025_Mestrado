rm(list = ls(all = T))

library(tidyverse)
library(readxl)
library(sf)
library(igraph)
library(ggnewscale)
library(ggtext)
library(showtext)
library(duckplyr)
library(ggspatial)

#### Mapa de Sertanópolis ####

# vou fazer no Qgis...

#### Mapa de distâncias entre pontos #### - obs. padronizar shp pra Sirgas2000 UTM (31982)
urb_area <- st_read("./data/shp/area_urbana_sertanopolis_250m_31982.shp")
cord_sf <- st_read("./data/shp/pontos_a50_2025-ago-19.shp")

# Shp do OpenStreetMap
ruas <- st_read("./data/shp/ruas.shp")
rios <- st_read("./data/shp/agua_fluvial.shp")
corpos_hidricos <- st_read("./data/shp/corpos_hidricos.shp")


proximidade <- st_is_within_distance(cord_sf, dist = 250)

cord_sf$grupos <- igraph::components(graph_from_adj_list(
  proximidade,
  mode = "all"
))$membership

cord_sf_metrico

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
  select(-Class, -grupo_cor) |>
  glimpse()

corte_mapa_sertanopolis <- st_coordinates(st_transform(
  st_sfc(
    st_point(c(-51.06, -23.042)),
    st_point(c(-51.013, -23.070)),
    crs = 4326
  ),
  crs = 31982
)) # (https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/)

dist_pontos <- ggplot() +
  # geom_sf(
  #   data = urb_area,
  #   aes(color = "#9c9c9cff"),
  #   fill = "#ff00000e",
  #   size = 0.3
  # ) + # Remover o contorno da área urbana
  geom_sf(data = ruas, aes(color = "#4a4a4ad8"), fill = NA, size = 0.3) +
  geom_sf(data = rios, aes(color = "#0044ffff"), fill = NA, size = 1) +
  geom_sf(
    data = corpos_hidricos,
    aes(color = "#0044ffff"),
    fill = "#0044ffff",
    size = .5
  ) +
  scale_color_manual(
    values = c(
      "#0044ffff",
      "#4a4a4ad8"
    ),
    labels = c(
      "Corpo Hídrico",
      "Estradas"
    ),
    name = "Legenda",
    guide = guide_legend(ncol = 1, position = "right")
  ) +
  new_scale_color() +
  geom_sf(data = cord_sf_metrico, aes(color = as.factor(grupo_id)), size = 3) +
  coord_sf(
    xlim = corte_mapa_sertanopolis[, 'X'],
    ylim = corte_mapa_sertanopolis[, 'Y'],
    datum = 31982,
    expand = FALSE,
    # clip = "off" # pode ser útil em alguns cenários
  ) +
  geom_sf_text(
    aes(label = armadilha),
    data = cord_sf_metrico,
    fontface = "bold",
    position = position_nudge(x = 65, y = 25),
    color = "black",
    size = 2 # antes era 3.5, veremos no arquivo final como ficará
  ) +
  scale_color_viridis_d(
    begin = 0,
    end = 1,
    option = "D",
    name = "Agrupamentos de pontos com até 250 metros de distância (G)",
    guide = guide_legend(
      ncol = 6,
      position = "bottom"
    )
  ) +
  labs(
    title = "Distância entre pontos de instalação de ovitrampas",
    subtitle = "Agrupamentos considerando raio de até 250 metros"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "serif", hjust = 0.5, size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "serif", hjust = 0.5, size = 12),
    legend.title = element_text(family = "serif", size = 10, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(family = "serif", size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = "serif", size = 6),
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )

dist_pontos

saveRDS(dist_pontos, "./fig/dist_pontos.RDS")
ggsave(
  "./fig/dist_pontos.png",
  plot = dist_pontos,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 400
)

# Fazer mapas de ovitrampa espacial --------------------------------------

library(fpp3)

dados <- read_excel("./data/dados.xlsx", col_names = TRUE, na = c("-", ""))

colnames(dados) <- janitor::excel_numeric_to_date(
  as.numeric(colnames(dados)),
  date_system = "modern"
) # lembrar que o erro é pq a primeira coluna é character

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
  filter(!is.na(ovos)) # adicionar filtros de NA para remover meses sem dados lá na frente.

View(df_ovos)


## organizar posição - corrigir por alteração do local de instalação -----

pont_v01 <- st_read("./data/shp/pontos_a36_2023-out-23.shp") |>
  select(-Class)

df_ovos_datas_v01 <- df_ovos |>
  filter(data < ymd("2023-12-21")) |>
  arrange(data)

df_ovos_cord_v01 <- df_ovos_datas_v01 |>
  left_join(pont_v01, join_by(armadilha == Pontos)) |>
  left_join(data.frame(
    ano_mes = rep(
      seq(yearmonth("2023-10"), yearmonth("2023-11"), 1),
      each = 2
    ),
    data = unique(df_ovos_datas_v01$data)
  ))


pont_v02 <- st_read("./data/shp/pontos_a46_2023-dez-04.shp") |> # dez/23 a jan/24
  select(-Class) |>
  rename(armadilha = ï..Pont) |>
  glimpse()

df_ovos_datas_v02 <- df_ovos |>
  filter(data > ymd("2023-12-04") & data < ymd("2025-01-21")) |>
  arrange(data)

df_ovos_cord_v02 <- df_ovos_datas_v02 |>
  left_join(pont_v02) |>
  left_join(data.frame(
    ano_mes = rep(
      seq(yearmonth("2023-12"), yearmonth("2025-01"), 1),
      each = 2
    ),
    data = unique(df_ovos_datas_v02$data)
  ))


pont_v03 <- st_read("./data/shp/pontos_a47_2025-jan-21.shp") |> # fev/24 a ago/25
  select(-Class) |>
  rename(armadilha = ï..Pont) |>
  glimpse()

df_ovos_datas_v03 <- df_ovos |>
  filter(data > ymd("2025-01-07") & data < ymd("2025-08-07")) |>
  arrange(data) |>
  glimpse()

df_ovos_cord_v03 <- df_ovos_datas_v03 |>
  left_join(pont_v03) |>
  left_join(data.frame(
    ano_mes = rep(
      seq(yearmonth("2025-02"), yearmonth("2025-08"), 1),
      each = 2
    ),
    data = unique(df_ovos_datas_v03$data)
  ))

pont_v04 <- st_read("./data/shp/pontos_a50_2025-ago-19.shp") |> # set/25 a jan/26
  select(-Class) |>
  rename(armadilha = ï..Pont) |>
  glimpse()

df_ovos_datas_v04 <- df_ovos |>
  filter(data > ymd("2025-07-23")) |>
  arrange(data) |>
  glimpse()

df_ovos_cord_v04 <- df_ovos_datas_v04 |>
  left_join(pont_v04) |>
  left_join(data.frame(
    ano_mes = rep(
      seq(yearmonth("2025-09"), yearmonth("2026-01"), 1),
      each = 2
    ),
    data = unique(df_ovos_datas_v04$data)
  ))

df_ovos_cord_full <- bind_rows(
  df_ovos_cord_v01,
  df_ovos_cord_v02,
  df_ovos_cord_v03,
  df_ovos_cord_v04
)

View(df_ovos_cord_full)

saveRDS(df_ovos_cord_full, "./data/df_ovos_corrigido_com_pontos.RDS")

## Plot ------------------------------------------------------------------

df_ovos_ido_cord_mes <- df_ovos_cord_full |>
  group_by(armadilha, ano_mes) |>
  reframe(
    IDO = if_else(
      is.nan(sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE)),
      0,
      sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE)
    ),
    IPO = (sum(ovos > 0, na.rm = TRUE) / sum(ovos >= 0, na.rm = TRUE)) *
      100,
    tot_ovos = sum(ovos, na.rm = TRUE),
    media = mean(ovos, na.rm = TRUE),
    desvio_padrao = sd(ovos, na.rm = TRUE),
    geometry = geometry
  ) |>
  ungroup() |>
  distinct() |>
  group_by(armadilha, ano_mes) |>
  mutate(
    classe = if_else(
      IDO < 21,
      "Satisfatório",
      if_else(IDO < 35, "Alerta", "Risco")
    )
  ) |>
  ungroup()

df_ovos_ido_cord_mes$classe <- factor(
  df_ovos_ido_cord_mes$classe,
  levels = c("Satisfatório", "Alerta", "Risco")
)


### plot piloto -------------------------------------------------------------

df_ovos_cord_ido_mes_arm_23_10 <- df_ovos_ido_cord_mes |>
  filter(ano_mes == yearmonth("2023-10")) |>
  st_as_sf(crs = 31982) |>
  st_buffer(dist = 250)


plot_23_10 <- ggplot() +
  geom_sf(data = ruas, aes(color = "Estradas"), fill = NA, size = 0.3) +
  geom_sf(data = rios, aes(color = "Corpos Hídricos"), fill = NA, size = 1) +
  geom_sf(
    data = corpos_hidricos,
    aes(color = "Corpos Hídricos"),
    fill = "#0044ffff",
    size = .5
  ) +
  scale_color_manual(
    name = "Legenda",
    values = c(
      "Estradas" = "#4a4a4ad8",
      "Corpos Hídricos" = "#0044ffff"
    ),
    # labels = c(
    #   "Corpo Hídrico",
    #   "Estradas"
    guide = guide_legend(ncol = 1, position = "right")
  ) +
  new_scale_color() +
  geom_sf(
    data = df_ovos_cord_ido_mes_arm_23_10,
    aes(fill = classe),
    color = "#6d6d6d16",
    linewidth = 0.1,
    alpha = 0.5
  ) +
  coord_sf(
    xlim = corte_mapa_sertanopolis[, 'X'],
    ylim = corte_mapa_sertanopolis[, 'Y'],
    datum = 31982,
    expand = FALSE
  ) +
  scale_fill_manual(
    name = "Nível de Infestação de Ovos",
    values = c(
      "Risco" = "#c91013",
      "Alerta" = "#dde026",
      "Satisfatório" = "#00e00b"
    )
  ) +
  annotation_scale(
    location = "br",
    line_width = .2,
    text_family = "serif"
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering(
      text_family = "serif",
      text_size = 10
    )
  ) +
  labs(
    title = "Infestação",
    subtitle = "Dados com base no IDO"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "serif", hjust = 0.5, size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "serif", hjust = 0.5, size = 12),
    legend.title = element_text(family = "serif", size = 10, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(family = "serif", size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = "serif", size = 6),
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )

plot_23_10 # Ok, funcionou, agora fazer um loop
ggsave(
  filename = "./fig/plot_ido_mes/teste_com_legendas.png",
  plot = plot_23_10,
  width = 8,
  height = 5,
  dpi = 300
)

## loop -------------------------------------------------------------------
### configurar para melhor encaixar no plot final

meses_total <- unique(df_ovos_ido_cord_mes$ano_mes)
plots_mapas_mes <- list()


for (i in seq_along(meses_total)) {
  mes_atual <- meses_total[i]

  dados_mes <- df_ovos_ido_cord_mes |>
    filter(ano_mes == mes_atual) |>
    st_as_sf(crs = 31982) |>
    st_buffer(dist = 250)

  plot <- ggplot() +
    geom_sf(data = ruas, aes(color = "Estradas"), fill = NA, size = 0.2) +
    geom_sf(
      data = rios,
      aes(color = "Corpos Hídricos"),
      fill = NA,
      size = 0.5
    ) +
    geom_sf(
      data = corpos_hidricos,
      aes(color = "Corpos Hídricos"),
      fill = "#0044ffff",
      size = .3
    ) +
    scale_color_manual(
      name = "Legenda",
      values = c(
        "Estradas" = "#4a4a4ad8",
        "Corpos Hídricos" = "#0044ffff"
      ),
      guide = guide_legend(ncol = 1)
    ) +
    geom_sf(
      data = dados_mes,
      aes(fill = classe),
      color = "#6d6d6d16",
      linewidth = 0.1,
      alpha = 0.5
    ) +
    scale_fill_manual(
      name = "Nível de Infestação de Ovos",
      values = c(
        "Risco" = "#c91013",
        "Alerta" = "#dde026",
        "Satisfatório" = "#00e00b"
      )
    ) +
    coord_sf(
      xlim = corte_mapa_sertanopolis[, 'X'],
      ylim = corte_mapa_sertanopolis[, 'Y'],
      datum = 31982,
      expand = FALSE
    ) +
    annotation_scale(
      location = "br",
      line_width = .15,
      height = unit(0.1, "cm"),
      text_family = "serif",
      text_cex = 0.5
    ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      height = unit(0.4, "cm"),
      width = unit(0.4, "cm"),
      pad_x = unit(0.05, "in"),
      pad_y = unit(0.05, "in"),
      style = north_arrow_fancy_orienteering(
        text_family = "serif",
        text_size = 5
      )
    ) +
    labs(
      title = format(as.Date(mes_atual), "%Y/%m")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(
        family = "serif",
        hjust = 0.5,
        size = 10,
        face = "bold"
      ),
      # plot.title.position = "plot",
      # plot.subtitle = element_text(family = "serif", hjust = 0.5, size = 12),
      # legend.position = "none", # adicionado para o Loop para montar os quadrantes
      # Config de posição de legendas e etc alteradas, para caber no arquivo
      legend.title = element_text(family = "serif", size = 8, hjust = 0.5),
      # legend.title.position = "top",
      legend.text = element_text(family = "serif", size = 6),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(family = "serif", size = 2),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      axis.ticks = element_line(linewidth = 0.1),
      axis.ticks.length = unit(0.01, "cm")
    )

  mes_mapa <- as.character(mes_atual)
  plots_mapas_mes[[mes_mapa]] <- plot

  num_limpos <- format(as.Date(mes_atual), "%Y_%m")

  cha_limpos <- str_replace_all(mes_mapa, " ", "_")

  # ggsave(
  #   filename = paste0(
  #     "./fig/plot_ido_mes/mapa_ido_",
  #     num_limpos,
  #     "_ou_",
  #     cha_limpos,
  #     ".png"
  #   ),
  #   plot = plot,
  #   width = 8,
  #   height = 5,
  #   dpi = 300
  # )
}

saveRDS(plots_mapas_mes, "./fig/ggplot_RDS/mapas_mensais.RDS")
