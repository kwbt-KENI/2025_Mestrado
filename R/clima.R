rm(list = ls())
require(tidyverse)
require(readxl)
require(sf)
require(igraph)
require(ggnewscale)
require(ggtext)
require(showtext)
require(duckplyr)
require(ggspatial)
require(flextable)
set_flextable_defaults(fonts_ignore = TRUE)
require(patchwork)
require(fpp3)
require(officer)
require(viridis)


# testes -----------------------------------------------------------------

dados_ovos_bruto <- readRDS("./data/df_ovos_corrigido_com_pontos.RDS") |>
  select(data) |>
  distinct() |>
  arrange(data) |>
  mutate(
    ciclo = row_number(),
    coleta = data,
    instalação = lag(data),
    instalação = coalesce(instalação, ymd("2023-10-09")),
    exposicao = as.numeric(coleta - instalação),
  )

dados_clima <- get_power(
  community = c("ag"),
  pars = c("T2M", "T2M_MAX", "T2M_MIN", "PRECTOTCORR", "RH2M", "WS2M"),
  temporal_api = "daily",
  lonlat = c(-51.03660, -23.05654),
  dates = c("2022-01-01", "2026-01-01"),
  time_standard = c("UTC")
)

clima_serta <- dados_clima |>
  select(
    data = YYYYMMDD,
    t_med = T2M,
    t_max = T2M_MAX,
    t_min = T2M_MIN,
    preci = PRECTOTCORR
  ) |>
  mutate(data = as.Date(data)) |>
  glimpse()

saveRDS(clima_serta, "./data/clima_serta.RDS")


## Calc val clim. para ciclos de exposição
clima_ciclo <- dados_ovos_bruto |>
  rowwise() |>
  mutate(
    t_med_ciclo = mean(
      clima_serta$t_med[
        clima_serta$data >= instalação & clima_serta$data <= coleta
      ],
      na.rm = TRUE
    ),
    t_max_ciclo = mean(
      clima_serta$t_max[
        clima_serta$data >= instalação & clima_serta$data <= coleta
      ],
      na.rm = TRUE
    ),
    t_min_ciclo = mean(
      clima_serta$t_min[
        clima_serta$data >= instalação & clima_serta$data <= coleta
      ],
      na.rm = TRUE
    ),
    t_preci_ciclo = sum(
      clima_serta$preci[
        clima_serta$data >= instalação & clima_serta$data <= coleta
      ],
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  glimpse()


saveRDS(clima_ciclo, "./data/clima_serta_ciclos.RDS")

d_ovos_clim <- dados_ovos_cor <- readRDS(
  "./data/df_ovos_cal_sem_corrigido.RDS"
) |>
  ungroup() |>
  left_join(clima_ciclo, by = "data") |>
  glimpse()


# plot_base --------------------------------------------------------------

break_plot_ido_sem_total_final <- dados_ovos_cor |>
  ungroup() |>
  select(ano_mes, data)
#   |>
#   distinct(ano_mes, .keep_all = T) # usar para caso for agrupar por pseudo "mês"

fundo <- data.frame(xmin = as.Date(break_plot_ido_sem_total_final$data)) |>
  mutate(xmax = lead(xmin)) |>
  filter(row_number() %% 2 != 0)

plot_ido_sem_total_final <- dados_ovos_cor |>
  ggplot() +
  geom_rect(
    data = fundo,
    aes(xmin = xmin, xmax = xmax),
    ymin = 0,
    ymax = 200,
    fill = "#FCF0E9",
    color = NA
  ) +
  geom_hline(
    yintercept = c(0, 20, 35, 100, 200),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = dados_ovos_cor,
    aes(x = data, y = ido),
    linewidth = 0.7
  ) +
  geom_point(aes(x = data, y = ido, color = n_armadilha)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(0, 20, 35, 100, 200), limits = c(0, 200)) +
  scale_x_date(
    breaks = dados_ovos_cor$data,
    labels = dados_ovos_cor$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  labs(
    x = "Data da coleta dos dados",
    y = "IDO",
    title = "Flutuação do Índice de Densidade de Ovos (IDO)",
    subtitle = "Cidade de Sertanópolis-PR",
    color = stringr::str_wrap(
      "Número de armadilhas instaladas no município",
      width = 12
    )
  ) +
  theme(
    text = element_text(family = "serif"),
    # plot.background = element_rect(fill = "#EEEEEE"),
    plot.background = element_rect(fill = "#eeeeee45"),
    # plot.background = element_blank(),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(5, 4, 5, 4),
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0.5
    ),
    plot.margin = margin(4, 15, 20, 10),
    axis.text.x = element_markdown(
      size = 8.5,
      margin = margin(t = 3),
      color = "black",
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    # legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # legend.title = element_blank()
  )

plot_ipo_sem_total_final <- dados_ovos_cor |>
  ggplot() +
  geom_rect(
    data = fundo,
    aes(xmin = xmin, xmax = xmax),
    ymin = 50,
    ymax = 100,
    fill = "#f4e9fc",
    color = NA
  ) +
  geom_hline(
    yintercept = c(50, 75, 100),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = dados_ovos_cor,
    aes(x = data, y = ipo),
    linewidth = 0.7
  ) +
  geom_point(aes(x = data, y = ipo, color = n_armadilha)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(50, 75, 100), limits = c(50, 100)) +
  scale_x_date(
    breaks = dados_ovos_cor$data,
    labels = dados_ovos_cor$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  labs(
    x = "Data da coleta dos dados",
    y = "IPO",
    title = "Flutuação do Índice de Positividade de Ovitrampas (IPO)",
    subtitle = "Cidade de Sertanópolis-PR",
    color = stringr::str_wrap(
      "Número de armadilhas instaladas no município",
      width = 12
    )
  ) +
  theme(
    text = element_text(family = "serif"),
    # plot.background = element_rect(fill = "#EEEEEE"),
    plot.background = element_rect(fill = "#eeeeee45"),
    # plot.background = element_blank(),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(5, 4, 5, 4),
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0.5
    ),
    plot.margin = margin(4, 15, 20, 10),
    axis.text.x = element_markdown(
      size = 8.5,
      margin = margin(t = 3),
      color = "black",
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    # legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # legend.title = element_blank()
  )


fig_ido_ipo_sem_total_final <- ((plot_ido_sem_total_final +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )) /
  plot_ipo_sem_total_final) +
  plot_layout(heights = c(3, 1), guides = 'collect') +
  plot_annotation(
    tag_levels = 'A',
    caption = "Fonte: O próprio autor.",
    theme = theme(
      plot.caption = element_markdown(family = "serif", size = 10, hjust = 0),
      plot.caption.position = "plot"
    )
  ) &
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.background = element_rect(fill = "#eeeeee45", color = NA),
    legend.position = "right",
    legend.title = element_text(family = "serif", size = 8),
    legend.text = element_text(family = "serif", size = 7)
  )


# Plots_test -------------------------------------------------------------

plot_ido_comp <- d_ovos_clim |>
  ggplot(aes(x = data)) +
  geom_line(aes(y = ido), linewidth = 0.7, color = "#2D1B69") +
  geom_point(aes(y = ido), size = 1.2, color = "#2D1B69") +
  geom_hline(
    yintercept = c(20, 35),
    linetype = "dashed",
    linewidth = 0.3,
    color = "gray60"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b/%Y"
  ) +
  labs(
    x = NULL,
    y = "IDO"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#eeeeee45"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


plot_ido_comp


plot_temp_comp <- d_ovos_clim |>
  select(data, t_med_ciclo, t_max_ciclo, t_min_ciclo) |>
  pivot_longer(
    cols = c(t_med_ciclo, t_max_ciclo, t_min_ciclo),
    names_to = "tipo_temp",
    values_to = "temperatura"
  ) |>
  mutate(
    tipo_temp = factor(
      tipo_temp,
      levels = c("t_max_ciclo", "t_med_ciclo", "t_min_ciclo"),
      labels = c("Máxima", "Média", "Mínima")
    )
  ) |>
  ggplot(aes(x = data, y = temperatura, color = tipo_temp)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.9) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b/%Y"
  ) +
  scale_color_manual(
    values = c(
      "Máxima" = "#D73027",
      "Média" = "#4A4A4A",
      "Mínima" = "#4575B4"
    )
  ) +
  labs(
    x = "Data da coleta",
    y = "Temperatura (°C)",
    color = NULL
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#eeeeee45"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

plot_temp_comp

fig_ido_temp_final <- (plot_ido_comp / plot_temp_comp) +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(
    tag_levels = "A",
    caption = "Fonte: o próprio autor; NASA POWER (NASA/LARC/SSAI, 2024).",
    theme = theme(
      plot.caption = element_markdown(
        family = "serif",
        size = 9,
        hjust = 0
      ),
      plot.caption.position = "plot"
    )
  ) &
  theme(plot.background = element_rect(fill = "#eeeeee45", color = NA))

fig_ido_temp_final

# Comparativo entre a flutuação do Índice de Densidade de Ovos (IDO) e as variáveis de temperatura média, mínima e máxima a 2 metros do solo, para o município de Sertanópolis-PR, no período de outubro de 2023 a dezembro de 2025. Os valores de temperatura representam a média diária calculadas durante o período de exposição de cada ciclo de coleta das armadilhas ovitrampa. A: Flutuação do IDO, com linhas tracejadas indicando os limiares de classificação "Satisfatório" (IDO < 20) e "Alerta" (IDO < 35); B: Temperaturas máxima (vermelho), média (cinza) e mínima (azul) diárias, obtidas a partir da base de dados NASA POWER.

# incorporação_melhorada -------------------------------------------------
rm(list = ls())
info_ciclos_clima <- readRDS("./data/clima_serta_ciclos.RDS") |>
  select(data, t_med_ciclo, t_max_ciclo, t_min_ciclo, t_preci_ciclo)
dados_ovos_cor <- readRDS("./data/df_ovos_cal_sem_corrigido.RDS") |>
  ungroup()

dados_ovos_cor_clima <- dados_ovos_cor |>
  ungroup() |>
  left_join(
    info_ciclos_clima,
    by = "data"
  )

break_plot_ido_sem_total_final <- dados_ovos_cor |>
  ungroup() |>
  select(ano_mes, data)

fundo <- data.frame(xmin = as.Date(break_plot_ido_sem_total_final$data)) |>
  mutate(xmax = lead(xmin)) |>
  filter(row_number() %% 2 != 0)

## plot ido --------------------------------------------------------------

plot_ido_sem_total_final <- dados_ovos_cor_clima |>
  ggplot() +
  geom_rect(
    data = fundo,
    aes(xmin = xmin, xmax = xmax),
    ymin = 0,
    ymax = 200,
    fill = "#FCF0E9",
    color = NA
  ) +
  geom_hline(
    yintercept = c(0, 20, 35, 100, 200),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = dados_ovos_cor_clima,
    aes(x = data, y = ido),
    linewidth = 0.7
  ) +
  geom_point(aes(x = data, y = ido, color = n_armadilha)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(0, 20, 35, 100, 200), limits = c(0, 200)) +
  scale_x_date(
    breaks = dados_ovos_cor_clima$data,
    labels = dados_ovos_cor_clima$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  labs(
    x = "Data da coleta dos dados",
    y = "IDO",
    title = "Flutuação do Índice de Densidade de Ovos (IDO)",
    subtitle = "Cidade de Sertanópolis-PR",
    color = stringr::str_wrap(
      "Número de armadilhas instaladas no município",
      width = 12
    )
  ) +
  theme(
    text = element_text(family = "serif"),
    # plot.background = element_rect(fill = "#EEEEEE"),
    plot.background = element_rect(fill = "#eeeeee45"),
    # plot.background = element_blank(),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(5, 4, 5, 4),
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0.5
    ),
    plot.margin = margin(4, 15, 20, 10),
    axis.text.x = element_markdown(
      size = 8.5,
      margin = margin(t = 3),
      color = "black",
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    # legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # legend.title = element_blank()
  )


## plot ipo --------------------------------------------------------------

plot_ipo_sem_total_final <- dados_ovos_cor_clima |>
  ggplot() +
  geom_rect(
    data = fundo,
    aes(xmin = xmin, xmax = xmax),
    ymin = 50,
    ymax = 100,
    fill = "#f4e9fc",
    color = NA
  ) +
  geom_hline(
    yintercept = c(50, 75, 100),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = dados_ovos_cor_clima,
    aes(x = data, y = ipo),
    linewidth = 0.7
  ) +
  geom_point(aes(x = data, y = ipo, color = n_armadilha)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(50, 75, 100), limits = c(50, 100)) +
  scale_x_date(
    breaks = dados_ovos_cor_clima$data,
    labels = dados_ovos_cor_clima$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  labs(
    x = "Data da coleta dos dados",
    y = "IPO",
    title = "Flutuação do Índice de Positividade de Ovitrampas (IPO)",
    subtitle = "Cidade de Sertanópolis-PR",
    color = stringr::str_wrap(
      "Número de armadilhas instaladas no município",
      width = 12
    )
  ) +
  theme(
    text = element_text(family = "serif"),
    # plot.background = element_rect(fill = "#EEEEEE"),
    plot.background = element_rect(fill = "#eeeeee45"),
    # plot.background = element_blank(),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(5, 4, 5, 4),
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0.5
    ),
    plot.margin = margin(4, 15, 20, 10),
    axis.text.x = element_markdown(
      size = 8.5,
      margin = margin(t = 3),
      color = "black",
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    # legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # legend.title = element_blank()
  )


## plot temp -------------------------------------------------------------
plot_temp_sem_total_final <- dados_ovos_cor_clima |>
  ggplot() +
  geom_rect(
    data = fundo,
    aes(xmin = xmin, xmax = xmax),
    ymin = 0,
    ymax = 40,
    fill = "#e9f4fc",
    color = NA
  ) +
  geom_hline(
    yintercept = c(10, 20, 30),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_ribbon(
    aes(x = data, ymin = t_min_ciclo, ymax = t_max_ciclo),
    fill = "#4575B4",
    alpha = 0.2
  ) + # Faixa entre mínima e máxima
  geom_line(
    aes(x = data, y = t_max_ciclo, linetype = "Máxima"),
    color = "#D73027",
    linewidth = 0.6
  ) + # linha temp max
  geom_line(
    aes(x = data, y = t_med_ciclo, linetype = "Média"),
    color = "#4A4A4A",
    linewidth = 0.7
  ) + # linha temp med
  geom_line(
    aes(x = data, y = t_min_ciclo, linetype = "Mínima"),
    color = "#4575B4",
    linewidth = 0.6
  ) + # linha temp min
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(0, 40)
  ) +
  scale_x_date(
    breaks = dados_ovos_cor$data,
    labels = dados_ovos_cor$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  scale_linetype_manual(
    name = "Temperatura",
    values = c("Máxima" = "solid", "Média" = "solid", "Mínima" = "solid"),
    guide = guide_legend(
      override.aes = list(
        color = c("#D73027", "#4A4A4A", "#4575B4"),
        linewidth = 0.8
      )
    )
  ) +
  labs(
    x = "Data da coleta dos dados",
    y = "Temp. (°C)",
    title = "Flutuação da média da temperatura durante os ciclos de coleta",
    subtitle = "Cidade de Sertanópolis-PR"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#eeeeee45"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(5, 4, 5, 4),
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0.5
    ),
    plot.margin = margin(4, 15, 20, 10),
    axis.text.x = element_markdown(
      size = 8.5,
      margin = margin(t = 3),
      color = "black",
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
  )


## plot pluviosidade ------------------------------------------------------

plot_prec_sem_total_final <- dados_ovos_cor_clima |>
  ggplot() +
  geom_rect(
    data = fundo,
    aes(xmin = xmin, xmax = xmax),
    ymin = 0,
    ymax = max(dados_ovos_cor_clima$t_preci_ciclo, na.rm = TRUE) * 1.1,
    fill = "#e9fcf0",
    color = NA
  ) +
  geom_hline(
    yintercept = seq(
      0,
      max(dados_ovos_cor_clima$t_preci_ciclo, na.rm = TRUE),
      by = 50
    ),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_col(
    aes(x = data, y = t_preci_ciclo),
    fill = "#2171B5",
    color = "#08519C",
    width = 6,
    linewidth = 0.2
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(
    limits = c(
      0,
      max(dados_ovos_cor_clima$t_preci_ciclo, na.rm = TRUE) * 1.1
    )
  ) +
  scale_x_date(
    breaks = dados_ovos_cor$data,
    labels = dados_ovos_cor$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  labs(
    x = "Data da coleta dos dados",
    y = "Prec. (mm)",
    title = "Pluviosidade acumulada por ciclo de coleta",
    subtitle = "Cidade de Sertanópolis-PR"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#eeeeee45"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 12,
      width = NULL,
      fill = NULL,
      padding = margin(5, 4, 5, 4),
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0.5
    ),
    plot.margin = margin(4, 15, 20, 10),
    axis.text.x = element_markdown(
      size = 8.5,
      margin = margin(t = 3),
      color = "black",
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
  )

# Plot total -------------------------------------------------------------
fig_ido_ipo_clima_total_final <- ((plot_ido_sem_total_final +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )) /
  (plot_ipo_sem_total_final +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )) /
  (plot_temp_sem_total_final +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )) /
  plot_prec_sem_total_final) +
  plot_layout(heights = c(3, 1, 2, 1.5), guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    caption = "Fonte: O próprio autor; dados climáticos da NASA POWER.",
    theme = theme(
      plot.caption = element_markdown(
        family = "serif",
        size = 10,
        hjust = 0
      ),
      plot.caption.position = "plot"
    )
  ) &
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.background = element_rect(fill = "#eeeeee45", color = NA),
    legend.position = "right",
    legend.title = element_text(family = "serif", size = 8),
    legend.text = element_text(family = "serif", size = 7)
  )

fig_ido_ipo_clima_total_final
