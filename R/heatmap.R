library(tidyverse)
library(duckplyr)
library(viridis)

rm(list = ls())
# Começo dos testes ------------------------------------------------------
dados_ovos_bruto <- readRDS("./data/df_ovos_corrigido_com_pontos.RDS")


hm_plot <- dados_ovos_bruto |>
  select(data, armadilha, ovos, ano_mes) |>
  left_join(data.frame(
    ciclo = seq(1, 56, 1),
    data = unique(dados_ovos_bruto$data)
  )) |>
  ungroup() |>
  mutate(armadilha = as.factor(armadilha))

hm_plot |>
  ggplot(aes(x = ciclo, y = armadilha, fill = ovos)) +
  geom_tile(color = "#e6e6e64d") +
  scale_fill_stepsn(
    colours = c(
      "#F4F3E8",
      "#fcfdbf",
      "#fed799",
      "#feb078",
      "#fc8961",
      "#f1605d",
      "#d8456c",
      "#b73779",
      "#932b80",
      "#721f81",
      "#51127c",
      "#2c115f",
      "#000004"
    ),
    values = c(0, 1, 5, 20, 35, 50, 70, 100, 200, 300, 500, 900, 1329) /
      1329,
    labels = c(0, 1, 5, 20, 35, 50, 70, 100, 200, 300, 500, "900+", 1330),
    breaks = c(0, 1, 5, 20, 35, 50, 70, 100, 200, 300, 500, 900, 1330),
    na.value = "#8282825b",
    # trans = "log1p"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Número de ovos contabilizados por coleta",
    fill = "Quantidade de Ovos contados",
    x = "Ciclo de coleta",
    y = "Número da Armadilha"
  ) +
  scale_x_continuous(breaks = seq(1, 56, 2)) +
  theme(
    text = element_text(family = "serif"),
    legend.margin = margin(r = 0),
    legend.position = "bottom",
    legend.justification.bottom = "right",
    legend.key.width = unit(55, unit = "pt"),
    legend.key.height = unit(4, units = "pt"),
    legend.ticks.length = unit(4, "pt"),
    legend.title = element_text(size = 7, vjust = 1),
    legend.text = element_text(size = 6),
    plot.title.position = "plot",
    plot.title = element_markdown(
      size = 12,
      hjust = 0.5
    ),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#eeeeee45"),
  )
