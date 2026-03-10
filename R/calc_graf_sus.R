rm(list = ls(all = T))
library(tidyverse)
library(duckplyr)
library(tsibble)
# library(showtext)
library(ggtext)
library(fpp3)

# font_add("times_new", "times.ttf") #aaaaaa
# font_add_google("Libre Franklin", "franklin")
# font.families()

# showtext_opts(dpi = 300)

d_geral <- readRDS("data/den_2015_2026_fil_PR_sem.RDS")

d_regional <- d_geral |>
  filter(ID_REGIONA == "1371") |>
  mutate(ID_MUNICIP = as.integer(ID_MUNICIP)) |>
  glimpse()

saveRDS(d_regional, "data/den_2015_2026_fil_Regional_sem.RDS")

d_regional = readRDS("data/den_2015_2026_fil_Regional_sem.RDS")
#
info_pop <- read.delim("data/pop_regional.txt", sep = ";")
#
pop_total_regional <- sum(info_pop$pop_2022)
#
# cal_regional <- d_regional |>
#   group_by(ano_sem) |>
#   summarise(total_casos = n()) |>
#   as_tsibble(index = ano_sem) |>
#   fill_gaps() |>
#   mutate(total_casos = replace_na(total_casos, 0)) |>
#   mutate(total_pop = pop_total_regional, porcent = ((total_casos * 100) /
#                                                       total_pop), ) |>
#   distinct()

# saveRDS(cal_regional, "data/den_2015_2026_calc_Regional_sem.RDS")

# autoplot(cal_regional, .vars = total_casos) +
#   geom_rect(
#     data = cal_regional,
#     aes(
#       xmin = yearweek("2024 W18", week_start = 7),
#       xmax = max(ano_sem),
#       ymin = -Inf,
#       ymax = Inf
#     ),
#     fill = "grey",
#     alpha = 0.2
#   ) +
#   autolayer(cal_regional) +
#   labs(x = "Semana epidemiológica [W]")

# calc_sertanopolis -------------------------------------------------------

cal_sertanopolis <- d_regional |>
  filter(ID_MUNICIP == 412650) |>
  group_by(ano_sem) |>
  summarise(total_casos_serta = n()) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(total_casos_serta = replace_na(total_casos_serta, 0)) |>
  mutate(
    total_pop = pop_total_regional,
    porcent_serta = ((total_casos_serta * 100) /
      total_pop),
  ) |>
  distinct() |>
  as_tsibble(index = ano_sem)

saveRDS(cal_sertanopolis, "data/den_2015_2026_calc_sertanopolis_sem.RDS")

cal_sertanopolis = readRDS("data/den_2015_2026_calc_sertanopolis_sem.RDS")

cal_sertanopolis_sorotipo <- d_regional |>
  filter(ID_MUNICIP == 412650) |>
  select(ano_sem, SOROTIPO) |>
  filter(!is.na(SOROTIPO))

saveRDS(cal_sertanopolis_sorotipo, "data/cal_sertanopolis_sorotipo_sem.RDS")

cal_sertanopolis_sorotipo <- readRDS(
  "data/cal_sertanopolis_sorotipo_sem.RDS"
)

cal_sertanopolis_sorotipo |>
  mutate(data = format(ano_sem, "%d/%m/%Y")) |>
  View()

fig_cal_sertanopolis_cap <- cal_sertanopolis_cap |>
  ggplot(aes(x = data, y = total_casos_serta, color = status_proj)) +
  geom_rect(
    xmin = ymd("2023-10-08"),
    xmax = ymd("2026-01-01"),
    ymin = -4,
    ymax = 200,
    fill = "#FCF0E9",
    color = NA
  ) +
  geom_hline(
    yintercept = c(0, 100, 200),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(linewidth = 0.7) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(0, 100, 200), limits = c(0, 200)) +
  scale_x_date(
    breaks = ymd(
      c(
        "2021-12-01",
        "2022-06-01",
        "2022-12-01",
        "2023-06-01",
        "2023-12-01",
        "2024-06-01",
        "2024-12-01",
        "2025-06-01",
        "2025-12-01"
      )
    ),
    labels = c(
      "2021/dez",
      "2022/jun",
      "2022/dez",
      "2023/jun",
      "2023/dez",
      "2024/jun",
      "2024/dez",
      "2025/jun",
      "2025/dez"
    )
  ) +
  scale_color_manual(
    breaks = c("antes", "depois"),
    values = c("#464749", "#2D7709"),
    labels = c("Sem monitoramento", "Com monitoramento")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Município de Sertanópolis/PR",
    subtitle = "Casos registrados de Dengue",
    caption =
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
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
    plot.margin = margin(4, 15, 10, 10),
    axis.text.x = element_text(
      size = 8.5,
      margin = margin(t = 3),
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = c(0.18, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )

plot(fig_cal_sertanopolis_cap)

cal_sertanopolis_cap <- cal_sertanopolis |>
  filter(ano_sem > yearweek("2021 W44", week_start = 7)) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(
    total_casos_serta = replace_na(total_casos_serta, 0),
    status_proj = if_else(
      ano_sem < yearweek("2023 W41", week_start = 7),
      "antes",
      "depois"
    ),
    total_casos_serta = as.numeric(total_casos_serta),
    data = as_date(ano_sem)
  )

# autoplot(cal_sertanopolis_cap, .vars = total_casos_serta, color = "red") +
#   geom_rect(
#     data = cal_sertanopolis_cap,
#     aes(
#       xmin = yearweek("2024 W18", week_start = 7),
#       xmax = max(ano_sem),
#       ymin = -Inf,
#       ymax = Inf
#     ),
#     fill = "grey85",
#     alpha = 0.2
#   ) +
#   autolayer(cal_regional_do_proj, color = "black") +
#   labs(x = "Semana epidemiológica [W]") +
#   theme_bw()

# Plot_sertanópolis -------------------------------------------------------

saveRDS(
  cal_sertanopolis_cap,
  "./data/den_sus_cal_sertanopolis_cap_2023_2026.RDS"
)
denv_sertanopolis <- cal_sertanopolis_cap |>
  ggplot(aes(x = data, y = total_casos_serta, color = status_proj)) +
  geom_rect(
    xmin = ymd("2023-10-08"),
    xmax = ymd("2026-01-01"),
    ymin = -4,
    ymax = 200,
    fill = "#FCF0E9",
    color = NA
  ) +
  geom_hline(
    yintercept = c(0, 100, 200),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(show.legend = FALSE, linewidth = 0.7) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(0, 100, 200), limits = c(0, 200)) +
  scale_x_date(
    breaks = ymd(
      c(
        "2021-12-01",
        "2022-06-01",
        "2022-12-01",
        "2023-06-01",
        # "2023-10-23",
        "2023-12-01",
        "2024-06-01",
        "2024-12-01",
        "2025-06-01",
        "2025-12-01"
      )
    ),
    labels = c(
      "2021/dez",
      "2022/jun",
      "2022/dez",
      "2023/jun",
      # "2023/out \n(Início do projeto)",
      "2023/dez",
      "2024/jun",
      "2024/dez",
      "2025/jun",
      "2025/dez"
    )
  ) +
  scale_color_manual(
    breaks = c("antes", "depois"),
    values = c("#464749", "#2D7709")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Município de Sertanópolis/PR",
    subtitle = "Casos semanais registrados de Dengue"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
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
    plot.margin = margin(4, 15, 10, 10),
    axis.text.x = element_text(
      size = 8.5,
      margin = margin(t = 3),
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA)
  )

denv_sertanopolis

# Calc_Regional -----------------------------------------------------------

cal_regional = readRDS("data/den_2015_2026_calc_Regional_sem.RDS")

cal_regional_cap <- cal_regional |>
  filter(ano_sem > yearweek("2021 W44", week_start = 7)) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(
    total_casos = replace_na(total_casos, 0),
    total_casos = as.numeric(total_casos),
    data = as_date(ano_sem)
  )

max(cal_regional_cap$total_casos)
saveRDS(cal_regional_cap, "ggplot/den_sus_cal_regional_cap.RDS")

seq_a <- seq(0, 9000, 100)
seq_b <- seq(0, 9000, 500)
seq_a <- seq_a[!seq_a %in% seq_b]

denv_regional <- cal_regional_cap |>
  ggplot(aes(x = data, y = total_casos)) +
  geom_hline(
    yintercept = c(seq_a, seq_b),
    linewidth = 0.1,
    color = c(rep("gray90", 72), rep("gray50", 19)),
    linetype = c(rep(1, 72), rep(3, 19))
  ) +
  geom_line(
    show.legend = FALSE,
    linewidth = 0.7,
    colour = "#464749"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(seq(0, 9000, 500)), limits = c(0, 10000)) +
  scale_x_date(
    breaks = ymd(
      c(
        "2021-12-01",
        "2022-06-01",
        "2022-12-01",
        "2023-06-01",
        "2023-10-23",
        "2023-12-01",
        "2024-06-01",
        "2024-12-01",
        "2025-06-01",
        "2025-12-01"
      )
    ),
    labels = c(
      "2021/dez",
      "2022/jun",
      "2022/dez",
      "2023/jun",
      "2023/out \n(Início do projeto)",
      "2023/dez",
      "2024/jun",
      "2024/dez",
      "2025/jun",
      "2025/dez"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Municípios da 17º Regional de Saúde do PR",
    subtitle = "Casos registrados de Dengue"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
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
    plot.margin = margin(4, 15, 10, 10),
    axis.text.x = element_text(
      size = 8.5,
      margin = margin(t = 3),
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA)
  )


# Divida_sertanopolis -----------------------------------------------------
# Londrina, Cambém, Ibiporã, Sertaneja, Bela Vista do Paraíso, Rancho Alegre e
# Primeiro de Maio.

d_geral <- readRDS("data/den_2015_2026_fil_PR_sem.RDS")

pop_divisa <- sum(555965, 5616, 10082, 3512, 51603, 107208, 14833)

d_divisa <- d_geral |>
  filter(
    ID_MUNICIP == "411370" |
      ID_MUNICIP == "412640" |
      ID_MUNICIP == "412050" |
      ID_MUNICIP == "412130" |
      ID_MUNICIP == "410980" |
      ID_MUNICIP == "410370" |
      ID_MUNICIP == "410280"
  )

cal_divisa <- d_divisa |>
  group_by(ano_sem) |>
  summarise(total_casos_divisa = n()) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(total_casos_divisa = replace_na(total_casos_divisa, 0)) |>
  mutate(
    total_pop_divisa = pop_divisa,
    porcent_divisa = ((total_casos_divisa * 100) / total_pop_divisa),
  ) |>
  distinct() |>
  as_tsibble(index = ano_sem)


cal_divisa_cap <- cal_divisa |>
  filter(ano_sem > yearweek("2021 W44", week_start = 7)) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(
    total_casos_divisa = replace_na(total_casos_divisa, 0),
    total_casos_divisa = as.numeric(total_casos_divisa),
    data = as_date(ano_sem)
  )

max(cal_divisa$total_casos_divisa)
seq_c <- seq(0, 8000, 100)
seq_d <- seq(0, 8000, 500)
seq_c <- seq_c[!seq_c %in% seq_d]


cal_divisa_cap

denv_divisa <- cal_divisa_cap |>
  ggplot(aes(x = data, y = total_casos_divisa)) +
  geom_hline(
    yintercept = c(seq_c, seq_d),
    linewidth = 0.1,
    color = c(rep("gray90", 64), rep("gray50", 17)),
    linetype = c(rep(1, 64), rep(3, 17))
  ) +
  geom_line(
    show.legend = FALSE,
    linewidth = 0.7,
    colour = "#464749"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(seq(0, 8000, 500)), limits = c(0, 10000)) +
  scale_x_date(
    breaks = ymd(
      c(
        "2021-12-01",
        "2022-06-01",
        "2022-12-01",
        "2023-06-01",
        "2023-10-23",
        "2023-12-01",
        "2024-06-01",
        "2024-12-01",
        "2025-06-01",
        "2025-12-01"
      )
    ),
    labels = c(
      "2021/dez",
      "2022/jun",
      "2022/dez",
      "2023/jun",
      "2023/out \n(Início do projeto)",
      "2023/dez",
      "2024/jun",
      "2024/dez",
      "2025/jun",
      "2025/dez"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Municípios que fazem divisa com Sertanópolis/PR",
    subtitle = "Casos registrados de Dengue"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
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
    plot.margin = margin(4, 15, 10, 10),
    axis.text.x = element_text(
      size = 8.5,
      margin = margin(t = 3),
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA)
  )

denv_divisa


# Municípios da 17 com menos de 15k de hab --------------------------------
# municípios com menos de 16 mil habitantes
# Da regional, exclui Londrina, Cambé, Ibiporã e Rolândia.

d_regional = readRDS("data/den_2015_2026_fil_Regional_sem.RDS")

d_regional_pequenos <- d_regional |>
  mutate(ID_MUNICIP = as.numeric(ID_MUNICIP)) |>
  filter(ID_MUNICIP != 411370) |>
  filter(ID_MUNICIP != 410370) |>
  filter(ID_MUNICIP != 412240) |>
  filter(ID_MUNICIP != 410980)

info_pop <- read.delim("data/pop_regional.txt", sep = ";")
pop_total_regional_pequenos <- (sum(info_pop$pop_2022) -
  sum(555965, 107208, 51603, 71670))

cal_regional_pequenos <- d_regional_pequenos |>
  group_by(ano_sem) |>
  summarise(total_casos_pequenos = n()) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(total_casos_pequenos = replace_na(total_casos_pequenos, 0)) |>
  mutate(
    total_pop_pequenos = pop_total_regional_pequenos,
    porcent = ((total_casos_pequenos * 100) / total_pop_pequenos),
  ) |>
  distinct()


cal_regional_pequenos_cap <- cal_regional_pequenos |>
  filter(ano_sem > yearweek("2021 W44", week_start = 7)) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(
    total_casos_pequenos = replace_na(total_casos_pequenos, 0),
    total_casos_pequenos = as.numeric(total_casos_pequenos),
    data = as_date(ano_sem)
  )

max(cal_regional_pequenos_cap$total_casos_pequenos)

seq_e = seq(0, 1500, by = 100)
seq_f = seq(0, 1500, by = 500)
seq_e <- seq_e[!seq_e %in% seq_f]


denv_regional_pequenos <- cal_regional_pequenos_cap |>
  ggplot(aes(x = data, y = total_casos_pequenos)) +
  geom_hline(
    yintercept = c(0, 100, 200, 500, 1000, 1500),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(show.legend = FALSE, linewidth = 0.7, colour = "#464749") +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(
    breaks = c(0, 100, 200, 500, 1000, 1500),
    limits = c(0, 2000)
  ) +
  scale_x_date(
    breaks = ymd(
      c(
        "2021-12-01",
        "2022-06-01",
        "2022-12-01",
        "2023-06-01",
        "2023-12-10",
        "2024-06-01",
        "2024-12-01",
        "2025-06-01",
        "2025-12-01"
      )
    ),
    labels = c(
      "2021/dez",
      "2022/jun",
      "2022/dez",
      "2023/jun",
      "2023/dez",
      "2024/jun",
      "2024/dez",
      "2025/jun",
      "2025/dez"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Municípios da 17º Regional de Saúde do PR com menos de 16 mil habtantes",
    subtitle = "Casos registrados de Dengue"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
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
    plot.margin = margin(4, 15, 10, 10),
    axis.text.x = element_text(
      size = 8.5,
      margin = margin(t = 3),
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA)
  )


denv_regional_pequenos
denv_sertanopolis

# graficos_comparacao_serta_x_regional ------------------------------------

d_regional_pequenos_sem_serta <- d_regional |>
  mutate(ID_MUNICIP = as.numeric(ID_MUNICIP)) |>
  filter(ID_MUNICIP != 411370) |>
  filter(ID_MUNICIP != 410370) |>
  filter(ID_MUNICIP != 412240) |>
  filter(ID_MUNICIP != 410980) |>
  filter(ID_MUNICIP != 412650)

info_pop <- read.delim("data/pop_regional.txt", sep = ";")
pop_total_regional_pequenos_sem_serta <- (sum(info_pop$pop_2022) -
  sum(555965, 107208, 51603, 71670, 15930))

cal_regional_pequenos_sem_serta <- d_regional_pequenos_sem_serta |>
  group_by(ano_sem) |>
  summarise(total_casos_pequenos_sem_serta = n()) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(
    total_casos_pequenos_sem_serta = replace_na(
      total_casos_pequenos_sem_serta,
      0
    )
  ) |>
  mutate(
    total_pop_pequenos_sem_serta = pop_total_regional_pequenos_sem_serta,
    porcent = ((total_casos_pequenos_sem_serta * 100) /
      total_pop_pequenos_sem_serta),
  ) |>
  distinct()


cal_regional_pequenos_sem_serta_cap <- cal_regional_pequenos_sem_serta |>
  filter(ano_sem > yearweek("2021 W44", week_start = 7)) |>
  as_tsibble(index = ano_sem) |>
  fill_gaps() |>
  mutate(
    total_casos_pequenos_sem_serta = replace_na(
      total_casos_pequenos_sem_serta,
      0
    ),
    total_casos_pequenos_sem_serta = as.numeric(total_casos_pequenos_sem_serta),
    data = as_date(ano_sem),
    status_proj = "antes"
  )

saveRDS(
  cal_regional_pequenos_sem_serta_cap,
  "ggplot/den_sus_regional_pequenos_sem_serta_cap.RDS"
)

max(cal_regional_pequenos_sem_serta_cap$total_casos_pequenos_sem_serta)

seq_e = seq(0, 1500, by = 100)
seq_f = seq(0, 1500, by = 500)
seq_e <- seq_e[!seq_e %in% seq_f]


denv_regional_pequenos_comparacao <- cal_sertanopolis_cap |>
  ggplot(aes(x = data, color = status_proj)) +
  geom_rect(
    xmin = ymd("2023-12-10"),
    xmax = ymd("2026-01-01"),
    ymin = -4,
    ymax = 1500,
    fill = "#FCF0E9",
    color = NA
  ) +
  geom_hline(
    yintercept = c(0, 100, 200, 500, 1000, 1500),
    linewidth = 0.2,
    color = "gray80",
    linetype = 1
  ) +
  geom_line(
    data = cal_regional_pequenos_sem_serta_cap,
    aes(x = data, y = total_casos_pequenos_sem_serta),
    show.legend = FALSE,
    linewidth = 0.7
  ) +
  geom_line(
    data = cal_sertanopolis_cap,
    aes(x = data, y = total_casos_serta),
    show.legend = FALSE,
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(
    breaks = c(0, 100, 200, 500, 1000, 1500),
    limits = c(0, 2000)
  ) +
  scale_x_date(
    breaks = ymd(
      c(
        "2021-12-01",
        "2022-06-01",
        "2022-12-01",
        "2023-06-01",
        "2023-10-23",
        "2023-12-01",
        "2024-06-01",
        "2024-12-01",
        "2025-06-01",
        "2025-12-01"
      )
    ),
    labels = c(
      "2021/dez",
      "2022/jun",
      "2022/dez",
      "2023/jun",
      "2023/out \n(Início do projeto)",
      "2023/dez",
      "2024/jun",
      "2024/dez",
      "2025/jun",
      "2025/dez"
    )
  ) +
  scale_color_manual(
    breaks = c("antes", "depois"),
    values = c("#464749", "red")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Municípios da 17º Regional de Saúde do PR com menos de 16 mil habtantes",
    subtitle = "Casos registrados de Dengue"
  ) +
  theme(
    text = element_text(family = "serif"),
    plot.background = element_rect(fill = "#EEEEEE"),
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
    plot.margin = margin(4, 15, 10, 10),
    axis.text.x = element_text(
      size = 8.5,
      margin = margin(t = 3),
      color = "black"
    ),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = NA)
  )

denv_regional_pequenos_comparacao
