rm(list = ls(all = T))


# Config ------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(fpp3)
library(ggridges)
library(viridis)
library(showtext)

font_add("times", "times.ttf")
showtext_auto()

# Carregar de dados -------------------------------------------------------

dados <- read_excel("./data/dados.xlsx", col_names = TRUE, na = c("-", ""))

colnames(dados) <- janitor::excel_numeric_to_date(
  as.numeric(colnames(dados)),
  date_system = "modern"
)
colnames(dados)[1] <- "armadilha"


# OrgD - Dados em Wide ----------------------------------------------------

df_ovos <- pivot_longer(
  dados,
  col = !armadilha,
  names_to = "data",
  values_to = "ovos"
) |>
  mutate(
    armadilha = as.character(armadilha),
    data = ymd(data),
    sem = yearweek(data),
    ovos = as.integer(ovos)
  )


## OrdD - Dados em Wide para RDS ------------------------------------------

df_ovos |>
  filter(!is.na(ovos)) |>
  select(-sem) |>
  relocate(armadilha, data, ovos) |>
  saveRDS("./data/df_ovos.RDS")


# PlotD - Densidade --------------------------------------------------------

## PlotD - Densidade/Semana -----------------------------------------------

df_ovos |>
  group_by(data) |>
  glimpse()


### (Teste) Densidade de ovos para cara semana analisada

df_ovos |>
  filter(!is.na(ovos)) |>
  mutate(sem = as.factor(sem)) |>
  ggplot(aes(x = ovos, y = sem, group = sem)) +
  geom_density_ridges_gradient(
    aes(height = after_stat(ndensity)),
    alpha = 0.6,
    stat = "density",
    scale = 4
  ) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    title = "Distribuição de Ovos por Dia Amostrado",
    x = "Contagem de Ovos",
    y = "Data"
  ) +
  theme_bw()

#### (comentário)
#### Ficou uma desgraça ...

# ------------------------------------------------------------------------ #

### (Teste) Densidade de ovos para cara semana analisada (colorido)

densidade_max <- df_ovos |>
  filter(!is.na(ovos)) |>
  group_by(sem) |>
  group_modify(
    ~ {
      dens <- density(.x$ovos, from = 0, na.rm = TRUE)
      tibble(densidade_max = max(dens$y))
    }
  ) |>
  pull(densidade_max) |>
  max()

df_ovos |>
  filter(!is.na(ovos)) |>
  mutate(armadilha = as.factor(armadilha), sem = as.factor(sem)) |>
  ggplot(aes(x = ovos, y = sem)) +
  geom_density_ridges_gradient(
    aes(height = after_stat(density), fill = after_stat(density)),
    stat = "density",
    scale = 4
  ) +
  scale_fill_viridis_c(
    name = "Densidade",
    option = "C",
    limits = c(0, densidade_max),
    alpha = 0.9
  ) +
  scale_x_continuous(limits = c(0, 1400)) +
  labs(
    title = "Distribuição de Ovos por Semana",
    x = "Contagem de Ovos",
    y = "Semana"
  ) +
  theme_bw()


#### (comentário)
#### Ficou uma desgraça também, e pesado ...
#### preciso achar uma outra forma de mostrar densidade
#### os testes até o momento n foram bons (n sei pq eu tentei fazer isso)

## PlotD - Densidade geral ------------------------------------------------

plot(density(df_ovos$ovos, na.rm = TRUE))

P_densidade <- df_ovos |>
  ggplot(aes(x = ovos)) +
  geom_histogram(alpha = 1, binwidth = 5) +
  labs(
    title = "Distribuição do número de Ovos contados em armadilhas",
    x = "Número de ovos contados (separados em grupos de 5 observações)",
    y = "Densidade de ovos"
  ) +
  theme_bw(base_family = "times")
P_densidade


ggsave(
  "./fig/Densidade_ovos.png",
  P_densidade,
  scale = 2,
  width = 16,
  height = 5,
  units = "cm"
)

df_sum_geral <- df_ovos |>
  summarise(
    Média = mean(ovos, na.rm = TRUE),
    Moda = DescTools::Mode(ovos, na.rm = TRUE),
    SD = sd(ovos, na.rm = TRUE),
    Total = sum(ovos, na.rm = TRUE)
  )

saveRDS(df_sum_geral, "./data/df_sum_geral.RDS")


## PlotD - Densidade por armadilha -----------------------------------------

df_ovos |>
  filter(!is.na(ovos))

df_ovos$armadilha <- factor(df_ovos$armadilha, levels = 50:1)

df_ovos |>
  ggplot(aes(x = ovos, y = armadilha, group = armadilha)) +
  geom_density_ridges_gradient(
    aes(height = after_stat(ndensity)),
    alpha = 0.6,
    stat = "density",
    scale = 4
  ) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    title = "Distribuição de Ovos por armadilha Amostrado",
    x = "Contagem de Ovos",
    y = "Armadilha"
  ) +
  theme_bw()


# Plot_IDO ----------------------------------------------------------------

## Plot_IDO - Calculo por semana ------------------------------------------
df_cal <- df_ovos |>
  group_by(sem) |>
  reframe(
    ido = sum(ovos, na.rm = TRUE) / sum(ovos > 0, na.rm = TRUE),
    ipo = (sum(ovos > 0, na.rm = TRUE) / sum(ovos >= 0, na.rm = TRUE)) *
      100,
    tot_ovos = sum(ovos, na.rm = TRUE),
    media = mean(ovos, na.rm = TRUE),
    desvio_padrao = sd(ovos, na.rm = TRUE),
    data = data
  ) |>
  unique() |>
  glimpse()

df_datas_coleta <- df_cal |>
  select(data, everything()) |>
  select(
    Data = data,
    "Semana" = sem,
    IDO = ido,
    IPO = ipo,
    Média = media,
    "SD" = desvio_padrao,
    "Total de Ovos" = tot_ovos
  )

saveRDS(df_datas_coleta, "./data/df_datas_coleta.RDS")

kableExtra::kbl(df_datas_coleta, booktabs = TRUE, align = "c") |>
  kableExtra::kable_styling(
    latex_options = c("striped", "hold_position"),
    full_width = FALSE,
    position = "center"
  ) |>
  kableExtra::footnote(
    general = "IDO = Índice de Densidade de Ovos, \n IPO = Índice de Positividade de Ovitrampas, \n SD = Desvio Padrão",
    general_title = ""
  )

df_cal_ts <- df_cal |>
  as_tsibble(index = sem)

df_cal_ts |>
  autoplot(tot_ovos)

df_cal_ts |>
  mutate(ano = as.factor(year(sem))) |>
  autoplot(ido) +
  geom_line(aes(color = ano)) +
  labs(
    title = "Densidade de Ovos por Semana",
    y = "Índice de Densidade",
    color = "Ano"
  )

View(df_cal_ts)

#### (comentário)
#### Falsas quinzenas - utilizar somente para plotagem comparativa!
#### Para análise é necessário excluir as duas ultimas quinzenas.

df_cal_ts_quin <- df_cal_ts |>
  mutate(
    sem = if_else(sem == yearweek("2025 W39"), yearweek("2025 W38"), sem)
  ) |>
  mutate(
    sem = if_else(sem == yearweek("2025 W43"), yearweek("2025 W42"), sem)
  ) |>
  mutate(
    sem = if_else(sem == yearweek("2025 W46"), yearweek("2025 W44"), sem)
  ) |>
  mutate(
    sem = if_else(sem == yearweek("2025 W48"), yearweek("2025 W46"), sem)
  ) |>
  fill_gaps() |>
  as_tibble() |>
  mutate(id_quinze = ceiling(row_number() / 2)) |>
  group_by(id_quinze) |>
  reframe(
    ido = ido,
    ipo = ipo,
    tot_ovos = tot_ovos,
    media = media,
    desvio_padrao = desvio_padrao,
    sem = min(sem),
    data = data
  ) |>
  ungroup() |>
  select(-id_quinze) |>
  remove_empty("rows", cutoff = 0.5) |>
  as_tsibble(index = sem)


#### (comentário)
#### Adicionar coluna para n armadilhas instaladas

df_cal_ts_quin <- df_cal_ts_quin |>
  mutate(n_armadilha = if_else(data < dmy("07-08-2025"), "47", "50")) |>
  mutate(n_armadilha = if_else(data < dmy("21-01-2025"), "46", n_armadilha)) |>
  mutate(n_armadilha = if_else(data < dmy("21-12-2023"), "36", n_armadilha))

df_cal_ts_quin_mes <- df_cal_ts_quin |>
  left_join(data.frame(
    ano_mes = rep(
      seq(yearmonth("2023-10"), yearmonth("2026-01"), 1),
      each = 2
    ),
    data = unique(df_cal_ts_quin$data)
  )) |>
  mutate(data_trans = format(data, "%d/%m/%y")) |>
  glimpse()

saveRDS(df_cal_ts_quin_mes, "./data/df_ovos_cal_sem_corrigido.RDS")


glimpse(df_cal_ts_quin_mes)
glimpse(uniao)
# plots da quali ----------------------------------------------------------

P_comp_ido <- df_cal_ts_quin |>
  fill_gaps() |>
  ggtime::gg_season(ido, labels = "both") +
  xlab("Tempo") +
  ylab("IDO") +
  labs(
    title = "Comparação anual: Índice de Densidade de Ovos (IDO)",
    subtitle = "Intervalos entre coletas padronizado para plotagem"
  ) +
  theme_bw() +
  scale_color_viridis(option = "H")

P_comp_ido

ggsave(
  "./fig/Comparacao_IDO_anual.png",
  P_comp_ido,
  scale = 2,
  width = 16,
  height = 5,
  units = "cm"
)

P_comp_tot_ovos <- df_cal_ts_quin |>
  mutate(n_armadilha = as.factor(n_armadilha)) |>
  autoplot(tot_ovos) +
  geom_line(color = "gray80", size = 0.5) +
  geom_point(aes(color = n_armadilha)) +
  xlab("Tempo [semanas]") +
  ylab("Número Total de Ovos coletados") +
  labs(
    title = "Total de Ovos coletados no município de Sertanópolis-PR",
    subtitle = "Intervalos entre coletas padronizado para plotagem",
    color = "Número de\n armadilhas\n instaladas\n no município"
  ) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_viridis_d(option = "H")

ggsave(
  "./fig/Total_ovos.png",
  P_comp_tot_ovos,
  scale = 2,
  width = 16,
  height = 5,
  units = "cm"
)


P_ovos_armadilha_destaques <- tibble(
  sem = yearweek(c("2024 W18", "2025 W02")),
  ovos = c(1022, 1346),
  rotulo = c("(A43)", "(A42)")
)

P_comp_tot_ovos

## Plot_IDO - Calculo por armadilha ---------------------------------------

P_ovos_armadilha <- df_ovos |>
  ggplot(aes(x = sem, y = ovos, colour = armadilha)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Número de Ovos coletado por armadilha instalada ao longo do tempo",
    subtitle = "Detaque para armadilhas A43 e A42",
    x = "Tempo [semanas]",
    y = "Número de ovos",
    color = "Número da Armadilha"
  ) +
  theme_bw() +
  scale_color_viridis_d(option = "H") +
  geom_text(
    data = P_ovos_armadilha_destaques,
    aes(x = sem, y = ovos, label = rotulo),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.7,
    size = 3
  )

ggsave(
  "./fig/Por_armadilha_ovos.png",
  P_ovos_armadilha,
  scale = 2,
  width = 16,
  height = 8,
  units = "cm"
)


# Plots - IDO sem - Final -------------------------------------------------

break_plot_ido_sem_total_final <- df_cal_ts_quin_mes |>
  ungroup() |>
  select(ano_mes, data) |>
  distinct(ano_mes, .keep_all = T)

glimpse(df_cal_ts_quin_mes)
fundo <- data.frame(xmin = as.Date(break_plot_ido_sem_total_final$data)) |>
  mutate(xmax = lead(xmin)) |>
  filter(row_number() %% 2 != 0)

plot_ido_sem_total_final <- df_cal_ts_quin_mes |>
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
    data = df_cal_ts_quin_mes,
    aes(x = data, y = ido),
    linewidth = 0.7
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(0, 20, 35, 100, 200), limits = c(0, 200)) +
  scale_x_date(
    breaks = df_cal_ts_quin_mes$data,
    labels = df_cal_ts_quin_mes$data_trans,
    guide = guide_axis(n.dodge = 2)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "",
    subtitle = ""
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
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      width = NULL,
      fill = NULL,
      padding = margin(1, 4, 5, 4),
      hjust = 0
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

plot_ido_sem_total_final # removi o título para construir por fora
# ggsave(
#   "./fig/ts_semanal_IDO_total.png",
#   plot_ido_sem_total_final,
#   width = 10,
#   height = 4,
#   units = "in",
#   dpi = 600
# )
