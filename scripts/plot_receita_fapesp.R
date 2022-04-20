# Carregando pacotes --------------------------------------------------------------------------
library(tidyverse)

# Lendo a base --------------------------------------------------------------------------------
df <- read_csv("adjusted_data/fapesp_receitas_1994_2021.csv")

# Plot ----------------------------------------------------------------------------------------

g <-
  df |>
  ggplot(aes(x = as.character(exercicios))) +
  geom_col(
    aes(y = total),
    size = 1,
    fill = "blue",
    alpha = .4
  ) +
  geom_line(
    aes(y = total),
    group = 1,
    size = 3.5,
    color = "white"
  ) +
  geom_line(
    aes(y = total,
        lty = "Evolução anual das Receitas - 1994 a 2011(R$)"),
    group = 1,
    size = 1.2,
    color = "blue"
  ) +
  scale_y_continuous("Milhões de reais (R$)") +
  theme(
    text = element_text(size = 20, family = "Times New Roman"),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(size = 12),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line.x.bottom = element_line(size = 1),
    axis.line.y.left = element_line(size = 1),
    panel.grid.major.y = element_line(size = 0.5, color = "gray95"),
    panel.grid.minor.y = element_line(size = 0.5, color = "gray95"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(
    size = 12,
    angle = 45,
    hjust = 1
    ),
    axis.ticks.x = element_line(size = 1),
    axis.ticks.y.left = element_line(size = 1),
    axis.ticks.length = unit(.25, "cm"),
    legend.text = element_text(size = 18),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    plot.caption = element_text(size = 12, color = "gray50"),
  ) +
  labs(caption = "https://fapesp.br/9250/evolucao-das-receitas")

g





