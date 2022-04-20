# Carregando pacotes --------------------------------------------------------------------------
library(tidyverse)
library(viridis)

# Lendo a base --------------------------------------------------------------------------------
df2 <- read.csv(file = "adjusted_data/fapesp_despachos_2010-2019.csv",
                encoding = "UTF-8")

# Plot ----------------------------------------------------------------------------------------

# Total de Despachos

g2 <-
  df2 |>
    filter(despachos == "Quantidade") |>
    ggplot(aes(
    x = as.factor(Ano),
    y = Quantidade,
    colour = "white",
    fill = Ano
  )) +
  geom_col(show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = FALSE) +
  labs(title = "TOTAL DE DESPACHOS REALIZADOS PELA FAPESP",
       subtitle = "De 2010 à 2019") +
  xlab("Ano") +
  theme_classic() +
  theme(text = element_text(size = 20,
                                     family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g2

# Prazo m?dio para o Despacho

g3 <-
  df2 |>
  filter(despachos == "Prazo médio (dias)") |>
  mutate(
    Quantidade = Quantidade/1000
  ) |>
  ggplot(aes(
    x = as.factor(Ano),
    y = Quantidade,
    colour = "white",
    fill = Ano
  )) +
  geom_col(show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = FALSE) +
  labs(title = "PRAZO M?DIO (DIAS) PARA DESPACHO REALIZADOS PELA FAPESP",
       subtitle = "De 2010 à 2019",
       caption = "https://fapesp.br/estatisticas/analise") +
  xlab("Ano") +
  ylab("Número de dias") +
  theme_classic() +
  theme(text = element_text(size = 20,
                                     family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g3

# LAYOUT --------------------------------------------------------------------------------------
library(patchwork)

g2/g3




