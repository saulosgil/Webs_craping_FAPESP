# Lendo a base --------------------------------------------------------------------------------
df2 <- read.csv(file = "adjusted_data/fapesp_despachos_2010-2019.csv",
                encoding = "UTF-8")

# Plot ----------------------------------------------------------------------------------------

# Total de Despachos

g2 <-
  df2 |>
    dplyr::filter(despachos == "Quantidade") |>
    ggplot2::ggplot(ggplot2::aes(
    x = as.factor(Ano),
    y = Quantidade,
    colour = "white",
    fill = Ano
  )) +
  ggplot2::geom_col(show.legend = FALSE) +
  viridis::scale_color_viridis(discrete = TRUE) +
  viridis::scale_fill_viridis(discrete = FALSE) +
  ggplot2::labs(title = "TOTAL DE DESPACHOS REALIZADOS PELA FAPESP",
       subtitle = "De 2010 à 2019") +
  ggplot2::xlab("Ano") +
  ggplot2::theme_classic() +
  ggplot2::theme(text = ggplot2::element_text(size = 20,
                                     family = "Times New Roman"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5))
g2

# Prazo m?dio para o Despacho

g3 <-
  df2 |>
  dplyr::filter(despachos == "Prazo médio (dias)") |>
  dplyr::mutate(
    Quantidade = Quantidade/1000
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = as.factor(Ano),
    y = Quantidade,
    colour = "white",
    fill = Ano
  )) +
  ggplot2::geom_col(show.legend = FALSE) +
  viridis::scale_color_viridis(discrete = TRUE) +
  viridis::scale_fill_viridis(discrete = FALSE) +
  ggplot2::labs(title = "PRAZO M?DIO (DIAS) PARA DESPACHO REALIZADOS PELA FAPESP",
       subtitle = "De 2010 à 2019",
       caption = "https://fapesp.br/estatisticas/analise") +
  ggplot2::xlab("Ano") +
  ggplot2::ylab("Número de dias") +
  ggplot2::theme_classic() +
  ggplot2::theme(text = ggplot2::element_text(size = 20,
                                     family = "Times New Roman"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5))
g3

# LAYOUT --------------------------------------------------------------------------------------
library(patchwork)

g2/g3




