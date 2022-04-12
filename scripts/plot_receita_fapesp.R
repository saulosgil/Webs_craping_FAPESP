# Lendo a base --------------------------------------------------------------------------------
df <- readr::read_csv("adjusted_data/fapesp_receitas_1994_2021.csv")

# Plot ----------------------------------------------------------------------------------------

g <-
  df |>
  ggplot2::ggplot(ggplot2::aes(x = as.character(exercicios))) +
  ggplot2::geom_col(
    ggplot2::aes(y = total),
    size = 1,
    fill = "blue",
    alpha = .4
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = total),
    group = 1,
    size = 3.5,
    color = "white"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = total,
                 lty = "Evolução anual das Receitas - 1994 a 2011(R$)"),
    group = 1,
    size = 1.2,
    color = "blue"
  ) +
  ggplot2::scale_y_continuous("Milhões de reais (R$)") +
  ggplot2::theme(
    text = ggplot2::element_text(size = 20, family = "Times New Roman"),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y.left = ggplot2::element_text(size = 12),
    axis.ticks.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white"),
    axis.line.x.bottom = ggplot2::element_line(size = 1),
    axis.line.y.left = ggplot2::element_line(size = 1),
    panel.grid.major.y = ggplot2::element_line(size = 0.5, color = "gray95"),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5, color = "gray95"),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(
      size = 12,
      angle = 45,
      hjust = 1
    ),
    axis.ticks.x = ggplot2::element_line(size = 1),
    axis.ticks.y.left = ggplot2::element_line(size = 1),
    axis.ticks.length = ggplot2::unit(.25, "cm"),
    legend.text = ggplot2::element_text(size = 18),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "top",
    legend.justification = "center",
    plot.caption = ggplot2::element_text(size = 12, color = "gray50"),
  ) +
  ggplot2::labs(caption = "https://fapesp.br/9250/evolucao-das-receitas")

g





