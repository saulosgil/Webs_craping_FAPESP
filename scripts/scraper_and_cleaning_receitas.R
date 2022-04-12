# Carregando pacotes --------------------------------------------------------------------------
library(tidyverse)
library(httr)
library(xml2)
library(rvest)

#  Tirar notação cientifica -------------------------------------------------------------------

options(scipen = 999)

# GET URL -------------------------------------------------------------------------------------

url_fapesp <- "https://fapesp.br/9250/evolucao-das-receitas"

get_url_fapesp <- httr::GET(url_fapesp)

# verificando status da requisição

get_url_fapesp


# Scraping o html --------------------------------------------------------------------------------

tb_fapesp <-
  get_url_fapesp |>
  read_html() |>
  xml_find_all(xpath = "//table") |>
  rvest::html_table(header = TRUE)

# Arrumando a base de dados

fapesp_arrumada <- tb_fapesp[[1]] |>
  janitor::clean_names() |> # arrumando os nomes
  mutate( # tirando o ponto dos vetores de caracteres para converter em numeric
    transferencias_do_tesouro = as.numeric(str_remove_all(transferencias_do_tesouro,
                                                          pattern = "[.]")),
    outras_receitas = as.numeric(str_remove_all(outras_receitas,
                                                pattern = "[.]")),
    total_da_receita_do_ano = as.numeric(str_remove_all(total_da_receita_do_ano,
                                                        pattern = "[.]")),
    reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores = as.numeric(str_remove_all(reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores,
                                                                                                    pattern = "[.]")),
    total = as.numeric(str_remove_all(total,
                                      pattern = "[.]"))
  ) |>
  dplyr::mutate(
    total = total/1000000,
  )

# Criando um .csv  ----------------------------------------------------------------------------

fapesp_arrumada |>
  filter(exercicios >= 1994 & exercicios < 2022) |>
  write_csv(file = "adjusted_data/fapesp_receitas_1994_2021.csv")

