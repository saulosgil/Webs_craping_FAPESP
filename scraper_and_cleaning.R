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
  )

# Separando as receitas para converter todas em real ------------------------------------------

real <- filter(fapesp_arrumada, receitas == "R$")
cruzeiro_real <- filter(fapesp_arrumada, receitas == "CR$")
cruzado_novo <- filter(fapesp_arrumada, receitas == "NCz$")
cruzado <- filter(fapesp_arrumada, receitas == "Cz$")
cruzeiro <- filter(fapesp_arrumada, receitas == "Cr$")
cruzeiro_novo <- filter(fapesp_arrumada, receitas == "NCr$")

# convertendo cruzeiro real para real

# função para converter cruzeiro real para real
cruzeiro_real_para_real <- function(x){
  (x*1000^4)*2.75
}

# Acertando a base com receita em cruzeiro real
cruzeiro_real <-
  cruzeiro_real |>
  mutate(
    transferencias_do_tesouro = cruzeiro_real_para_real(transferencias_do_tesouro),
    outras_receitas = cruzeiro_real_para_real(outras_receitas),
    total_da_receita_do_ano = cruzeiro_real_para_real(total_da_receita_do_ano),
    reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores = cruzeiro_real_para_real(reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores),
    total = cruzeiro_real_para_real(total)
  )

# convertendo cruzado novo para real

# função para converter cruzado novo para real

cruzado_novo_para_real <- function(x){
  (x*1000^2)*2.75
}

cruzado_novo <-
  cruzado_novo |>
  mutate(
    transferencias_do_tesouro = cruzado_novo_para_real(transferencias_do_tesouro),
    outras_receitas = cruzado_novo_para_real(outras_receitas),
    total_da_receita_do_ano = cruzado_novo_para_real(total_da_receita_do_ano),
    reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores = cruzado_novo_para_real(reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores),
    total = cruzado_novo_para_real(total)
  )

# convertendo cruzado para real

# função para converter cruzado para real

cruzado_para_real <- function(x){
  (x*1000^3)*2.75
}

cruzado <-
  cruzado |>
  mutate(
    transferencias_do_tesouro = cruzado_para_real(transferencias_do_tesouro),
    outras_receitas = cruzado_para_real(outras_receitas),
    total_da_receita_do_ano = cruzado_para_real(total_da_receita_do_ano),
    reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores = cruzado_para_real(reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores),
    total = cruzado_para_real(total)
  )

# convertendo cruzeiro para real

# função para cruzeiro para real

cruzeiro_para_real <- function(x){
  (x*1000^5)*2.75
}

cruzeiro <-
  cruzeiro |>
  mutate(
    transferencias_do_tesouro = cruzeiro_para_real(transferencias_do_tesouro),
    outras_receitas = cruzeiro_para_real(outras_receitas),
    total_da_receita_do_ano = cruzeiro_para_real(total_da_receita_do_ano),
    reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores = cruzeiro_para_real(reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores),
    total = cruzeiro_para_real(total)
  )

# convertendo cruzeiro novo para real

# função para cruzeiro novo para real

cruzeiro_novo_para_real <- function(x){
  (x*1000^4)*2.75
}

cruzeiro_novo <-
  cruzeiro_novo |>
  mutate(
    transferencias_do_tesouro = cruzeiro_novo_para_real(transferencias_do_tesouro),
    outras_receitas = cruzeiro_novo_para_real(outras_receitas),
    total_da_receita_do_ano = cruzeiro_novo_para_real(total_da_receita_do_ano),
    reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores = cruzeiro_novo_para_real(reversao_de_diferimento_de_receitas_saldos_de_exercicios_anteriores),
    total = cruzeiro_novo_para_real(total)
  )


rbind(real,
      cruzeiro_real,
      cruzado_novo,
      cruzado,
      cruzeiro,
      cruzeiro_novo
) |>
  as.tibble() |>
  view()

# CONVERÇÕES ESTÃO ERRADAS PRECISA ARRUMAR
