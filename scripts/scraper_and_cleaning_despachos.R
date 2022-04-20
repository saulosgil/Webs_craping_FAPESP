# Carregando pacotes --------------------------------------------------------------------------
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(janitor)

#  Tirar notação cientifica -------------------------------------------------------------------

options(scipen = 999)

# GET URL -------------------------------------------------------------------------------------

url_fapesp_despacho <- "https://fapesp.br/estatisticas/analise"

get_url_fapesp_despacho <- GET(url_fapesp_despacho)

# verificando status da requisição

get_url_fapesp_despacho

# Scraping o html --------------------------------------------------------------------------------

get_url_fapesp_despacho <-
  get_url_fapesp_despacho |>
  read_html() |>
  xml_find_all(xpath = "//table") |>
  html_table(header = TRUE) |>
  pluck(1)

# Arrumando a base - TOTAL DE DESPACHOS

total_despachos <-
   get_url_fapesp_despacho[1:2,2:12] |>
   clean_names() |>
   mutate(
     x2010 = as.numeric(x2010),
     x2011 = as.numeric(x2011),
     x2012 = as.numeric(x2012),
     x2013 = as.numeric(x2013),
     x2014 = as.numeric(x2014),
     x2015 = as.numeric(x2015),
     x2016 = as.numeric(x2016),
     x2017 = as.numeric(x2017),
     x2018 = as.numeric(x2018),
     x2019 = as.numeric(x2019)
   ) |>
     pivot_longer(cols = !x,names_to = "Ano",values_to = "Quantidade") |>
     mutate(
       Ano = as.factor(str_remove_all(string = Ano, pattern = "[x]")),
       Quantidade = as.numeric(Quantidade*1000)
       ) |>
  rename(
   despachos = x
  )

total_despachos

# Criando um .csv -----------------------------------------------------------------------------

total_despachos |>
  write_csv(file = "adjusted_data/fapesp_despachos_2010-2019.csv")




