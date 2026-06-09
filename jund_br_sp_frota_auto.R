library(tidyverse)
library(readr)
library(dplyr)
library(remotes)
library(tidyverse)
library(data.table)
library(writexl)
library(readxl)
library(purrr)


#Importa as bases dos anos separadamente

frota_2001 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2001.xls", col_types = "text")
frota_2002 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2002.xls", col_types = "text")
frota_2003 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2003.xls", col_types = "text")
frota_2004 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2004.xls", col_types = "text")
frota_2005 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2005.xls", col_types = "text")
frota_2006 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2006.xls", col_types = "text")
frota_2007 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2007.xls", col_types = "text")
frota_2008 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2008.xls", col_types = "text")
frota_2009 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2009.xls", col_types = "text")
frota_2010 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2010.xls", col_types = "text")
frota_2011 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2011.xls", col_types = "text")
frota_2012 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2012.xls", col_types = "text")
frota_2013 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2013.xls", col_types = "text")
frota_2014 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2014.xls", col_types = "text")
frota_2015 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2015.xls", col_types = "text")
frota_2016 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2016.xls", col_types = "text")
frota_2017 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2017.xls", col_types = "text")
frota_2018 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2018.xls", col_types = "text")
frota_2019 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2019.xls", col_types = "text")
frota_2020 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2020.xls", col_types = "text")
frota_2021 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2021.xls", col_types = "text")
frota_2022 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2022.xls", col_types = "text")
frota_2023 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2023.xls", col_types = "text")
frota_2024 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2024.xls", col_types = "text")
frota_2025 <- read_excel("C:/Users/observatorio/Desktop/Pasta/Indicadores/Frota/frota_mun_tipo_2025.xls", col_types = "text")


# Junta todas as tabelas em uma só
frota_completa <- bind_rows(frota_2001,
                            frota_2002,
                            frota_2003,
                            frota_2004,
                            frota_2005,
                            frota_2006,
                            frota_2007,
                            frota_2008,
                            frota_2009,
                            frota_2010,
                            frota_2011,
                            frota_2012,
                            frota_2013,
                            frota_2014,
                            frota_2015,
                            frota_2016,
                            frota_2017,
                            frota_2018,
                            frota_2019,
                            frota_2020,
                            frota_2021,
                            frota_2022,
                            frota_2023,
                            frota_2024,
                            frota_2025)



#Transforma a coluna AUTOMOVEL em número pra poder fazer a soma
frota_completa$AUTOMOVEL <- as.numeric(frota_completa$AUTOMOVEL)

#Filtra Jundiaí por ano
tab_frota_jundiai <- frota_completa %>%
  filter(MUNICIPIO == "JUNDIAÍ" | MUNICIPIO == "JUNDIAI") %>%
  group_by(ANO) %>%
  summarise(TOTAL_AUTOMOVEIS = sum(AUTOMOVEL, na.rm = TRUE)) %>%
  mutate(LOCAL = "Jundiaí") #Cria a coluna pra saber de onde é o dado

#Filtra SP por ano
tab_frota_sp <- frota_completa %>%
  filter(UF == "SP") %>%
  group_by(ANO) %>%
  summarise(TOTAL_AUTOMOVEIS = sum(AUTOMOVEL, na.rm = TRUE)) %>%
  mutate(LOCAL = "SP")

#Soma Brasil por ano
tab_frota_brasil <- frota_completa %>%
  group_by(ANO) %>%
  summarise(TOTAL_AUTOMOVEIS = sum(AUTOMOVEL, na.rm = TRUE)) %>%
  mutate(LOCAL = "Brasil")

#Junta Brasil, SP, Jundiaí na ordem em que está na planilha
tab_frota_completa <- bind_rows(tab_brasil, tab_sp, tab_jundiai) %>%
  #Reorganiza as colunas para o LOCAL aparecer primeiro
  select(ANO, LOCAL, TOTAL_AUTOMOVEIS) %>%
  #Organiza por ano (2001, 2002...) e, dentro do ano, força a ordem: Brasil, SP, Jundiaí
  arrange(ANO, match(LOCAL, c("Brasil", "Jundiaí", "SP")))

#Salva em Excel
write_xlsx(tabela_final, "C:/Users/observatorio/Documents/r/teste/script/frota_auto.xlsx")

