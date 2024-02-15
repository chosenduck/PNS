## PNS

# Limpando todos os objetos atualmente ativos no Ambiente (Environment)
rm(list=ls(all=T))

# Definindo diretorio de trabalho
setwd("")

############################ CARREGANDO PACOTES ############################### 
if(!require(dplyr)){install.packages("dplyr")};library(dplyr)
if(!require(plyr)){install.packages("plyr")};library(plyr)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(bigrquery)){install.packages("bigrquery")};library(bigrquery)
if(!require(survey)){install.packages("survey")};library(survey)
if(!require(odbc)){install.packages("odbc")};library(odbc)
if(!require(srvyr)){install.packages("srvyr")};library(srvyr)
if(!require(magrittr)){install.packages("magrittr")};library(magrittr)
if(!require(PNSIBGE)){install.packages("PNSIBGE")};library(PNSIBGE)
if(!require(ggplot2)){install.packages("ggplot2")};library(ggplot2)
if(!require(tictoc)){install.packages("tictoc")};library(tictoc)
if(!require(foreign)){install.packages("foreign")};library(foreign)
if(!require(forcats)){install.packages("forcats")};library(forcats)


######################### CARREGANDO BASE DE DADOS ######################### 
# Obtendo ajuda sobre o pacote PNSIBGE
help("get_pns")

# Baixando PNS para cada ano, sem design amostral
pns2013svy_semda <- get_pns(year=2013, selected=F, anthropometry=T,
                      labels=T, deflator=T, design=F)

str(pns2013svy_semda)
dim(pns2013svy_semda)

pns2019svy_semda <- get_pns(year=2019, selected=F, anthropometry=T,
                      labels=T, deflator=T, design=F)

str(pns2019svy_semda)
dim(pns2019svy_semda)

# Baixando PNS para cada ano, com design amostral
pns2013svy_comda <- get_pns(year=2013, selected=F, anthropometry=T,
                            labels=T, deflator=T, design=F)

str(pns2013svy_comda)
dim(pns2013svy_comda)

pns2019svy_comda <- get_pns(year=2019, selected=F, anthropometry=T,
                            labels=T, deflator=T, design=F)

str(pns2019svy_comda)
dim(pns2019svy_comda)

######################### PRÉ-PROCESSAMENTO ######################### 
# desativando a exibição por notação científica
options(scipen=999)

# Definição do peso
pns2013.1 <- pns2013svy_comda %>% mutate(peso_morador_selec=((V00291*(60202/145572211)))) # Peso do morador selecionado com calibração
summary(pns2013.1$peso_morador_selec)

pns2019.1 <- pns2019svy_comda %>% mutate(peso_antropom=((V00301*(7060/168426190)))) # Peso do morador selecionado com calibração
summary(pns2019.1$peso_antropom)

# Criação de variáveis dos indicadores e recodificações
# Cálculo IMC
pns2013.1 <- pns2013.1 %>% mutate(altura_metro = W00203 / 100)
pns2013.1 <- pns2013.1 %>% mutate(IMC = W00103 / (altura_metro * altura_metro))
summary(pns2013.1$IMC)

pns2019.1 <- pns2019.1 %>% mutate(altura_metro = W00203 / 100)
pns2019.1 <- pns2019.1 %>% mutate(IMC = W00103 / (altura_metro * altura_metro))
summary(pns2019.1$IMC)

# Classificação IMC
pns2013.1 <- pns2013.1 %>% 
  mutate(IMC_classif= cut(IMC,
                          breaks=c(-Inf, 18.4999, 24.9999, 29.9999, 34.9999, 39.9999, Inf),
                          labels=c("Peso_baixo", "Eutrofia", "Sobrepeso", 
                                   "Obesidade_grau_I", "Obesidade_grau_II", 
                                   "Obesidade_grau_III")))

table(pns2013.1$IMC_classif, useNA = "always")


pns2019.1 <- pns2019.1 %>% 
  mutate(IMC_classif= cut(IMC,
                          breaks=c(-Inf, 18.4999, 24.9999, 29.9999, 34.9999, 39.9999, Inf),
                          labels=c("Peso_baixo", "Eutrofia", "Sobrepeso", 
                                   "Obesidade_grau_I", "Obesidade_grau_II", 
                                   "Obesidade_grau_III")))

table(pns2019.1$IMC_classif, useNA = "always")

# Sexo - C006
pns2013.1 <- pns2013.1 %>% rename(Sexo=C006)
summary(pns2013.1$Sexo)

pns2019.1 <- pns2019.1 %>% rename(Sexo=C006)
summary(pns2019.1$Sexo)

#Faixas Etárias - C008
pns2013.1 <-  pns2013.1 %>% mutate(faixaeta=cut(C008, 
                                                   breaks = c(18, 25, 35, 45, 55, 65,Inf),
                                                   labels = c("18 a 24 anos","25 a 34 anos","35 a 44 anos","45 a 54 anos", "55 a 64 anos", "65 anos ou mais"), 
                                                   ordered_result = TRUE, right = FALSE))

summary(pns2013.1$faixaeta)


pns2019.1 <-  pns2019.1 %>% mutate(faixaeta=cut(C008, 
                                                breaks = c(18, 25, 35, 45, 55, 65,Inf),
                                                labels = c("18 a 24 anos","25 a 34 anos","35 a 44 anos","45 a 54 anos", "55 a 64 anos", "65 anos ou mais"), 
                                                ordered_result = TRUE, right = FALSE))
summary(pns2019.1$faixaeta)

# Estados - V0001
pns2013.1 <- pns2013.1 %>% rename(UF=V0001)
summary(pns2013.1$UF)

pns2019.1 <- pns2019.1 %>% rename(UF=V0001)
summary(pns2019.1$UF)

# Macro Regiões
pns2013.1 <- pns2013.1 %>% 
  mutate(MacroRegioes = fct_collapse(UF, 
                                       `Norte` = c("Rondônia","Acre","Amazonas","Roraima","Pará", "Amapá","Tocantins"),
                                       `Nordeste` = c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba","Pernambuco", "Alagoas","Sergipe","Bahia"),
                                       `Sudeste` = c("Minas Gerais", "Espírito Santo","Rio de Janeiro", "São Paulo"), 
                                       `Sul` = c("Paraná", "Santa Catarina", "Rio Grande do Sul"),
                                       `Centro-Oeste`= c("Mato Grosso do Sul","Mato Grosso", "Goiás", "Distrito Federal")))
summary(pns2013.1$MacroRegioes)

pns2019.1 <- pns2019.1 %>% 
  mutate(MacroRegioes = fct_collapse(UF, 
                                     `Norte` = c("Rondônia","Acre","Amazonas","Roraima","Pará", "Amapá","Tocantins"),
                                     `Nordeste` = c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba","Pernambuco", "Alagoas","Sergipe","Bahia"),
                                     `Sudeste` = c("Minas Gerais", "Espírito Santo","Rio de Janeiro", "São Paulo"), 
                                     `Sul` = c("Paraná", "Santa Catarina", "Rio Grande do Sul"),
                                     `Centro-Oeste`= c("Mato Grosso do Sul","Mato Grosso", "Goiás", "Distrito Federal")))
summary(pns2019.1$MacroRegioes)

# Capitais
pns2013.1<- pns2013.1 %>% mutate(capitais= fct_collapse(UF,
                                                       `Porto Velho`= "Rondônia", 
                                                       `Boa Vista`= "Roraima",              
                                                       `Rio Branco`= "Acre", 
                                                       `Manaus` = "Amazonas",
                                                       `Belém` = "Pará" ,
                                                       `Macapá`= "Amapá",
                                                       `Palmas` = "Tocantins",
                                                       `São Luís` = "Maranhão",
                                                       `Teresina`= "Piauí" ,
                                                       `Fortaleza`= "Ceará",
                                                       `Natal`= "Rio Grande do Norte",
                                                       `João Pessoa`= "Paraíba",
                                                       `Recife`= "Pernambuco",
                                                       `Maceió`= "Alagoas",
                                                       `Aracaju`= "Sergipe",
                                                       `Salvador`= "Bahia",
                                                       `Belo Horizonte`= "Minas Gerais",
                                                       `Vitória`= "Espírito Santo",
                                                       `Rio de Janeiro`= "Rio de Janeiro",
                                                       `São Paulo`= "São Paulo",
                                                       `Curitiba`= "Paraná",
                                                       `Florianópolis`= "Santa Catarina",
                                                       `Porto Alegre`= "Rio Grande do Sul",
                                                       `Campo Grande`=  "Mato Grosso do Sul",
                                                       `Cuiabá`= "Mato Grosso",
                                                       `Goiânia` = "Goiás",
                                                       `Brasília`= "Distrito Federal"))
summary(pns2013.1$capitais)


pns2019.1<- pns2019.1 %>% mutate(capitais= fct_collapse(UF,
                                                        `Porto Velho`= "Rondônia", 
                                                        `Boa Vista`= "Roraima",              
                                                        `Rio Branco`= "Acre", 
                                                        `Manaus` = "Amazonas",
                                                        `Belém` = "Pará" ,
                                                        `Macapá`= "Amapá",
                                                        `Palmas` = "Tocantins",
                                                        `São Luís` = "Maranhão",
                                                        `Teresina`= "Piauí" ,
                                                        `Fortaleza`= "Ceará",
                                                        `Natal`= "Rio Grande do Norte",
                                                        `João Pessoa`= "Paraíba",
                                                        `Recife`= "Pernambuco",
                                                        `Maceió`= "Alagoas",
                                                        `Aracaju`= "Sergipe",
                                                        `Salvador`= "Bahia",
                                                        `Belo Horizonte`= "Minas Gerais",
                                                        `Vitória`= "Espírito Santo",
                                                        `Rio de Janeiro`= "Rio de Janeiro",
                                                        `São Paulo`= "São Paulo",
                                                        `Curitiba`= "Paraná",
                                                        `Florianópolis`= "Santa Catarina",
                                                        `Porto Alegre`= "Rio Grande do Sul",
                                                        `Campo Grande`=  "Mato Grosso do Sul",
                                                        `Cuiabá`= "Mato Grosso",
                                                        `Goiânia` = "Goiás",
                                                        `Brasília`= "Distrito Federal"))
summary(pns2019.1$capitais)

# Raça - C009
pns2013.1 <- pns2013.1 %>% rename(racacor=C009)
summary(pns2013.1$racacor)

pns2019.1 <- pns2019.1 %>% rename(racacor=C009)
summary(pns2019.1$racacor)

# Rendimento per capita - VDF003 (2013) e VDF004 (2019)
pns2013.1 <- pns2013.1 %>% mutate(rend_per_capita=cut(VDF003,
                                                      breaks = c(-Inf,1/2 * 1412, 1 * 1412, 2 * 1412, 3 * 1412,Inf),
                                                      labels=c("Até 1/2 SM","1/2 até 1 SM","1 até 2 SM","2 até 3 SM","Mais de 3 SM"), 
                                                      ordered_result = TRUE, right = TRUE, na.exclude= TRUE))

summary(pns2013.1$rend_per_capita)


class(pns2019.1$VDF004)

pns2019.1 <- pns2019.1 %>%
  mutate(VDF004 = case_when(
    VDF004 %in% c('Até ¼ salário mínimo', 'Mais de ¼ até ½ salário mínimo') ~ 'Até 1/2 SM',
    VDF004 == 'Mais de ½ até 1 salário mínimo' ~ '1/2 até 1 SM',
    VDF004 == 'Mais de 1 até 2 salários mínimos' ~ '1 até 2 SM',
    VDF004 == 'Mais de 2 até 3 salários mínimos' ~ '2 até 3 SM',
    VDF004 %in% c('Mais de 3 até 5 salários mínimos', 'Mais de 5 salários mínimos') ~ 'Mais de 3 SM',
    TRUE ~ VDF004
  )) %>%
  mutate(VDF004 = if_else(is.na(VDF004), 'Ignorado', VDF004))

pns2019.1 <- pns2019.1 %>% rename(rend_per_capita=VDF004)

table(pns2019.1$rend_per_capita)

# Escolaridade - VDD004A
pns2013.1 <- pns2013.1 %>%
  mutate(VDD004A = case_when(
    VDD004A %in% c('Fundamental incompleto ou equivalente', 'Fundamental completo ou equivalente') ~ 'Fundamental',
    VDD004A %in% c('Médio incompleto ou equivalente', 'Médio completo ou equivalente') ~ 'Ensino Médio',
    VDD004A %in% c('Superior incompleto ou equivalente', 'Superior completo') ~ 'Superior',
    TRUE ~ VDD004A
  )) %>%
  mutate(VDD004A = if_else(is.na(VDD004A), 'Ignorado', VDD004A))

pns2013.1 <- pns2013.1 %>% rename(escolaridade=VDD004A)

table(pns2013.1$escolaridade)


pns2019.1 <- pns2019.1 %>%
  mutate(VDD004A = case_when(
    VDD004A %in% c('Fundamental incompleto ou equivalente', 'Fundamental completo ou equivalente') ~ 'Fundamental',
    VDD004A %in% c('Médio incompleto ou equivalente', 'Médio completo ou equivalente') ~ 'Ensino Médio',
    VDD004A %in% c('Superior incompleto ou equivalente', 'Superior completo') ~ 'Superior',
    TRUE ~ VDD004A
  )) %>%
  mutate(VDD004A = if_else(is.na(VDD004A), 'Ignorado', VDD004A))

pns2019.1 <- pns2019.1 %>% rename(escolaridade=VDD004A)

table(pns2019.1$escolaridade)

######################### CRIANDO INDICADORES ######################### 
# survey design da pesquisa
help("pns_design")

PNS2013 <- pns_design(data_pns=pns2013.1)
PNS2019 <- pns_design(data_pns=pns2019.1)

# obtendo prevalências
help("svymean")
help("svyby")

# estados nutricionais: totais
en_total_13 <- svymean(~IMC_classif, PNS2013, na.rm = TRUE) %>%
  as.data.frame()
print(en_total_13)

en_total_19 <- svymean(~IMC_classif, PNS2019, na.rm = TRUE) %>%
  as.data.frame()
print(en_total_19)

# estados nutricionais: sexo (relatório faz a partir de 20 anos (17,9H e 25,7M), aqui trabalhamos a partir de 18)
en_sexo_13 <- svyby(~IMC_classif, ~Sexo, PNS2013, svymean, na.rm = T, keep.var = F)
print(en_sexo_13)

en_sexo_19 <- svyby(~IMC_classif, ~Sexo, PNS2019, svymean, na.rm = T, keep.var = F)
print(en_sexo_19)

# estados nutricionais: faixa etária
en_faixaeta_13 <- svyby(~IMC_classif, ~faixaeta, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_faixaeta_13)

en_faixaeta_19 <- svyby(~IMC_classif, ~faixaeta, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_faixaeta_19)

# estados nutricionais: faixa etária por sexo
en_faixaeta_sexo_13 <- svyby(~IMC_classif, ~faixaeta + Sexo, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_faixaeta_sexo_13)

en_faixaeta_sexo_19 <- svyby(~IMC_classif, ~faixaeta + Sexo, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_faixaeta_sexo_19)

# estados nutricionais: cidades
en_cidades_13 <- svyby(~IMC_classif, ~capitais, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_cidades_13)

en_cidades_19 <- svyby(~IMC_classif, ~capitais, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_cidades_19)

# estados nutricionais: macro regiões
en_cidades_13 <- svyby(~IMC_classif, ~MacroRegioes, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_cidades_13)

en_cidades_19 <- svyby(~IMC_classif, ~MacroRegioes, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_cidades_19)

# estados nutricionais: cidades por faixa etária
en_cidades_faixaeta_13 <- svyby(~IMC_classif, ~capitais + faixaeta, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_cidades_faixaeta_13)

en_cidades_faixaeta_19 <- svyby(~IMC_classif, ~capitais + faixaeta, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_cidades_faixaeta_19)

# estados nutricionais: cidades por sexo
en_cidades_sexo_13 <- svyby(~IMC_classif, ~capitais + Sexo, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_cidades_sexo_13)

en_cidades_sexo_19 <- svyby(~IMC_classif, ~capitais + Sexo, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_cidades_sexo_19)

# estados nutricionais: raça
en_racacor_13 <- svyby(~IMC_classif, ~racacor, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_racacor_13)

en_racacor_19 <- svyby(~IMC_classif, ~racacor, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_racacor_19)

# estados nutricionais: escolaridade 
en_escolaridade_13 <- svyby(~IMC_classif, ~escolaridade, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_escolaridade_13)

en_escolaridade_19 <- svyby(~IMC_classif, ~escolaridade, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_escolaridade_19)

# estados nutricionais: renda per capita
en_renda_13 <- svyby(~IMC_classif, ~rend_per_capita, PNS2013, na.rm = T, svymean, keep.var = F)
print(en_renda_13)

en_renda_19 <- svyby(~IMC_classif, ~rend_per_capita, PNS2019, na.rm = T, svymean, keep.var = F)
print(en_renda_19)

######################### SALVANDO TABELAS ######################### 
help("svytable")
help ("prop.table")

# 2013
tab_fato_PNS2013 <- svytable(~IMC_classif + UF + capitais + MacroRegioes + 
                               faixaeta + rend_per_capita + racacor + Sexo + 
                               escolaridade, PNS2013) %>% 
  as.data.frame() %>%
  filter(Freq != 0)

head(tab_fato_PNS2013)

write.csv(tab_fato_PNS2013, "", row.names = T)

# 2019
tab_fato_PNS2019 <- svytable(~IMC_classif + UF + capitais + MacroRegioes + 
                               faixaeta + rend_per_capita + racacor + Sexo + 
                               escolaridade, PNS2019) %>% 
  as.data.frame()%>%
  filter(Freq != 0)

head(tab_fato_PNS2019)

write.csv(tab_fato_PNS2019, "", row.names = T)
