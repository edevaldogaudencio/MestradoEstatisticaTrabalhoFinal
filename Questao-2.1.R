# Metrado Profissional em Econômia
# Alunos: Jeferson, Alfredo e Edevaldo
# Disciplina: Estatística
# Professor: Bruno De Oliveira Cruz
# Monitor: hiago Mendes Rosa
#### Partes dos códigos utilizados neste programa foram desenvolvidos a partir 
#### dos exemplos disponibilizados pelo monitor Thiago Mendes Rosa

# Carregar as biblioteca necessárias para a execução do Trabalho Final;
library(tidyverse)
library(survey)
library(dplyr)
library(srvyr)

#
# Parte Inicial - Carregamento dos dados e criação da amostra segundo os parâmetros da CODEPLAN
#
# Carregar base de moradores
pdad_2018_moradores <- data.table::fread("http://www.codeplan.df.gov.br/wp-content/uploads/2020/06/mor2018_31ras.csv",
                                         dec = ",", encoding = "Latin-1", data.table=F, integer64="character")

# Carregar a base de domicílios
pdad_2018_domicilios <- data.table::fread("http://www.codeplan.df.gov.br/wp-content/uploads/2020/06/dom2018_31ras.csv",
                                          dec=",", encoding="Latin-1", data.table=F, integer64="character")

# Consultar o índice das colunas de mesmo nome entre as bases de moradores e domicílios
x <- which((names(pdad_2018_domicilios) %in% names(pdad_2018_moradores)))
# Verificar quais são as colunas repetidas além da chave primária
names(pdad_2018_domicilios)[x]

# Fazer o join à esquerda entre as duas bases apenas pela chave primária A01nFicha
pdad <- pdad_2018_moradores %>% left_join(pdad_2018_domicilios %>% select(-c(A01ra,FATOR_PROJ)))

# Defenir uma semente para reprodutibilidade
set.seed(8888)

# Declarar o desenho incial
sample.pdad <- svydesign(id = ~A01nFicha, # Identificador único da unidade amostrada
                         strata = ~A01setor, # Identificação do estrato
                         weights = ~PESO_PRE, # Probabilidade da unidade ser sorteada
                         nest=TRUE, # Parâmetro de tratamento para dos IDs dos estratos
                         data=pdad # Declarar a base a ser utilizada
)

# Criar um objeto para pós estrato
post.pop <- pdad %>% group_by(POS_ESTRATO) %>% summarise(Freq=max(POP_AJUSTADA_PROJ))

# Declarar o objeto de pós-estrato. Estamos dizendo nesse passo qual é a
# população alvo para cada pós-estrato considerado
sample.pdad <- postStratify(sample.pdad,~POS_ESTRATO,post.pop)

# Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
# J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
# Vol. 83, No. 401 (Mar., 1988), pp. 231-241
amostra <- as.svrepdesign(sample.pdad, type = "subbootstrap")

# Ajustar estratos com apenas uma UPA (adjust=centered)
options(survey.lonely.psu = "adjust")

# Ajustar objeto de amostra, para uso com o pacote srvyr
amostra <- as_survey(amostra)



#
# ITEM 2.1
#
# Calcule o intervalo de confiança a 95% de confiança da renda média
# domiciliar e do tempo dedicado a afazeres domésticos para Samanmbaia.
# Foi considerado como Renda Domiciliar os seguintes parâmetros:
#   * G16 - Renda Primária;
#   * G19 - Renda Sedundária;
#   * G201 - Aposentadoria;
#   * G202 - Pensão;
#   * G203 - Outras Rendas;
#
amostra %>% filter(A01ra==12) %>% #Renda Média em Samambaia
  mutate(renda_prim=case_when(G16 == 77777 ~ 0L,
                              G16 == 88888 ~ 0L,
                              G16 == 99999 ~ 0L,
                              TRUE ~ G16)) %>%
  mutate(renda_sec=case_when(G19 == 66666 ~ 0L,
                             G19 == 77777 ~ 0L,
                             G19 == 88888 ~ 0L,
                             G19 == 99999 ~ 0L,
                             TRUE ~ G19)) %>%
  mutate(aposentadoria=case_when(G201 == 66666 ~ 0L,
                                 G201 == 77777 ~ 0L,
                                 G201 == 88888 ~ 0L,
                                 G201 == 99999 ~ 0L,
                                 TRUE ~ G201)) %>%
  mutate(pensao=case_when(G202 == 66666 ~ 0L,
                          G202 == 77777 ~ 0L,
                          G202 == 88888 ~ 0L,
                          G202 == 99999 ~ 0L,
                          TRUE ~ G202)) %>%
  mutate(outros=case_when(G203 == 66666 ~ 0L,
                          G203 == 77777 ~ 0L,
                          G203 == 88888 ~ 0L,
                          G203 == 99999 ~ 0L,
                          TRUE ~ G203)) %>%
  mutate(beneficios=case_when(G204 == 66666 ~ 0L,
                              G204 == 77777 ~ 0L,
                              G204 == 88888 ~ 0L,
                              G204 == 99999 ~ 0L,
                              TRUE ~ G204)) %>%
  summarise("Renda Média Samambaia"=survey_mean(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios,
                                                na.rm = TRUE, vartype = "ci", level = 0.95))

amostra %>% filter(A01ra==12) %>% # Horas Semanais Média nos Afazeres Domésticos em Samambaia
  mutate(tempo_afazeres=case_when(G18 == 88888 ~ NA_integer_,
                              G18 == 99999 ~ NA_integer_,
                              TRUE ~ G18)) %>%
  summarise("Horas Afazeres Samambaia"=survey_mean(tempo_afazeres,
                                         na.rm = TRUE,
                                         vartype = "ci",
                                         level = 0.95))

#
# ITEM - 2.2
# Faça um teste de hipótese que o número médio de automóveis por domicílio em
# Samambaia é igual a 1, com grau de confiança de 95% (bi-caudal).
#
svyttest(carro~1,
         amostra %>% 
           filter(E02 == 1 & A01ra == 12, ) %>% 
           mutate(carro=case_when(C011 == 88888 ~ NA_integer_,
                                  TRUE ~ C011)),
         na.rm = TRUE)

#
# ITEM - 2.3
# Faça um teste de média e compare a renda domiciliar média de Samambaia com o
# Plano Piloto. Repita o mesmo exercício comparando Samambaia e o DF.
#
svyttest(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios~A01ra==12,
         amostra %>% filter(A01ra %in% c(12, 1)) %>% #Samambaia x Plano Piloto
           mutate(renda_prim=case_when(G16 == 77777 ~ 0L,
                                       G16 == 88888 ~ 0L,
                                       G16 == 99999 ~ 0L,
                                       TRUE ~ G16)) %>%
           mutate(renda_sec=case_when(G19 == 66666 ~ 0L,
                                      G19 == 77777 ~ 0L,
                                      G19 == 88888 ~ 0L,
                                      G19 == 99999 ~ 0L,
                                      TRUE ~ G19)) %>%
           mutate(aposentadoria=case_when(G201 == 66666 ~ 0L,
                                          G201 == 77777 ~ 0L,
                                          G201 == 88888 ~ 0L,
                                          G201 == 99999 ~ 0L,
                                          TRUE ~ G201)) %>%
           mutate(pensao=case_when(G202 == 66666 ~ 0L,
                                   G202 == 77777 ~ 0L,
                                   G202 == 88888 ~ 0L,
                                   G202 == 99999 ~ 0L,
                                   TRUE ~ G202)) %>%
           mutate(outros=case_when(G203 == 66666 ~ 0L,
                                   G203 == 77777 ~ 0L,
                                   G203 == 88888 ~ 0L,
                                   G203 == 99999 ~ 0L,
                                   TRUE ~ G203)) %>%
           mutate(beneficios=case_when(G204 == 66666 ~ 0L,
                                       G204 == 77777 ~ 0L,
                                       G204 == 88888 ~ 0L,
                                       G204 == 99999 ~ 0L,
                                       TRUE ~ G204)),
         na.rm = TRUE)

svyttest(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios~A01ra==12,
         amostra %>% #Samambaia x Distrito Federal
           mutate(renda_prim=case_when(G16 == 77777 ~ 0L,
                                       G16 == 88888 ~ 0L,
                                       G16 == 99999 ~ 0L,
                                       TRUE ~ G16)) %>%
           mutate(renda_sec=case_when(G19 == 66666 ~ 0L,
                                      G19 == 77777 ~ 0L,
                                      G19 == 88888 ~ 0L,
                                      G19 == 99999 ~ 0L,
                                      TRUE ~ G19)) %>%
           mutate(aposentadoria=case_when(G201 == 66666 ~ 0L,
                                          G201 == 77777 ~ 0L,
                                          G201 == 88888 ~ 0L,
                                          G201 == 99999 ~ 0L,
                                          TRUE ~ G201)) %>%
           mutate(pensao=case_when(G202 == 66666 ~ 0L,
                                   G202 == 77777 ~ 0L,
                                   G202 == 88888 ~ 0L,
                                   G202 == 99999 ~ 0L,
                                   TRUE ~ G202)) %>%
           mutate(outros=case_when(G203 == 66666 ~ 0L,
                                   G203 == 77777 ~ 0L,
                                   G203 == 88888 ~ 0L,
                                   G203 == 99999 ~ 0L,
                                   TRUE ~ G203)) %>%
           mutate(beneficios=case_when(G204 == 66666 ~ 0L,
                                       G204 == 77777 ~ 0L,
                                       G204 == 88888 ~ 0L,
                                       G204 == 99999 ~ 0L,
                                       TRUE ~ G204)),
         na.rm = TRUE)

#
# ITEM 2.4
# Faça agora um teste de hipótese compara a variância da renda média domiciliar
# de Samambaia e do Plano Piloto.Repita o mesmo teste comparando Samambaia com
# o Distrito Federal.
#
summary(svyglm(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios~A01ra==12,
        amostra %>% filter(A01ra %in% c(12, 1)) %>% #Samambaia x Plano Piloto
          mutate(renda_prim=case_when(G16 == 77777 ~ 0L,
                                      G16 == 88888 ~ 0L,
                                      G16 == 99999 ~ 0L,
                                      TRUE ~ G16)) %>%
          mutate(renda_sec=case_when(G19 == 66666 ~ 0L,
                                     G19 == 77777 ~ 0L,
                                     G19 == 88888 ~ 0L,
                                     G19 == 99999 ~ 0L,
                                     TRUE ~ G19)) %>%
          mutate(aposentadoria=case_when(G201 == 66666 ~ 0L,
                                         G201 == 77777 ~ 0L,
                                         G201 == 88888 ~ 0L,
                                         G201 == 99999 ~ 0L,
                                         TRUE ~ G201)) %>%
          mutate(pensao=case_when(G202 == 66666 ~ 0L,
                                  G202 == 77777 ~ 0L,
                                  G202 == 88888 ~ 0L,
                                  G202 == 99999 ~ 0L,
                                  TRUE ~ G202)) %>%
          mutate(outros=case_when(G203 == 66666 ~ 0L,
                                  G203 == 77777 ~ 0L,
                                  G203 == 88888 ~ 0L,
                                  G203 == 99999 ~ 0L,
                                  TRUE ~ G203)) %>%
          mutate(beneficios=case_when(G204 == 66666 ~ 0L,
                                      G204 == 77777 ~ 0L,
                                      G204 == 88888 ~ 0L,
                                      G204 == 99999 ~ 0L,
                                      TRUE ~ G204))))

summary(svyglm(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios~A01ra==12,
         amostra %>% #Samambaia x Distrito Federal
           mutate(renda_prim=case_when(G16 == 77777 ~ 0L,
                                       G16 == 88888 ~ 0L,
                                       G16 == 99999 ~ 0L,
                                       TRUE ~ G16)) %>%
           mutate(renda_sec=case_when(G19 == 66666 ~ 0L,
                                      G19 == 77777 ~ 0L,
                                      G19 == 88888 ~ 0L,
                                      G19 == 99999 ~ 0L,
                                      TRUE ~ G19)) %>%
           mutate(aposentadoria=case_when(G201 == 66666 ~ 0L,
                                          G201 == 77777 ~ 0L,
                                          G201 == 88888 ~ 0L,
                                          G201 == 99999 ~ 0L,
                                          TRUE ~ G201)) %>%
           mutate(pensao=case_when(G202 == 66666 ~ 0L,
                                   G202 == 77777 ~ 0L,
                                   G202 == 88888 ~ 0L,
                                   G202 == 99999 ~ 0L,
                                   TRUE ~ G202)) %>%
           mutate(outros=case_when(G203 == 66666 ~ 0L,
                                   G203 == 77777 ~ 0L,
                                   G203 == 88888 ~ 0L,
                                   G203 == 99999 ~ 0L,
                                   TRUE ~ G203))  %>%
           mutate(beneficios=case_when(G204 == 66666 ~ 0L,
                                       G204 == 77777 ~ 0L,
                                       G204 == 88888 ~ 0L,
                                       G204 == 99999 ~ 0L,
                                       TRUE ~ G204))))
