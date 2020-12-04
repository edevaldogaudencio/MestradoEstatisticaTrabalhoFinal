######Carregar pacotes necessários
library(readr)
library(survey)
library(dplyr)
library(srvyr)
library(Hmisc)
library(lubridate) 

######## Preparando o ambiente

      ######Importaçõa de domicílios
      pdad_2018_domicilios <- read_delim("dados/PDAD_2018_dom2018_31ras.csv", 
                                         ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                         trim_ws = TRUE)
      
      
      ######Importaçõa de moradores
      pdad_2018_moradores <- read_delim("dados/PDAD_2018_mor2018_31ras.csv", 
                                            ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                            trim_ws = TRUE)
      
      
      ###### Importando o dicionário de dados moradores
      dic_moradores <- readxl::read_excel("dados/Dicionario_de_Variaveis_PDAD_2018.xlsx",
                                          skip = 1,
                                          sheet = 2)
      
      ### Adicionar rótulos à base da pdad
      # Criar um objeto com os rótulos
      var.labels <- dic_moradores$`Descrição da coluna` %>%
        # Retirar as linhas ausentes
        na.omit
      
      # Nomear esses rótulos com o nome das variáveis do nosso banco de dados  
      names(var.labels) <- names(pdad_2018_moradores)
      
      # Adicionar os rótulos ao nosso banco de dados
      pdad_2018_moradores <- Hmisc::upData(pdad_2018_moradores, labels = var.labels)
      
      # Verificar o resultado
      #Hmisc::describe(pdad_2018_moradores)
      
      
      ###### Importando o dicionário de dados domicilios
      dic_domicilios <- readxl::read_excel("dados/Dicionario_de_Variaveis_PDAD_2018.xlsx",
                                          skip = 1,
                                          sheet = 1)
      # Adicionar rótulos à base da pdad
      # Criar um objeto com os rótulos
      var.labelsDom <- dic_domicilios$`Descrição da coluna` %>%
        # Retirar as linhas ausentes
        na.omit
      
      # Nomear esses rótulos com o nome das variáveis do nosso banco de dados  
      names(var.labelsDom) <- names(pdad_2018_domicilios)
      
      # Adicionar os rótulos ao nosso banco de dados
      pdad_2018_domicilios <- Hmisc::upData(pdad_2018_domicilios, labels = var.labelsDom)
      
      # Verificar o resultado
      #Hmisc::describe(pdad_2018_domicilios)
      
      
      ###### Unificar arquivos de dados a partir do identificador único da fixa
      # Fazer o join das bases
      pdad <- pdad_2018_moradores %>%
        # Entrar com a função para left join
        dplyr::left_join(
          # Informar a base que iremos unir, filtrando para colunas repetidas
          pdad_2018_domicilios %>%
            # Filtrar as colunas repetidas
            dplyr::select(-c(A01ra)),
          by=c("A01nFicha"="A01nFicha")) %>% 
        # Mudar a variável pos-estrato para o tipo character
        dplyr::mutate(POS_ESTRATO=as.character(POS_ESTRATO))
      
      
      ###### Criar o desenho inicial da pesquisa
      #Defenir uma semente para reprodutibilidade
      set.seed(8888)
      
      # Declarar o desenho incial
      sample.pdad <- 
        survey::svydesign(id = ~A01nFicha, # Identificador único da unidade amostrada
                          strata = ~A01setor, # Identificação do estrato
                          weights = ~PESO_PRE, # Probabilidade da unidade ser sorteada
                          nest=TRUE, # Parâmetro de tratamento para dos IDs dos estratos
                          data=pdad # Declarar a base a ser utilizada
                          )
      # Criar um objeto para pós estrato
      post.pop <- pdad %>%
        dplyr::group_by(POS_ESTRATO) %>% # Agrupar por pos-estrato
        dplyr::summarise(Freq=max(POP_AJUSTADA_PROJ)) # Capturar o total da população
      
      # Declarar o objeto de pós-estrato
      # Estamos dizendo nesse passo qual é a população alvo para cada
      # pós-estrato considerado
      sample.pdad <- survey::postStratify(sample.pdad,~POS_ESTRATO,post.pop)
      
      # Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
      # J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
      # Vol. 83, No. 401 (Mar., 1988), pp. 231-241
      amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")
      
      # Ajustar estratos com apenas uma UPA (adjust=centered)
      options( survey.lonely.psu = "adjust")
      
      # Ajustar objeto de amostra, para uso com o pacote srvyr
      amostra <- srvyr::as_survey(amostra)
      
      
      ###### Criar um objeto com as variáveis para as próximas questões
      sm <- 954
      vars_relatorio <- amostra %>%
        # Criar variável de sexo
        srvyr::mutate(sexo=case_when(E03==1~"Masculino",
                                     E03==2~"Feminino"),
                      # Criar variável de faixas de idade
                      idade_faixas=cut(idade_calculada,
                                       breaks = c(-Inf,seq(4,84,by=5),Inf),
                                       labels = c("0 a 4 anos","5 a 9 anos",
                                                  "10 a 14 anos","15 a 19 anos",
                                                  "20 a 24 anos","25 a 29 anos",
                                                  "30 a 34 anos","35 a 39 anos",
                                                  "40 a 44 anos","45 a 49 anos",
                                                  "50 a 54 anos","55 a 59 anos",
                                                  "60 a 64 anos","65 a 69 anos",
                                                  "70 a 74 anos","75 a 79 anos",
                                                  "80 a 84 anos","Mais de 85 anos"),
                                       ordered_result = T),
                      # Criar variável para as RAs
                      RA=factor(A01ra,
                                levels=1:31,
                                labels=c('Plano Piloto',      
                                         'Gama',
                                         'Taguatinga',
                                         'Brazlândia',
                                         'Sobradinho',
                                         'Planaltina',
                                         'Paranoá',
                                         'Núcleo Bandeirante',
                                         'Ceilândia',
                                         'Guará',
                                         'Cruzeiro',
                                         'Samambaia',
                                         'Santa Maria',
                                         'São Sebastião',
                                         'Recanto das Emas',
                                         'Lago Sul',
                                         'Riacho Fundo',
                                         'Lago Norte',
                                         'Candangolândia',
                                         'Águas Claras',
                                         'Riacho Fundo II',
                                         'Sudoeste/Octogonal',
                                         'Varjão',
                                         'Park Way',
                                         'SCIA-Estrutural',
                                         'Sobradinho II',
                                         'Jardim Botânico',
                                         'Itapoã',
                                         'SIA',
                                         'Vicente Pires',
                                         'Fercal'))) %>%
        # Transformar em fator variáveis do tipo character
        srvyr::mutate_if(is.character,list(~factor(.))) %>%
        # Selecionar as variáveis criadas e algumas variáveis auxiliares
        srvyr::select(RA,E02,idade_calculada,G05,sexo,idade_faixas)

########Questões 
      
##### 1.1 Apresente um perfil da RA X, Plano Piloto e do Distrito Federal, estimando as seguintes variáveis

###### i)	População total Plano Piloto e Samambaia
      amostra %>%
        # Filtrar Plano Piloto e Samambaia
        #srvyr::filter(A01ra==1 | A01ra==12) %>%
        # Ajustar nome das variáveis
        srvyr::mutate(A01ra=factor(case_when(A01ra==1~"Plano Piloto",
                                            A01ra==12~"Samambaia",
                                            TRUE~"Outras"))) %>%
        # Agrupar por cidade
        srvyr::group_by(A01ra) %>%
        # Calcular o total e o Percentual da população, com seu intervalo de confiança
        srvyr::summarise("População Total"=survey_total(vartype = "ci"),
                         # Calcular o percentual da população
                         pct=survey_mean(vartype = "ci"))

###### i)	População total DF
      amostra %>%
        # Filtrar População Total
        srvyr::filter(A01ra >= 1 ) %>%
        # Criar contador
        srvyr::mutate(count=1) %>%
        # Calcular o total
        srvyr::summarise("População Total"=survey_total(count, vartype = "ci"))


##### ii)	Distribuição etária da população (faça uma pirâmide etária, 
#####     separando homens e mulheres, com classes variando de 5 em 5 anos de 0-4; 5-9; 10-14...)

### Distribuição etária DF
      piramide <-
        vars_relatorio %>%
        # Agrupar por faixas de idade e sexo
        srvyr::group_by(idade_faixas,sexo) %>%
        # Calcular os totais
        srvyr::summarise(n=survey_total(na.rm = T, vartype = "ci"))
      
      # Fazer o gráfico com a pirâmide
      piramide_grafico <- piramide %>%
        # Construir um plot com as idades no eixo x, as quantidades no eixo y,
        #  preenchimento com a variável sexo, e os intervalos de confiança
        # inferiores e superiores
        ggplot(aes(x=idade_faixas,y=n, fill=sexo, ymin=n_low,ymax=n_upp))+
        # Fazer o gráfico de barras para o sexo Feminino
        geom_bar(data = dplyr::filter(piramide, sexo == "Feminino"),
                 stat = "identity") +
        # Fazer o gráfico de barras para o sexo Masculino
        geom_bar(data = dplyr::filter(piramide, sexo == "Masculino"),
                 stat = "identity",
                 position = "identity",
                 # Negativar os valores para espelhar no eixo
                 mapping = aes(y = -n))+
        # Plotar os erros para o sexo Masculino, negativando os valores para espelhar o eixo
        geom_errorbar(data = dplyr::filter(piramide, sexo == "Masculino"),
                      mapping = aes(ymin = -n_low,ymax=-n_upp),
                      width=0,
                      color="black")+
        # Plotar os erros para o sexo Feminino
        geom_errorbar(data = dplyr::filter(piramide, sexo == "Feminino"),
                      width=0,
                      color="black")+
        # Inverter os eixos, fazendo com que o gráfico de colunas verticais fique
        # horizontal
        coord_flip() + 
        # Ajustar as configurações de escala
        scale_y_continuous(labels = function(x) format(abs(x), 
                                                       big.mark = ".",
                                                       scientific = FALSE,
                                                       decimal.mark=",")) +
        # Suprimir os nomes dos eixos
        labs(x="",y="") +
        # Nome do gráfico
        scale_fill_discrete(name = "Distribuição etária DF")
        # Plotar gráfico
        piramide_grafico


### Distribuição etária Plano Piloto
      piramide <-
        vars_relatorio %>%
        srvyr::filter(RA == "Plano Piloto" ) %>%
        # Agrupar por faixas de idade e sexo
        srvyr::group_by(idade_faixas,sexo) %>%
        # Calcular os totais
        srvyr::summarise(n=survey_total(na.rm = T, vartype = "ci"))
      
      
      # Fazer o gráfico com a pirâmide
      piramide_grafico <- piramide %>%
        # Construir um plot com as idades no eixo x, as quantidades no eixo y,
        #  preenchimento com a variável sexo, e os intervalos de confiança
        # inferiores e superiores
        ggplot(aes(x=idade_faixas,y=n, fill=sexo, ymin=n_low,ymax=n_upp))+
        # Fazer o gráfico de barras para o sexo Feminino
        geom_bar(data = dplyr::filter(piramide, sexo == "Feminino"),
                 stat = "identity") +
        # Fazer o gráfico de barras para o sexo Masculino
        geom_bar(data = dplyr::filter(piramide, sexo == "Masculino"),
                 stat = "identity",
                 position = "identity",
                 # Negativar os valores para espelhar no eixo
                 mapping = aes(y = -n))+
        # Plotar os erros para o sexo Masculino, negativando os valores para espelhar o eixo
        geom_errorbar(data = dplyr::filter(piramide, sexo == "Masculino"),
                      mapping = aes(ymin = -n_low,ymax=-n_upp),
                      width=0,
                      color="black")+
        # Plotar os erros para o sexo Feminino
        geom_errorbar(data = dplyr::filter(piramide, sexo == "Feminino"),
                      width=0,
                      color="black")+
        # Inverter os eixos, fazendo com que o gráfico de colunas verticais fique
        # horizontal
        coord_flip() + 
        # Ajustar as configurações de escala
        scale_y_continuous(labels = function(x) format(abs(x), 
                                                       big.mark = ".",
                                                       scientific = FALSE,
                                                       decimal.mark=",")) +
        # Suprimir os nomes dos eixos
        labs(x="",y="") +
        # Nome do gráfico
        scale_fill_discrete(name = "Distribuição etária Plano Piloto")
        # Plotar gráfico
        piramide_grafico


### Distribuição etária Samambaia
      piramide <-
        vars_relatorio %>%
        srvyr::filter(RA == "Samambaia" ) %>%
        # Agrupar por faixas de idade e sexo
        srvyr::group_by(idade_faixas,sexo) %>%
        # Calcular os totais
        srvyr::summarise(n=survey_total(na.rm = T, vartype = "ci"))
      
      
      # Fazer o gráfico com a pirâmide
      piramide_grafico <- piramide %>%
        # Construir um plot com as idades no eixo x, as quantidades no eixo y,
        #  preenchimento com a variável sexo, e os intervalos de confiança
        # inferiores e superiores
        ggplot(aes(x=idade_faixas,y=n, fill=sexo, ymin=n_low,ymax=n_upp))+
        # Fazer o gráfico de barras para o sexo Feminino
        geom_bar(data = dplyr::filter(piramide, sexo == "Feminino"),
                 stat = "identity") +
        # Fazer o gráfico de barras para o sexo Masculino
        geom_bar(data = dplyr::filter(piramide, sexo == "Masculino"),
                 stat = "identity",
                 position = "identity",
                 # Negativar os valores para espelhar no eixo
                 mapping = aes(y = -n))+
        # Plotar os erros para o sexo Masculino, negativando os valores para espelhar o eixo
        geom_errorbar(data = dplyr::filter(piramide, sexo == "Masculino"),
                      mapping = aes(ymin = -n_low,ymax=-n_upp),
                      width=0,
                      color="black")+
        # Plotar os erros para o sexo Feminino
        geom_errorbar(data = dplyr::filter(piramide, sexo == "Feminino"),
                      width=0,
                      color="black")+
        # Inverter os eixos, fazendo com que o gráfico de colunas verticais fique
        # horizontal
        coord_flip() + 
        # Ajustar as configurações de escala
        scale_y_continuous(labels = function(x) format(abs(x), 
                                                       big.mark = ".",
                                                       scientific = FALSE,
                                                       decimal.mark=",")) +
        # Suprimir os nomes dos eixos
        labs(x="",y="") +
        # Nome do gráfico
        scale_fill_discrete(name = "Distribuição etária Samambaia")
        # Plotar gráfico
        piramide_grafico


###### iii)	Número de domicílios 
        #Plano Piloto e Samambaia
        amostra %>%
          # Filtrar Proprietários das casas
          srvyr::filter(E02==1) %>%
          # Ajustar nome das variáveis
          srvyr::mutate(A01ra=factor(case_when(A01ra==1~"Plano Piloto",
                                               A01ra==12~"Samambaia",
                                               TRUE~"Outras"))) %>%
          # Agrupar por cidade
          srvyr::group_by(A01ra) %>%
          # Calcular o total e o Percentual da população, com seu intervalo de confiança
          srvyr::summarise("População Total"=survey_total(vartype = "ci"),
                           # Calcular o percentual da população
                           pct=survey_mean(vartype = "ci"))
        
        #Distrito Federal 
        amostra %>%
          # Filtrar População DF pelo proprietário da casa
          srvyr::filter(E02==1) %>%
          # Criar contador
          srvyr::mutate(count=1) %>%
          # Calcular o total
          srvyr::summarise("População Total"=survey_total(count, vartype = "ci"))
        
      
      
###### iv)	Naturalidade dos residentes (Região de Nascimento use as grandes regiões 
#####       Norte, Nordeste, Centro-Oeste sem DF, Sudeste e Sul, crie uma categoria especial nascido DF.
        amostra %>%
          # Ajustar nome das variáveis
          srvyr::mutate(E142=factor(case_when(E142==11~"Norte",
                                              E142==12~"Norte",
                                              E142==13~"Norte",
                                              E142==14~"Norte",
                                              E142==15~"Norte",
                                              E142==16~"Norte",
                                              E142==17~"Centro-Oeste",
                                              E142==21~"Nordeste",
                                              E142==22~"Nordeste",
                                              E142==23~"Nordeste",
                                              E142==24~"Nordeste",
                                              E142==25~"Nordeste",
                                              E142==26~"Nordeste",
                                              E142==27~"Nordeste",
                                              E142==28~"Nordeste",
                                              E142==29~"Nordeste",
                                              E142==31~"Sudeste",
                                              E142==32~"Sudeste",
                                              E142==33~"Sudeste",
                                              E142==35~"Sudeste",
                                              E142==41~"Sul",
                                              E142==42~"Sul",
                                              E142==43~"Sul",
                                              E142==50~"Centro-Oeste",
                                              E142==51~"Centro-Oeste",
                                              E142==52~"Centro-Oeste",
                                              E142==53~"Centro-Oeste",
                                              E142==99~"Distrito Federal",
                                              E142==88~"Não Sabe",
                                              E142==0~"Outro País"))) %>%
          # Agrupar por cidade
          srvyr::group_by(E142) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Naturalidade"=survey_total(vartype = "ci"),
                           # Calcular o percentual 
                           pct=survey_mean(vartype = "ci"))
        

##### 1.2 Calcular a Renda Domiciliar do  Distrito Federal, Plano Piloto e Samambaia
        # Foi considerado como Renda Domiciliar os seguintes parâmetros:
        #   * G16 - Renda Primária;
        #   * G19 - Renda Sedundária;
        #   * G201 - Aposentadoria;
        #   * G202 - Pensão;
        #   * G203 - Outras Rendas;
        #   * G204 - Benefícios Sociais;
        options(dplyr.width = Inf)

                
###### i)	Renda domiciliar per capita (calcule também Quantis Q1, Q3 e o 
        # percentil 99, ou seja, o valor do 1% mais rico daquela RA)   
        
        #Calculos para o DF
        amostra %>% 
          srvyr::mutate(renda_prim=case_when(G16 == 77777~NA_real_,
                                      G16 == 88888~NA_real_,
                                      G16 == 99999~0,
                                      TRUE~as.numeric(G16))) %>%
          srvyr::mutate(renda_sec=case_when(G19 == 66666~0,
                                     G19 == 77777~NA_real_,
                                     G19 == 88888~NA_real_,
                                     G19 == 99999~0,
                                     TRUE ~as.numeric(G19))) %>%
          srvyr::mutate(aposentadoria=case_when(G201 == 66666~0,
                                         G201 == 77777~NA_real_,
                                         G201 == 88888~NA_real_,
                                         G201 == 99999~0,
                                         TRUE ~as.numeric(G201))) %>%
          srvyr::mutate(pensao=case_when(G202 == 66666~0,
                                  G202 == 77777~NA_real_,
                                  G202 == 88888~NA_real_,
                                  G202 == 99999~0,
                                  TRUE ~as.numeric(G202))) %>%
          srvyr:: mutate(outros=case_when(G203 == 66666~0,
                                  G203 == 77777~NA_real_,
                                  G203 == 88888~NA_real_,
                                  G203 == 99999~0,
                                  TRUE ~as.numeric(G203))) %>%
          srvyr:: mutate(beneficios=case_when(G204 == 66666~0,
                                          G204 == 77777~NA_real_,
                                          G204 == 88888~NA_real_,
                                          G204 == 99999~0,
                                          TRUE ~as.numeric(G204))) %>%
          srvyr::summarise("Renda Total DF"=survey_total(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Média DF"=survey_mean(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Desvio DF"=survey_sd(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Q1 (<25%) DF"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.25, na.rm=TRUE),
                    "Renda Q3 (<75%) DF"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.75, na.rm=TRUE),
                    "Renda Q99 (<99%) DF"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.99, na.rm=TRUE))
        

        # Calculos para o Plano Piloto
        amostra %>% filter(A01ra==1) %>% 
          srvyr::mutate(renda_prim=case_when(G16 == 77777~NA_real_,
                                             G16 == 88888~NA_real_,
                                             G16 == 99999~0,
                                             TRUE~as.numeric(G16))) %>%
          srvyr::mutate(renda_sec=case_when(G19 == 66666~0,
                                            G19 == 77777~NA_real_,
                                            G19 == 88888~NA_real_,
                                            G19 == 99999~0,
                                            TRUE ~as.numeric(G19))) %>%
          srvyr::mutate(aposentadoria=case_when(G201 == 66666~0,
                                                G201 == 77777~NA_real_,
                                                G201 == 88888~NA_real_,
                                                G201 == 99999~0,
                                                TRUE ~as.numeric(G201))) %>%
          srvyr::mutate(pensao=case_when(G202 == 66666~0,
                                         G202 == 77777~NA_real_,
                                         G202 == 88888~NA_real_,
                                         G202 == 99999~0,
                                         TRUE ~as.numeric(G202))) %>%
          srvyr:: mutate(outros=case_when(G203 == 66666~0,
                                          G203 == 77777~NA_real_,
                                          G203 == 88888~NA_real_,
                                          G203 == 99999~0,
                                          TRUE ~as.numeric(G203))) %>%
          srvyr:: mutate(beneficios=case_when(G204 == 66666~0,
                                              G204 == 77777~NA_real_,
                                              G204 == 88888~NA_real_,
                                              G204 == 99999~0,
                                              TRUE ~as.numeric(G204))) %>%
          summarise("Renda Total Plano"=survey_total(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Média Plano"=survey_mean(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Desvio Plano"=survey_sd(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Q1 (<25%) Plano"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.25, na.rm=TRUE),
                    "Renda Q3 (<75%) Plano"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.75, na.rm=TRUE),
                    "Renda Q99 (<99%) Plano"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.99, na.rm=TRUE))
        
        # Calculos para Samambaia
        amostra %>% filter(A01ra==12) %>% 
          srvyr::mutate(renda_prim=case_when(G16 == 77777~NA_real_,
                                             G16 == 88888~NA_real_,
                                             G16 == 99999~0,
                                             TRUE~as.numeric(G16))) %>%
          srvyr::mutate(renda_sec=case_when(G19 == 66666~0,
                                            G19 == 77777~NA_real_,
                                            G19 == 88888~NA_real_,
                                            G19 == 99999~0,
                                            TRUE ~as.numeric(G19))) %>%
          srvyr::mutate(aposentadoria=case_when(G201 == 66666~0,
                                                G201 == 77777~NA_real_,
                                                G201 == 88888~NA_real_,
                                                G201 == 99999~0,
                                                TRUE ~as.numeric(G201))) %>%
          srvyr::mutate(pensao=case_when(G202 == 66666~0,
                                         G202 == 77777~NA_real_,
                                         G202 == 88888~NA_real_,
                                         G202 == 99999~0,
                                         TRUE ~as.numeric(G202))) %>%
          srvyr:: mutate(outros=case_when(G203 == 66666~0,
                                          G203 == 77777~NA_real_,
                                          G203 == 88888~NA_real_,
                                          G203 == 99999~0,
                                          TRUE ~as.numeric(G203))) %>%
          srvyr:: mutate(beneficios=case_when(G204 == 66666~0,
                                              G204 == 77777~NA_real_,
                                              G204 == 88888~NA_real_,
                                              G204 == 99999~0,
                                              TRUE ~as.numeric(G204))) %>%
          summarise("Renda Total Samambaia"=survey_total(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Média Samambaia"=survey_mean(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Desvio Samambaia"=survey_sd(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, na.rm=TRUE),
                    "Renda Q1 (<25%) Samambaia"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.25, na.rm=TRUE),
                    "Renda Q3 (<75%) Samambaia"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.75, na.rm=TRUE),
                    "Renda Q99 (<99%) Samambaia"=survey_quantile(renda_prim+renda_sec+aposentadoria+pensao+outros+beneficios, 0.99, na.rm=TRUE))
        
                    
        # Agora os  cálculos apenas para a Renda Primária NÃO PER CAPITA
        # Calculos para o DF
        amostra %>% 
          mutate(renda_prim=case_when(G16 == 77777 ~ NA_real_,
                                      G16 == 88888 ~ NA_real_,
                                      G16 == 99999 ~ 0,
                                      TRUE ~as.numeric(G16))) %>%
          summarise("Renda Total DF"=survey_total(renda_prim, na.rm=TRUE),
                    "Renda Média DF"=survey_mean(renda_prim, na.rm=TRUE),
                    "Renda Desvio DF"=survey_sd(renda_prim, na.rm=TRUE),
                    "Renda Q1 (<25%) DF"=survey_quantile(renda_prim, 0.25, na.rm=TRUE),
                    "Renda Q3 (<75%) DF"=survey_quantile(renda_prim, 0.75, na.rm=TRUE),
                    "Renda Q99 (<99%) DF"=survey_quantile(renda_prim, 0.99, na.rm=TRUE)) #DF
        
        # Calculos para o Plano Piloto
        amostra %>% filter(A01ra==1) %>% 
          mutate(renda_prim=case_when(G16 == 77777 ~ NA_real_,
                                      G16 == 88888 ~ NA_real_,
                                      G16 == 99999 ~ 0,
                                      TRUE ~as.numeric(G16))) %>%
          summarise("Renda Total Plano"=survey_total(renda_prim, na.rm=TRUE),
                    "Renda Média Plano"=survey_mean(renda_prim, na.rm=TRUE),
                    "Renda Desvio Plano"=survey_sd(renda_prim, na.rm=TRUE),
                    "Renda Q1 (<25%) Plano"=survey_quantile(renda_prim, 0.25, na.rm=TRUE),
                    "Renda Q3 (<75%) Plano"=survey_quantile(renda_prim, 0.75, na.rm=TRUE),
                    "Renda Q99 (<99%) Plano"=survey_quantile(renda_prim, 0.99, na.rm=TRUE)) #PP
        
        amostra %>% filter(A01ra==12) %>% # Calculos para Samambaia
          mutate(renda_prim=case_when(G16 == 77777 ~ NA_real_,
                                      G16 == 88888 ~ NA_real_,
                                      G16 == 99999 ~ 0,
                                      TRUE ~as.numeric(G16))) %>%
          summarise("Renda Total Samambaia"=survey_total(renda_prim, na.rm=TRUE),
                    "Renda Média Samambaia"=survey_mean(renda_prim, na.rm=TRUE),
                    "Renda Desvio Samambaia"=survey_sd(renda_prim, na.rm=TRUE),
                    "Renda Q1 (<25%) Samambaia"=survey_quantile(renda_prim, 0.25, na.rm=TRUE),
                    "Renda Q3 (<75%) Samambaia"=survey_quantile(renda_prim, 0.75, na.rm=TRUE),
                    "Renda Q99 (<99%) Samambaia"=survey_quantile(renda_prim, 0.99, na.rm=TRUE))
        
        # Escolaridade das pessoa de  25 anos ou mais
        amostra %>% # Calculos para o DF
          srvyr::filter(idade_calculada >= 25) %>%
          mutate(escolaridade=case_when(F11 == 1 ~ "alfabetização",
                                        F11 == 2 ~ "fundamental",
                                        F11 == 3 ~ "fundamental",
                                        F11 == 4 ~ "medio",
                                        F11 == 5 ~ "fundamental",
                                        F11 == 6 ~ "medio",
                                        F11 == 7 ~ "superior",
                                        F11 == 8 ~ "especialização",
                                        F11 == 9 ~ "mestrado",
                                        F11 == 10 ~ "dotorado",
                                        TRUE ~ NA_character_)) %>%
          group_by(escolaridade) %>%
          summarise("Escolaridade DF"=survey_total(na.rm=TRUE))
        
        amostra %>% # Calculos para o Plano Piloto
          filter(A01ra == 1) %>% 
          filter(idade_calculada >= 25) %>%
          mutate(escolaridade=case_when(F11 == 1 ~ "alfabetização",
                                        F11 == 2 ~ "fundamental",
                                        F11 == 3 ~ "fundamental",
                                        F11 == 4 ~ "medio",
                                        F11 == 5 ~ "fundamental",
                                        F11 == 6 ~ "medio",
                                        F11 == 7 ~ "superior",
                                        F11 == 8 ~ "especialização",
                                        F11 == 9 ~ "mestrado",
                                        F11 == 10 ~ "dotorado",
                                        TRUE ~ NA_character_)) %>%
          group_by(escolaridade) %>%
          summarise("Escolaridade Plano"=survey_total(na.rm=TRUE))
        
        amostra %>% # Calculos para Samambaia
          filter(A01ra == 12) %>% 
          filter(idade_calculada >= 25) %>%
          mutate(escolaridade=case_when(F11 == 1 ~ "alfabetização",
                                        F11 == 2 ~ "fundamental",
                                        F11 == 3 ~ "fundamental",
                                        F11 == 4 ~ "medio",
                                        F11 == 5 ~ "fundamental",
                                        F11 == 6 ~ "medio",
                                        F11 == 7 ~ "superior",
                                        F11 == 8 ~ "especialização",
                                        F11 == 9 ~ "mestrado",
                                        F11 == 10 ~ "dotorado",
                                        TRUE ~ NA_character_)) %>%
          group_by(escolaridade) %>%
          summarise("Escolaridade Samambaia"=survey_total(na.rm=TRUE))
        
###### iv)	Modo de transporte para o trabalho (apenas uma variável qualitativa
        
        # Como existem pessoas com mais de uma opção de transporte para o trabalhoa,
        # optamos por agrupar as diferentes formas numa matrix de possibilidades.
        amostra %>% # Cálculos para o DF
          mutate(Onibus=factor(case_when(G141 == 1 ~ "onibus",
                                         TRUE ~ NA_character_))) %>%
          mutate(Automovel=factor(case_when(G142 == 1 ~ "automovel",
                                            TRUE ~ NA_character_))) %>%
          mutate(Utilitario=factor(case_when(G143 == 1 ~ "utilitario",
                                             TRUE ~ NA_character_))) %>%
          mutate(Metro=factor(case_when(G144 == 1 ~ "metro",
                                        TRUE ~ NA_character_))) %>%
          mutate(Moto=factor(case_when(G145 == 1 ~ "moto",
                                       TRUE ~ NA_character_))) %>%
          mutate(Bike=factor(case_when(G146 == 1 ~ "bike",
                                       TRUE ~ NA_character_))) %>%
          mutate(Andando=factor(case_when(G144 == 1 ~ "andando",
                                          TRUE ~ NA_character_))) %>%
          group_by(Onibus, Automovel,Utilitario, Metro, Moto, Bike, Andando) %>%
          summarise("Quantidade DF"=survey_total(na.rm=TRUE)) %>% print(n=100)
        
        amostra %>% # Cálculos para o Plano Piloto
          filter(A01ra == 1) %>% 
          mutate(Onibus=factor(case_when(G141 == 1 ~ "onibus",
                                         TRUE ~ NA_character_))) %>%
          mutate(Automovel=factor(case_when(G142 == 1 ~ "automovel",
                                            TRUE ~ NA_character_))) %>%
          mutate(Utilitario=factor(case_when(G143 == 1 ~ "utilitario",
                                             TRUE ~ NA_character_))) %>%
          mutate(Metro=factor(case_when(G144 == 1 ~ "metro",
                                        TRUE ~ NA_character_))) %>%
          mutate(Moto=factor(case_when(G145 == 1 ~ "moto",
                                       TRUE ~ NA_character_))) %>%
          mutate(Bike=factor(case_when(G146 == 1 ~ "bike",
                                       TRUE ~ NA_character_))) %>%
          mutate(Andando=factor(case_when(G144 == 1 ~ "andando",
                                          TRUE ~ NA_character_))) %>%
          group_by(Onibus, Automovel,Utilitario, Metro, Moto, Bike, Andando) %>%
          summarise("Qtd. Plano"=survey_total(na.rm=TRUE)) %>% print(n=100)
        
        amostra %>% # Cálculos para Samambaia
          filter(A01ra == 12) %>% 
          mutate(Onibus=factor(case_when(G141 == 1 ~ "onibus",
                                         TRUE ~ NA_character_))) %>%
          mutate(Automovel=factor(case_when(G142 == 1 ~ "automovel",
                                            TRUE ~ NA_character_))) %>%
          mutate(Utilitario=factor(case_when(G143 == 1 ~ "utilitario",
                                             TRUE ~ NA_character_))) %>%
          mutate(Metro=factor(case_when(G144 == 1 ~ "metro",
                                        TRUE ~ NA_character_))) %>%
          mutate(Moto=factor(case_when(G145 == 1 ~ "moto",
                                       TRUE ~ NA_character_))) %>%
          mutate(Bike=factor(case_when(G146 == 1 ~ "bike",
                                       TRUE ~ NA_character_))) %>%
          mutate(Andando=factor(case_when(G144 == 1 ~ "andando",
                                          TRUE ~ NA_character_))) %>%
          group_by(Onibus, Automovel,Utilitario, Metro, Moto, Bike, Andando) %>%
          summarise("Qtd. Samambaia"=survey_total(na.rm=TRUE)) %>% print(n=100)
        
###### v)	Tempo gasto de deslocamento ao trabalho (veja que a variável está em classes, 
        # podemos colocar o ponto médio do intervalo para o cálculo de medidas de posição).  
        
        # Calculos para o DF
        amostra %>% 
          #filter(A01ra == 12) %>% 
          mutate(deslocamento=case_when(G15 == 1 ~ 7L,
                                        G15 == 2 ~ 22L,
                                        G15 == 3 ~ 37L,
                                        G15 == 4 ~ 52L,
                                        G15 == 5 ~ 75L,
                                        G15 == 6 ~ 97L,
                                        G15 == 7 ~ 112L,
                                        G15 == 8 ~ 120L,
                                        TRUE ~ NA_integer_)) %>%
          summarise("Tempo Médio Trabalho DF"=survey_mean(deslocamento,na.rm=TRUE),
                    "Tempo Mediana Trabalho DF"=survey_median(deslocamento,na.rm=TRUE),
                    "Tempo Variância Trabalho DF"=survey_var(deslocamento,na.rm=TRUE),
                    "Tempo Desvio Padrão Trabalho DF"=survey_sd(deslocamento,na.rm=TRUE),
                    "Tempo Q1 (<25%) Trabalho DF"=survey_quantile(deslocamento, 0.25, na.rm=TRUE),
                    "Tempo Q3 (<75%) Trabalho DF"=survey_quantile(deslocamento, 0.75, na.rm=TRUE),
                    "Tempo Q99 (<99%) Trabalho DF"=survey_quantile(deslocamento, 0.99, na.rm=TRUE))
        
        # Calculos para o Plano Piloto
        amostra %>% 
          filter(A01ra == 1) %>% 
          mutate(deslocamento=case_when(G15 == 1 ~ 7L,
                                        G15 == 2 ~ 22L,
                                        G15 == 3 ~ 37L,
                                        G15 == 4 ~ 52L,
                                        G15 == 5 ~ 75L,
                                        G15 == 6 ~ 97L,
                                        G15 == 7 ~ 112L,
                                        G15 == 8 ~ 120L,
                                        TRUE ~ NA_integer_)) %>%
          summarise("Tempo Médio Trabalho Plano"=survey_mean(deslocamento,na.rm=TRUE),
                    "Tempo Mediana Trabalho Plano"=survey_median(deslocamento,na.rm=TRUE),
                    "Tempo Variância Trabalho Plano"=survey_var(deslocamento,na.rm=TRUE),
                    "Tempo Desvio Padrão Trabalho Plano"=survey_sd(deslocamento,na.rm=TRUE),
                    "Tempo Q1 (<25%) Trabalho Plano"=survey_quantile(deslocamento, 0.25, na.rm=TRUE),
                    "Tempo Q3 (<75%) Trabalho Plano"=survey_quantile(deslocamento, 0.75, na.rm=TRUE),
                    "Tempo Q99 (<99%) Trabalho Plano"=survey_quantile(deslocamento, 0.99, na.rm=TRUE))
        
        # Calculos para Samambaia
        amostra %>% 
          filter(A01ra == 12) %>% 
          mutate(deslocamento=case_when(G15 == 1 ~ 7L,
                                        G15 == 2 ~ 22L,
                                        G15 == 3 ~ 37L,
                                        G15 == 4 ~ 52L,
                                        G15 == 5 ~ 75L,
                                        G15 == 6 ~ 97L,
                                        G15 == 7 ~ 112L,
                                        G15 == 8 ~ 120L,
                                        TRUE ~ NA_integer_)) %>%
          summarise("Tempo Médio Trabalho Samambaia"=survey_mean(deslocamento,na.rm=TRUE),
                    "Tempo Mediana Trabalho Samambaia"=survey_median(deslocamento,na.rm=TRUE),
                    "Tempo Variância Trabalho Samambaia"=survey_var(deslocamento,na.rm=TRUE),
                    "Tempo Desvio Padrão Trabalho Samambaia"=survey_sd(deslocamento,na.rm=TRUE),
                    "Tempo Q1 (<25%) Trabalho Samambaia"=survey_quantile(deslocamento, 0.25, na.rm=TRUE),
                    "Tempo Q3 (<75%) Trabalho Samambaia"=survey_quantile(deslocamento, 0.75, na.rm=TRUE),
                    "Tempo Q99 (<99%) Trabalho Samambaia"=survey_quantile(deslocamento, 0.99, na.rm=TRUE))
        
###### vi) Número de automóveis no domicilio   
        # Cálculos para DF
        amostra %>% 
          filter(E02 == 1) %>% 
          mutate(carro=case_when(C011 == 88888 ~NA_real_,
                                 TRUE ~ as.numeric(C011))) %>%
          summarise("Total da Carros DF"=survey_total(carro, na.rm=TRUE),
                    "Média de Carros DF"=survey_mean(carro, na.rm=TRUE),
                    "Desvio de Carros DF"=survey_sd(carro, na.rm=TRUE),
                    "Carros Q1 (<25%) DF"=survey_quantile(carro, 0.25, na.rm=TRUE),
                    "Carros Q3 (<75%) DF"=survey_quantile(carro, 0.75, na.rm=TRUE),
                    "Carros Q99 (<99%) DF"=survey_quantile(carro, 0.99, na.rm=TRUE))
        
        amostra %>% # Cálculos para o Plano Piloto
          filter(E02 == 1 & A01ra == 1) %>% 
          mutate(carro=case_when(C011 == 88888 ~NA_real_,
                                 TRUE ~ as.numeric(C011))) %>%
          summarise("Total da Carros Plano"=survey_total(carro, na.rm=TRUE),
                    "Média de Carros Plano"=survey_mean(carro, na.rm=TRUE),
                    "Desvio de Carros Plano"=survey_sd(carro, na.rm=TRUE),
                    "Carros Q1 (<25%) Plano"=survey_quantile(carro, 0.25, na.rm=TRUE),
                    "Carros Q3 (<75%) Plano"=survey_quantile(carro, 0.75, na.rm=TRUE),
                    "Carros Q99 (<99%) Plano"=survey_quantile(carro, 0.99, na.rm=TRUE))
        
        amostra %>% # Cálculos para Samambaia
          filter(E02 == 1 & A01ra == 12) %>% 
          mutate(carro=case_when(C011 == 88888 ~NA_real_,
                                 TRUE ~ as.numeric(C011))) %>%
          summarise("Total da Carros Samambaia"=survey_total(carro, na.rm=TRUE),
                    "Média de Carros Samambaia"=survey_mean(carro, na.rm=TRUE),
                    "Desvio de Carros Samambaia"=survey_sd(carro, na.rm=TRUE),
                    "Carros Q1 (<25%) Samambaia"=survey_quantile(carro, 0.25, na.rm=TRUE),
                    "Carros Q3 (<75%) Samambaia"=survey_quantile(carro, 0.75, na.rm=TRUE),
                    "Carros Q99 (<99%) Samambaia"=survey_quantile(carro, 0.99, na.rm=TRUE))
        
###### vii) Número de pessoas no domicilio      
        amostra %>% filter(E02==1) %>% #Cálculo para o Distrito Federal
          summarise("Média p/ Domicílio DF"=survey_mean(A01nPessoas,na.rm=TRUE),
                    "Mediana p/ Domicílio DF"=survey_median(A01nPessoas,na.rm=TRUE),
                    "Variancia p/ Domicílio DF"=survey_mean(A01nPessoas,na.rm=TRUE),
                    "Desvio Padrão p/ Domicílio DF"=survey_sd(A01nPessoas,na.rm=TRUE),
                    "Q1 (<25%) p/ Domicílio DF"=survey_quantile(A01nPessoas,0.25,na.rm=TRUE),
                    "Q3 (<75%) p/ Domicílio DF"=survey_quantile(A01nPessoas,0.75,na.rm=TRUE),
                    "Q3 (<75%) p/ Domicílio DF"=survey_quantile(A01nPessoas,0.99,na.rm=TRUE))
        
        amostra %>% filter(E02==1 & A01ra==1) %>%  # Cálculo para o Plano Piloto
          summarise("Média p/ Domicílio Plano"=survey_mean(A01nPessoas,na.rm=TRUE),
                    "Mediana p/ Domicílio Plano"=survey_median(A01nPessoas,na.rm=TRUE),
                    "Variancia p/ Domicílio Plano"=survey_mean(A01nPessoas,na.rm=TRUE),
                    "Desvio Padrão p/ Domicílio Plano"=survey_sd(A01nPessoas,na.rm=TRUE),
                    "Q1 (<25%) p/ Domicílio Plano"=survey_quantile(A01nPessoas,0.25,na.rm=TRUE),
                    "Q3 (<75%) p/ Domicílio Plano"=survey_quantile(A01nPessoas,0.75,na.rm=TRUE),
                    "Q3 (<75%) p/ Domicílio Plano"=survey_quantile(A01nPessoas,0.99,na.rm=TRUE))
        
        amostra %>% filter(E02==1 & A01ra==12) %>% # Cálculo para Samambaia
          summarise("Média p/ Domicílio Samambaia"=survey_mean(A01nPessoas,na.rm=TRUE),
                    "Mediana p/ Domicílio Samambaia"=survey_median(A01nPessoas,na.rm=TRUE),
                    "Variancia p/ Domicílio Samambaia"=survey_mean(A01nPessoas,na.rm=TRUE),
                    "Desvio Padrão p/ Domicílio Samambaia"=survey_sd(A01nPessoas,na.rm=TRUE),
                    "Q1 (<25%) p/ Domicílio Samambaia"=survey_quantile(A01nPessoas,0.25,na.rm=TRUE),
                    "Q3 (<75%) p/ Domicílio Samambaia"=survey_quantile(A01nPessoas,0.75,na.rm=TRUE),
                    "Q3 (<75%) p/ Domicílio Samambaia"=survey_quantile(A01nPessoas,0.99,na.rm=TRUE))
  
###### viii)Número de dormitórios no domicílio               
        amostra %>% filter(E02==1) %>% #Cálculo para o Distrito Federal
          summarise("Média p/ Cômodo DF"=survey_mean(B12,na.rm=TRUE),
                    "Mediana p/ Cômodo DF"=survey_median(B12,na.rm=TRUE),
                    "Variancia p/ Cômodo DF"=survey_mean(B12,na.rm=TRUE),
                    "Desvio Padrão p/ Cômodo DF"=survey_sd(B12,na.rm=TRUE),
                    "Q1 (<25%) p/ Cômodo DF"=survey_quantile(B12,0.25,na.rm=TRUE),
                    "Q3 (<75%) p/ Cômodo DF"=survey_quantile(B12,0.75,na.rm=TRUE),
                    "Q3 (<75%) p/ Cômodo DF"=survey_quantile(B12,0.99,na.rm=TRUE))
        
        amostra %>% filter(E02==1 & A01ra==1) %>%  # Cálculo para o Plano Piloto
          summarise("Média p/ Cômodo Plano"=survey_mean(B12,na.rm=TRUE),
                    "Mediana p/ Cômodo Plano"=survey_median(B12,na.rm=TRUE),
                    "Variancia p/ Cômodo Plano"=survey_mean(B12,na.rm=TRUE),
                    "Desvio Padrão p/ Cômodo Plano"=survey_sd(B12,na.rm=TRUE),
                    "Q1 (<25%) p/ Cômodo Plano"=survey_quantile(B12,0.25,na.rm=TRUE),
                    "Q3 (<75%) p/ Cômodo Plano"=survey_quantile(B12,0.75,na.rm=TRUE),
                    "Q3 (<75%) p/ Cômodo Plano"=survey_quantile(B12,0.99,na.rm=TRUE))
        
        amostra %>% filter(E02==1 & A01ra==12) %>% # Cálculo para Samambaia
          summarise("Média p/ Cômodo Samambaia"=survey_mean(B12,na.rm=TRUE),
                    "Mediana p/ Cômodo Samambaia"=survey_median(B12,na.rm=TRUE),
                    "Variancia p/ Cômodo Samambaia"=survey_mean(B12,na.rm=TRUE),
                    "Desvio Padrão p/ Cômodo Samambaia"=survey_sd(B12,na.rm=TRUE),
                    "Q1 (<25%) p/ Cômodo Samambaia"=survey_quantile(B12,0.25,na.rm=TRUE),
                    "Q3 (<75%) p/ Cômodo Samambaia"=survey_quantile(B12,0.75,na.rm=TRUE),
                    "Q3 (<75%) p/ Cômodo Samambaia"=survey_quantile(B12,0.99,na.rm=TRUE))
        
        
##### 1.3. Faça um histograma (com barras e alisado) para as variáveis renda domiciliar 
    # per capita e número de automóveis no domicílio para a RA X´ com o Plano Piloto e o Distrito Federal
  



