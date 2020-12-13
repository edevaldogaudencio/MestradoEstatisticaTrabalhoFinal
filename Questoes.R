######Carregar pacotes necessários
library(readr)
library(survey)
library(dplyr)
library(srvyr)
library(Hmisc)
library(lubridate) 
library(tidyverse)
library(convey)
library(ineq)


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
      
      # Criar variável para escolaridade
      pdad_2018_moradores <- pdad_2018_moradores %>%
        # Fazer a escolaridade de quem não estuda
        dplyr::mutate(escolaridade_nao_estuda=case_when(F02==4~"Sem escolaridade",
                                                        F11==1~"Sem escolaridade",
                                                        F11==2&F12==10~"Sem escolaridade",
                                                        F11==3&F12==10~"Sem escolaridade",
                                                        F11==2&F12 %in% c(1:7)~"Fundamental incompleto",
                                                        F11==3&F12 %in% c(1:8)~"Fundamental incompleto",
                                                        F11==5&F13==2~"Fundamental incompleto",
                                                        F11==2&F12==8~"Fundamental completo",
                                                        F11==3&F12==9~"Fundamental completo",
                                                        F11==5&F13==1~"Fundamental completo",
                                                        F11==4&F12==10~"Fundamental completo",
                                                        F11==4&F12 %in% c(1:2)~"Médio incompleto",
                                                        F11==6&F13==2~"Médio incompleto",
                                                        F11==4&F12 %in% c(3,4)~"Médio completo",
                                                        F11==6&F13==1~"Médio completo",
                                                        F11==7&F13==2~"Superior incompleto",
                                                        F11==7&F13==1~"Superior completo",
                                                        F11 %in% c(8:10)~"Superior completo",
                                                        TRUE~NA_character_),
                      # Fazer a escolaridade de quem estuda
                      escolaridadet=case_when(F07 %in% c(1,2,3)~"Sem escolaridade",
                                              F07 %in% c(4,7)~"Fundamental incompleto",
                                              F07 %in% c(5,6,8)~"Médio incompleto",
                                              F07==9~"Superior incompleto",
                                              F07 %in% c(10:12)~"Superior completo",
                                              TRUE~escolaridade_nao_estuda),
                      # Ajustar a escolaridade de quem já concluiu outro curso superior
                      escolaridadet=case_when(F09==1&F10 %in% c(1:4)~"Superior completo",
                                              TRUE~escolaridadet),
                      # Ajustar o fator ordenado
                      escolaridade=factor(ordered(case_when(escolaridadet=="Sem escolaridade"~1,
                                                            escolaridadet=="Fundamental incompleto"~2,
                                                            escolaridadet=="Fundamental completo"~3,
                                                            escolaridadet=="Médio incompleto"~4,
                                                            escolaridadet=="Médio completo"~5,
                                                            escolaridadet=="Superior incompleto"~6,
                                                            escolaridadet=="Superior completo"~7),
                                                  levels=c(1:7),
                                                  labels=c("Sem escolaridade",
                                                           "Fundamental incompleto",
                                                           "Fundamental completo",
                                                           "Médio incompleto",
                                                           "Médio completo",
                                                           "Superior incompleto",
                                                           "Superior completo"))))
      # Criando variável idoso
      pdad_2018_moradores <- pdad_2018_moradores %>%
        dplyr::mutate(MelhorIdade=case_when(idade_calculada>=60~"1",
                                          idade_calculada<60~"0",
                                                        TRUE~NA_character_),)
      
    
      
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
        dplyr::mutate(POS_ESTRATO=as.character(POS_ESTRATO)) %>% 
        dplyr::mutate(
          RA_nome=factor(case_when(
            A01ra==1~"Plano Piloto",
            A01ra==2~"Gama",
            A01ra==3~"Taguatinga",
            A01ra==4~"Brazlândia",
            A01ra==5~"Sobradinho",
            A01ra==6~"Planaltina",
            A01ra==7~"Paranoá",
            A01ra==8~"Núcleo Bandeirante",
            A01ra==9~"Ceilândia",
            A01ra==10~"Guará",
            A01ra==11~"Cruzeiro",
            A01ra==12~"Samambaia",
            A01ra==13~"Santa Maria",
            A01ra==14~"São Sebastião",
            A01ra==15~"Recanto das Emas",
            A01ra==16~"Lago Sul",
            A01ra==17~"Riacho Fundo",
            A01ra==18~"Lago Norte",
            A01ra==19~"Candangolândia",
            A01ra==20~"Águas Claras",
            A01ra==21~"Riacho Fundo II",
            A01ra==22~"Sudoeste/Octogonal",
            A01ra==23~"Varjão",
            A01ra==24~"Park Way",
            A01ra==25~"Scia/Estrutural",
            A01ra==26~"Sobradinho II",
            A01ra==27~"Jardim Botânico",
            A01ra==28~"Itapoã",
            A01ra==29~"SIA",
            A01ra==30~"Vicente Pires",
            A01ra==31~"Fercal")),
          
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
          # Criar variável de sexo
          sexo=factor(case_when(E03==1~"Masculino",
                                E03==2~"Feminino")),
          
          Regiao=factor(case_when(E142 %in% c(11:17)~"Norte",
                                  E142 %in% c(21:29)~"Nordeste",
                                  E142 %in% c(31:35)~"Sudeste",
                                  E142 %in% c(41:43)~"Sul",
                                  E142 %in% c(50:52)~"Centro-oeste",
                                  E13==1~"DF")),
        
          renda_trab=case_when(G16 %in% c(77777,88888,99999)~NA_real_,
                               TRUE~as.numeric(G16)),
          
          onibus_trab=case_when(G141==1~"Sim",
                                G141==2~"Não",
                                G141==88~"Não sabe"),
          
          
          transp_trab=case_when(G141==1|G144==1~"Sim",
                                G141==2&G144==2~"Não",
                                G141==88&G144==88~"Não sabe"),
          
          tempo_trab=case_when(G15==1~"Até 15 minutos",
                               G15==2~"Mais de 0:15 até 0:30",
                               G15==3~"Mais de 0:30 até 0:45",
                               G15==4~"Mais de 0:45 até 1:00",
                               G15==5~"Mais de 1:00 até 1:30",
                               G15==6~"Mais de 1:30 até 1:45",
                               G15==7~"Mais de 1:45 até 2:00",
                               G15==8~"Mais de 2:00",
                               G15==88~"Não sabe"),
          
          tempo_trab_c=case_when(G15==1~7.5,
                                 G15==2~22.5,
                                 G15==3~37.5,
                                 G15==4~52.5,
                                 G15==5~75,
                                 G15==6~97.5,
                                 G15==5~112.5,
                                 G15==5~120),
          
          automoveis=case_when(C011==88888~NA_real_,
                               TRUE~as.numeric(C011)),
          
          tempo_afazeres=case_when(G18 %in% c(88888,99999)~NA_real_,
                                   TRUE~as.numeric(G18)),
          
          idoso=case_when(idade_calculada>=60~1,
                          TRUE~0),
          
          pessoa_dorm=A01nPessoas/B12,
          
          internet=factor(case_when(C041==1|C042==1|C043==1|C044==1~"Sim",
                                    C041==88&C042==88&C043==88&C044==88~NA_character_,
                                    TRUE~"Não")),
          
          set_educacao=factor(case_when(G06==16~"Sim",
                                        G06 %in% c(1:15,17:21)~"Não")),
          
          horas_trab=case_when(G17%in%c(88888,99999)~NA_real_,
                               TRUE~as.numeric(G17)),
          
          tempo_uso=horas_trab+(tempo_trab_c/60*10)+tempo_afazeres,
          
          freq_escola=factor(case_when(F02==1~"Pública",
                                       F02==2~"Privada",
                                       TRUE~"Não estuda")),
          
          nivel_escola=factor(case_when(F07%in%c(1,2)~"Creche/Educação Infantil",
                                        F07==4~"Ensino Fundamental",
                                        F07%in%c(5,6)~"Ensino Médio",
                                        F07 %in% c(3,7,8)~"AJA e EJA",
                                        F07 %in% c(9:12)~"Ensino Superior")),
          
          local_estudo=factor(case_when(
            F04==1~"Plano Piloto",
            F04==2~"Gama",
            F04==3~"Taguatinga",
            F04==4~"Brazlândia",
            F04==5~"Sobradinho",
            F04==6~"Planaltina",
            F04==7~"Paranoá",
            F04==8~"Núcleo Bandeirante",
            F04==9~"Ceilândia",
            F04==10~"Guará",
            F04==11~"Cruzeiro",
            F04==12~"Samambaia",
            F04==13~"Santa Maria",
            F04==14~"São Sebastião",
            F04==15~"Recanto das Emas",
            F04==16~"Lago Sul",
            F04==17~"Riacho Fundo",
            F04==18~"Lago Norte",
            F04==19~"Candangolândia",
            F04==20~"Águas Claras",
            F04==21~"Riacho Fundo II",
            F04==22~"Sudoeste/Octogonal",
            F04==23~"Varjão",
            F04==24~"Park Way",
            F04==25~"Scia/Estrutural",
            F04==26~"Sobradinho II",
            F04==27~"Jardim Botânico",
            F04==28~"Itapoã",
            F04==29~"SIA",
            F04==30~"Vicente Pires",
            F04==31~"Fercal",
            F04 %in% 32:45~"Fora do DF")),
          
          desloc_escola=factor(case_when(F05==1~"Ônibus",
                                         F05==2~"Transporte escolar público",
                                         F05==3~"Transporte escolar privado",
                                         F05==4~"Automóvel",
                                         F05==5~"Utilitário",
                                         F05==6~"Metrô",
                                         F05==7~"Motocicleta",
                                         F05==8~"Bicicleta",
                                         F05==9~"A pé",
                                         F05==10~"Outros")),
          
          transp_escola=factor(case_when(F05==1|F05==2|F05==3|F05==6~"Sim",
                                         TRUE~"Não")),
          
          pos_dom=case_when(E02==1~"Responsável",
                            E02 %in% c(2,3)~"Cônjuge",
                            TRUE~"Outro"),
          
          crianca_estuda=case_when(idade_calculada<12&F02%in%c(1,2)~1,
                                   TRUE~0),
          
          count=1) %>% 
        
        dplyr::group_by(A01nFicha) %>% 
        dplyr::mutate(idoso=sum(idoso),
                      crianca_estuda=sum(crianca_estuda)) %>% 
        dplyr::ungroup()
        
      
      # Armazenar informação em um objeto
      renda_domiciliar <- pdad_2018_moradores %>%
        # Vamos mudar para ausente os valores das variáveis G16,G19,G201 até G204
        # com códigos 77777 ou 88888.
        # Vamos também mudar para 0 quando os valores que não se aplicarem
        # ou não forem observados rendimentos
        dplyr::mutate_at(vars(G16,G19,G201:G204), # Variáveis a serem alteradas
                         # Função a ser aplicada
                         list(M=~case_when(. %in% c(77777,88888)~NA_real_,
                                           . %in% c(66666,99999)~0,
                                           TRUE~as.numeric(.)))) %>%
        # Selecionar apenas as variáveis de interesse
        dplyr::select(A01nFicha,E02,G16,G19,G201:G204,G16_M:G204_M) %>%
        # Somar as variáveis modificadas para construir a renda individual
        dplyr::mutate(renda_individual=rowSums(.[,c("G16_M","G19_M",
                                                    "G201_M","G202_M",
                                                    "G203_M","G204_M")],na.rm = F)) %>%
        # Desconsiderar os empregados domesticos moradores e seus parentes
        dplyr::filter(!E02 %in% c(16,17,18)) %>%
        # Agrupar por domicílio
        dplyr::group_by(A01nFicha) %>%
        # Somar os valores por domicílios
        dplyr::summarise(renda_dom=sum(renda_individual, na.rm = F),
                         # Construir o número de pessoas no domicílio, por esse critério de rendiment0
                         pessoas=n(),
                         # Calcular a renda domiciliar per capita
                         renda_dom_pc=renda_dom/pessoas)
      # Juntar as bases
      pdad <- pdad %>% dplyr::left_join(renda_domiciliar)
      
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
          srvyr::mutate(E142=factor(case_when(E142 %in% c(11:17)~"Norte",
                                             E142 %in% c(21:29)~"Nordeste",
                                             E142 %in% c(31:35)~"Sudeste",
                                             E142 %in% c(41:43)~"Sul",
                                             E142 %in% c(50:52)~"Centro-oeste",
                                             E142==88~"Z_Não Sabe",
                                             E142==0~"Z_Outro País",
                                             E13==1~"DF"))) %>%
        # Agrupar por região
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
          srvyr::filter(E02==1) %>% 
          srvyr::summarise("Renda domiciliar per capita DF"=survey_mean(renda_dom_pc,na.rm=T),
                           "Variância DF"=survey_var(renda_dom_pc,na.rm=T),
                           "Mediana DF"=survey_median(renda_dom_pc,na.rm=T),
                           "Desvio Padrão DF"=survey_sd(renda_dom_pc,na.rm=T),
                           "Renda Q1 (<25%) DF"=survey_quantile(renda_dom_pc, 0.25, na.rm=TRUE),
                           "Renda Q3 (<75%) DF"=survey_quantile(renda_dom_pc, 0.75, na.rm=TRUE),
                           "Renda Q99 (<99%) DF"=survey_quantile(renda_dom_pc, 0.99, na.rm=TRUE))
                           

        # Calculos para o Plano Piloto
        amostra %>% 
          srvyr::filter(E02==1&A01ra==1) %>% 
          srvyr::summarise("Renda domiciliar per capita Plano"=survey_mean(renda_dom_pc,na.rm=T),
                           "Variância Plano"=survey_var(renda_dom_pc,na.rm=T),
                           "Mediana Plano"=survey_median(renda_dom_pc,na.rm=T),
                           "Desvio Padrão Plano"=survey_sd(renda_dom_pc,na.rm=T),
                           "Renda Q1 (<25%) Plano"=survey_quantile(renda_dom_pc, 0.25, na.rm=TRUE),
                           "Renda Q3 (<75%) Plano"=survey_quantile(renda_dom_pc, 0.75, na.rm=TRUE),
                           "Renda Q99 (<99%) Plano"=survey_quantile(renda_dom_pc, 0.99, na.rm=TRUE))
        
  
        # Calculos para Samambaia
        amostra %>% 
          srvyr::filter(E02==1&A01ra==12) %>% 
          srvyr::summarise("Renda domiciliar per capita Samambaia"=survey_mean(renda_dom_pc,na.rm=T),
                           "Variância Samambaia"=survey_var(renda_dom_pc,na.rm=T),
                           "Mediana Samambaia"=survey_median(renda_dom_pc,na.rm=T),
                           "Desvio Padrão Samambaia"=survey_sd(renda_dom_pc,na.rm=T),
                           "Renda Q1 (<25%) Samambaia"=survey_quantile(renda_dom_pc, 0.25, na.rm=TRUE),
                           "Renda Q3 (<75%) Samambaia"=survey_quantile(renda_dom_pc, 0.75, na.rm=TRUE),
                           "Renda Q99 (<99%) Samambaia"=survey_quantile(renda_dom_pc, 0.99, na.rm=TRUE))
        

                    
        # Agora os  cálculos apenas para a Renda Primária NÃO PER CAPITA
        # Calculos para o DF
        amostra %>% 
          mutate(renda_prim=case_when(G16 == 77777 ~ NA_real_,
                                      G16 == 88888 ~ NA_real_,
                                      G16 == 99999 ~ NA_real_,
                                      TRUE ~as.numeric(G16))) %>%
          srvyr::summarise("Renda Média Trabalho Principal DF"=survey_mean(renda_prim,na.rm=T),
                           "Variância DF"=survey_var(renda_prim,na.rm=T),
                           "Mediana DF"=survey_median(renda_prim,na.rm=T),
                           "Desvio Padrão DF"=survey_sd(renda_prim,na.rm=T),
                           "Renda Q1 (<25%) DF"=survey_quantile(renda_prim, 0.25, na.rm=TRUE),
                           "Renda Q3 (<75%) DF"=survey_quantile(renda_prim, 0.75, na.rm=TRUE),
                           "Renda Q99 (<99%) DF"=survey_quantile(renda_prim, 0.99, na.rm=TRUE))
       
        
        # Calculos para o Plano Piloto
        amostra %>% filter(A01ra==1) %>% 
          mutate(renda_prim=case_when(G16 == 77777 ~ NA_real_,
                                      G16 == 88888 ~ NA_real_,
                                      G16 == 99999 ~ NA_real_,
                                      TRUE ~as.numeric(G16))) %>%
          srvyr::summarise("Renda Média Trabalho Principal Plano"=survey_mean(renda_prim,na.rm=T),
                           "Variância Plano"=survey_var(renda_prim,na.rm=T),
                           "Mediana Plano"=survey_median(renda_prim,na.rm=T),
                           "Desvio Padrão Plano"=survey_sd(renda_prim,na.rm=T),
                           "Renda Q1 (<25%) Plano"=survey_quantile(renda_prim, 0.25, na.rm=TRUE),
                           "Renda Q3 (<75%) Plano"=survey_quantile(renda_prim, 0.75, na.rm=TRUE),
                           "Renda Q99 (<99%) Plano"=survey_quantile(renda_prim, 0.99, na.rm=TRUE))
        
        # Calculos para Samambaia
        amostra %>% filter(A01ra==12) %>% 
          mutate(renda_prim=case_when(G16 == 77777 ~ NA_real_,
                                      G16 == 88888 ~ NA_real_,
                                      G16 == 99999 ~ NA_real_,
                                      TRUE ~as.numeric(G16))) %>%
          srvyr::summarise("Renda Média Trabalho Principal Samambaia"=survey_mean(renda_prim,na.rm=T),
                           "Variância Samambaia"=survey_var(renda_prim,na.rm=T),
                           "Mediana Samambaia"=survey_median(renda_prim,na.rm=T),
                           "Desvio Padrão Samambaia"=survey_sd(renda_prim,na.rm=T),
                           "Renda Q1 (<25%) Samambaia"=survey_quantile(renda_prim, 0.25, na.rm=TRUE),
                           "Renda Q3 (<75%) Samambaia"=survey_quantile(renda_prim, 0.75, na.rm=TRUE),
                           "Renda Q99 (<99%) Samambaia"=survey_quantile(renda_prim, 0.99, na.rm=TRUE))
    
###### iii)	Escolaridade das pessoas de 25 anos ou mais. 
        # (note que é variável qualitativa ordenada)          
        
        # Calculos para o DF      
        amostra %>% 
          srvyr::filter(is.na(escolaridade)==F) %>% 
          srvyr::filter(idade_calculada >= 25) %>%
          srvyr::group_by(escolaridade) %>% 
          srvyr::summarise(n=survey_total())
        
        # Calculos para o Plano     
        amostra %>% 
          srvyr::filter(is.na(escolaridade)==F) %>% 
          srvyr::filter(idade_calculada >= 25 & A01ra == 1) %>%
          srvyr::group_by(escolaridade) %>% 
          srvyr::summarise(n=survey_total())
        
        # Calculos para a Samambaia     
        amostra %>% 
          srvyr::filter(is.na(escolaridade)==F) %>% 
          srvyr::filter(idade_calculada >= 25 & A01ra == 12) %>%
          srvyr::group_by(escolaridade) %>% 
          srvyr::summarise(n=survey_total())

        
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
  
        # Histograma renda domiciliar per capita 
        #Cálculo para o Distrito Federal
        survey::svyhist(~renda_dom_pc, probability = FALSE, xlim=c(0, 30000), 
                        main = "Histograma renda domiciliar per capita - DF", 
                        breaks=c(0,1000,2000,3000,4000,5000,6000,10000,20000,500000), 
                        subset(amostra,E02==1))
        
        #Cálculo para o Plano
        survey::svyhist(~renda_dom_pc, probability = FALSE, xlim=c(0, 30000), 
                        main = "Histograma renda domiciliar per capita - Plano", 
                        breaks=c(0,1000,2000,3000,4000,5000,6000,10000,20000,500000), 
                        subset(amostra,E02==1 & A01ra==1))

        
        #Cálculo para o Samambaia
        survey::svyhist(~renda_dom_pc, probability = FALSE, xlim=c(0, 10000), 
                        main = "Histograma renda domiciliar per capita - Samambaia", 
                        breaks=c(0,1000,2000,3000,4000,5000,6000,10000,20000), 
                        subset(amostra,E02==1 & A01ra==12))
        
        
##### Parte 3 – Tópicos Especiais        
##### 3.1. Calcule o Gini da renda domiciliar per capita para o DF, RA X´ 
    # e o Plano Piloto. Qual a sua conclusão?
      
        # Preparar a base
        amostra_gini<- convey::convey_prep(amostra)

        
        # Cálculo Gini para o DF 
        convey::svygini(~renda_dom_pc,
                        subset(amostra_gini,E02==1),
                        na.rm=T)
        
        # Cálculo Gini para o Plano
        convey::svygini(~renda_dom_pc,
                        subset(amostra_gini,E02==1 & A01ra==1),
                        na.rm=T)
        
        # Cálculo Gini para a Sammabaia
        convey::svygini(~renda_dom_pc,
                        subset(amostra_gini,E02==1 & A01ra==12),
                        na.rm=T)        
        
        
        
##### 3.2 – Vamos supor que o grupo seja convidado a opinar sobre a discussão 
    # da gestão da pandemia de Covid-19:   
    
##### i)	Para subsidiar a discussão, primeiro calcule o número de crianças e 
    # adolescentes (0 a 18 anos) que mora com pessoas mais de 60 anos. Calcule 
    # o intervalo de confiança desta estimativa. Faça isso para RA X´ e para o 
    # DF.
        
        # Cálculo para o DF 
        amostra %>%
          # Filtrar População que mora com idoso e que tenho 18 anos ou menos
          srvyr::filter(mora_com_idoso ==1 & idade_calculada <=18 ) %>%
          # Criar contador
          srvyr::mutate(count=1) %>%
          # Calcular o total
          srvyr::summarise("População Total"=survey_total(count, vartype = "ci"))
        
        
        # Cálculo para o Plano
        amostra %>%
          # Filtrar População que mora com idoso e que tenho 18 anos ou menos
          srvyr::filter(A01ra == 1 & mora_com_idoso ==1 & idade_calculada <=18 ) %>%
          # Criar contador
          srvyr::mutate(count=1) %>%
          # Calcular o total
          srvyr::summarise("População Total"=survey_total(count, vartype = "ci"))
        
        
        
        # Cálculo para o Samambaia
        amostra %>%
          # Filtrar População que mora com idoso e que tenho 18 anos ou menos
          srvyr::filter(A01ra == 12 & mora_com_idoso ==1 & idade_calculada <=18 ) %>%
          # Criar contador
          srvyr::mutate(count=1) %>%
          # Calcular o total
          srvyr::summarise("População Total"=survey_total(count, vartype = "ci"))
        
        
        
        # Crie um filtro para alunos de escolas particulares e públicas
        # Cálculo para alunos de escolas particulares e públicas -  DF 
        amostra %>%
          # Filtrar População que mora com idoso e que tenho 18 anos ou menos e filtrar quem estuda
          srvyr::filter(A01ra >= 1 & mora_com_idoso ==1 & idade_calculada <=18
                        & F02 <=2 ) %>%
          
          # Ajustar nome das variáveis
          srvyr::mutate(F02=factor(case_when(F02 %in% 1 ~"Pública",
                                             F02 %in% 2 ~"Privada",
                                             TRUE ~ NA_character_))) %>%
          # Agrupar por região
          srvyr::group_by(F02) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Escolas"=survey_total(vartype = "ci"),
                           # Calcular o percentual 
                           pct=survey_mean(vartype = "ci"))
      
       
         # Cálculo para alunos de escolas particulares e públicas - Plano 
        amostra %>%
          # Filtrar População que mora com idoso e que tenho 18 anos ou menos e filtrar quem estuda
          srvyr::filter(A01ra == 1 & mora_com_idoso ==1 & idade_calculada <=18 
                        & F02 <=2 ) %>%
          
          # Ajustar nome das variáveis
          srvyr::mutate(F02=factor(case_when(F02 %in% 1 ~"Pública",
                                             F02 %in% 2 ~"Privada",
                                             TRUE ~ NA_character_))) %>%
          # Agrupar por região
          srvyr::group_by(F02) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Escolas"=survey_total(vartype = "ci"),
                           # Calcular o percentual 
                           pct=survey_mean(vartype = "ci"))
        
        
        # Cálculo para alunos de escolas particulares e públicas - Samambaia 
        amostra %>%
          # Filtrar População que mora com idoso e que tenho 18 anos ou menos e filtrar quem estuda
          srvyr::filter(A01ra == 12 & mora_com_idoso ==1 & idade_calculada <=18 
                        & F02 <=2 ) %>%
          
          # Ajustar nome das variáveis
          srvyr::mutate(F02=factor(case_when(F02 %in% 1 ~"Pública",
                                             F02 %in% 2 ~"Privada",
                                             TRUE ~ NA_character_))) %>%
          # Agrupar por região
          srvyr::group_by(F02) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Escolas"=survey_total(vartype = "ci"),
                           # Calcular o percentual 
                           pct=survey_mean(vartype = "ci"))       
        
        
        
##### ii)	Calcule o número médio de pessoas por dormitório desses 
    # domicílios com crianças para a RA X´ e o DF. Crie um filtro para alunos
    # de escolas particulares e públicas.    (crianças de 0 a 12 anos)        
        
        #Cálculo para o Distrito Federal
        amostra %>% 
          srvyr::filter(A01ra >= 1 & idade_calculada <=18) %>%
          filter(E02==1) %>% 
          summarise("Média p/ Dormitório DF"=survey_mean(A01nPessoas/B12,na.rm=TRUE))
        
        #Cálculo para o Plano Piloto
        amostra %>% 
          srvyr::filter(A01ra == 1 & idade_calculada <=18) %>%
          filter(E02==1) %>% 
          summarise("Média p/ Dormitório Plano"=survey_mean(A01nPessoas/B12,na.rm=TRUE))
        
        #Cálculo para Samambaia
        amostra %>% 
          srvyr::filter(A01ra == 1 & idade_calculada <=18) %>%
          summarise("Média p/ Dormitório Samambaia"=survey_mean(A01nPessoas/B12,na.rm=TRUE))
        
        
        # Crie um filtro para alunos de escolas particulares e públicas.
        #Cálculo para o Distrito Federal
        amostra %>% 
          srvyr::filter(A01ra >= 1 & mora_com_idoso ==1 & idade_calculada <=18 
                      & F02 <=2 ) %>%
          srvyr::mutate(F02=factor(case_when(F02 %in% 1 ~"Pública",
                                             F02 %in% 2 ~"Privada",
                                             TRUE ~ NA_character_))) %>%
          # Agrupar por região
          srvyr::group_by(F02) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Média de Pessoas/Domitório"=survey_mean(A01nPessoas/B12, vartype = "ci"))
          
        
        #Cálculo para o Plano Piloto
        amostra %>% 
          srvyr::filter(A01ra == 1 & mora_com_idoso ==1 & idade_calculada <=18 
                        & F02 <=2 ) %>%
          srvyr::mutate(F02=factor(case_when(F02 %in% 1 ~"Pública",
                                             F02 %in% 2 ~"Privada",
                                             TRUE ~ NA_character_))) %>%
          # Agrupar por região
          srvyr::group_by(F02) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Média de Pessoas/Domitório"=survey_mean(A01nPessoas/B12, vartype = "ci"))
        
        
        #Cálculo para o Samambaia
        amostra %>% 
          srvyr::filter(A01ra == 12 & mora_com_idoso ==1 & idade_calculada <=18 
                        & F02 <=2 ) %>%
          srvyr::mutate(F02=factor(case_when(F02 %in% 1 ~"Pública",
                                             F02 %in% 2 ~"Privada",
                                             TRUE ~ NA_character_))) %>%
          # Agrupar por região
          srvyr::group_by(F02) %>%
          # Calcular o total e o Percentual, com seu intervalo de confiança
          srvyr::summarise("Média de Pessoas/Domitório"=survey_mean(A01nPessoas/B12, vartype = "ci"))