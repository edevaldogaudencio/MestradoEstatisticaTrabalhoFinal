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
                      # Criar variável de esgotamento sanitário
                      esgotamento_caesb=case_when(B151==1~"Com Rede Geral",
                                                  TRUE~"Sem Rede Geral"),
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
                      # Criar variável de faixas de salário do trabalho principal
                      faixas_salario=cut(case_when(G16 %in% c(77777,88888,99999)~NA_real_,
                                                   TRUE~as.numeric(G16)),
                                         breaks = c(-Inf,sm,2*sm,4*sm,10*sm,20*sm,Inf),
                                         labels = c("Até 1 salário","Mais de 1 até 2 salários",
                                                    "Mais de 2 até 4 salários",
                                                    "Mais de 4 até 10 salários",
                                                    "Mais de 10 até 20 salários",
                                                    "Mais de 20 salários")),
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
        srvyr::select(RA,E02,idade_calculada,G05,sexo,esgotamento_caesb,idade_faixas,faixas_salario)

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
        
        
        
       




