# Detecção de Fraudes em Transações de Vendas Online


# Carregando nossos pacotes que ultilizamos nesse LAB
library(ggplot2)
library(caret)
library(dplyr)
library(tibble)
library(tidyverse)
library(data.table)

# Instalando nosso Pacote e carregando 
# install.packages("arsenal")
library(arsenal)

# Carregando minhas base de dados 

treino_id <-  read.csv(file = "dados/train_identity.csv", na.strings = "", stringsAsFactors = T)
treino_transacoes <-  read.csv(file = "dados/train_transaction.csv", na.strings = "",stringsAsFactors = T)
teste_id <-  read.csv(file = "dados/test_identity.csv", na.strings = "",stringsAsFactors = T)
teste_transacoes <-  read.csv("dados/test_transaction.csv", na.strings = "",stringsAsFactors = T)



# Exploração Inicial dos Dados Históricos de Transações Online

# Vizulizando nossas dimensoes Dados de treino

# Id das transações
dim(treino_id)
# Treino de nossas transações
dim(treino_transacoes)

# Veriricado como estão nossos dados
str(treino_id)
str(treino_transacoes)

#  Dimensoes Dados de teste
dim(teste_id)
dim(teste_transacoes)

# Vizualizando nosso resumo estatistico dos dados
str(teste_id)
str(teste_transacoes)


#  Fazendo nosso passo de Engenharia de Atributos ou seja
# transformação dos dados de um estado bruto para um estado mais 
# adequado para a modelagem


# Vizulizando nossos Nomes das colunas de treino
names(treino_transacoes)
names(teste_transacoes)



# Ajustando a variável target em treino e teste para ficar no formato fator
treino_transacoes[,"isFraud"] <-  factor(treino_transacoes[,"isFraud"])

# Nesse caso irei criar uma coluna para esse dataset Receber Valores NA
teste_transacoes$isFraud <- NA


teste_transacoes$isFraud

# Conferimos as dimensões para so dados
dim(treino_transacoes)
dim(teste_transacoes)



# Fazendo nosso Merge dos dataframnes para facilitar o trabalho de limpeza ou
# seja a junção dos dados de treino e teste

#  by = "TransactionID" --> nossa coluna ID do merge

dados_treino <- merge(x = treino_id, y = treino_transacoes, by = "TransactionID")
dados_teste <-  merge(x = teste_id, y = teste_transacoes, by = "TransactionID")

# Vizulaizando nossas Dimensões
dim(dados_treino)
dim(dados_teste)

# Criando  uma coluna para identificar mais tarde se o registro é de treino ou teste
dados_treino$label =  "treino"
dados_teste$label = "teste"


# Vizualizando nossas novas Dimensões
dim(dados_treino)
dim(dados_teste)

# fazendo TAREFA DE TROUBLESHOOTING - para vizualizamos nosso dados pois temos
# uma divergencia sobre nossas colunas do dataset

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Vamos organizar os nomes das colunas nesse caso trazendo menos as colunas
# label e frade 
dados_treino <- dados_treino %>%
  select(-label, -isFraud, everything())

dados_teste <- dados_teste %>%
  select(-label, -isFraud, everything())

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Dimensões
dim(dados_treino)
dim(dados_teste)

# Vamos gerar o dataset como as duas amostras podemos observar que ira dar erro
dados_full <- rbind(dados_treino, dados_teste)

# Vamos comparar os dataframes com nossa função
?comparedf
comparedf(dados_treino, dados_teste)

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Criando uma Lista de dataframes
dfs <- c("dados_treino", "dados_teste")
dfs

# Loop por todas as colunas dos dataframes para ajustar os nomes das colunas de id
# pois nela temos a divergencia dos nomes

for (eachdf in dfs) {
  df.tmp <- get(eachdf) 
  for (eachcol in 1:length(df.tmp)){
    colnames(df.tmp)[eachcol] <- str_trim(str_to_lower(str_replace_all(colnames(df.tmp)[eachcol], "_", ".")))
  }
  assign(eachdf, df.tmp) 
}

# Nomes das colunas podemos vizulizar agora os pontos que ficamos nas colunas oi seja
# no ID 
names(dados_treino)
names(dados_teste)

# Vamos gerar o dataset como as duas amostras ous seja um datastet final com 
# dados de treino e teste

dados_full <- rbind(dados_treino, dados_teste)
dim(dados_full)
str(dados_full)


# Tratamento de Valores Ausentes

# Estratégia 1 - Remover variáveis cujo percentual de valor ausente for superior a 50%
# Estratégia 2 - Para as variáveis remanescentes, atribuir o valor "Desconhecido" se for variável categórica
# Estratégia 3 - Para as variáveis remanescentes, atribuir a média se for variável quantitativa

# Aplicando a Estratégia 1

# Calculando o percentual de valores ausentes por coluna
percentual_valores_ausentes = (colSums(is.na(dados_full)) / nrow(dados_full)) * 100
percentual_valores_ausentes


# Criando um Dataframe com o resultado anterior para criar o plot

df_percent_NA <- data.frame(colnames(dados_full), percentual_valores_ausentes)

# Atribuindo os para nossas colunas
colnames(df_percent_NA) <- c("Variavel", "Percentual_Ausente")

df_percent_NA <- df_percent_NA[order(df_percent_NA$Percentual_Ausente, decreasing = TRUE), ]
df_percent_NA



# Gerando um Plot
plot(df_percent_NA$Percentual_Ausente, 
     ylab = "% de Valores Ausentes", 
     main = "Percentual de Valores Ausentes")



# Vamos remover as colunas com mais de 50% de valores ausentes

# Verificando nossa dimensão
dim(dados_full)

# Removendo nossos dados 
dados_full <- dados_full[percentual_valores_ausentes < 50]

dim(dados_full)




# Aplicando as Estratégias 2 e 3

# Colunas ainda com valores ausentes

outrasNAcol <- (dados_full)[!colSums(is.na(dados_full))==0]
outrasNAcol

# Atribuidos nome das colunas
outrasNAcol <- colnames(outrasNAcol)
outrasNAcol

# Vamos colocar o valor "Desconhecido" onde estiver NA se for variável qualitativa
# Para variáveis quantitativas substituímos NA pela méda

for(f in outrasNAcol){
  
  if(any(is.na(dados_full[[f]]))){
    
    if(is.factor(dados_full[,f])){
      
      dados_full[,f] <- as.character(dados_full[,f])
      
      # Estratégia 2
      dados_full[,f][is.na(dados_full[,f])] <- "Desconhecido"
      dados_full[,f] <- factor(dados_full[,f])
      
    }
    else{
      
      # Estratégia 3
      dados_full[is.na(dados_full[,f]),f] <- mean(dados_full[,f], na.rm = TRUE)
    }
  }
}

# Verifica os dados dataframe
str(dados_full)
names(dados_full)
dim(dados_full)

# Verificando nosso valores NA
sum(is.na(dados_full))


# Pré-Processamento das Variáveis Categóricas

# Convertemos as variáveis categóoricas para o tipo fator
str(dados_full)
dados_full[,"card1"] <- factor(dados_full[,"card1"])
dados_full[,"card2"] <- factor(dados_full[,"card2"])
dados_full[,"card3"] <- factor(dados_full[,"card3"])
dados_full[,"card4"] <- factor(dados_full[,"card4"])
dados_full[,"card5"] <- factor(dados_full[,"card5"])
dados_full[,"card6"] <- factor(dados_full[,"card6"])
dados_full[,"addr1"] <- factor(dados_full[,"addr1"])
dados_full[,"addr2"] <- factor(dados_full[,"addr2"])
dados_full[,"p.emaildomain"] <- factor(dados_full[,"p.emaildomain"])
dados_full[,"r.emaildomain"] <- factor(dados_full[,"r.emaildomain"])
dados_full[,"devicetype"] <- factor(dados_full[,"devicetype"])
dados_full[,"deviceinfo"] <- factor(dados_full[,"deviceinfo"])
dados_full[,"id.12"] <- factor(dados_full[,"id.12"]) 
dados_full[,"id.13"] <- factor(dados_full[,"id.13"]) 
dados_full[,"id.14"] <- factor(dados_full[,"id.14"]) 
dados_full[,"id.15"] <- factor(dados_full[,"id.15"])
dados_full[,"id.16"] <- factor(dados_full[,"id.16"]) 
dados_full[,"id.17"] <- factor(dados_full[,"id.17"])
dados_full[,"id.19"] <- factor(dados_full[,"id.19"])
dados_full[,"id.20"] <- factor(dados_full[,"id.20"]) 
dados_full[,"id.28"] <- factor(dados_full[,"id.28"]) 
dados_full[,"id.29"] <- factor(dados_full[,"id.29"]) 
dados_full[,"id.30"] <- factor(dados_full[,"id.30"]) 
dados_full[,"id.31"] <- factor(dados_full[,"id.31"]) 
dados_full[,"id.32"] <- factor(dados_full[,"id.32"]) 
dados_full[,"id.33"] <- factor(dados_full[,"id.33"])
dados_full[,"id.34"] <- factor(dados_full[,"id.34"])
dados_full[,"id.35"] <- factor(dados_full[,"id.35"]) 
dados_full[,"id.36"] <- factor(dados_full[,"id.36"]) 
dados_full[,"id.37"] <- factor(dados_full[,"id.37"]) 
dados_full[,"id.38"] <- factor(dados_full[,"id.38"]) 


# Em variáveis do tipo texto vamos aplicar limpeza ao texto para poder separar as categorias

# Variável deviceinfo

View(table(dados_full$deviceinfo))
names_deviceinfo <- dados_full$deviceinfo
dados_full$deviceinfo <- factor(gsub("([A-Za-z]+).*",  "\\1", names_deviceinfo, ignore.case = FALSE))
View(table(dados_full$deviceinfo))


# Variável id.30
View(table(dados_full$id.30))
names_id.30 <- dados_full$id.30
dados_full$id.30 <- factor(gsub("([A-Za-z]+).*",  "\\1", names_id.30, ignore.case = FALSE))
View(table(dados_full$id.30))



# Variável card4 ou seja nossos cartões de credito
View(table(dados_full$card4))
dados_full$card4 = recode_factor(dados_full$card4, 
                                 'american express' = "OTHER",  
                                 'discover' = "OTHER", 
                                 'visa' = "visa",   
                                 'mastercard' = "mastercard",   
                                 .default = "OTHER")
View(table(dados_full$card4))

# Variável card6
View(table(dados_full$card6))
dados_full$card6 <- recode_factor(dados_full$card6,
                                 'credit' = "credit", 
                                 'debit' = "debit",
                                 .default = "OTHER")
View(table(dados_full$card6))

# Ajusta a variável alvo removendo o nível onde a categoria for "Desconhecido" 

View(table(dados_full$isfraud))

# exclude = "Desconhecido" -->excluindo nossa variavel

dados_full$isfraud = factor(x = dados_full$isfraud, exclude = "Desconhecido")
View(table(dados_full$isfraud))



# vizualizando nossas  Dimensões
dim(dados_full)
colnames(dados_full)


# Divisão dos Dados em Treino e Teste

dados_treino_final <- subset(dados_full, label == "treino")
dados_teste_final <- subset(dados_full, label == "teste")



# Dimensões de nossos dados de treinamento e teste
dim(dados_treino_final)
dim(dados_teste_final)

# Colunas
names(dados_treino_final)
names(dados_teste_final)

# Análise Exploratória de nossod ados 

# Gerando nosso grafico de barras para uma vizuliaçção qual tipo de disposivo
# em questão da Fraude 
ggplot(dados_treino_final,
       aes(x = factor(devicetype), fill = isfraud)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Dark2")

dados_full$productcd

# Gerando nosso grafico em compração do código do produto, o produto para cada transação
# com fraude
ggplot(dados_treino_final, 
       aes(x = factor(productcd), fill = isfraud)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Set2")

# Gerando nosso grafico em questão do dominio de Email com fraude
ggplot(dados_treino_final,
       aes(x = factor(p.emaildomain), fill = isfraud)) +
  geom_bar() + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1")


# Checando outliers na quantidade das transações das fraudes
ggplot(dados_treino_final,
       aes(factor(isfraud), transactionamt)) +
  geom_boxplot()


# Proporção por classe de fraude temos mais da clase 0 do que para a classe 1
table(dados_treino_final$isfraud)



# Preparação dos Dados Para Modelagem do algorittmo

# Divisão em dados de treino e teste

# Gerando uma semente aleatorio
set.seed(100)

# Ultilizando nossa funnção para criar nosso indice de seprar nossos dados
# ?createDataPartition

# p = .7 --> porcentagem de dados para Treino ou seja 70%
# list = F --> caso False nao trazer os resultado sem uma lista 

indice <- createDataPartition(dados_treino_final$isfraud, p = .7, list = F) 

# Nossos dados de Treino e teste "nesse caso iremos chamar de valid"

# treino nssa variavel isfraude
df_treino <- dados_treino_final[indice, ]
df_valid <- dados_treino_final[-indice, ]

# Dimensões de dados 
dim(df_treino)
dim(df_valid)

# Remove a coluna de label e amostra os dados 

set.seed(100)

names(df_treino)

# Selecionando nossa coluna especifica 
df_treino_final <- select(df_treino, -395)
dim(df_treino_final)

# Podemos vizulizar nossa nomes e label nao estara la
names(df_treino_final)

# Criando nossa amostra dos dados
df_treino_final_sample <- sample(df_treino_final, replace = FALSE, prob = NULL)
dim(df_treino_final_sample)
names(df_treino_final_sample)


str(df_valid)


# Modelagem de nosso algoritmo

# Modelo de Regressão Logística para esse exemplo

# Criando nosso modelo 

# productcd + card4 + card6 + devicetype + id.30 --> nossa variaveis para esse caso

modelo_v1 <- glm(formula = isfraud ~ productcd + card4 + card6 + devicetype + id.30,
                 data = df_treino_final_sample,
                 family = "binomial")

# Vizualizando nosso modelo
summary(modelo_v1)

# Avaliação do Modelo

#  newdata <- df_valid --> nossos dados de teste para o modelo
previsoes <- predict(modelo_v1, newdata = df_valid, type = "response")
View(previsoes)



# nossa estatistica Cutoff ou seja transformando nosso valor se for maior que
# 0.5 sera 1 s nao 0
y_pred_num <- ifelse(previsoes > 0.5, 1, 0)
y_pred_num


# Transformando para factor nossaPrevisões de classe
y_pred <- factor(y_pred_num, levels = c(0, 1))

# Valor real de y
y_act <- df_valid$isfraud
y_act

y_act <- factor(y_act, levels=c(0, 1))



# Gerando Matriz de Confusão e Acurácia
confusionMatrix(y_act, y_pred)

# Previsões com Novos Dados

previsoes_novos_dados = predict(modelo_v1, newdata = dados_teste_final, type = "response")

# Gerando um dataframe com nossas previoes
previsoes <- data.frame(TransactionID = dados_teste_final$transactionid , fraud = previsoes_novos_dados)

# Vizulizando nossas previsoes
View(previsoes)

# Fim



