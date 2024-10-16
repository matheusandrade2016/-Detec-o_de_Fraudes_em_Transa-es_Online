## Detecção de Fraudes em Transações Online

**Introdução**

Este projeto visa desenvolver um modelo de machine learning capaz de identificar transações fraudulentas em um conjunto de dados de e-commerce. Utilizando técnicas de aprendizado supervisionado, o modelo analisa diversas características das transações, como informações do cartão de crédito, endereço do cliente e padrões de comportamento.

**Objetivo**

O objetivo principal é construir um modelo preciso e robusto para detectar fraudes em transações online, minimizando perdas financeiras e protegendo a integridade do sistema.

**Metodologia**

1. Coleta e Preparação dos Dados:
   
  * Coleta de dados históricos de transações.
  * Limpeza e tratamento dos dados, incluindo a remoção de outliers e a codificação de variáveis categóricas.

2. Engenharia de Fetures:
   
   * Criação de novas features relevantes, como combinações de variáveis existentes.
   * Seleção das features mais importantes para o modelo.

3. Modelagem:
   
   * Utilização da Regressão Logística para modelar a probabilidade de uma transação ser fraudulenta.
   * Treinamento do modelo em um conjunto de dados de treinamento e avaliação em um conjunto de teste.

4. Avaliação:
   
   * Cálculo de métricas de desempenho, como acurácia, precisão, recall e F1-score.
   * Análise da matriz de confusão para avaliar a performance do modelo.

**Resultados**

O modelo de Regressão Logística obteve uma acurácia de 92% na classificação de transações como fraudulentas ou legítimas. A matriz de confusão indica uma boa performance do modelo em identificar transações fraudulentas.

**Tecnologias Utilizadas**

*Linguagem:** R
*Bibliotecas:** caret, dplyr, tidyverse, data.table, ggplot2

