#Receita Líquida
library(ggplot2)

#Dados
anos <- c(2019, 2020, 2021, 2022, 2023)
receliq <- c(1668.6, 1804.4, 2280.9, 2740.7, 2669.1)  #Receita líquida em milhões

dados <- data.frame(Anos = factor(anos, levels = anos), Receita = receliq)

ggplot(dados, aes(x = Anos, y = Receita)) +
  geom_bar(stat = "identity", fill = "#17BA6C") +
  geom_text(aes(label = Receita, vjust = -0.5), size = 4) +  # Adiciona os valores das barras
  labs(title = "Receita Líquida da Vipal Borrachas \n (2019-2023)",
       x = "Anos",
       y = "Receita Líquida (R$ milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#EBITDA
anos <- c(2019, 2020, 2021, 2022, 2023)
ebitda <- c(197.9, 356.2, 358.6, 394.1, 564.8)  #EBITDA em milhões
margem_ebitda <- c(11.9, 19.7, 15.7, 14.4, 21.2)  #Margem EBITDA em porcentagem

dados <- data.frame(Anos = factor(anos, levels = anos), EBITDA = ebitda, Margem_EBITDA = margem_ebitda)

ggplot(dados, aes(x = Anos, y = EBITDA)) +
  geom_bar(stat = "identity", fill = "#17BA6C") +
  geom_text(aes(label = EBITDA, y = EBITDA + 20), size = 4) +
  geom_text(aes(label = paste0("Margem: ", Margem_EBITDA, "%"), y = EBITDA - 20), size = 2.5, color = "black") +
  labs(title = "EBITDA da Vipal Borrachas\n(2019-2023)",
       x = "Anos",
       y = "EBITDA (R$ milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#Lucro Líquido
anos <- c(2019, 2020, 2021, 2022, 2023)
lucro_liquido <- c(24.1, 95.7, 202.7, 178.5, 310.6)  #Lucro Líquido em milhões
margem_liquida <- c(1.4, 5.3, 8.9, 6.5, 11.6)  #Margem Líquida em porcentagem

dados <- data.frame(Anos = factor(anos, levels = anos), Lucro_Liquido = lucro_liquido, Margem_Liquida = margem_liquida)

ggplot(dados, aes(x = Anos, y = Lucro_Liquido)) +
  geom_bar(stat = "identity", fill = "#17BA6C") +
  geom_text(aes(label = Lucro_Liquido, y = Lucro_Liquido + 10), size = 4) +
  geom_text(aes(label = paste0("Margem: ", Margem_Liquida, "%"), y = Lucro_Liquido - 10), size = 2.2, color = "black") +
  labs(title = "Lucro Líquido da Vipal Borrachas\n(2019-2023)",
       x = "Anos",
       y = "Lucro Líquido (R$ milhões)",
       caption = "Fonte: Demonstrações Financeiras da Vipal Borrachas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#Fluxo de Caixa Operacional
anos <- c(2019, 2020, 2021, 2022, 2023)
fco <- c(202.7, 419.7, 367.4, 351.6, 691.7)  #FCO em milhões

dados <- data.frame(Anos = factor(anos, levels = anos), FCO = fco)

ggplot(dados, aes(x = Anos, y = FCO)) +
  geom_bar(stat = "identity", fill = "#17BA6C") +
  geom_text(aes(label = FCO, vjust = -0.5), size = 4) +  #Adiciona os valores das barras
  labs(title= "Fluxo de Caixa Operacional\n(2019-2023)",
       x = "Anos",
       y = "FCO (R$ milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#Capex
anos <- c(2019, 2020, 2021, 2022, 2023)
imobilizado <- c(24.4, 20.8, 56.9, 59.6, 83.9)  #Capex em milhões

library(ggplot2)
#Criando o data frame
dados <- data.frame(Anos = factor(anos, levels = anos), Imobilizado = imobilizado)

#Criando o gráfico
ggplot(dados, aes(x = Anos, y = Imobilizado)) +
  geom_bar(stat = "identity", fill = "#17BA6C") +  # Barras com cor verde
  geom_text(aes(label = paste0(Imobilizado)), vjust = -0.5, size = 4) +  # Adiciona os valores das barras
  labs(title = "Capex (Imobilizado) \n (2019-2023)",
       x = "Anos",
       y = "Capex (R$ milhões)") +  # Rótulos dos eixos
  theme_minimal() +  # Tema minimalista
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),  # Centraliza e formata o título
        axis.title = element_text(size = 12),  # Tamanho dos rótulos dos eixos
        axis.text = element_text(size = 10))  # Tamanho dos textos dos eixos

#Comparação borracha x vipal x inflação
library(ggplot2)

#Dados
anos <- c(2021, 2022, 2023)

#Preços da borracha natural (R$/kg)
preco_borracha <- c(10.42, 14.00, 17.60)

#Inflação anual (%)
inflacao_anual <- c(10.74, 5.79, 4.62)

#Calculando a inflação acumulada corretamente
inflacao_acumulada <- c(0, inflacao_anual[2], sum(inflacao_anual[2],inflacao_anual[3]))

#Normalizando os dados
preco_borracha_normalizado <- ((preco_borracha - preco_borracha[1]) / preco_borracha[1]) * 100
inflacao_acumulada_normalizada <- inflacao_acumulada

#Criando o data frame
dados <- data.frame(
  Anos = factor(anos, levels = anos),
  Preco_Borracha_Normalizado = preco_borracha_normalizado,
  Inflacao_Acumulada_Normalizada = inflacao_acumulada_normalizada
)

#Gráfico
ggplot(dados, aes(x = Anos)) +
  geom_line(aes(y = Preco_Borracha_Normalizado, color = "Preço da Borracha (Crescimento %)", group = 1), size = 1.5) +
  geom_point(aes(y = Preco_Borracha_Normalizado, color = "Preço da Borracha (Crescimento %)", group = 1), size = 4) +
  geom_text(aes(y = Preco_Borracha_Normalizado, label = paste0(round(Preco_Borracha_Normalizado, 1), "%")), 
            vjust = -1, size = 5, color = "black") +
  
  geom_line(aes(y = Inflacao_Acumulada_Normalizada, color = "Inflação Acumulada (%)", group = 1), size = 1.5) +
  geom_point(aes(y = Inflacao_Acumulada_Normalizada, color = "Inflação Acumulada (%)", group = 1), size = 4) +
  geom_text(aes(y = Inflacao_Acumulada_Normalizada, label = paste0(round(Inflacao_Acumulada_Normalizada, 1), "%")), 
            vjust = -1, size = 5, color = "black") +
  
  labs(title = "Preço da Borracha vs Inflação Acumulada\n(2021-2023)",
       x = "Anos",
       y = "Crescimento (%)",
       caption = "Fonte: Instituto de Economia Agrícola de São Paulo (IEA-SP)",
       color = "Legenda") +
  scale_color_manual(values = c("Preço da Borracha (Crescimento %)" = "#17BA6C",
                                "Inflação Acumulada (%)" = "darkblue")) +
  theme_minimal() +
  ylim(c(0, max(preco_borracha_normalizado, inflacao_acumulada_normalizada) + 10)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom")


