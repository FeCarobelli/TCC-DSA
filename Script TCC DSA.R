# Instalação dos pacotes
install.packages("here")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stats")

# Carregar a biblioteca readxl
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)

# Caminho do banco de dados
caminho_arquivo <- here("Questionário TCC DSA.xlsx")

# Leitura do arquivo Excel
dados_tcc <- read_excel(caminho_arquivo)

# Renomear as variáveis
colnames(dados_tcc) <- c("id_pessoa", "faixa_etaria", "genero", "segmento_empresa", 
                         "modelo_trabalho", "alteracao_modelo_trabalho", "desemprego_2022_2023", 
                         "recolocacao", "novo_cargo_pos_pandemia", "nivel_tecnologico_antes_pandemia", 
                         "nivel_tecnologico_pos_pandemia", "renda_familiar", "variacao_renda_familiar", "realizacao_profissional")

# Visualizar os primeiros registros dos dados
head(dados_tcc)

# Remover valores vazios em modelo_trabalho
dados_sem_vazios <- dados_tcc[!is.na(dados_tcc$modelo_trabalho),]

# Excluir entradas com "Desempregado" em modelo_trabalho
dados_sem_desempregado <- dados_sem_vazios[dados_sem_vazios$modelo_trabalho != "Desempregado", ]

# Calcular as porcentagens
porcentagens <- prop.table(table(dados_sem_desempregado$modelo_trabalho)) * 100

# Criar um data frame com os resultados
porcentagens_df <- data.frame(modelo_trabalho = names(porcentagens), percentual = porcentagens)

# Formatando os valores da coluna "percentual"
porcentagens_df$percentual <- paste0(format(round(porcentagens_df$percentual, 2), nsmall = 2), "%")

# Remover a coluna "percentual.Var1" (se houver)
porcentagens_df <- porcentagens_df[, !grepl("percentual.Var1", names(porcentagens_df))]
porcentagens_df <- porcentagens_df[, !grepl("percentual.Freq", names(porcentagens_df))]

# Visualizar o resultado
print(porcentagens_df)

# Filtrar os dados para excluir "Não se aplica" em segmento_empresa e valores vazios em modelo_trabalho
dados_filtrados <- dados_tcc %>%
  filter(segmento_empresa != "Não se aplica" & modelo_trabalho != "")

# Calcular as frequências de cada modelo_trabalho por segmento_empresa
dados_por_segmento <- dados_filtrados %>%
  group_by(segmento_empresa, modelo_trabalho) %>%
  summarise(n = n()) %>%
  group_by(segmento_empresa) %>%
  mutate(percentual = n / sum(n) * 100)

# Definir cores para cada modelo_trabalho
cores_modelo_trabalho <- c("Híbrido" = "#118DFF", "Presencial" = "#12239E", "Remoto" = "#E66C37")

# Criar o gráfico de barras empilhadas com rótulos de dados
ggplot(dados_por_segmento, aes(x = segmento_empresa, y = percentual, fill = modelo_trabalho)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(sprintf("%.2f", percentual), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 3, 
            fontface = "bold",
            label.padding = unit(0.2, "lines")) + # Adicionar caixa ao redor dos rótulos de dados
  labs(x = NULL, y = NULL, fill = "Modelos de Trabalho") + # Remover os nomes dos eixos
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Escala de 1 para exibir os valores percentuais corretamente
  scale_fill_manual(values = cores_modelo_trabalho) + # Definir cores para cada modelo_trabalho
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggtitle("Aderência dos modelos de trabalho por segmento de empresa")

# Função para calcular o percentual de cada categoria
calculate_percentual <- function(x) {
  percentual <- prop.table(table(x)) * 100
  return(percentual)
}

# Agrupar os dados por faixa_etaria e calcular os percentuais das variáveis
tabela_comparativa <- dados_tcc %>%
  group_by(faixa_etaria) %>%
  summarise(
    percentual_nivel_tecnologico_antes_pandemia = list(calculate_percentual(nivel_tecnologico_antes_pandemia)),
    percentual_nivel_tecnologico_pos_pandemia = list(calculate_percentual(nivel_tecnologico_pos_pandemia))
  ) %>%
  ungroup() %>%
  mutate(
    percentual_nivel_tecnologico_antes_pandemia = map(percentual_nivel_tecnologico_antes_pandemia, ~ if(length(.) == 0) NA else .),
    percentual_nivel_tecnologico_pos_pandemia = map(percentual_nivel_tecnologico_pos_pandemia, ~ if(length(.) == 0) NA else .)
  ) %>%
  unnest_wider(c(percentual_nivel_tecnologico_antes_pandemia, percentual_nivel_tecnologico_pos_pandemia), names_sep = "_") %>%
  select(-matches("NA")) # Remove as colunas que contenham NA no nome

tabela_comparativa <- tabela_comparativa %>%
  select(faixa_etaria, 
         `percentual_nivel_tecnologico_antes_pandemia_Não possuía conhecimento`,
         `percentual_nivel_tecnologico_antes_pandemia_Baixo`,
         `percentual_nivel_tecnologico_pos_pandemia_Baixo`,
         `percentual_nivel_tecnologico_antes_pandemia_Médio`,
         `percentual_nivel_tecnologico_pos_pandemia_Médio`,
         `percentual_nivel_tecnologico_antes_pandemia_Alto`, 
         `percentual_nivel_tecnologico_pos_pandemia_Alto`) %>%
  rename("Faixa Etária" = faixa_etaria,
         "Alto Antes" = `percentual_nivel_tecnologico_antes_pandemia_Alto`,
         "Médio Antes" = `percentual_nivel_tecnologico_antes_pandemia_Médio`,
         "Baixo Antes" = `percentual_nivel_tecnologico_antes_pandemia_Baixo`,
         "Não Possuía Conhecimento Antes" = `percentual_nivel_tecnologico_antes_pandemia_Não possuía conhecimento`,
         "Alto Depois" = `percentual_nivel_tecnologico_pos_pandemia_Alto`,
         "Baixo Depois" = `percentual_nivel_tecnologico_pos_pandemia_Baixo`,
         "Médio Depois" = `percentual_nivel_tecnologico_pos_pandemia_Médio`)

# Formatar os percentuais com duas casas decimais
tabela_comparativa[, -1] <- lapply(tabela_comparativa[, -1], function(x) {
  ifelse(is.na(x), NA, paste0(format(x, digits = 2, nsmall = 2), "%"))
})

# Mostrar a tabela
View(tabela_comparativa)

# Calcular a frequência do termo "Desempregado" na variável "modelo_trabalho"
frequencia_desempregado <- sum(dados_tcc$modelo_trabalho == "Desempregado", na.rm = TRUE)
total_registros <- nrow(dados_tcc)
percentual_desempregado <- round((frequencia_desempregado / total_registros) * 100, 2)

# Calcular a frequência do termo "Sim" na variável "desemprego_2022_2023"
frequencia_sim <- sum(dados_tcc$desemprego_2022_2023 == "Sim", na.rm = TRUE)
percentual_sim <- round((frequencia_sim / total_registros) * 100, 2)

# Mostrar os resultados
print(sprintf("Percentual de vezes que 'Desempregado' aparece em 'modelo_trabalho': %.2f %%", percentual_desempregado))
print(sprintf("Percentual de vezes que 'Sim' aparece em 'desemprego_2022_2023': %.2f %%", percentual_sim))

# Criar a tabela de contingência cruzando as variáveis categóricas
tabela_contingencia <- table(dados_tcc$genero, dados_tcc$faixa_etaria, dados_tcc$renda_familiar)

# Calcular as margens da tabela
margens_linha <- rowSums(tabela_contingencia)
margens_coluna <- colSums(tabela_contingencia)

# Selecionar apenas as linhas e colunas com totais não zero
tabela_contingencia <- tabela_contingencia[rowSums(tabela_contingencia) > 0, colSums(tabela_contingencia) > 0]

# Realizar o teste qui-quadrado de independência
resultado_teste <- chisq.test(tabela_contingencia)

# Exibir os resultados do teste
print(resultado_teste)


# Definir as variáveis
generos <- unique(dados_tcc$genero)
faixas_etarias <- unique(dados_tcc$faixa_etaria)
rendas_familiares <- unique(dados_tcc$renda_familiar)

# Inicializar uma matriz tridimensional para armazenar as contagens
tamanho_matriz <- c(length(generos), length(faixas_etarias), length(rendas_familiares))
tabela_contingencia_bidimensional <- array(0, dim = tamanho_matriz)

# Preencher a matriz com as contagens
for (i in 1:length(generos)) {
  for (j in 1:length(faixas_etarias)) {
    for (k in 1:length(rendas_familiares)) {
      # Filtrar os dados para a combinação atual de gênero, faixa etária e renda familiar
      dados_filtrados <- subset(dados_tcc, genero == generos[i] & faixa_etaria == faixas_etarias[j] & renda_familiar == rendas_familiares[k])
      # Calcular o número de observações para essa combinação
      contagem <- nrow(dados_filtrados)
      # Armazenar a contagem na matriz tridimensional
      tabela_contingencia_bidimensional[i, j, k] <- contagem
    }
  }
}

# Inicialize uma matriz vazia para armazenar a tabela bidimensional
tabela_contingencia_bidimensional_bivariada <- matrix(0, nrow = nrow(tabela_contingencia_bidimensional), ncol = ncol(tabela_contingencia_bidimensional))

# Combine as matrizes correspondentes a cada nível da variável em uma única matriz bidimensional
tabela_contingencia_bidimensional_bivariada <- apply(tabela_contingencia_bidimensional, c(1, 2), sum)

# Verifique a estrutura da nova tabela
str(tabela_contingencia_bidimensional_bivariada)

# Execute o teste qui-quadrado de independência
resultado_teste <- chisq.test(tabela_contingencia_bidimensional_bivariada)

# Imprima os resultados do teste
print(resultado_teste)


# Calcular os percentuais de renda familiar para cada combinação de gênero e faixa etária
dados_tcc <- transform(dados_tcc, genero_faixa_etaria = paste(genero, faixa_etaria))

# Convertendo a coluna "renda_familiar" para um fator ordenado
dados_tcc$renda_familiar <- factor(dados_tcc$renda_familiar, levels = c("Reduziu", "Manteve", "Aumentou"))

# Calculando as frequências de cada combinação de gênero e faixa etária
frequencias <- table(dados_tcc$genero_faixa_etaria, dados_tcc$renda_familiar)

# Convertendo as frequências em percentuais
percentuais <- prop.table(frequencias, margin = 1) * 100

# Convertendo os percentuais em data frame
dados_percentuais_df <- as.data.frame(percentuais)
dados_percentuais_df$genero_faixa_etaria <- rownames(dados_percentuais_df)

# Renomear as colunas
colnames(dados_percentuais_df) <- c("Reduziu", "Manteve", "Aumentou", "genero_faixa_etaria")

# Convertendo as colunas de percentuais para numérico
dados_percentuais_df <- dados_percentuais_df %>%
  mutate(across(c(Reduziu, Manteve, Aumentou), as.numeric))

# Verificando a estrutura dos dados novamente
str(dados_percentuais_df)


# Verificando novamente os dados
str(dados_percentuais_df)


# Convertendo a coluna "genero_faixa_etaria" para fator
dados_percentuais_df$genero_faixa_etaria <- factor(dados_percentuais_df$genero_faixa_etaria, levels = unique(dados_percentuais_df$genero_faixa_etaria))

# Convertendo todas as colunas, exceto a "genero_faixa_etaria", para fatores
dados_percentuais_df[, -4] <- lapply(dados_percentuais_df[, -4], function(x) as.factor(as.character(x)))

# Verificando os níveis em todas as colunas
sapply(dados_percentuais_df, levels)

# Agora criamos o formato longo
dados_percentuais_long <- pivot_longer(dados_percentuais_df, 
                                       cols = c(Reduziu, Manteve, Aumentou), 
                                       names_to = "renda_familiar", 
                                       values_to = "percentual")

# Plot do gráfico
ggplot(dados_percentuais_long, aes(x = genero_faixa_etaria, y = percentual, fill = renda_familiar)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentual), "%")), 
            position = position_stack(vjust = 1.1), 
            size = 3, 
            colour = "white") + # adiciona um contorno branco para melhorar a legibilidade
  labs(x = "Gênero e Faixa Etária", y = "Percentual (%)", fill = "Renda Familiar") +
  ggtitle("Percentual de Renda Familiar por Gênero e Faixa Etária") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  guides(fill = guide_legend(title = "Renda Familiar"))

# ANOVA
modelo_anova <- aov(variacao_renda_familiar ~ genero + faixa_etaria, data = dados_tcc)
resumo_anova <- summary(modelo_anova)

# Extrair os valores p e F
p_valor_genero <- format(resumo_anova[[1]]$`Pr(>F)`[1], digits = 3)
f_valor_genero <- format(resumo_anova[[1]]$`F value`[1], digits = 3)
p_valor_faixa_etaria <- format(resumo_anova[[1]]$`Pr(>F)`[2], digits = 3)
f_valor_faixa_etaria <- format(resumo_anova[[1]]$`F value`[2], digits = 3)

# Definir a posição da caixa de texto
posicao_x <- Inf  # Canto direito
posicao_y <- Inf  # Canto superior

# Texto para a caixa
texto <- paste(
  "Gênero:\n",
  "  p =", p_valor_genero, ", F =", f_valor_genero, "\n",
  "Faixa Etária:\n",
  "  p =", p_valor_faixa_etaria, ", F =", f_valor_faixa_etaria
)

# Gráfico de barras
ggplot(dados_tcc, aes(x = genero, y = variacao_renda_familiar, fill = faixa_etaria)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "Variação da Renda Familiar por Gênero e Faixa Etária",
       x = "Gênero",
       y = "Variação da Renda Familiar",
       fill = "Faixa Etária") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(aes(x = posicao_x, y = posicao_y, label = texto), size = 5, hjust = 1, vjust = 1)

# Filtrar os dados para incluir apenas as linhas com o termo "Desempregado" no modelo_trabalho
dados_desempregados <- dados_tcc %>% 
  filter(modelo_trabalho == "Desempregado")

# Calcular o total de observações de desempregados
total_desempregados <- nrow(dados_desempregados)

# Criar a tabela de frequência
tabela_percentual2 <- dados_desempregados %>%
  group_by(genero, faixa_etaria) %>%
  summarise(percentual = n() / total_desempregados * 100)

# Exibir a tabela de percentual
print(tabela_percentual2)

# Filtrar os dados para incluir apenas as linhas com o termo "Desempregado" no modelo_trabalho
dados_recolocacao <- dados_tcc %>% 
  filter(recolocacao == "Sim")

# Calcular o total de observações de desempregados
total_recolocacao <- nrow(dados_recolocacao)

# Criar a tabela de frequência
tabela_percentual3 <- dados_recolocacao %>%
  group_by(genero, faixa_etaria) %>%
  summarise(percentual = n() / total_recolocacao * 100)

# Exibir a tabela de percentual
print(tabela_percentual3)

# Filtrar os dados para incluir apenas as linhas com o termo "Desempregado" no modelo_trabalho
dados_desemprego_2022_2023 <- dados_tcc %>% 
  filter(desemprego_2022_2023 == "Sim")

# Calcular o total de observações de desempregados
total_desemprego_2022_2023 <- nrow(dados_desemprego_2022_2023)

# Criar a tabela de frequência
tabela_percentual4 <- dados_desemprego_2022_2023 %>%
  group_by(genero, faixa_etaria) %>%
  summarise(percentual = n() / total_desemprego_2022_2023 * 100)

# Exibir a tabela de percentual
View(tabela_percentual4)

# Filtrar os dados para incluir apenas as linhas com novo_cargo_pos_pandemia igual a "Sim"
dados_novo_cargo_pos_pandemia <- dados_tcc %>% 
  filter(novo_cargo_pos_pandemia == "Sim")

# Calcular o total de observações com novo cargo após a pandemia
total_novo_cargo_pos_pandemia <- nrow(dados_novo_cargo_pos_pandemia)

# Criar a tabela de frequência
tabela_percentual5 <- dados_novo_cargo_pos_pandemia %>%
  group_by(segmento_empresa) %>%
  summarise(percentual = (n() / total_novo_cargo_pos_pandemia) * 100)

# Exibir a tabela de percentual
print(tabela_percentual5)

# Calcular o percentual para cada segmento de empresa
dados_grafico <- tabela_percentual5
dados_grafico$percentual <- paste0(round(dados_grafico$percentual, 1), "%")

# Calcular o percentual para cada segmento de empresa
dados_grafico <- tabela_percentual5
dados_grafico$percentual <- paste0(round(dados_grafico$percentual, 1), "%")

# Criar o gráfico de barras
ggplot(dados_grafico, aes(x = segmento_empresa, y = percentual, fill = segmento_empresa)) +
  geom_bar(stat = "identity") +
  labs(title = "Novos Cargos após a Pandemia por Segmento de Empresa",
       x = "Segmento de Empresa",
       y = "Percentual",
       fill = "Segmento de Empresa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calcular a tabela de frequência
tabela_frequencia <- table(dados_tcc$realizacao_profissional)

# Calcular a frequência total
frequencia_total <- sum(tabela_frequencia)

# Calcular a frequência percentual
frequencia_percentual <- (tabela_frequencia / frequencia_total) * 100

# Exibir a tabela de frequência percentual
print(frequencia_percentual)






