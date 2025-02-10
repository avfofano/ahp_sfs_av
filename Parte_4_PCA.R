

# Instala bibliotecas

library(tidyverse)
library(dplyr)
library(psych)
library(gt)
library(readxl)
library(ggplot2)


# Carrega dados

cam <- "C:/Users/Silvio/Desktop/MEUS DOCUMENTOS/Programação em R/ahp_edifcontrol/Dadosinspecoes160424.xlsx"

carrega_planilhas <- function(caminho) {
  
  # Pega os nomes das pastas
  nomes_pastas <- excel_sheets(caminho)
  
  # Inicialize uma lista vazia
  dados <- list()
  
  # Faz um loop nas planilhas lendo os conteúdos
  for (i in seq_along(nomes_pastas)) {
    dados[[i]] <- read_excel(caminho, sheet = nomes_pastas[i])
  }
  
  # Combina os dados num único dataframe
  final_data <- do.call(rbind, dados)
  
  return(final_data)
}

dados_ahp_original <- carrega_planilhas(cam)

dados_ahp_original[dados_ahp_original == "N/A"] <- NA

dados_ahp <- dados_ahp_original %>% na.omit()

dados_ahp <- dados_ahp %>%
  mutate(sistema = case_when(
    sistema == "C" ~ "Civil",
    sistema == "E" ~ "Eletrico",
    sistema == "M" ~ "Mecanico",
    TRUE ~ sistema  # Para manter os outros valores inalterados
  ))



locais <- n_distinct(dados_ahp$local)
tot_elem <- n_distinct(dados_ahp$elementos)
mec <- n_distinct(dados_ahp$elementos[dados_ahp$sistema == "Mecanico"])
civ <- n_distinct(dados_ahp$elementos[dados_ahp$sistema == "Civil"])
ele <- n_distinct(dados_ahp$elementos[dados_ahp$sistema == "Eletrico"])


## Descrição dos dados

describe(dados_ahp[5:11])

# Pressupostos para aplicação da PCA

## Teste de Esfericidade de Bartlett

teb <-cortest.bartlett(dados_ahp[5:11])


# Medida de Adequação da Amostra (MSA)

teste_kmo <- KMO(dados_ahp[5:11])

# Determinação do número de fatores a extrair da PCA

## Scree plot

scree(dados_ahp[5:11])

## Análise paralela

paralela <- fa.parallel(dados_ahp[5:11])

# Análise de Componentes Principais (PCA)

df_criterios <- scale(dados_ahp[5:11]) # no presente caso, a padronização das variáveis é opcional.

n_fatores <- paralela$ncomp # número de componentes obtidos da análise paralela

# Aplicação da PCA com múltiplos fatores (por exemplo, 2 fatores)
pca_resultado <- principal(df_criterios, nfactors = n_fatores, rotate = "none")


# Diagrama de componentes

pca_resultado$loadings

fa.diagram(pca_resultado, digits = 3, main = "Componentes principais")

## Cálculo dos scores das componentes principais

### Obter os scores dos componentes principais
pca_scores <- as.data.frame(pca_resultado$scores)


### Ajustar os scores dos componentes principais para serem positivos
pca_scores_ajustados <- pca_scores + abs(min(pca_scores))


# Os scores ajustados são adicionados aos dados originais

dados_pca <- cbind(dados_ahp, pca_scores_ajustados)
dados_10_linhas <- head(dados_pca, 10)


# Cálculo do grau de adequação como a média dos scores ajustados
dados_pca$grau_de_adequacao <- rowMeans(pca_scores_ajustados, na.rm = TRUE)

# Cálculo do grau de adequação médio por local
resultado_final <- dados_pca %>%
  group_by(local) %>%
  summarize(Grau_de_Adequacao = mean(grau_de_adequacao, na.rm = TRUE))

res_civil <- dados_pca %>% 
  filter(sistema == "Civil") %>% 
  group_by(local) %>%
  summarize(Grau_de_Adequacao_civil = mean(grau_de_adequacao, na.rm = TRUE))

res_mec <- dados_pca %>% 
  filter(sistema == "Mecanico") %>% 
  group_by(local) %>%
  summarize(Grau_de_Adequacao_mec = mean(grau_de_adequacao, na.rm = TRUE))

res_ele <- dados_pca %>% 
  filter(sistema == "Eletrico") %>% 
  group_by(local) %>%
  summarize(Grau_de_Adequacao_ele = mean(grau_de_adequacao, na.rm = TRUE))


# Resultado final

res_final <- resultado_final
resultado_final <- cbind(res_final, res_civil[2], res_mec[2], res_ele[2])


# Graus de Adequação por sistemas

resultado_sistemas <- dados_pca %>%
  group_by(sistema) %>%
  summarize(Grau_de_Adequacao = mean(grau_de_adequacao, na.rm = TRUE), desv_padrao = sd(grau_de_adequacao, na.rm = TRUE)) %>% 
  arrange(desc(Grau_de_Adequacao))


# Graus de Adequação por sistemas e elementos

resultado_sistema_elementos <- dados_pca %>%
  group_by(sistema, elementos) %>%
  summarize(Grau_de_Adequacao = mean(grau_de_adequacao, na.rm = TRUE), desv_padrao = sd(grau_de_adequacao, na.rm = TRUE)) %>% 
  arrange(desc(Grau_de_Adequacao))

# Graus de Adequação por elementos: Civil

elementos_civil <- resultado_sistema_elementos %>% filter(sistema == "Civil")

# Graus de Adequação por elementos: Mecânico

elementos_mecanico <- resultado_sistema_elementos %>% filter(sistema == "Mecanico")

# Graus de Adequação por elementos: Elétrico

elementos_eletrico <- resultado_sistema_elementos %>% filter(sistema == "Eletrico")

# Graus de Adequação por locais e sistemas

resultado_local_sistema <- dados_pca %>%
  group_by(local, sistema) %>%
  summarize(Grau_de_Adequacao = mean(grau_de_adequacao, na.rm = TRUE), desv_padrao = sd(grau_de_adequacao, na.rm = TRUE)) %>% 
  arrange(desc(Grau_de_Adequacao))


# resultados de graus por locais e elementos

# A variável resultante daqui é usada no código a seguir
resultado_local_elementos <- dados_pca %>%
  group_by(local, sistema, elementos) %>%
  summarize(Grau_de_Adequacao = mean(grau_de_adequacao, na.rm = TRUE)) %>% 
  arrange(desc(Grau_de_Adequacao))


# (VER GRÁFICOS E DESCRIÇÃO EM PCA_Edificações_26082024.qmd)



























