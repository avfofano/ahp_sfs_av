# graficos
library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(purrr)

#####################################
##
## Carrega dados
##
#####################################

caminho_comp <- "C:/Users/Silvio/Desktop/MEUS DOCUMENTOS/Programação em R/ahp_edifcontrol/Modelagem AHP 31102024.xlsx"

#####################################
## 
## Carrega dados dos sistemas e métodos
##
#####################################

## Geral: AHP x PCA
ahp_pca_geral <- read_excel(caminho_comp, sheet="Correlação", range = "b2:d13")
colnames(ahp_pca_geral) <- c("Geral", "AHP", "PCA")

# Geral: AHP x Laudos
ahp_laudos_geral <- read_excel(caminho_comp, sheet="Correlação", range = "b16:d27")
colnames(ahp_laudos_geral) <- c("Geral", "AHP", "Laudos")

# Civil: AHP xPCA
ahp_pca_civil <- read_excel(caminho_comp, sheet="Correlação", range = "o2:q13")
colnames(ahp_pca_civil) <- c("Civil", "AHP", "PCA")

# Civil: AHP x Laudos
ahp_laudos_civil <- read_excel(caminho_comp, sheet="Correlação", range = "o16:q27")
colnames(ahp_laudos_civil) <- c("Civil", "AHP", "Laudos")

# Elétrico: AHP x PCA
ahp_pca_eletrico <- read_excel(caminho_comp, sheet="Correlação", range = "s2:u13")
colnames(ahp_pca_eletrico) <- c("Elétrico", "AHP", "PCA")

# Elétrico: AHP x Laudos
ahp_laudos_eletrico <- read_excel(caminho_comp, sheet="Correlação", range = "s16:u27")
colnames(ahp_laudos_eletrico) <- c("Elétrico", "AHP", "Laudos")

# Mecânico: AHP x PCA
ahp_pca_mecanico <- read_excel(caminho_comp, sheet="Correlação", range = "w2:y13")
colnames(ahp_pca_mecanico) <- c("Mecânico", "AHP", "PCA")

# Mecânico: AHP x Laudos
ahp_laudos_mecanico <- read_excel(caminho_comp, sheet="Correlação", range = "w16:y27")
colnames(ahp_laudos_mecanico) <- c("Mecânico", "AHP", "Laudos")



###############################################################################
##
## função: graf_comp_sistemas(dados_ranking)
##
## argumento: dados_ranking: dataframe com dados sobre as instalações e 
## resultados dos rankings de acordo com os métodos: AHP, PCA ou laudos
## para os sistemas> Geral, Mecânico, Elétrico e Civil
###############################################################################

graf_comp_sistemas <- function(dados_ranking) {

  # dados_ranking: DataFrame contendo o nome das instalações e os rankings correspondentes aos métodos.
  # Os argumentos `sistema`, `met_1`, e `met_2` devem estar inclusos como colunas nomeadas do DataFrame.

  # Criando os rankings decrescentes para os métodos especificados
  dados_ranking <- dados_ranking %>%
    mutate(rank_met_1 = round(rank(-dados_ranking[[2]]), 0)) %>% 
    mutate(rank_met_2 = round(rank(-dados_ranking[[3]]), 0))

  # Calculando a diferença entre os rankings
  dados_ranking <- dados_ranking %>% 
    mutate(diferenca = abs(rank_met_1 - rank_met_2))

  # Criando o modelo de regressão linear entre os rankings
  modelo <- lm(rank_met_1 ~ rank_met_2, data = dados_ranking)
  R2 <- summary(modelo)$r.squared
  intersecao <- coef(modelo)[1]
  inclinacao <- coef(modelo)[2]

  # Extraindo o nome do sistema e os métodos comparados
  sistema <- colnames(dados_ranking)[1]
  met_1 <- colnames(dados_ranking)[2]
  met_2 <- colnames(dados_ranking)[3]

  # Criando a equação da reta para exibir no gráfico
  equacao_reta <- paste0("Ranking ", met_2, " = ", round(intersecao, 3), 
                         " + ", round(inclinacao, 3), " * Ranking ", met_1)

  # Definindo rótulos e limites para o gráfico
  rotulos <- dados_ranking[[1]]
  limite_x <- max(dados_ranking$rank_met_1, na.rm = TRUE) + 1
  limite_y <- max(dados_ranking$rank_met_2, na.rm = TRUE) + 1

  # Gerando o gráfico
  ggplot(dados_ranking, aes(x = rank_met_1, y = rank_met_2, label = rotulos, color = diferenca)) +
    geom_abline(slope = inclinacao, intercept = intersecao, color = "gray", linetype = "dashed", linewidth = 0.5) +
    geom_point() +
    geom_text_repel(size = 3, min.segment.length = 0, box.padding = 0.5, point.padding = unit(5, "pt"), max.overlaps = 15) +
    coord_equal(xlim = c(0, limite_x), ylim = c(0, limite_y)) +
    scale_color_viridis_c(option = "H") +
    labs(
      x = paste0("Ranking ", met_1),
      y = paste0("Ranking ", met_2),
      title = paste0("Correlação entre rankings - métodos ", met_1, " x ", met_2),
      subtitle = paste0("Sistema: ", sistema),
      color = "Diferenças\nentre rankings"
    ) +
    annotate("text", x = 0.2, y = limite_y - 0.5, label = paste("R² =", round(R2, 3)), color = "black", size = 3, hjust = 0) +
    annotate("text", x = 0.2, y = limite_y - 1.3, label = equacao_reta, color = "black", size = 3, hjust = 0) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray", linewidth = 0.1),
      panel.background = element_rect(fill = "#F5FFF6"),
      axis.ticks = element_blank(),
      axis.text = element_text(face = "bold", color = "black"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      plot.title.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = 9, face = "bold"),
      legend.key.height = unit(10, "pt"),
      legend.text = element_text(size = 8, margin = margin(t = 3))
    )
}



graf_comp_sistemas(ahp_laudos_geral)
graf_comp_sistemas(ahp_pca_geral)




graf_comp_sistemas(ahp_pca_eletrico)



graf_comp_sistemas(ahp_laudos_eletrico)

graf_comp_sistemas(ahp_laudos_civil)

graf_comp_sistemas(ahp_laudos_mecanico)

graf_comp_sistemas(ahp_pca_civil)

graf_comp_sistemas(ahp_pca_mecanico)





################################################################################










