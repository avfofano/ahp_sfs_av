library(dplyr)
library(readxl)

################################################################################
###                                                                   
### AGREGAÇÃO: CÁLCULO DOS PRODUTÓRIOS E AGREGAÇÃO                   
###                                                                  
################################################################################
# quant_espec: quantidade de especialistas
# Critérios: 1 (TEC_MAN_USO); 2 (PROJ_SUST); 3 (PREV_CORR) e 4 (INT_DES_SEG)
# Saída: ambiente com objetos Especialista_AHP para agregação AIJ usando produtório

# Inicializa um ambiente para armazenar todas as informações dos objetos Especialista_AHP

################################################################################
##
## CRIAÇÃO DE AMBIENTE: Todos esses resultados podem ser armazenados num ambiente com:
## Exemplos de acesso: 
## amb_esp$esp_1$razao_consistencia_tec_man_uso_esp
## amb_esp$esp_2$print()
################################################################################


# Inicializa ambiente com objetos Especialista_AHP
quant_espec <- nrow(df_qualif_esp)
amb_esp <- new.env()
for (i in 1:quant_espec) {
  amb_esp[[paste0("esp_", i)]] <- Especialista_AHP$new(i, df_qualif_esp, df_comparacoes_esp)
}

# Função genérica para calcular produtório e normalização
calcular_pesos_ahp <- function(quant_espec, ambiente, matriz_nome) {
  matriz_dim <- dim(ambiente[["esp_1"]][[matriz_nome]])
  resultado <- matrix(nrow = matriz_dim[1], ncol = matriz_dim[2])
  
  for (j in 1:matriz_dim[1]) {
    for (k in 1:matriz_dim[2]) {
      resultado[j, k] <- prod(sapply(1:quant_espec, function(i) {
        especialista <- ambiente[[paste0("esp_", i)]]
        as.numeric(especialista[[matriz_nome]][j, k]) ^ especialista$nivel_importancia_relativa_esp
      }))
    }
  }
  
  # Normalização
  normalizado <- sweep(resultado, 2, colSums(resultado), FUN = "/")
  pesos <- rowMeans(normalizado)
  t(as.data.frame(pesos))

  }



# Função para gerar pesos ahp para as quatro matrizes

gerar_pesos_ahp <- function(){

  pesos_proj_sust <- calcular_pesos_ahp(quant_espec, amb_esp, "df_comp_proj_sust")
  colnames(pesos_proj_sust) <- c("subcrit_tec_projeto", "subcrit_sustentabilidade")
  
  pesos_prev_corr <- calcular_pesos_ahp(quant_espec, amb_esp, "df_comp_prev_corr")
  colnames(pesos_prev_corr) <- c("subcrit_man_prev", "subcrit_man_corretiva")
  
  pesos_int_des_seg <- calcular_pesos_ahp(quant_espec, amb_esp, "df_comp_int_des_seg")
  colnames(pesos_int_des_seg) <- c("subcrit_uso_integridade", "subcrit_uso_desempenho", "subcrit_uso_seguranca")
  
  pesos_tec_man_uso <- calcular_pesos_ahp(quant_espec, amb_esp, "df_comp_tec_man_uso")
  colnames(pesos_tec_man_uso) <- c("crit_tecnico", "crit_manutencao", "crit_uso")
  
  # Combinação final
  total_pesos_ahp <- cbind(pesos_proj_sust, pesos_prev_corr, pesos_int_des_seg, pesos_tec_man_uso)
  
  return(total_pesos_ahp)

}




