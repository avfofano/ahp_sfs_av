# cria_classe_especialista.R
################################################################################
##
## CARREGAR BIBLIOTECAS
##
################################################################################
library(dplyr)
library(readxl)
library(R6)
 
################################################################################
## 
## CARREGAR DADOS RELACIONADOS AOS ESPECIALISTAS E CRIA A CLASSE ESPECIALISTAS
## Nesse caso a classe é específica para o método AHP
## 
################################################################################

caminho <- "C:/Users/Silvio/Desktop/MEUS DOCUMENTOS/Programação em R/ahp_edifcontrol/dados_brutos_20112024_af_sfs.xlsx"

# Dados especificos dos especialistas
df_qualif_esp <- read_excel(caminho, sheet="dados_qualifica_especialistas")
df_comparacoes_esp <- read_excel(caminho, sheet="dados_respostas_especialistas")   ## REVER A PLANILHA COM FOFANO

# Pressupõe-se que os nomes das colunas estão de acordo com o template. Não foi feita função para validação

# Dados que vão ser usados para calcular a importância relativa dos especialistas
# pesos_qualificacao <- read_excel(caminho, sheet="pesos", range = "a1:c2")
 
################################################################################
## CRIAÇÃO DA CLASSE ESPECIALISTA
## A classe especialista gera e contém as informações que são usadas para calcular  
## alguns atributos da classe Instalacao. 
## pelas comparações par-a-par do método AHP
##
################################################################################
# Definindo a classe Especialista
 
Especialista_AHP <- R6Class("Especialista_AHP",
     public = list(
        id_esp = NULL,
        formacao_esp = NULL,
        experiencia_esp = NULL,
        atuacao_esp = NULL,
        peso_formacao_esp = NULL,
        peso_experiencia_esp = NULL,
        peso_atuacao_esp = NULL,
        nivel_importancia_relativa_esp = NULL, # Nivel de importancia relativa; usada no expoente do produtório quando usar o AHP AIJ
         
        # criterios
        df_comp_tec_man_uso = NULL,     
        
        # pesos dos criterios
        df_pesos_criterios_tec_man_uso = NULL,  # esse peso e os outros pesos são usados pela Instalacao_AHP
        
        # subcriterios
        df_comp_proj_sust = NULL,
        df_comp_prev_corr = NULL,
        df_comp_int_des_seg = NULL,
        
        # pesos dos subcriterios
        df_pesos_sc_proj_sust = NULL,
        df_pesos_sc_prev_corr = NULL,
        df_pesos_sc_int_des_seg = NULL,
        
        razao_consistencia_tec_man_uso_esp = NULL, 
        razao_consistencia_proj_sust = NULL,           # nesse caso, e no caso de prev_corr, o valor esperado é zero
        razao_consistencia_prev_corr = NULL,
        razao_consistencia_int_des_seg_esp = NULL,
        
        # Inicializa os campos e métodos da classe
        initialize = function(id_esp, df_qualif, df_compara) {
         # if (!is.null(id_esp) && id_esp <= nrow(df_qualif)) {
            
          if (id_esp >= 1 && id_esp <= nrow(df_qualif)) {      
          self$id_esp <- as.numeric(df_qualif[id_esp, 1])
                
          #dados para qualificação e determinação do nivel de importancia relativo  
          self$formacao_esp <- as.character(df_qualif[id_esp, 2])
          self$experiencia_esp <- as.character(df_qualif[id_esp, 3]) 
          self$atuacao_esp <- as.character(df_qualif[id_esp, 4])  
          self$peso_formacao_esp <- as.numeric(df_qualif[id_esp, 5])
          self$peso_experiencia_esp <- as.numeric(df_qualif[id_esp, 6]) 
          self$peso_atuacao_esp <- as.numeric(df_qualif[id_esp, 7])
                   
          self$calc_nivel_importancia(id_esp, df_qualif)
     
          self$carrega_comparacoes(id_esp, df_compara)
                
          self$df_pesos_criterios_tec_man_uso <- self$calc_pesos_criterios_e_subcrit(self$df_comp_tec_man_uso)
          self$df_pesos_sc_proj_sust <- self$calc_pesos_criterios_e_subcrit(self$df_comp_proj_sust)
          self$df_pesos_sc_prev_corr <- self$calc_pesos_criterios_e_subcrit(self$df_comp_prev_corr)
          self$df_pesos_sc_int_des_seg <- self$calc_pesos_criterios_e_subcrit(self$df_comp_int_des_seg)
          
          # A razão de consistência (RC) só é calculada para tec_man_uso porque trata-se de um dataframe 3 x 3. Nos outros casos RC = 0       
          self$razao_consistencia_tec_man_uso_esp <- self$calc_razao_de_consistencia(self$df_comp_tec_man_uso, self$df_pesos_criterios_tec_man_uso)
           
          self$razao_consistencia_int_des_seg_esp <- self$calc_razao_de_consistencia(self$df_comp_int_des_seg, self$df_pesos_sc_int_des_seg)
         
          } else {
                    cat("Erro: ID do especialista inválido ou fora do índice.\n")
          }
        },
       
        carrega_comparacoes = function(id, df_comp) {
      
          self$df_comp_tec_man_uso  <- df_comp %>%
            filter(especialista == id) %>%
            select(tecnica_ahp, manutencao_ahp, uso_ahp)
         
          self$df_comp_proj_sust  <- df_comp %>%
            filter(especialista == id) %>%
            select(projeto_ahp, sustentabilidade_ahp) %>% head(., 2) # nesse caso, pega só as duas primeiras linhas
          
          self$df_comp_prev_corr  <- df_comp %>%
            filter(especialista == id) %>%
            select(preventiva_ahp, corretiva_ahp) %>% head(., 2) # nesse caso, pega só as duas primeiras linhas
         
          self$df_comp_int_des_seg  <- df_comp %>%
            filter(especialista == id) %>%
            select(integridade_ahp, desempenho_ahp, seguranca_ahp)

        },
        
        calc_nivel_importancia = function(id, df_qual){
               
          # Cálculo do nível de importância relativo (envolve todos os dados
          # de qualificação de todos os especialistas para calcular o nível de importância relativo)
          # OBSERVAÇAO: o cálculo do NIR não depende da soma dos pesos da formação!
              
          soma_pesos_for_exp_atu <-  sum(df_qual[id, 5:7])         # soma de todos os pesos de formacao, experiencia e atuacao do especialista id
          soma_total <- sum(unlist(df_qual[ , 5:7]), na.rm = TRUE)  # soma de todos os pesos de todos os especialistas
              
          self$nivel_importancia_relativa_esp <- soma_pesos_for_exp_atu/soma_total
              
          # self$carrega_df_comparacoes_esp
               
          },
        
        calc_pesos_criterios_e_subcrit = function(df){
           
          # pesos_crit_ou_sc <- df
          df_normalizado <- df
          soma_colunas <- colSums(df)

          # Loop for para dividir cada elemento de A pela soma da respectiva coluna e preencher B
          for (j in seq_len(ncol(df))) {   # Percorre cada coluna
              for (i in seq_len(nrow(df))) { # Percorre cada linha
                  df_normalizado[i, j] <- df[i, j] / soma_colunas[j]
              }
          }
          
          pesos_crit_ou_sc <- data.frame(pesos = rowMeans(df_normalizado))
          
          return(pesos_crit_ou_sc)
              
        },
        
        #  =>  Esta função está apresentando erro NULL para a razao de consistencia
        calc_razao_de_consistencia = function (df_comp, pesos){
          
          if (length(df_comp) <= 2) {
            
              return(RC <- 0) # retorna zero se a matriz de comprações for 1 x 1 ou 2 x 2 (ver tab_RI)
            
          } else {
          
              # Tabela de RI (índices de consistência aleatória para diferentes números de critérios.
              # Dados da literatura sobre AHP
              tab_RI <- data.frame(n = 1:10, RI = c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49))
          
              # Inicializando C com as mesmas dimensões de A
              df_normal <- df_comp
              RC <- pesos
              # Loop aninhado para multiplicar conforme o padrão
              for (j in seq_len(ncol(df_comp))) {    # Percorre cada coluna
                  for (i in seq_len(nrow(df_comp))) {  # Percorre cada linha
                      df_normal[i, j] <- df_comp[i, j] * pesos[j, 1] # Multiplica o elemento correspondente
                  }
              }
          
              soma_df_normal <- data.frame(Sum_of_Rows = rowSums(df_normal))
          
              lambda_max <- max(soma_df_normal/pesos)
          
              tamanho_matriz <- length(df_normal)
              
              indice_consistencia <- (lambda_max - tamanho_matriz)/(tamanho_matriz - 1)
              
              RC <- indice_consistencia / tab_RI[tamanho_matriz, 2]
          
                if ( RC > 0.1){

                    cat("Erro: razão de consistência incompatível. Reveja as comparações par-a-par\n")

                    cat(RC,"\n")

                }
  
              return (RC)
          }
        },
        
        # Método para impressão dos dados do especialista na tela
        print = function(...) {
          cat("Especialista:\n")
          cat("  ID: ", self$id_esp, "\n")
          cat("  Formação: ", self$formacao_esp, "\n")
          cat("  Experiência: ", self$experiencia_esp, "\n")
          cat("  Atuação: ", self$atuacao_esp, "\n")
          cat("  Peso da Formação: ", self$peso_formacao_esp, "\n")
          cat("  Peso da Experiência: ", self$peso_experiencia_esp, "\n")
          cat("  Peso da Atuação: ", self$peso_atuacao_esp, "\n")
          cat("  Nível de Importância Relativa (Lambda): ", self$nivel_importancia_relativa_esp, "\n")
          cat("\n")
                
          cat("  Matrizes de comparações par-a-par\n")
          cat("    Critéros: Técnico, Manutenção e Uso:\n")
          print(self$df_comp_tec_man_uso)
          cat("    Razão de Consistência: ", self$razao_consistencia_tec_man_uso_esp, "\n")
         
          cat("    Subcritéros: Projeto e Sustentabilidade:\n")
          print(self$df_comp_proj_sust)
                 
          cat("    Subcritéros: Preventiva e Corretiva:\n")
          print(self$df_comp_prev_corr)
                 
          cat("    Subcritéros: Integridade, Desempenho e Segurança:\n")
          print(self$df_comp_int_des_seg)
          
          cat("    Razão de Consistência: ", self$razao_consistencia_int_des_seg_esp, "\n")
           
                  
          cat("  Pesos dos Critérios: \n")
          cat("    Técnico, Manutenção e Uso:\n")
          print(self$df_pesos_criterios_tec_man_uso)
                  
          cat("  Pesos dos Subcritérios: \n")
          cat("    [Técnico] Projeto e Sustentabilidade:\n")
          print(self$df_pesos_sc_proj_sust)
                  
          cat("    [Manutenção] Preventiva e Corretiva:\n")
          print(self$df_pesos_sc_prev_corr)
                  
          cat("    [Uso] Integridade, Desempenho e Segurança:\n")
          print(self$df_pesos_sc_int_des_seg)
}
     )
)

################################################################################
##
## CRIAR AS INSTÂNCIAS OU SUBCLASSES
##
################################################################################
 
esp_1 <- Especialista_AHP$new(1, df_qualif_esp, df_comparacoes_esp)
esp_2 <- Especialista_AHP$new(2, df_qualif_esp, df_comparacoes_esp)
esp_3 <- Especialista_AHP$new(3, df_qualif_esp, df_comparacoes_esp)
esp_4 <- Especialista_AHP$new(4, df_qualif_esp, df_comparacoes_esp)
esp_5 <- Especialista_AHP$new(5, df_qualif_esp, df_comparacoes_esp)
esp_6 <- Especialista_AHP$new(6, df_qualif_esp, df_comparacoes_esp)
esp_7 <- Especialista_AHP$new(7, df_qualif_esp, df_comparacoes_esp)
esp_8 <- Especialista_AHP$new(8, df_qualif_esp, df_comparacoes_esp)
esp_9 <- Especialista_AHP$new(9, df_qualif_esp, df_comparacoes_esp)
esp_10 <- Especialista_AHP$new(10, df_qualif_esp, df_comparacoes_esp)
esp_11 <- Especialista_AHP$new(11, df_qualif_esp, df_comparacoes_esp)
esp_12 <- Especialista_AHP$new(12, df_qualif_esp, df_comparacoes_esp)
esp_13 <- Especialista_AHP$new(13, df_qualif_esp, df_comparacoes_esp)
esp_14 <- Especialista_AHP$new(14, df_qualif_esp, df_comparacoes_esp)
esp_15 <- Especialista_AHP$new(15, df_qualif_esp, df_comparacoes_esp)


esp_2$df_comp_proj_sust 



###################################     TESTES     ###################################


################################################################################
##
## FORMAS DE ACESSAR OS ATRIBUTOS DAS INSTÂNCIAS
##
################################################################################
 
esp_1$formacao_esp
esp_1$id_esp
esp_1$experiencia_esp
esp_1$atuacao_esp
 
esp_1$nivel_importancia_relativa_esp
esp_2$nivel_importancia_relativa_esp 
print(esp_1$formacao_esp)

################################################################################
###
### Apresentar o nível de importância relativa dos especialistas

imp_relativa_esp <- data.frame(
  especialista = character(), 
  importancia_relativa= integer(), 
  formacao = character(), 
  experiencia = character(),
  atuacao= character(),
  stringsAsFactors = FALSE)

for (i in 1:15) {
  imp_relativa_esp[i, 1] <- paste0("esp_", i)
  imp_relativa_esp[i, 2] <- get(paste0("esp_", i))$nivel_importancia_relativa_esp
  imp_relativa_esp[i, 3] <- get(paste0("esp_", i))$formacao_esp
  imp_relativa_esp[i, 4] <- get(paste0("esp_", i))$experiencia_esp
  imp_relativa_esp[i, 5] <- get(paste0("esp_", i))$atuacao_esp
}

imp_relativa_esp <- arrange(imp_relativa_esp, desc(importancia_relativa))

###
###
################################################################################


################################################################################
###
### Apresentar a razão de consistência dos especialistas

razao_consistencia_esp <- data.frame(
  especialista = character(), 
  rc_tec_man_uso = numeric(), 
  rc_proj_sust= numeric(), 
  rc_prev_corr = numeric(), 
  rc_int_des_seg_esp = numeric(), 
  stringsAsFactors = FALSE)

for (i in 1:15) {
  razao_consistencia_esp[i, 1] <- paste0("esp_", i)
  razao_consistencia_esp[i, 2] <- get(paste0("esp_", i))$razao_consistencia_tec_man_uso_esp
  razao_consistencia_esp[i, 3] <- 0 # a razao de consistencia é sempre zero, pois trata-se da comparação entre dois subcritérios
  razao_consistencia_esp[i, 4] <- 0
razao_consistencia_esp[i, 5] <- get(paste0("esp_", i))$razao_consistencia_int_des_seg_esp

if ((razao_consistencia_esp[i, 2] > 0.1) || (razao_consistencia_esp[i, 5] > 0.1)) {
  razao_consistencia_esp[i, 1] <- paste0("esp_", i, " *")
  message(paste0("esp_", i), " com razão de consistência > 0.1, reavaliar comparações par-a-par")
  # razao_consistencia_esp[i, 6] <- "Razão de consistência > 0.1, reavaliar comparações par-a-par"
}

}


# Visualizando o dataframe final
print(razao_consistencia_esp)




razao_consistencia_esp

esp_2$razao_consistencia_tec_man_uso_esp
esp_2$razao_consistencia_proj_sust

esp_2$razao_consistencia_prev_corr
esp_2$razao_consistencia_int_des_seg_esp


###
###
################################################################################




 
df_comp_prev_corr  <- df_comp %>%
  filter(especialista == id) %>%
  select(projeto_ahp, sustentabilidade_ahp) %>% head(., 2)
 
 

# A função pull é similar a $, com a vantagem de ser usada com pipes (%>%)
esp_1$df_comp_int_des_seg %>% pull(1)


# Exemplo de A e B para testar o código
df_comp <- esp_1$df_comp_tec_man_uso
pesos <- esp_1$df_pesos_criterios_tec_man_uso



 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

