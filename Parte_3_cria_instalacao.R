# cria_instalacao.R, 12/11/2024
 
library(dplyr)
library(readxl)
library(R6)
 
################################################################################
##
##       CRIAÇÃO DA CLASSE INSTALAÇÃO
##
################################################################################

# Informa caminho dos dados

caminho <- "C:/Users/Silvio/Desktop/MEUS DOCUMENTOS/Programação em R/ahp_edifcontrol/dados_brutos_20112024_af_sfs.xlsx"

############################### 
#### FUNÇÃO: Carregar e valida os dados

carrega_e_valida_dados <- function(caminho){

  dados <- read_excel(caminho, sheet="dados_laudos")
    
# Validação dos nomes das pastas necessárias
  
  pastas_necessarias <- c("dicionario_de_dados", "dados_laudos", "dados_qualifica_especialistas", "dados_respostas_especialistas", "pesos")  
  pastas_fornecidas <- excel_sheets(caminho)

  if (!identical(pastas_fornecidas, pastas_necessarias)) {
        stop("A planilha fornecida não possui as pastas necessárias. Carregue o banco de dados de acordo com o template")
   }


# Validação dos nomes das colunas da pasta "dados_laudos" do banco de dados 
    
  nomes_colunas_possiveis <- c("nome_local", "nome_sistema", "nome_elemento","tec_projeto", "tec_sustentabilidade",
          "man_preventiva", "man_corretiva","uso_integridade", "uso_desempenho", "uso_seguranca")

  colunas_fornecidas <- colnames(dados)
    
  if (!identical(colunas_fornecidas, nomes_colunas_possiveis)) {
      stop("Os nomes das colunas não estão de acordo com o modelo do banco de dados. Carregue o banco de dados de acordo com o template")
   }

  
# Validação da quantidade de locais. Devem existir dois ou mais locais a serem comparados  
  
   quant_locais <- dados %>% select(nome_local) %>% distinct() %>% count()
   
   if (quant_locais < 2) {
      stop("Existem menos de dois locais no banco de dados. São necessários pelo menos dois para comparar. Carregue o banco de dados de acordo com o template")
   }
  
  
# Se tudo for validado, retorna o banco de dados  

    return(dados)
}

#### FIM DA FUNÇÃO
############################### 

df_original <- carrega_e_valida_dados(caminho)

## PESOS DOS LAUDOS
pesos <- read_excel(caminho, sheet="pesos", range = "e1:n3") ## Esses pesos devem vir da agregação aij
 
pesos_laudos <- pesos[1,] # pesos dos criterios e subcriterios dos LAUDOS: as colunas 1 a 7 tem pesos dos subcriterios; as colunas 8 a 10 tem os pesos dos criterios
 

# PESOS AHP

# pesos_ahp <- pesos[2,] # pesos dos criterios e subcriterios do método AHP: as colunas 1 a 7 tem pesos dos subcriterios; as colunas 8 a 10 tem os pesos dos criterios

pesos_ahp <- gerar_pesos_ahp()


###################################
##
## Definição da classe R6, Instalacao
##
###################################
Instalacao <- R6Class(
  "Instalacao",
 
  public = list(
    nome = NULL,
    dados_originais_completos = NULL,
    dados_filtrados = NULL,
    pesos_criterios = NULL,          # recebe os pesos como resultado dos métodos AHP, PCA ou LAUDOS
    pesos_subcriterios = NULL,       # recebe os pesos como resultado dos métodos AHP, PCA ou LAUDOS
    ranking_elementos = 0,
    ranking_local = 0,
    ranking_civil = 0,
    ranking_mecanico = 0,
    ranking_eletrico = 0,
    metodo = NULL, # pode ser laudo, ahp, PCA
  
    ### Função de inicialização dos atributos
    initialize = function(nome, dados, pesos_c, metodo = "não informado") {
      
      # pesos_sc <- pesos_c[1:7]
      # pesos_crit <- pesos_c[8:10]
      
      # Aceitar: as strings "laudo" ou "ahp"
      
      self$metodo <- metodo
      self$validacao_dos_locais(nome, dados)
      
      self$dados_originais_completos <- dados
      self$nome <- nome
      self$dados_filtrados <- self$filtrar_dados(nome, dados)
      self$pesos_criterios <- pesos_c[8:10]
      self$pesos_subcriterios <- pesos_c[1:7]
    
      # Calcula o ranking dos elementos
      self$ranking_elementos <- self$calc_ranking_elementos(self$dados_filtrados, self$pesos_subcriterios, self$pesos_criterios)
    
      # Calcula o ranking do local
      self$ranking_local <- mean(self$ranking_elementos[, 3])
     
      self$ranking_civil <- self$calc_ranking_sistemas("C")
      self$ranking_mecanico <- self$calc_ranking_sistemas("M")
      self$ranking_eletrico <- self$calc_ranking_sistemas("E")
    },
  
    ### Função para validação dos nomes dos locais fornecidos pelo usuário. Se o nome não existir a criação da instalação é encerrada.
  
    validacao_dos_locais = function(nome, dados) {
      # Validação dos nomes dos locais
      lista_nomes_possiveis <- dados %>% select(nome_local) %>% distinct() %>% pull(nome_local)
    
      existe <- nome %in% lista_nomes_possiveis
    
      if (!existe) {
        print("Esse local não existe no banco de dados.")
      
        locais_disponiveis <- dados %>% select(nome_local) %>% distinct()
      
        cat("Locais disponíveis: \n")
        print(locais_disponiveis)
      
        stop("Programa encerrado")
      }
    
    },
    
    ### Funcao para filtrar os dados
  
    filtrar_dados = function(nome, dados) {
        df_filtrados <- dados %>%
        filter(nome_local == nome)
     
        return(df_filtrados)
    },
    
    ### Função para calcular os rankings dos elementos de uma instalacao
  
    calc_ranking_elementos = function(dados_filtrados, pesos_sc, pesos_crit) {
      # Cria um dataframe vazio com as mesmas colunas que dados_filtrados[, 4:10]
      df_elementos_pesos_sc <- data.frame(matrix(ncol = 7, nrow = nrow(dados_filtrados)))
    
      # Define os nomes das colunas
      colnames(df_elementos_pesos_sc) <- colnames(dados_filtrados[, 4:10])
    
      # Multiplica cada linha de dados_filtrados pelo vetor pesos_sc
      for (i in 1:nrow(dados_filtrados)) {
        df_elementos_pesos_sc[i,] <- dados_filtrados[i, 4:10] / 3 * pesos_sc
      }
    
      # Calcula os pesos críticos
      tec <- as.data.frame(rowSums(df_elementos_pesos_sc[, 1:2])) * as.numeric(pesos_crit[1])
      man <- as.data.frame(rowSums(df_elementos_pesos_sc[, 3:4])) * as.numeric(pesos_crit[2])
      uso <- as.data.frame(rowSums(df_elementos_pesos_sc[, 5:7])) * as.numeric(pesos_crit[3])
    
      # Calcula o ranking
      ranking_elem <- data.frame()
      sistema <- dados_filtrados$nome_sistema
      elementos <- dados_filtrados$nome_elemento
    
      ranking_elem <- as.data.frame(rowSums(tec + man + uso))
    
      ranking_elem <- cbind(sistema, elementos, ranking_elem)
      colnames(ranking_elem) <- c("nome_sistema", "elementos", "ranking_elementos")
    
      return(ranking_elem)
    },
   
    calc_ranking_sistemas = function(sistema){
     
      ranking_sistema <- self$ranking_elementos %>%
        filter(nome_sistema == sistema) %>%
        summarise(media_ranking = mean(ranking_elementos, na.rm = TRUE))
     
      return(as.numeric(ranking_sistema))
     
     
    },
   
    ### Função para mostrar os nome do local e ranking
  
    print = function(...) {
      cat("Instalação: \n")
      cat("  Método: ", self$metodo, "\n", sep = "")
      cat("  Nome: ", self$nome, "\n", sep = "")
      cat("  Ranking da Instalação (todos os sistemas): ", self$ranking_local, "\n", sep = "")
      cat("  Ranking Sistema Civil: ", self$ranking_civil, "\n", sep = "")
      cat("  Ranking Sistema Mecanico: ", self$ranking_mecanico, "\n", sep = "")
      cat("  Ranking Sistema Eletrico: ", self$ranking_eletrico, "\n", sep = "")
    }
  
  )
)


# Exemplo de uso da classe: criação das instâncias (subclasses) da classe Instalacao
 
# Dados com pesos AHP
aguas_claras <- Instalacao$new("Águas Claras", df_original, pesos_ahp, "ahp") # pesos_ahp[1:7] corresponde aos pesos dos subcriterios; 8 a 10: pesos dos criterios
brasilia <- Instalacao$new("Brasilia", df_original, pesos_ahp, "ahp")
bandeirante <- Instalacao$new("Bandeirante", df_original, pesos_ahp, "ahp")
brazlandia <- Instalacao$new("Brazlandia", df_original, pesos_ahp, "ahp")
ceilandia <- Instalacao$new("Ceilandia", df_original, pesos_ahp, "ahp")
guara_I <- Instalacao$new("Guará I", df_original, pesos_ahp, "ahp")
guara_II <- Instalacao$new("Guará II", df_original, pesos_ahp, "ahp")
mirabete <- Instalacao$new("Mirabete", df_original, pesos_ahp, "ahp")
planaltina <- Instalacao$new("Planaltina", df_original, pesos_ahp, "ahp")
recanto <- Instalacao$new("Recanto", df_original, pesos_ahp, "ahp")
riacho_fundo <- Instalacao$new("Riacho Fundo", df_original, pesos_ahp, "ahp")



# dados com pesos dos laudos
# Não vai ser necesário usar os dados de instalação no aplicativo. Esses resultados são usados para a comparação entre as metodologias:
# laudos, ahp e PCA

aguas_claras_L <- Instalacao$new("Águas Claras", df_original, pesos_laudos, "laudo") # pesos_laudos[1:7] corresponde aos pesos dos subcriterios; 8 a 10: pesos dos criterios
brasilia_L <- Instalacao$new("Brasilia", df_original, pesos_laudos, "laudo")
bandeirante_L <- Instalacao$new("Bandeirante", df_original, pesos_laudos, "laudo")
brazlandia_L <- Instalacao$new("Brazlandia", df_original, pesos_laudos, "laudo")
ceilandia_L <- Instalacao$new("Ceilandia", df_original, pesos_laudos, "laudo")
guara_I_L <- Instalacao$new("Guará I", df_original, pesos_laudos, "laudo")
guara_II_L <- Instalacao$new("Guará II", df_original, pesos_laudos, "laudo")
mirabete_L <- Instalacao$new("Mirabete", df_original, pesos_laudos, "laudo")
planaltina_L <- Instalacao$new("Planaltina", df_original, pesos_laudos, "laudo")
recanto_L <- Instalacao$new("Recanto", df_original, pesos_laudos, "laudo")
riacho_fundo_L <- Instalacao$new("Riacho Fundo", df_original, pesos_laudos, "laudo")
 




###################################     TESTES     ###################################

# RL <- rank(-c(riacho_fundo_L$ranking_local, recanto_L$ranking_local, aguas_claras_L$ranking_local)) # o sinal '-' serve para ordenar do maior para o menor
# R_AHP <- rank(-c(riacho_fundo$ranking_local, recanto$ranking_local, aguas_claras$ranking_local))

# local_1<- Instalacao$new("Águas Claras", df_original, pesos_laudos[1:7], pesos_laudos[8:10])
 
 
brasilia$nome
brasilia$dados_filtrados
brasilia$pesos_criterios
brasilia$pesos_subcriterios
brasilia$ranking_elementos
brasilia$ranking_local
 
 
guara_I$nome
guara_I$dados_filtrados
guara_I$pesos_criterios
guara_I$pesos_subcriterios
guara_I$ranking_elementos
guara_I$ranking_local
 
guara_I
brasilia
 
 
###################################     TESTES     ###################################





