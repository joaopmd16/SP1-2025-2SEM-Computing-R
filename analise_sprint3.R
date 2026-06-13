# === 2º SEMESTRE – CHALLENGE SPRINT 3 (versão CSV/XLSX robusta) ===
# Arquivo: analise_sprint3_csv.R
# Integrantes:
# - João Pedro de Moura – RM: 561738
# - Nelson Felix – RM: 565603
# - Vitor Soares – RM: 566181
# - Pietro Boroto – RM: 562407
#
# ENTREGAS:
# - Base de dados: dados_challenge.xlsx (gerada a partir do CSV)
# - Código em R: analise_sprint3_csv.R
#
# Observações:
# 1) Este script lê .csv OU .xlsx. Ajuste 'caminho_base' e 'tipo_base' abaixo.
# 2) Se 'var_name' não existir na base, o script escolhe a primeira coluna numérica.
# 3) Para Windows, use caminhos com barra '/': C:/Users/SeuUsuario/...
#    ou mantenha o .xlsx/.csv na mesma pasta do script.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

# ======= CONFIGURAÇÃO DO ARQUIVO =======
# Se for usar seu CSV antigo, aponte para ele e defina tipo_base = "csv".
# Se preferir, use o .xlsx já gerado e defina tipo_base = "xlsx".
caminho_base <- "C:/Users/labsfiap/Desktop/R Trabalho/dados_challenge.xlsx" # ou "dados_challenge.csv"
tipo_base    <- "xlsx"                   # "csv" ou "xlsx"

# ======= LEITURA DA BASE =======
if (tipo_base == "csv") {
  df <- tryCatch(
    read.csv(caminho_base, check.names = FALSE),
    error = function(e) {
      read.csv(caminho_base, fileEncoding = "latin1", check.names = FALSE)
    }
  )
} else if (tipo_base == "xlsx") {
  df <- readxl::read_excel(caminho_base)
} else {
  stop("tipo_base deve ser 'csv' ou 'xlsx'.")
}

# Mostra as colunas disponíveis
cat("Colunas disponíveis na base:\n")
print(names(df))
cat("\n")

# ======= VARIÁVEL QUANTITATIVA =======
var_name <- NULL   # Exemplo: "Altura", "medida", "Custo"

escolher_primeira_numerica <- function(d) {
  nums <- sapply(d, function(col) is.numeric(col) || is.integer(col))
  if (!any(nums)) return(NULL)
  return(names(d)[which(nums)[1]])
}

if (is.null(var_name) || !(var_name %in% names(df))) {
  auto_var <- escolher_primeira_numerica(df)
  if (is.null(auto_var)) {
    stop("Não há colunas numéricas disponíveis. Verifique sua base.")
  } else {
    var_name <- auto_var
    cat("Aviso: 'var_name' não especificado/encontrado. Usando automaticamente a coluna numérica: '", var_name, "'\n\n", sep="")
  }
}

x <- df[[var_name]]
x <- x[is.finite(x)]  # remove NA/Inf

# ======= ESTATÍSTICAS =======
media   <- mean(x)
mediana <- median(x)
dp      <- sd(x)
n_total <- length(x)

cat("=== Estatísticas (", var_name, ") ===\n", sep="")
cat("n =", n_total, "| média =", round(media, 4), "| mediana =", round(mediana, 4), "| desvio-padrão =", round(dp, 4), "\n\n")

classificar_prob <- function(p) {
  if (p < 0.01) return("Evento extremamente raro (<1%)")
  if (p < 0.05) return("Evento raro (1%–5%)")
  if (p < 0.20) return("Pouco provável (5%–20%)")
  if (p <= 0.80) return("Probabilidade moderada (20%–80%)")
  if (p <= 0.95) return("Muito provável (80%–95%)")
  return("Altamente provável (>95%)")
}

# ======= 01) PROBABILIDADES =======
z_mediana <- (mediana - media) / dp
p_maior_que_mediana <- pnorm(z_mediana, lower.tail = FALSE)
cat("01a) P(X >", round(mediana, 4), ") =", round(p_maior_que_mediana, 6), "\n")
cat("Classificação:", classificar_prob(p_maior_que_mediana), "\n\n")

p_intervalo <- pnorm(2) - pnorm(-2)
cat("01b) P(μ-2σ <= X <= μ+2σ) ~= ", round(p_intervalo, 6), "\n", sep="")
cat("Classificação:", classificar_prob(p_intervalo), "\n\n")

# ======= 02) TESTES DE HIPÓTESE =======
sigma_ref <- dp

# (a) Unilateral à esquerda (α=5%, n=20)
set.seed(123)
n_a <- 20
amostra_a <- sample(x, size = n_a, replace = length(x) < n_a)
xbar_a <- mean(amostra_a)
mu0_a  <- media
z_a    <- (xbar_a - mu0_a) / (sigma_ref / sqrt(n_a))
pval_a <- pnorm(z_a, lower.tail = TRUE)

cat("02a) Teste unilateral à esquerda (α = 5%)\n")
cat("  n =", n_a, "| x̄ =", round(xbar_a,4), "| μ0 =", round(mu0_a,4), "| σ =", round(sigma_ref,4), "\n")
cat("  z =", round(z_a,4), "| p-valor =", signif(pval_a, 6), "\n")
if (pval_a < 0.05) {
  cat("  Decisão: Rejeitar H0. Evidência de que μ < μ0 (5%).\n\n")
} else {
  cat("  Decisão: Não rejeitar H0. Sem evidência de que μ < μ0 (5%).\n\n")
}

# (b) Bicaudal (α=1%, n=15)
set.seed(456)
n_b <- 15
amostra_b <- sample(x, size = n_b, replace = length(x) < n_b)
xbar_b <- mean(amostra_b)
mu0_b  <- media + 2
z_b    <- (xbar_b - mu0_b) / (sigma_ref / sqrt(n_b))
pval_b <- 2 * pnorm(abs(z_b), lower.tail = FALSE)

cat("02b) Teste bicaudal (α = 1%)\n")
cat("  n =", n_b, "| x̄ =", round(xbar_b,4), "| μ0 =", round(mu0_b,4), "| σ =", round(sigma_ref,4), "\n")
cat("  z =", round(z_b,4), "| p-valor =", signif(pval_b, 6), "\n")
if (pval_b < 0.01) {
  cat("  Decisão: Rejeitar H0. Evidência de que μ ≠ μ0 (1%).\n")
} else {
  cat("  Decisão: Não rejeitar H0. Sem evidência de que μ ≠ μ0 (1%).\n")
}

cat("\n=== Fim da análise (", var_name, "). ===\n", sep="")
