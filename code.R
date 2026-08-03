library(quantmod)
library(xts)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(jsonlite)

# ---------------------------------------------------------------------------
# Ativos arriscados e período de análise
# ---------------------------------------------------------------------------
# Tickers da B3 mudam com o tempo (ex.: EMBR3 -> EMBJ3 em nov/2025, JBSS3
# saiu de listagem em jun/2025). Se algum getSymbols abaixo falhar com erro
# 404, confira se o ticker ainda está ativo no Yahoo Finance.

data_inicio <- "2022-01-01"
data_fim <- "2024-12-31"

getSymbols("CYRE3.SA", from = data_inicio, to = data_fim)  # Cyrela
getSymbols("BEEF3.SA", from = data_inicio, to = data_fim)  # BEEF
getSymbols("PETR4.SA", from = data_inicio, to = data_fim)  # Petrobras
getSymbols("ITUB3.SA", from = data_inicio, to = data_fim)  # Itaú
getSymbols("EMBJ3.SA", from = data_inicio, to = data_fim)  # Embraer

# Visualiza os dados
head(CYRE3.SA)
head(BEEF3.SA)
head(PETR4.SA)
head(ITUB3.SA)
head(EMBJ3.SA)

# Extraindo as cotações ajustadas
CYRE <- CYRE3.SA[, "CYRE3.SA.Adjusted"]
BEEF <- BEEF3.SA[, "BEEF3.SA.Adjusted"]
PETR <- PETR4.SA[, "PETR4.SA.Adjusted"]
ITUB <- ITUB3.SA[, "ITUB3.SA.Adjusted"]
EMBJ <- EMBJ3.SA[, "EMBJ3.SA.Adjusted"]

# Juntar os dados em um único objeto xts (uma série temporal) para tabelar
dados <- merge(CYRE, BEEF, join = "inner")
dados <- merge(dados, PETR, join = "inner")
dados <- merge(dados, ITUB, join = "inner")
dados <- merge(dados, EMBJ, join = "inner")

# Nomear as colunas para facilitar visualização
colnames(dados) <- c("CYRE3", "BEEF3", "PETR4", "ITUB3", "EMBJ3")

# Baixa a SELIC efetiva diária (código 432 do Banco Central) direto da API do BCB
url_selic <- paste0(
  "https://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados?",
  "formato=json&dataInicial=", format(as.Date(data_inicio), "%d/%m/%Y"),
  "&dataFinal=", format(as.Date(data_fim), "%d/%m/%Y")
)

selic_df <- fromJSON(url_selic)
selic_df$data <- as.Date(selic_df$data, format = "%d/%m/%Y")
selic_df$valor <- as.numeric(selic_df$valor)

# Converte a série para xts (série temporal com índice de datas)
selic_xts <- xts(selic_df$valor, order.by = selic_df$data)

# Converte SELIC anual para taxa equivalente diária (base 252 dias úteis)
selic_diaria <- (1 + selic_xts / 100)^(1 / 252) - 1

# Converter SELIC diária acumulada (como se fosse um ativo de renda fixa)
selic_acumulada <- cumprod(1 + selic_diaria)  # Começa em 1 e cresce como um preço teórico

# Nomear a coluna para facilitar a leitura
colnames(selic_acumulada) <- "SELIC"

# Alinhar datas com os dados de preços ajustados
selic_plot <- selic_acumulada[index(dados)]

# Adicionar ao conjunto de dados
dados_com_selic <- merge(dados, selic_plot, join = "inner")

# Gráfico: Ações vs SELIC
plot.zoo(dados_com_selic, plot.type = "single",
         col = c("blue", "red", "green", "orange", "purple", "black"),
         lwd = 2, xlab = "Data", ylab = "Preço / Índice (baseado em R$1)",
         main = "Comparação: Ações vs SELIC (2022–2024)")

legend("topleft", legend = c("CYRE3", "BEEF3", "PETR4", "ITUB3", "EMBJ3", "SELIC"),
       col = c("blue", "red", "green", "orange", "purple", "black"), lty = 1, lwd = 2, cex = 0.8)

# ---------------------------------------------------------------------------
# Retornos diários
# ---------------------------------------------------------------------------
ret_CYRE <- dailyReturn(CYRE)
ret_BEEF <- dailyReturn(BEEF)
ret_PETR <- dailyReturn(PETR)
ret_ITUB <- dailyReturn(ITUB)
ret_EMBJ <- dailyReturn(EMBJ)

# Junta os retornos diários dos ativos em um mesmo objeto
retornos <- merge(ret_CYRE, ret_BEEF, join = "inner")
retornos <- merge(retornos, ret_PETR, join = "inner")
retornos <- merge(retornos, ret_ITUB, join = "inner")
retornos <- merge(retornos, ret_EMBJ, join = "inner")
colnames(retornos) <- c("CYRE3", "BEEF3", "PETR4", "ITUB3", "EMBJ3")

# ---------------------------------------------------------------------------
# Estatísticas descritivas
# ---------------------------------------------------------------------------
estatisticas <- function(x) {
  c(
    Média = mean(x),
    Mediana = median(x),
    Desvio_Padrão = sd(x),
    Mínimo = min(x),
    Máximo = max(x)
  )
}

# Estatísticas para preços ajustados
estat_preco <- data.frame(
  CYRE3 = estatisticas(as.numeric(CYRE)),
  BEEF3 = estatisticas(as.numeric(BEEF)),
  PETR4 = estatisticas(as.numeric(PETR)),
  ITUB3 = estatisticas(as.numeric(ITUB)),
  EMBJ3 = estatisticas(as.numeric(EMBJ))
)

# Estatísticas para retornos diários
estat_retorno <- data.frame(
  CYRE3 = estatisticas(as.numeric(ret_CYRE)),
  BEEF3 = estatisticas(as.numeric(ret_BEEF)),
  PETR4 = estatisticas(as.numeric(ret_PETR)),
  ITUB3 = estatisticas(as.numeric(ret_ITUB)),
  EMBJ3 = estatisticas(as.numeric(ret_EMBJ))
)

print("Estatísticas Descritivas - Preços Ajustados (2022-2024, Frequência Diária)")
print(estat_preco)
print("Estatísticas Descritivas - Retornos Diários (2022-2024, Frequência Diária)")
print(estat_retorno)

# ---------------------------------------------------------------------------
# Sharpe individual de cada ativo
# ---------------------------------------------------------------------------
# Alinhar a SELIC com as datas dos retornos
selic_alinhada_CYRE <- selic_diaria[index(ret_CYRE)]
selic_alinhada_BEEF <- selic_diaria[index(ret_BEEF)]
selic_alinhada_PETR <- selic_diaria[index(ret_PETR)]
selic_alinhada_ITUB <- selic_diaria[index(ret_ITUB)]
selic_alinhada_EMBJ <- selic_diaria[index(ret_EMBJ)]

# Excesso de retorno
excesso_CYRE <- ret_CYRE - selic_alinhada_CYRE
excesso_BEEF <- ret_BEEF - selic_alinhada_BEEF
excesso_PETR <- ret_PETR - selic_alinhada_PETR
excesso_ITUB <- ret_ITUB - selic_alinhada_ITUB
excesso_EMBJ <- ret_EMBJ - selic_alinhada_EMBJ

# Cálculo do Sharpe para CYRE
media_ret_CYRE <- mean(ret_CYRE, na.rm = TRUE)
media_selic_CYRE <- mean(selic_alinhada_CYRE, na.rm = TRUE)
desvio_CYRE <- sd(ret_CYRE, na.rm = TRUE)
sharpe_CYRE <- (media_ret_CYRE - media_selic_CYRE) / desvio_CYRE
sharpe_CYRE

# Cálculo do Sharpe para BEEF
media_ret_BEEF <- mean(ret_BEEF, na.rm = TRUE)
media_selic_BEEF <- mean(selic_alinhada_BEEF, na.rm = TRUE)
desvio_BEEF <- sd(ret_BEEF, na.rm = TRUE)
sharpe_BEEF <- (media_ret_BEEF - media_selic_BEEF) / desvio_BEEF
sharpe_BEEF

# Cálculo do Sharpe para Petrobras
media_ret_PETR <- mean(ret_PETR, na.rm = TRUE)
media_selic_PETR <- mean(selic_alinhada_PETR, na.rm = TRUE)
desvio_PETR <- sd(ret_PETR, na.rm = TRUE)
sharpe_PETR <- (media_ret_PETR - media_selic_PETR) / desvio_PETR
sharpe_PETR

# Cálculo do Sharpe para Itaú
media_ret_ITUB <- mean(ret_ITUB, na.rm = TRUE)
media_selic_ITUB <- mean(selic_alinhada_ITUB, na.rm = TRUE)
desvio_ITUB <- sd(ret_ITUB, na.rm = TRUE)
sharpe_ITUB <- (media_ret_ITUB - media_selic_ITUB) / desvio_ITUB
sharpe_ITUB

# Cálculo do Sharpe para EMBJ
media_ret_EMBJ <- mean(ret_EMBJ, na.rm = TRUE)
media_selic_EMBJ <- mean(selic_alinhada_EMBJ, na.rm = TRUE)
desvio_EMBJ <- sd(ret_EMBJ, na.rm = TRUE)
sharpe_EMBJ <- (media_ret_EMBJ - media_selic_EMBJ) / desvio_EMBJ
sharpe_EMBJ

# ---------------------------------------------------------------------------
# Matriz de correlação (retornos)
# ---------------------------------------------------------------------------
matriz_correlacao <- cor(retornos, use = "complete.obs")
print("Matriz de Correlação entre Retornos Diários (2024)")
print(round(matriz_correlacao, 3))

# ---------------------------------------------------------------------------
# Passo 1: carteira tangente (analítica)
# ---------------------------------------------------------------------------
selic_alinhada <- selic_diaria[index(retornos)]
retornos_excesso <- sweep(retornos, 1, selic_alinhada, "-")
retornos_medios_excesso <- colMeans(retornos_excesso, na.rm = TRUE)
cov_matrix <- cov(retornos, use = "complete.obs")

# Pesos ótimos da carteira tangente
# Retorno, risco e Sharpe da carteira tangente
# Obs.: essa fórmula fechada não restringe os pesos a serem não-negativos,
# ou seja, permite venda a descoberto (short selling). Se a carteira precisar
# ser "long only", é necessário otimizar com restrições (ex.: pacote quadprog).
w_tangente <- solve(cov_matrix) %*% retornos_medios_excesso
w_tangente <- w_tangente / sum(w_tangente)  # Normalizar para soma 1

ret_tangente <- as.numeric(t(w_tangente) %*% colMeans(retornos, na.rm = TRUE))  # retorno bruto
risco_tangente <- sqrt(as.numeric(t(w_tangente) %*% cov_matrix %*% w_tangente))
rf <- mean(selic_alinhada, na.rm = TRUE)
sharpe_tangente <- (ret_tangente - rf) / risco_tangente

print("Pesos da carteira tangente (analítica):")
print(round(w_tangente, 4))
cat("Retorno esperado da tangente:", round(ret_tangente, 5), "\n")
cat("Risco da tangente:", round(risco_tangente, 5), "\n")
cat("Índice de Sharpe da tangente:", round(sharpe_tangente, 5), "\n")

# Série de retorno diário da carteira tangente, usada mais adiante no cálculo dos betas.
ret_carteira_tangente <- xts(
  rowSums(sweep(retornos, 2, as.numeric(w_tangente), `*`)),
  order.by = index(retornos)
)

# ---------------------------------------------------------------------------
# Passo 2: simulação de portfólios (fronteira média-variância)
# ---------------------------------------------------------------------------
n_sim <- 30000
set.seed(42)
pesos <- matrix(runif(n_sim * 5), ncol = 5)
pesos <- pesos / rowSums(pesos)  # Normalizar para somar 1

# Retorno e risco de cada carteira simulada
retornos_medios <- colMeans(retornos, na.rm = TRUE)
retorno_port <- pesos %*% retornos_medios
risco_port <- apply(pesos, 1, function(w) sqrt(t(w) %*% cov_matrix %*% w))

# SELIC média como retorno livre de risco
rf <- mean(selic_alinhada, na.rm = TRUE)

# Índice de Sharpe de cada carteira simulada
sharpe <- (retorno_port - rf) / risco_port

# Carteira tangente simulada (maior Sharpe)
idx_max_sharpe <- which.max(sharpe)

# ---------------------------------------------------------------------------
# Carteira de Mínima Variância (GMV)
# ---------------------------------------------------------------------------
ones <- rep(1, ncol(retornos))
w_gmv <- solve(cov_matrix) %*% ones
w_gmv <- w_gmv / sum(w_gmv)

ret_gmv <- as.numeric(t(w_gmv) %*% colMeans(retornos, na.rm = TRUE))
risco_gmv <- sqrt(as.numeric(t(w_gmv) %*% cov_matrix %*% w_gmv))

# ---------------------------------------------------------------------------
# Gráfico: fronteira média-variância
# ---------------------------------------------------------------------------
plot(risco_port, retorno_port, col = rgb(0.2, 0.5, 0.8, 0.4), pch = 16,
     xlab = "Risco (Desvio Padrão)", ylab = "Retorno Esperado",
     main = "Fronteira Média-Variância e Curva de Mercado",
     xlim = c(0.005, 0.03),
     ylim = c(0, 0.0015))

# Carteira com maior índice de Sharpe (simulada)
points(risco_port[idx_max_sharpe], retorno_port[idx_max_sharpe], col = "red", pch = 19, cex = 1.5)

# Linha de mercado de capitais (CAL)
abline(a = rf, b = sharpe[idx_max_sharpe], col = "darkgreen", lwd = 2, lty = 2)

# Ponto da carteira de mínima variância
points(risco_gmv, ret_gmv, col = "purple", pch = 15, cex = 1.5)
text(risco_gmv, ret_gmv, labels = "Mínima Variância", pos = 3, col = "purple", cex = 0.7)

# Ativos individuais
retornos_ativos <- retornos
retornos_medios_individuais <- colMeans(retornos_ativos, na.rm = TRUE)
riscos_individuais <- apply(retornos_ativos, 2, sd, na.rm = TRUE)
points(riscos_individuais, retornos_medios_individuais, col = "black", pch = 18, cex = 1.2)
text(riscos_individuais, retornos_medios_individuais,
     labels = names(retornos_ativos), pos = 1, cex = 0.7, col = "black")

legend("bottomright",
       legend = c("Carteiras Simuladas", "Tangente (Simulação)", "Curva de Mercado (CAL)", "Mínima Variância"),
       col = c(rgb(0.2, 0.5, 0.8, 0.6), "red", "darkgreen", "purple"),
       pch = c(16, 19, NA, 15), lty = c(NA, NA, 2, NA),
       pt.cex = c(1, 1.5, NA, 1.5), lwd = c(NA, NA, 2, NA),
       cex = 0.8)

# ---------------------------------------------------------------------------
# CAPM: betas em relação à carteira tangente
# ---------------------------------------------------------------------------
ret_excesso_tangente <- ret_carteira_tangente - selic_diaria[index(ret_carteira_tangente)]

# O merge explícito abaixo garante que ret_ativo e ret_mercado fiquem
# alinhados por data antes da regressão
calcular_beta <- function(ret_ativo, ret_mercado) {
  excesso_ativo <- ret_ativo - selic_diaria[index(ret_ativo)]
  dados_reg <- merge(excesso_ativo, ret_mercado, join = "inner")
  colnames(dados_reg) <- c("excesso_ativo", "excesso_mercado")
  modelo <- lm(excesso_ativo ~ excesso_mercado, data = dados_reg)
  coef(modelo)[2]
}

beta_BEEF <- calcular_beta(ret_BEEF, ret_excesso_tangente)
beta_CYRE <- calcular_beta(ret_CYRE, ret_excesso_tangente)
beta_PETR <- calcular_beta(ret_PETR, ret_excesso_tangente)
beta_ITUB <- calcular_beta(ret_ITUB, ret_excesso_tangente)
beta_EMBJ <- calcular_beta(ret_EMBJ, ret_excesso_tangente)

cat("Beta - BEEF3:", round(beta_BEEF, 4), "\n")
cat("Beta - CYRE3:", round(beta_CYRE, 4), "\n")
cat("Beta - PETR4:", round(beta_PETR, 4), "\n")
cat("Beta - ITUB3:", round(beta_ITUB, 4), "\n")
cat("Beta - EMBJ3:", round(beta_EMBJ, 4), "\n")

# ---------------------------------------------------------------------------
# Matriz de correlação (heatmap)
# ---------------------------------------------------------------------------
cor_matrix <- cor(retornos, use = "pairwise.complete.obs")

ggcorrplot(cor_matrix, method = "square", type = "full", lab = TRUE,
           colors = c("blue", "white", "#00E5FF"),
           title = "Matriz de Correlação dos Retornos",
           ggtheme = ggplot2::theme_minimal())
