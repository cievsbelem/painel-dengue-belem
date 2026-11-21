# ==============================================================================
# SCRIPT DE IMPLANTAÇÃO OFICIAL NO SHINYAPPS.IO
# Painel Epidemiológico da Dengue - CIEVS Belém / SESMA
# ==============================================================================

# 1. Carregar pacote rsconnect
options(timeout = 180)
options(rsconnect.http.timeout = 180)
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}
library(rsconnect)

# 2. Configurar credenciais do Shinyapps.io
# Obtenha estas informações em: https://www.shinyapps.io/admin/#/tokens
# Substitua os valores abaixo com as credenciais da sua conta:

# rsconnect::setAccountInfo(
#   name   = "<SEU_NOME_DE_CONTA>",
#   token  = "<SEU_TOKEN>",
#   secret = "<SEU_SECRET>"
# )

# 3. Publicar aplicação
message("Iniciando publicação do Painel da Dengue no shinyapps.io...")

arquivos_www <- list.files("www", full.names = TRUE)
arquivos_deploy <- c(
  "dengue.R",
  "app.R",
  "dados_historico_dengue.rds",
  "dados_historico_dengue_exemplo.rds",
  "mapa_bairros_belem.rds",
  "mapa_distritos_belem.rds",
  "mapa_ilhas_belem.rds",
  "estabelecimentos_pa.rds",
  "notificacoes_estabelecimentos.rds",
  arquivos_www
)

# Filtra apenas os arquivos que existem
arquivos_existentes <- arquivos_deploy[file.exists(arquivos_deploy)]

sucesso <- FALSE
tentativas <- 3
for (t in 1:tentativas) {
  message(sprintf("Tentativa %d de %d...", t, tentativas))
  res <- try({
    rsconnect::deployApp(
      appDir = getwd(),
      appPrimaryDoc = "dengue.R",
      appName = "painel-dengue-belem",
      appTitle = "Painel Epidemiológico da Dengue - CIEVS Belém",
      appFiles = arquivos_existentes,
      forceUpdate = TRUE
    )
  }, silent = FALSE)
  if (!inherits(res, "try-error")) {
    sucesso <- TRUE
    break
  }
  message("Aguardando 5 segundos antes de tentar novamente...")
  Sys.sleep(5)
}

if (sucesso) {
  message("Publicação concluída com sucesso! Verifique o link público gerado.")
} else {
  stop("Falha na publicação após tentativas.")
}
