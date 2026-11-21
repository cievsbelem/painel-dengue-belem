# ==============================================================================
# GERADOR DE BASE DE DADOS SINTÉTICA (DEMONSTRAÇÃO / LGPD COMPLIANT)
# Painel Epidemiológico da Dengue - Belém/PA
# ==============================================================================
# Este script gera uma base sintética e 100% fictícia para fins de teste,
# demonstração pública e desenvolvimento sem exposição de dados pessoais
# ou dados sensíveis de saúde protegidos pela LGPD (Lei nº 13.709/2018)
# e pelo sigilo oficial do SINAN / Ministério da Saúde.
# ==============================================================================

set.seed(42)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

cat("Gerando dados sintéticos para demonstração segura...\n")

# Carrega bairros reais do mapa para consistência espacial
caminho_mapa <- "mapa_bairros_belem.rds"
bairros <- if (file.exists(caminho_mapa)) {
  mb <- readRDS(caminho_mapa)
  sort(unique(as.character(mb$nm_bairro)))
} else {
  c("MARCO", "PEDREIRA", "UMARIZAL", "GUAMA", "TERRA FIRME", "CREMACAO",
    "JURUNAS", "BATISTA CAMPOS", "NAZARE", "CANUDOS", "SAO BRAS", "TELEGRAFO")
}

# Unidades notificadoras fictícias/públicas
unidades <- c(
  "HOSPITAL REGIONAL DR ABELARDO SANTOS",
  "HPSM DR HUMBERTO MARADEI PEREIRA",
  "HPSM MARIO PINOTTI",
  "UPA DA MARAMBAIA",
  "UPA DE ICOARACI",
  "UPA DA TERRA FIRME",
  "DEVS SESMA",
  "CENTRO DE SAUDE ESCOLA DO MARCO",
  "HOSPITAL DA ORDEM TERCEIRA",
  "HOSPITAL ADVENTISTA DE BELEM"
)

# Anos simulados (inclui série de referência para canal endêmico e anos recentes)
anos_simulados <- c(2015, 2016, 2017, 2018, 2019, 2022, 2023, 2024, 2025, 2026)
n_por_ano <- c(
  "2015" = 350, "2016" = 400, "2017" = 300, "2018" = 280, "2019" = 320,
  "2022" = 380, "2023" = 310, "2024" = 550, "2025" = 450, "2026" = 350
)

registros_lista <- list()

for (ano in anos_simulados) {
  n_ano <- n_por_ano[as.character(ano)]
  semanas_possiveis <- if (ano == 2026) 1:44 else 1:52
  
  semanas <- sample(semanas_possiveis, size = n_ano, replace = TRUE,
                    prob = c(rep(1.8, min(20, length(semanas_possiveis))),
                             rep(1.0, max(0, length(semanas_possiveis) - 20))))
  
  # Datas fictícias correspondentes às semanas
  datas_inicio_ano <- as.Date(paste0(ano, "-01-01"))
  datas_notif <- datas_inicio_ano + (semanas - 1) * 7 + sample(0:6, size = n_ano, replace = TRUE)
  datas_sintomas <- datas_notif - sample(1:7, size = n_ano, replace = TRUE)
  
  classificacoes <- sample(
    c("Confirmado", "Descartado", "Em Investigação/Suspeito", "Inconclusivo"),
    size = n_ano, replace = TRUE, prob = c(0.40, 0.45, 0.12, 0.03)
  )
  
  sexos <- sample(c("Feminino", "Masculino", "Ignorado"), size = n_ano, replace = TRUE, prob = c(0.53, 0.46, 0.01))
  racas <- sample(c("Parda", "Branca", "Preta", "Amarela", "Indígena", "Ignorado/Sem Info"),
                  size = n_ano, replace = TRUE, prob = c(0.70, 0.15, 0.08, 0.02, 0.01, 0.04))
  
  idades <- pmin(95, pmax(0, round(rnorm(n_ano, mean = 34, sd = 18))))
  
  evolucoes <- case_when(
    classificacoes == "Descartado" ~ "Ignorado/Investigação",
    classificacoes == "Confirmado" ~ sample(c("Cura", "Óbito por dengue", "Óbito por outras causas"),
                                            size = n_ano, replace = TRUE, prob = c(0.985, 0.010, 0.005)),
    TRUE ~ "Ignorado/Investigação"
  )
  
  criterios <- case_when(
    classificacoes == "Confirmado" ~ sample(c("Laboratorial", "Clínico-Epidemiológico"), size = n_ano, replace = TRUE, prob = c(0.65, 0.35)),
    TRUE ~ "Em Investigação"
  )
  
  sorotipos <- case_when(
    classificacoes == "Confirmado" & criterios == "Laboratorial" ~ sample(
      c("DENV 1", "DENV 2", "Não Realizado/Sem Info"), size = n_ano, replace = TRUE, prob = c(0.40, 0.30, 0.30)
    ),
    TRUE ~ "Não Realizado/Sem Info"
  )
  
  bairros_sel <- sample(bairros, size = n_ano, replace = TRUE)
  unidades_sel <- sample(unidades, size = n_ano, replace = TRUE)
  
  hashes_mock <- paste0("DEMO_", ano, "_", sprintf("%05d", seq_len(n_ano)))
  
  df_ano <- data.frame(
    ID_NOTIFICACAO_HASH     = hashes_mock,
    DT_NOTIFICACAO          = as.character(datas_notif),
    ANO_NOTIFICACAO         = as.numeric(ano),
    SEMANA_NOTIFICACAO      = as.numeric(semanas),
    DT_SINTOMAS             = as.character(datas_sintomas),
    SEMANA_SINTOMAS         = as.numeric(semanas),
    SEXO                    = sexos,
    IDADE_ANOS              = as.numeric(idades),
    GESTANTE                = "8",
    RACA_COR                = racas,
    ESCOLARIDADE            = "Não Informado",
    CD_MUNICIPIO_NOT        = "150140",
    CD_MUNICIPIO_RES        = "150140",
    IS_RESIDENTE_BELEM      = TRUE,
    BAIRRO_RES              = bairros_sel,
    CLASSIFICACAO_FINAL     = classificacoes,
    CRITERIO_DIAGNOSTICO    = criterios,
    EVOLUCAO_CASO           = evolucoes,
    HOSPITALIZACAO          = sample(c("Sim", "Não"), size = n_ano, replace = TRUE, prob = c(0.12, 0.88)),
    DT_HOSPITALIZACAO       = NA_character_,
    CD_SETOR                = NA_real_,
    PRECISAO_GEOCODIFICACAO = "Bairro",
    COD_MUN_RES             = "150140",
    DT_NOTIFIC              = datas_notif,
    ANO_NOTIF               = as.numeric(ano),
    SEMANA_EPID             = as.numeric(semanas),
    CS_SEXO_VAL             = substr(sexos, 1, 1),
    CS_SEXO                 = sexos,
    CS_RACA_VAL             = "4",
    CS_RACA                 = racas,
    CLASSI_FIN_VAL          = case_when(classificacoes == "Confirmado" ~ "10", classificacoes == "Descartado" ~ "5", TRUE ~ "0"),
    CLASSI_FIN              = classificacoes,
    EVOLUCAO_VAL            = case_when(evolucoes == "Cura" ~ "1", evolucoes == "Óbito por dengue" ~ "2", TRUE ~ "9"),
    EVOLUCAO                = evolucoes,
    RESUL_SORO              = sorotipos,
    CRITERIO_FINAL          = criterios,
    NM_BAIRRO               = bairros_sel,
    NM_BAIRRO_MAPA          = bairros_sel,
    UNIDADE_NOTIF           = unidades_sel,
    ORIGEM_DADO             = "dados_sinteticos_demo_lgpd",
    TP_SOROTIPO             = sorotipos,
    tp_sorotipo             = sorotipos,
    stringsAsFactors        = FALSE
  )
  
  registros_lista[[as.character(ano)]] <- df_ano
}

df_sintetico <- bind_rows(registros_lista)

caminho_saida <- "dados_historico_dengue_exemplo.rds"
saveRDS(df_sintetico, caminho_saida)
cat("Base sintética gerada com sucesso!\n")
cat("Arquivo salvo em:", caminho_saida, "\n")
cat("Total de linhas geradas:", nrow(df_sintetico), "\n")
cat("Anos contemplados:", paste(sort(unique(df_sintetico$ANO_NOTIF)), collapse = ", "), "\n")
cat("Em conformidade com a LGPD (Lei nº 13.709/2018): 100% dos dados são sintéticos e sem identificadores pessoais.\n")
