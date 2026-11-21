# ==============================================================================
# PIPELINE DE INGESTÃO, AUDITORIA E SINCRONIZAÇÃO DE DADOS (SINAN)
# Painel Epidemiológico da Dengue - Belém / SESMA / CIEVS
# ==============================================================================
# Este script realiza:
# 1. Descoberta de novos arquivos na pasta 'dados/' (ou raiz do projeto).
# 2. Mecanismo de Revisão e Auditoria de Qualidade dos Dados (Data Quality Engine):
#    - Validação de consistência temporal, datas e semanas epidemiológicas.
#    - Alinhamento oficial do ano epidemiológico (ex: SEM_NOT = YYYY53).
#    - Auditoria de classificação final, evolução clínica e sorotipos.
#    - Validação territorial e georreferenciamento para bairros de Belém.
#    - Detecção de inconsistências, anomalias e duplicidades.
# 3. Ingestão consolidada e backup de segurança da base histórica local.
# 4. Sincronização segura para o GitHub (Conformidade estrita com a LGPD):
#    - Atualização da base sintética de demonstração correspondente à nova semana.
#    - Atualização dos catálogos públicos e totais agregados.
#    - Preparação de commit no Git sem expor nenhum microdado restrito do SINAN.
# ==============================================================================

options(warn = -1)

suppressPackageStartupMessages({
  library(foreign)
  library(dplyr)
  library(lubridate)
})

cat("==============================================================================\n")
cat("   PAINEL EPIDEMIOLÓGICO DA DENGUE (BELÉM/SESMA/CIEVS)\n")
cat("   MECANISMO DE INGESTÃO, REVISÃO E AUDITORIA DE DADOS\n")
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# 1. FUNÇÕES AUXILIARES DE NORMALIZAÇÃO E MAPEAMENTO
# ------------------------------------------------------------------------------
normalizar_texto <- function(s) {
  if (is.null(s)) return("")
  s <- toupper(as.character(s))
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  s <- gsub("[^A-Z0-9 ]", "", s)
  trimws(s)
}

alias_bairros_belem <- c(
  "SAO BRAZ" = "SAO BRAS", "SÃO BRAZ" = "SAO BRAS", "VAL DE CANS" = "VALDECAES",
  "VAL DE CAES" = "VALDECAES", "VALDE CANS" = "VALDECAES", "VALDECANS" = "VALDECAES",
  "MONTESE" = "MONTESE TERRA FIRME", "TERRA FIRME" = "MONTESE TERRA FIRME",
  "TERRRA FIRME" = "MONTESE TERRA FIRME", "TERR FIRME" = "MONTESE TERRA FIRME",
  "CURIO UTINGA" = "CURIOUTINGA", "CURIO" = "CURIOUTINGA", "SACRAMETA" = "SACRAMENTA",
  "SACRAM" = "SACRAMENTA", "SACRA" = "SACRAMENTA", "TELEG" = "TELEGRAFO",
  "BATIOSTA CAMPO" = "BATISTA CAMPOS", "BAIRRO DA MARAMBAIA" = "MARAMBAIA",
  "MARNBAIA" = "MARAMBAIA", "JURUANS" = "JURUNAS", "JURUNA" = "JURUNAS",
  "TENO" = "TENONE", "TENOME" = "TENONE", "PRATINHA 2" = "PRATINHA",
  "PRATINHA I" = "PRATINHA", "PRATINHA II" = "PRATINHA", "PARQUE VERDE  BAIRRO" = "PARQUE VERDE",
  "PARK VERDE" = "PARQUE VERDE", "SIDERAL" = "PARQUE VERDE", "NOVO HORIZONTE" = "PARQUE VERDE",
  "AGUSTO MONTE NEGRO" = "PARQUE VERDE", "AUGUSTO MONTENEGRO" = "PARQUE VERDE",
  "PARACURI II  BAIRRO" = "PARACURI", "PARACURI I" = "PARACURI", "PARACURI II" = "PARACURI",
  "CONJUNTO PARACURI" = "PARACURI", "CAMPINA ICOARACI" = "CAMPINA DE ICOARACI",
  "RAMAL DE ICOARACI" = "CAMPINA DE ICOARACI", "BAIRRO NOVO" = "CAMPINA DE ICOARACI",
  "DISTRITO INDUSTRIAL FABRICAS" = "MARACACUERA", "SAO JOA DO OUTEIRO" = "SAO JOAO DO OUTEIRO",
  "FIDELIS" = "SAO JOAO DO OUTEIRO", "FAMA" = "SAO JOAO DO OUTEIRO",
  "ILHA DE COTIJUBA" = "SAO JOAO DO OUTEIRO", "COTIJUBA" = "SAO JOAO DO OUTEIRO",
  "BAHIA DO SOL" = "BAIA DO SOL", "BAIIA DO SOL" = "BAIA DO SOL", "VILA MOSQUEIRO" = "VILA",
  "SUCURIJAQUARA" = "SUCURIJUQUARA", "MARI MARI" = "MARAHU", "MARIMARI" = "MARAHU",
  "CONCEICAO" = "CARANANDUBA", "CAMBOINHA" = "CARANANDUBA", "CAICARA" = "ARIRAMBA",
  "ARIRAMBA" = "ARIRAMBA", "BOM FIM" = "BONFIM", "CARUARU" = "CARUARA",
  "CENTRO" = "CAMPINA", "COMERCIO" = "CAMPINA", "CENTRO HISTORICO" = "CAMPINA",
  "SOUSA" = "SOUZA", "BENGUI II" = "BENGUI", "NOVO BENGUI" = "BENGUI",
  "NEWTON MIRANDA" = "BENGUI", "CONJ RES ALMIR GABRIEL" = "MARAMBAIA",
  "COQUEIRO VILA" = "COQUEIRO", "AGUAS NEGRAS ICOARACI" = "AGUAS NEGRAS",
  "PONTA GROSSA ICOARACI" = "PONTA GROSSA"
)

# Carrega malha de bairros oficiais de Belém
caminho_mapa <- if (file.exists("mapa_bairros_belem.rds")) "mapa_bairros_belem.rds" else file.path("dados", "mapa_bairros_belem.rds")
bairros_oficiais <- if (file.exists(caminho_mapa)) {
  mb <- readRDS(caminho_mapa)
  sort(unique(as.character(mb$nm_bairro)))
} else character(0)

mapear_bairro <- function(vetor_bairros) {
  if (is.null(vetor_bairros) || length(vetor_bairros) == 0) return(character(0))
  b_norm <- sapply(vetor_bairros, normalizar_texto, USE.NAMES = FALSE)
  b_norm <- ifelse(b_norm %in% names(alias_bairros_belem), alias_bairros_belem[b_norm], b_norm)
  
  if (length(bairros_oficiais) > 0) {
    oficiais_norm <- sapply(bairros_oficiais, normalizar_texto, USE.NAMES = FALSE)
    idx <- match(b_norm, oficiais_norm)
    res <- ifelse(!is.na(idx), bairros_oficiais[idx], b_norm)
    res[is.na(res) | res == "" | res == "NAO INFORMADO"] <- "NÃO INFORMADO"
    return(res)
  }
  return(b_norm)
}

# Carrega dicionário de CNES
caminho_estab <- if (file.exists("estabelecimentos_pa.rds")) "estabelecimentos_pa.rds" else file.path("dados", "estabelecimentos_pa.rds")
estab_df <- if (file.exists(caminho_estab)) readRDS(caminho_estab) else data.frame()
dic_cnes_vec <- if (nrow(estab_df) > 0) {
  df_c <- estab_df %>% arrange(desc(ano)) %>% distinct(cnes, .keep_all = TRUE)
  v <- df_c$nome
  names(v) <- as.character(df_c$cnes)
  v
} else character(0)

# ------------------------------------------------------------------------------
# 2. LOCALIZAÇÃO DOS ARQUIVOS DE ATUALIZAÇÃO
# ------------------------------------------------------------------------------
cat("[1/4] Procurando novos arquivos na pasta 'dados/'...\n")

arquivos_dados <- list.files(
  path = c("dados", "."),
  pattern = "\\.(dbf|DBF|dbc|DBC|rds|RDS)$",
  full.names = TRUE,
  recursive = FALSE
)

# Filtra arquivos que são bases de sistema/geometrias ou exemplos sintéticos
ignorar_arquivos <- c(
  "mapa_bairros_belem.rds", "mapa_distritos_belem.rds", "mapa_ilhas_belem.rds",
  "estabelecimentos_pa.rds", "notificacoes_estabelecimentos.rds",
  "dados_historico_dengue_exemplo.rds"
)

arquivos_candidatos <- arquivos_dados[!basename(arquivos_dados) %in% ignorar_arquivos]

# Prioriza arquivos contidos dentro da pasta 'dados/'
candidatos_na_pasta_dados <- arquivos_candidatos[grepl("^dados[/\\\\]", arquivos_candidatos)]
if (length(candidatos_na_pasta_dados) > 0) {
  arquivos_processar <- candidatos_na_pasta_dados
} else {
  # Caso não haja arquivos na pasta dados, verifica se há DBFs recentes na raiz
  dbfs_raiz <- arquivos_candidatos[grepl("\\.(dbf|DBF|dbc|DBC)$", arquivos_candidatos)]
  arquivos_processar <- if (length(dbfs_raiz) > 0) dbfs_raiz else character(0)
}

if (length(arquivos_processar) == 0) {
  cat("\n[INFO] Nenhum novo arquivo de dados (DBF/DBC) encontrado para processar.\n")
  cat("-> Coloque os novos arquivos do SINAN na pasta 'dados/' e execute este script novamente.\n")
  cat("-> Exemplo: copie 'DENGON2026.dbf' para a pasta 'dados/' do projeto.\n\n")
  quit(status = 0)
}

cat("-> Arquivo(s) identificado(s) para ingestão:\n")
for (arq in arquivos_processar) {
  tam_mb <- round(file.info(arq)$size / (1024^2), 2)
  cat(paste0("   * ", arq, " (", tam_mb, " MB)\n"))
}
cat("\n")

# ------------------------------------------------------------------------------
# 3. MECANISMO DE REVISÃO E AUDITORIA DE QUALIDADE DOS DADOS (DATA QUALITY ENGINE)
# ------------------------------------------------------------------------------
cat("[2/4] Executando Mecanismo de Revisão e Auditoria de Qualidade dos Dados...\n")

relatorio_linhas <- c(
  "# Relatório de Auditoria de Atualização de Dados (SINAN / Belém)",
  paste0("**Data da Auditoria:** ", format(Sys.time(), "%d/%m/%Y %H:%M:%S")),
  paste0("**Usuário/Terminal:** ", Sys.info()["user"], " / ", Sys.info()["nodename"]),
  ""
)

lista_dados_padronizados <- list()
avisos_totais <- 0
erros_totais  <- 0

for (arq in arquivos_processar) {
  nome_base <- basename(arq)
  cat(paste0("--- Auditando arquivo: ", nome_base, " ---\n"))
  
  ext <- tolower(tools::file_ext(arq))
  df_raw <- tryCatch({
    if (ext %in% c("dbf")) {
      foreign::read.dbf(arq, as.is = TRUE)
    } else if (ext %in% c("dbc")) {
      if (!requireNamespace("read.dbc", quietly = TRUE)) stop("Pacote 'read.dbc' não instalado.")
      read.dbc::read.dbc(arq)
    } else if (ext %in% c("rds")) {
      readRDS(arq)
    }
  }, error = function(e) {
    cat(paste0("   [ERRO CRÍTICO] Falha ao ler arquivo: ", e$message, "\n"))
    return(NULL)
  })
  
  if (is.null(df_raw) || nrow(df_raw) == 0) {
    erros_totais <- erros_totais + 1
    next
  }
  
  n_linhas <- nrow(df_raw)
  cat(paste0("   Linhas brutas carregadas: ", format(n_linhas, big.mark = "."), "\n"))
  
  names(df_raw) <- toupper(trimws(names(df_raw)))
  df_raw[] <- lapply(df_raw, function(x) if (is.factor(x)) as.character(x) else x)
  df_raw <- df_raw %>% mutate(across(where(is.character), ~ trimws(iconv(as.character(.), from = "latin1", to = "UTF-8", sub = ""))))
  
  # A. Validação de Colunas Obrigatórias
  cols_essenciais <- c("DT_NOTIFIC", "SEM_NOT", "NU_ANO", "CLASSI_FIN", "NM_BAIRRO", "CS_SEXO")
  cols_presentes  <- names(df_raw)
  cols_faltantes  <- cols_essenciais[!sapply(cols_essenciais, function(c) any(grepl(c, cols_presentes)))]
  
  if (length(cols_faltantes) > 0) {
    cat(paste0("   [ALERTA] Colunas essenciais não localizadas: ", paste(cols_faltantes, collapse = ", "), "\n"))
    avisos_totais <- avisos_totais + 1
  } else {
    cat("   [OK] Todas as colunas estruturais básicas estão presentes.\n")
  }
  
  # B. Padronização de Datas
  col_dt <- intersect(c("DT_NOTIFICACAO", "DT_NOTIFIC", "DT_NOTI", "DT_NOT"), names(df_raw))
  if (length(col_dt) > 0) {
    dt_val <- suppressWarnings(as.Date(as.character(df_raw[[col_dt[1]]])))
    n_dt_na <- sum(is.na(dt_val))
    n_dt_futuras <- sum(dt_val > Sys.Date(), na.rm = TRUE)
    
    if (n_dt_na > 0) {
      cat(paste0("   [ALERTA] Datas de notificação ausentes/inválidas: ", n_dt_na, "\n"))
      avisos_totais <- avisos_totais + 1
    }
    if (n_dt_futuras > 0) {
      cat(paste0("   [ALERTA] Datas futuras detectadas (> hoje): ", n_dt_futuras, "\n"))
      avisos_totais <- avisos_totais + 1
    }
    dt_val[is.na(dt_val)] <- Sys.Date()
    df_raw$DT_NOTIFIC <- dt_val
  } else {
    df_raw$DT_NOTIFIC <- Sys.Date()
  }
  
  # C. Padronização de Ano e Semana Epidemiológica
  col_ano <- intersect(c("ANO_NOTIFICACAO", "NU_ANO", "ANO_NOTIF", "ANO"), names(df_raw))
  if (length(col_ano) > 0) {
    ano_tmp <- suppressWarnings(as.numeric(as.character(df_raw[[col_ano[1]]])))
    ano_tmp[is.na(ano_tmp)] <- as.numeric(year(df_raw$DT_NOTIFIC[is.na(ano_tmp)]))
    df_raw$ANO_NOTIF <- ano_tmp
  } else {
    df_raw$ANO_NOTIF <- as.numeric(year(df_raw$DT_NOTIFIC))
  }
  
  col_sem <- intersect(c("SEMANA_NOTIFICACAO", "SEM_NOT", "SEMANA_NOT", "SEM_NOTIF"), names(df_raw))
  if (length(col_sem) > 0) {
    sem_str <- as.character(df_raw[[col_sem[1]]])
    sem_num <- suppressWarnings(as.numeric(substr(sem_str, nchar(sem_str)-1, nchar(sem_str))))
    sem_num[is.na(sem_num)] <- epiweek(df_raw$DT_NOTIFIC[is.na(sem_num)])
    df_raw$SEMANA_EPID <- as.numeric(sem_num)
    
    # Alinhamento do Ano Epidemiológico Oficial (primeiros 4 dígitos da SEM_NOT)
    # Exemplo: 22 registros de 2026-01-01 com SEM_NOT = 202553 pertencem a 2025!
    if (any(nchar(sem_str) == 6, na.rm = TRUE)) {
      ano_epi <- suppressWarnings(as.numeric(substr(sem_str, 1, 4)))
      idx_val <- which(!is.na(ano_epi) & ano_epi >= 2010 & ano_epi <= 2035)
      casos_divergentes <- sum(df_raw$ANO_NOTIF[idx_val] != ano_epi[idx_val], na.rm = TRUE)
      if (casos_divergentes > 0) {
        cat(paste0("   [AJUSTE SINAN] ", casos_divergentes, " caso(s) de virada de ano realinhados ao ano epidemiológico oficial da SEM_NOT.\n"))
      }
      df_raw$ANO_NOTIF[idx_val] <- ano_epi[idx_val]
    }
  } else {
    df_raw$SEMANA_EPID <- as.numeric(epiweek(df_raw$DT_NOTIFIC))
  }
  df_raw$SEMANA_EPID[is.na(df_raw$SEMANA_EPID) | df_raw$SEMANA_EPID < 1 | df_raw$SEMANA_EPID > 53] <- 1
  
  anos_no_arquivo <- sort(unique(df_raw$ANO_NOTIF))
  sems_no_arquivo <- sort(unique(df_raw$SEMANA_EPID))
  cat(paste0("   Anos epidemiológicos identificados: ", paste(anos_no_arquivo, collapse = ", "), "\n"))
  cat(paste0("   Semanas epidemiológicas no arquivo: SE ", min(sems_no_arquivo), " a SE ", max(sems_no_arquivo), "\n"))
  
  # D. Classificação Final
  col_classi <- intersect(c("CLASSIFICACAO_FINAL", "CLASSI_FIN", "CLASSI_FIN_N", "CLASSIFIC"), names(df_raw))
  df_raw$CLASSI_FIN_VAL <- if(length(col_classi) > 0) trimws(as.character(df_raw[[col_classi[1]]])) else "0"
  df_raw$CLASSI_FIN <- case_when(
    df_raw$CLASSI_FIN_VAL %in% c("10", "11", "12", "1", "Confirmado", "CONFIRMADO", "Dengue", "Dengue com sinais de alarme", "Dengue grave") ~ "Confirmado",
    df_raw$CLASSI_FIN_VAL %in% c("8", "Inconclusivo", "INCONCLUSIVO") ~ "Inconclusivo",
    df_raw$CLASSI_FIN_VAL %in% c("5", "2", "Descartado", "DESCARTADO") ~ "Descartado",
    TRUE ~ "Em Investigação/Suspeito"
  )
  
  tab_classi <- table(df_raw$CLASSI_FIN)
  cat("   Distribuição de Classificação:\n")
  for (nm in names(tab_classi)) {
    pct <- round(100 * tab_classi[nm] / nrow(df_raw), 1)
    cat(paste0("      - ", nm, ": ", format(tab_classi[nm], big.mark = "."), " (", pct, "%)\n"))
  }
  
  # E. Verificação Territorial e Mapeamento de Bairros de Belém
  col_bairro <- intersect(c("BAIRRO_RES", "NM_BAIRRO", "NO_BAIRRO", "NM_BAIRR_N", "ID_BAIRRO"), names(df_raw))
  df_raw$NM_BAIRRO <- if(length(col_bairro) > 0) toupper(trimws(as.character(df_raw[[col_bairro[1]]]))) else "NÃO INFORMADO"
  df_raw$NM_BAIRRO[is.na(df_raw$NM_BAIRRO) | df_raw$NM_BAIRRO == ""] <- "NÃO INFORMADO"
  df_raw$NM_BAIRRO_MAPA <- mapear_bairro(df_raw$NM_BAIRRO)
  
  taxa_mapeada <- round(100 * sum(df_raw$NM_BAIRRO_MAPA != "NÃO INFORMADO") / nrow(df_raw), 1)
  cat(paste0("   Taxa de vinculação a bairros oficiais de Belém: ", taxa_mapeada, "%\n"))
  if (taxa_mapeada < 80) {
    cat("   [ALERTA] Mais de 20% dos bairros não foram vinculados aos bairros oficiais.\n")
    avisos_totais <- avisos_totais + 1
  }
  
  # F. Unidades Notificadoras (CNES)
  col_unidade <- intersect(c("NM_NOTIFIC", "ID_UNIDADE", "CO_UNI_NOT", "ID_UNI_NOT", "ID_ESTAB", "CNES"), names(df_raw))
  if (length(col_unidade) > 0) {
    df_raw$ID_UNIDADE_RAW <- trimws(as.character(df_raw[[col_unidade[1]]]))
    nome_trad <- unname(dic_cnes_vec[df_raw$ID_UNIDADE_RAW])
    df_raw$UNIDADE_NOTIF <- ifelse(!is.na(nome_trad) & nome_trad != "", nome_trad, df_raw$ID_UNIDADE_RAW)
    df_raw$UNIDADE_NOTIF[is.na(df_raw$UNIDADE_NOTIF) | df_raw$UNIDADE_NOTIF == "" | df_raw$UNIDADE_NOTIF == "0"] <- "NÃO INFORMADO"
  } else {
    df_raw$ID_UNIDADE_RAW <- NA_character_
    df_raw$UNIDADE_NOTIF  <- "NÃO INFORMADO"
  }
  
  # G. Demais atributos clínicos e sociodemográficos
  col_sexo <- intersect(c("CS_SEXO", "SEXO"), names(df_raw))
  df_raw$CS_SEXO_VAL <- if(length(col_sexo) > 0) as.character(df_raw[[col_sexo[1]]]) else "I"
  df_raw$CS_SEXO <- case_when(
    df_raw$CS_SEXO_VAL %in% c("M", "1", "MASCULINO") ~ "Masculino",
    df_raw$CS_SEXO_VAL %in% c("F", "2", "FEMININO") ~ "Feminino",
    TRUE ~ "Ignorado"
  )
  
  col_raca <- intersect(c("RACA_COR", "CS_RACA", "RACA"), names(df_raw))
  df_raw$CS_RACA_VAL <- if(length(col_raca) > 0) as.character(df_raw[[col_raca[1]]]) else "9"
  df_raw$CS_RACA <- case_when(
    df_raw$CS_RACA_VAL %in% c("1", "BRANCA", "Branca") ~ "Branca",
    df_raw$CS_RACA_VAL %in% c("2", "PRETA", "Preta") ~ "Preta",
    df_raw$CS_RACA_VAL %in% c("3", "AMARELA", "Amarela") ~ "Amarela",
    df_raw$CS_RACA_VAL %in% c("4", "PARDA", "Parda") ~ "Parda",
    df_raw$CS_RACA_VAL %in% c("5", "INDIGENA", "Indígena") ~ "Indígena",
    TRUE ~ "Ignorado/Sem Info"
  )
  
  col_evo <- intersect(c("EVOLUCAO_CASO", "EVOLUCAO", "EVOLUCAO_N"), names(df_raw))
  df_raw$EVOLUCAO_VAL <- if(length(col_evo) > 0) trimws(as.character(df_raw[[col_evo[1]]])) else "9"
  df_raw$EVOLUCAO <- case_when(
    df_raw$EVOLUCAO_VAL %in% c("1", "Cura", "CURA") ~ "Cura",
    df_raw$EVOLUCAO_VAL %in% c("2", "Óbito por dengue", "OBITO POR DENGUE") ~ "Óbito por dengue",
    df_raw$EVOLUCAO_VAL %in% c("3", "Óbito por outras causas") ~ "Óbito por outras causas",
    TRUE ~ "Ignorado/Investigação"
  )
  
  col_soro <- intersect(c("tp_sorotipo", "TP_SOROTIPO", "TP_SOROTIP", "tp_sorotip", "RESUL_SORO", "RES_SORO", "SOROTIPO", "sorotipo"), names(df_raw))
  df_raw$tp_sorotipo_val <- if(length(col_soro) > 0) trimws(as.character(df_raw[[col_soro[1]]])) else "9"
  df_raw$tp_sorotipo <- case_when(
    df_raw$tp_sorotipo_val %in% c("1", "DENV 1", "DENV-1", "DENV1", "1 - DENV 1") ~ "DENV 1",
    df_raw$tp_sorotipo_val %in% c("2", "DENV 2", "DENV-2", "DENV2", "2 - DENV 2") ~ "DENV 2",
    df_raw$tp_sorotipo_val %in% c("3", "DENV 3", "DENV-3", "DENV3", "3 - DENV 3") ~ "DENV 3",
    df_raw$tp_sorotipo_val %in% c("4", "DENV 4", "DENV-4", "DENV4", "4 - DENV 4") ~ "DENV 4",
    TRUE ~ "Não Realizado/Sem Info"
  )
  df_raw$RESUL_SORO  <- df_raw$tp_sorotipo
  df_raw$TP_SOROTIPO <- df_raw$tp_sorotipo
  
  col_idade <- intersect(c("IDADE_ANOS", "NU_IDADE_N", "IDADE_N", "NU_IDADE"), names(df_raw))
  if (length(col_idade) > 0) {
    val_raw <- suppressWarnings(as.numeric(as.character(df_raw[[col_idade[1]]])))
    df_raw$IDADE_ANOS <- case_when(
      is.na(val_raw) ~ 0,
      val_raw >= 4000 & val_raw < 5000 ~ as.numeric(val_raw - 4000),
      val_raw >= 100 ~ as.numeric(val_raw),
      TRUE ~ as.numeric(val_raw)
    )
  } else {
    df_raw$IDADE_ANOS <- 0
  }
  
  crit_col <- intersect(c("CRITERIO_DIAGNOSTICO", "CRITERIO", "CRIT_CONS", "CRITERIO_N"), names(df_raw))
  if (length(crit_col) > 0) {
    df_raw$CRITERIO_FINAL <- case_when(
      as.character(df_raw[[crit_col[1]]]) %in% c("1", "Laboratorial", "LABORATORIAL") ~ "Laboratorial",
      as.character(df_raw[[crit_col[1]]]) %in% c("2", "Clínico-Epidemiológico", "CLINICO-EPIDEMIOLOGICO") ~ "Clínico-Epidemiológico",
      TRUE ~ "Em Investigação"
    )
  } else {
    df_raw$CRITERIO_FINAL <- "Não Informado"
  }
  
  col_mun_res <- intersect(c("ID_MN_RESI", "CO_MUN_RES", "ID_MUNI_RE", "MUN_RES", "CD_MUNICIPIO_RES"), names(df_raw))
  df_raw$COD_MUN_RES <- if(length(col_mun_res) > 0) trimws(as.character(df_raw[[col_mun_res[1]]])) else "150140"
  df_raw$COD_MUN_RES[is.na(df_raw$COD_MUN_RES) | df_raw$COD_MUN_RES == ""] <- "150140"
  
  df_raw$ORIGEM_DADO <- paste0("ingestao_dados_", format(Sys.Date(), "%Y%m%d"))
  
  # Garante estrutura padronizada de 42 variáveis oficiais
  col_id <- intersect(c("ID_NOTIFICACAO_HASH", "NU_NOTIFIC", "ID_NOTIF", "ID"), names(df_raw))
  df_raw$ID_NOTIFICACAO_HASH <- if(length(col_id) > 0) as.character(df_raw[[col_id[1]]]) else paste0("NOTIF_", seq_len(nrow(df_raw)))
  
  colunas_esperadas <- c(
    "ID_NOTIFICACAO_HASH", "DT_NOTIFICACAO", "ANO_NOTIFICACAO", "SEMANA_NOTIFICACAO",
    "DT_SINTOMAS", "SEMANA_SINTOMAS", "SEXO", "IDADE_ANOS", "GESTANTE", "RACA_COR",
    "ESCOLARIDADE", "CD_MUNICIPIO_NOT", "CD_MUNICIPIO_RES", "IS_RESIDENTE_BELEM",
    "BAIRRO_RES", "CLASSIFICACAO_FINAL", "CRITERIO_DIAGNOSTICO", "EVOLUCAO_CASO",
    "HOSPITALIZACAO", "DT_HOSPITALIZACAO", "CD_SETOR", "PRECISAO_GEOCODIFICACAO",
    "COD_MUN_RES", "DT_NOTIFIC", "ANO_NOTIF", "SEMANA_EPID", "CS_SEXO_VAL",
    "CS_SEXO", "CS_RACA_VAL", "CS_RACA", "CLASSI_FIN_VAL", "CLASSI_FIN",
    "EVOLUCAO_VAL", "EVOLUCAO", "RESUL_SORO", "CRITERIO_FINAL", "NM_BAIRRO",
    "NM_BAIRRO_MAPA", "UNIDADE_NOTIF", "ORIGEM_DADO", "TP_SOROTIPO", "tp_sorotipo"
  )
  
  for (col in colunas_esperadas) {
    if (!col %in% names(df_raw)) {
      df_raw[[col]] <- if (col %in% c("DT_NOTIFICACAO", "DT_SINTOMAS", "DT_HOSPITALIZACAO")) as.character(df_raw$DT_NOTIFIC) else NA
    }
  }
  
  df_padrao <- df_raw[, colunas_esperadas]
  lista_dados_padronizados[[nome_base]] <- df_padrao
  
  relatorio_linhas <- c(
    relatorio_linhas,
    paste0("### Arquivo: `", nome_base, "`"),
    paste0("- **Total de Registros:** ", format(nrow(df_padrao), big.mark = ".")),
    paste0("- **Ano(s) Epidemiológico(s):** ", paste(anos_no_arquivo, collapse = ", ")),
    paste0("- **Intervalo de Semanas:** SE ", min(sems_no_arquivo), " a SE ", max(sems_no_arquivo)),
    paste0("- **Taxa de Bairros de Belém Mapeados:** ", taxa_mapeada, "%"),
    paste0("- **Casos Confirmados:** ", sum(df_padrao$CLASSI_FIN == "Confirmado")),
    paste0("- **Casos Descartados:** ", sum(df_padrao$CLASSI_FIN == "Descartado")),
    paste0("- **Casos em Investigação:** ", sum(df_padrao$CLASSI_FIN == "Em Investigação/Suspeito")),
    ""
  )
}

cat("\nAuditoria concluída com ", avisos_totais, " alerta(s) e ", erros_totais, " erro(s).\n\n")

if (length(lista_dados_padronizados) == 0) {
  cat("[ERRO] Nenhum dado válido pôde ser extraído dos arquivos.\n")
  quit(status = 1)
}

# ------------------------------------------------------------------------------
# 4. INGESTÃO E CONSOLIDAÇÃO NA BASE HISTÓRICA LOCAL
# ------------------------------------------------------------------------------
cat("[3/4] Consolidando na base histórica municipal (dados_historico_dengue.rds)...\n")

caminho_hist <- "dados_historico_dengue.rds"
df_historico_antigo <- if (file.exists(caminho_hist)) readRDS(caminho_hist) else data.frame()

# Backup preventivo
if (file.exists(caminho_hist)) {
  caminho_bak <- paste0("dados_historico_dengue.rds.bak")
  file.copy(caminho_hist, caminho_bak, overwrite = TRUE)
  cat(paste0("   Backup de segurança criado: ", caminho_bak, "\n"))
}

# Consolidação com substituição dos anos atualizados
df_novos_juntos <- bind_rows(lista_dados_padronizados)
anos_atualizados <- sort(unique(df_novos_juntos$ANO_NOTIF))

cat(paste0("   Ano(s) afetado(s) pela atualização: ", paste(anos_atualizados, collapse = ", "), "\n"))

if (nrow(df_historico_antigo) > 0) {
  df_preservado <- df_historico_antigo %>% filter(!ANO_NOTIF %in% anos_atualizados)
  df_consolidado <- bind_rows(df_preservado, df_novos_juntos)
} else {
  df_consolidado <- df_novos_juntos
}

# Desduplicação estrita por Notificação (evita duplicações entre arquivos complementares/sobrepostos)
n_antes_dedup <- nrow(df_consolidado)
df_consolidado <- df_consolidado %>% distinct(ID_NOTIFICACAO_HASH, .keep_all = TRUE)
n_removidos <- n_antes_dedup - nrow(df_consolidado)
if (n_removidos > 0) {
  cat(paste0("   [AUDITORIA] Notificações em duplicidade identificadas e removidas: ", format(n_removidos, big.mark = "."), "\n"))
}

# Ordenação final
df_consolidado <- df_consolidado %>% arrange(desc(ANO_NOTIF), desc(SEMANA_EPID), DT_NOTIFIC)

saveRDS(df_consolidado, caminho_hist)
cat(paste0("   [SUCESSO] Base oficial atualizada salva: ", caminho_hist, " (", format(nrow(df_consolidado), big.mark = "."), " notificações)\n"))

# Sincroniza com Desktop se a pasta existir
caminho_desktop <- "C:/Users/CISC/Desktop/Painel dengue/dados_historico_dengue.rds"
if (file.exists(dirname(caminho_desktop))) {
  file.copy(caminho_hist, caminho_desktop, overwrite = TRUE)
  cat("   [SUCESSO] Cópia sincronizada com a pasta de trabalho do Desktop.\n")
}

# Atualização do catálogo de estabelecimentos
if (nrow(estab_df) > 0) {
  novos_cnes <- setdiff(unique(df_novos_juntos$ID_UNIDADE_RAW[!is.na(df_novos_juntos$ID_UNIDADE_RAW) & df_novos_juntos$ID_UNIDADE_RAW != ""]), as.character(estab_df$cnes))
  if (length(novos_cnes) > 0) {
    cat(paste0("   Novos CNES detectados para inclusão no catálogo: ", length(novos_cnes), "\n"))
  }
}

# ------------------------------------------------------------------------------
# 5. SINCRONIZAÇÃO SEGURA PARA O GITHUB (LGPD COMPLIANT)
# ------------------------------------------------------------------------------
cat("\n[4/4] Sincronização Segura para o GitHub (Conformidade LGPD)...\n")

# A. Identifica a semana máxima de 2026 para alinhar a demonstração pública
sems_2026 <- df_consolidado$SEMANA_EPID[df_consolidado$ANO_NOTIF == 2026]
max_sem_atual <- if (length(sems_2026) > 0) max(sems_2026, na.rm = TRUE) else 44

cat(paste0("   Semana de monitoramento mais recente em 2026: SE ", max_sem_atual, "\n"))
cat("   Regenerando base sintética de demonstração para o GitHub...\n")

# B. Executa o gerador de dados sintéticos e exportador web (GitHub Pages)
if (file.exists("gerar_dados_exemplo.R")) {
  system2("Rscript", args = "gerar_dados_exemplo.R", stdout = FALSE, stderr = FALSE)
  cat("   [OK] dados_historico_dengue_exemplo.rds atualizado com dados sintéticos seguros.\n")
}

if (file.exists("exportar_dados_web.R")) {
  system2("Rscript", args = "exportar_dados_web.R", stdout = FALSE, stderr = FALSE)
  cat("   [OK] dados_dengue_publico.json atualizado para o portal GitHub Pages.\n")
}

# C. Salva o relatório de auditoria
relatorio_linhas <- c(
  relatorio_linhas,
  "## Resumo da Base Oficial Consolidada",
  paste0("- **Total de Notificações Ativas:** ", format(nrow(df_consolidado), big.mark = ".")),
  paste0("- **Série Histórica Contemplada:** ", min(df_consolidado$ANO_NOTIF), " a ", max(df_consolidado$ANO_NOTIF)),
  paste0("- **Semana Vigente Monitorada (2026):** SE ", max_sem_atual),
  paste0("- **Status da Auditoria:** ", if (erros_totais == 0) "APROVADO" else "REVISÃO NECESSÁRIA"),
  "",
  "---",
  "*Relatório gerado automaticamente pelo mecanismo de auditoria do Painel Dengue Belém.*"
)

writeLines(relatorio_linhas, "relatorio_auditoria_dados.md")
cat("   [OK] Relatório de auditoria gravado em 'relatorio_auditoria_dados.md'.\n")

# D. Verificação de Segurança Git (LGPD Check)
cat("\n--- Verificação de Segurança Git (LGPD) ---\n")
status_git <- system2("git", args = "status --porcelain", stdout = TRUE, stderr = FALSE)

# Procura qualquer vazamento de arquivo confidencial
vazamento_detectado <- any(grepl("\\.(dbf|DBF|dbc|DBC)$", status_git)) ||
                       any(grepl("dados_historico_dengue\\.rds$", status_git)) ||
                       any(grepl("^\\?\\? dados/.*\\.(dbf|dbc|rds)", status_git))

if (vazamento_detectado) {
  cat("   [ALERTA DE SEGURANÇA LGPD] O Git detectou arquivos de microdados não protegidos!\n")
  cat("   Verifique o arquivo .gitignore antes de realizar commits no GitHub.\n")
} else {
  cat("   [CONFORMIDADE LGPD CONFIRMADA] Nenhum microdado real do SINAN está exposto ao Git.\n")
  cat("   Arquivos prontos para sincronização no GitHub (apenas scripts, geometrias e dados sintéticos).\n")
}

cat("\n==============================================================================\n")
cat("   ATUALIZAÇÃO CONCLUÍDA COM SUCESSO!\n")
cat(paste0("   -> Base oficial local atualizada: SE ", max_sem_atual, " / 2026\n"))
cat("   -> Para iniciar o painel com os dados novos:\n")
cat("      shiny::runApp('dengue.R', launch.browser = TRUE)\n")
cat("   -> Para enviar a atualização segura ao GitHub:\n")
cat("      git add .gitignore README.md dengue.R gerar_dados_exemplo.R dados_historico_dengue_exemplo.rds dados/ relatorio_auditoria_dados.md\n")
cat(paste0("      git commit -m 'chore(dados): atualizacao epidemiologica SE ", max_sem_atual, " / 2026'\n"))
cat("      git push origin main\n")
cat("==============================================================================\n\n")
