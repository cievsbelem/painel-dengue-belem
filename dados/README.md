# Pasta de Atualização de Dados (Uso Local / Restrito)

Esta pasta (`dados/`) é o diretório destinado ao recebimento de novos arquivos de atualização epidemiológica (como arquivos `.dbf` ou `.dbc` do SINAN, por exemplo: `DENGON2026.dbf`).

## 🔒 Proteção e Conformidade com a LGPD

* **Segurança total no Git:** Conforme definido no arquivo `.gitignore`, **nenhum arquivo inserido nesta pasta será rastreado, commitado ou enviado para o GitHub**.
* O versionamento no GitHub conterá apenas os scripts, códigos-fonte e os arquivos de demonstração com dados sintéticos.

## 📥 Como Inserir e Processar Atualizações

1. Copie o arquivo bruto do SINAN (ex: `DENGON2855999_00-2026.dbf`) diretamente para esta pasta `dados/`.
2. Para processar os novos dados e executar a auditoria de qualidade:
   * **Opção A (Via Terminal / Linha de Comando):**
     Execute na raiz do projeto:
     ```bash
     Rscript atualizar_dados.R
     ```
   * **Opção B (Via Painel Shiny):**
     Inicie o painel (`shiny::runApp("dengue.R")`) e utilize o botão **"Processar Pasta 'dados'"** na barra lateral.

3. O sistema executará o **Mecanismo de Revisão e Auditoria**:
   * Validação de colunas e datas.
   * Correção e alinhamento do ano epidemiológico oficial.
   * Verificação de duplicidades e consistência de bairros oficiais de Belém.
   * Atualização segura da base histórica local e da base de demonstração sintética para o GitHub.
