# Guia de Publicação no GitHub Pages
### Painel Oficial da Dengue — Belém / SESMA / CIEVS

Este guia explica como ativar o **GitHub Pages** para disponibilizar o painel interativo diretamente na internet com acesso público e gratuito, sem necessidade de servidores pagos ou limites de horas (exatamente como foi feito no *Painel de Calor*).

---

## 🌐 Link Público Final após a ativação:

> ### 👉 **`https://cievsbelem.github.io/painel-dengue-belem/`**

---

## 🚀 Passo a Passo para Ativar o GitHub Pages (Leva 1 minuto):

### 1. Acesse o Repositório no GitHub
Abra no seu navegador:
👉 **[https://github.com/cievsbelem/painel-dengue-belem](https://github.com/cievsbelem/painel-dengue-belem)**

### 2. Entre nas Configurações do Repositório
No menu superior horizontal (onde fica *Code*, *Issues*, *Pull requests*...), clique na última aba à direita:
👉 **Settings** *(ícone de engrenagem)*

### 3. Abra a Seção "Pages"
No menu vertical à esquerda, na seção **Code and automation**, clique em:
👉 **Pages**

### 4. Configure a Origem da Publicação (Build and deployment)
* Em **Source**, confirme que está selecionado: **Deploy from a branch**
* Em **Branch**:
  * No primeiro seletor onde diz *None*, mude para: **`main`**
  * No segundo seletor onde diz */ (root)*, mantenha: **`/ (root)`**
* Clique no botão **Save**.

### 5. Aguarde 60 Segundos e Acesse o Link!
O GitHub levará cerca de 30 a 60 segundos para publicar a página. Quando concluir, uma barra verde aparecerá no topo da página de configurações dizendo:
> *"Your site is live at https://cievsbelem.github.io/painel-dengue-belem/"*

Basta clicar no link ou enviar para gestores, técnicos e população!

---

## 🔒 Garantia de Sigilo da LGPD no GitHub Pages

O portal web consome exclusivamente o arquivo `dados_dengue_publico.json`, que contém apenas **totais agregados estatísticos** por semana, ano, bairro e faixa etária. 
Nenhum dado pessoal, número de prontuário, CPF ou ficha individual de paciente do SINAN é exposto na internet.

---

## 🔄 Como Atualizar os Dados do Site no Futuro

Sempre que você rodar o script de atualização na sua máquina:
```bash
Rscript atualizar_dados.R
```
Ou clicar no botão **"Processar Pasta 'dados'"** dentro do painel Shiny:
1. Os dados da pasta `dados/` serão auditados e consolidados.
2. O arquivo público `dados_dengue_publico.json` será automaticamente regravado com os novos números.
3. Para atualizar o site no GitHub Pages, basta enviar para o GitHub:
   ```bash
   git add .
   git commit -m "chore(dados): atualizacao semanal do painel web"
   git push origin main
   ```
O GitHub Pages atualizará os gráficos automaticamente na web em poucos segundos!
