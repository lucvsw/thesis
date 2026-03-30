# CLAUDE.md — Contexto Metodológico da Tese

## Visão Geral do Projeto

**Título:** Does Metro Access Increase Income? Evidence from Brasília
**Autor:** Lucas Dutra (Universidade Católica de Brasília)
**Pergunta de pesquisa:** A construção e operação do Metrô de Brasília aumentou o crescimento de renda nas áreas com melhor acesso ao sistema entre 2000 e 2010?

---

## Estrutura do Projeto (Pipeline `targets`)

O projeto usa o pacote `{targets}` para orquestrar o pipeline de dados. O arquivo principal é `_targets.R`.

### Scripts em `R/`

| Arquivo | Descrição |
|---|---|
| `1_shapefiles.R` | Carrega shapefiles (estações, linhas, projeto, RAs, setores 2000/2010) |
| `2_malha_censitaria.R` | Une shapefiles com dados tabulares do censo |
| `3_compatibilidade.R` | Harmoniza setores censitários de 2000 e 2010 |
| `4_novas_variaveis.R` | Cria variáveis de distância, dummies de exposição, variáveis derivadas |
| `6_regressoes.R` | Roda os modelos 2SLS (IV) de efeito do metrô sobre renda |
| `7_plots.R` | Gera mapas e gráficos |

### Targets principais (em ordem do pipeline)

```
estacoes_sf             → get_estacoes_sf()
linhas_sf               → get_linhas_sf()
projeto_metro_sf        → get_projeto()         # alinhamento planejado/descartado (instrumento)
RAs_sf                  → get_RAs()
censo_sf_2000_completo  → get_censo_sf_2000()
censo_sf_2010           → get_censo_sf_2010()
censo_2000_DFs          → get_censo_2000_DF()
censo_2010_DFs          → get_censo_2010_DF()
censo_2000_completo     → unir_dados_sf_2000(...)
censo_2010_completo     → unir_dados_sf_2010(...)
censo_para_compatibilizar → preparar_para_compatibilizar(...)
censo_compatibilizado   → uniao_setores(...)     # harmonização 2000 ↔ 2010
censo_com_novas_variaveis → novas_variaveis(...)  # distâncias e dummies
duplicatas_excluidas    → excluir_duplicatas(...)
censo_variacoes         → criar_variacoes(...)   # Δlog(renda), etc.
censo_final             → criar_coeficientes(...)
```

---

## Dados

### Fontes

- **IBGE** — Dados de setores censitários de Brasília, Censos 2000 e 2010
- **GeoPortal/DF** — Shapefiles das linhas e estações do metrô, limites das Regiões Administrativas
- **Instituto Mauá de Tecnologia (IMT, 1986–1987)** — Projeto de alinhamento de metrô planejado/descartado (usado como instrumento)

### Arquivos de dados em `dados/`

```
dados/
├── dados_censitarios/          # Dados tabulares do censo (XLS/CSV)
├── estacao_de_metro/           # Shapefile das estações
├── linha_de_metro/             # Shapefile das linhas construídas
├── projeto_metro/              # Shapefile do alinhamento planejado/descartado (IMT)
├── regioes_administrativas/    # Shapefile das RAs do DF
├── shapefile_setores_censitarios_2000/
├── shapefile_setores_censitarios_2010/
├── shapefile_setores_censitarios_rural_2000/
├── DMSP - Brasilia/            # Dados de luminosidade noturna
└── censo_bsb_serie_temporal_mod.gpkg  # Dado final processado
```

Dados tabulares adicionais na raiz do projeto:
- `Domicilio01_DF.XLS`, `Domicilio_DF.XLS`
- `censo_2000_basico.XLS`, `censo_2000_Instrucao1.XLS`, `censo_2000_Pessoa1.XLS`, `censo_2000_Responsavel1.XLS`
- `censo_2010_basico.XLS`, `censo_2010_ResponsavelRenda.XLS`

---

## Variável Dependente

```
var_renda_per_capita_log = log(renda_per_capita_2010) - log(renda_per_capita_2000)
```

Crescimento da renda per capita (em log) do setor censitário entre 2000 e 2010. Estrutura de **cross-section de variações** (cada setor entra uma vez com a variação já calculada).

---

## Restrição de Amostra

A análise é restrita a setores censitários dentro de um raio de **10 km** da estação de metrô mais próxima.

- **Variável:** `dummy_metro_10km == 1`
- **Justificativa:** Garante comparabilidade entre setores tratados e controles, evitando que diferenças espaciais grandes (forma urbana, regulação de uso do solo) contaminem a estimação.

---

## Definição de Exposição ao Metrô (Tratamento)

A exposição é definida por **indicadores binários de proximidade** por diferentes raios de distância:

```r
# Para cada threshold k ∈ {500, 1000, 1500, 2000, 2500} metros:
D_i(k) = 1 se distância do centróide do setor à estação mais próxima ≤ k metros
D_i(k) = 0 caso contrário
```

**Nomes das variáveis no dataset:**

| Threshold | Tratamento (D) | Instrumento (Z) |
|---|---|---|
| 500m | `dummy_500m` | `dummyp_500m` |
| 1000m | `dummy_1000m` | `dummyp_1000m` |
| 1500m | `dummy_1500m` | `dummyp_1500m` |
| 2000m | `dummy_2000m` | `dummyp_2000m` |
| 2500m | `dummy_2500m` | `dummyp_2500m` |

> **Prefixo `p`** indica instrumento derivado do **projeto planejado/descartado** (IMT).
> Instrumento construído como distância mínima do centróide ao **segmento mais próximo** do alinhamento planejado, binarizado pelo mesmo threshold k.

O threshold preferencial na especificação principal é **1000m**, seguindo o critério do ITDP (*People Near Transit*, raio de 1 km como distância máxima de caminhada aceitável).

---

## Estratégia de Identificação — 2SLS (IV)

### Problema de endogeneidade

O posicionamento das estações de metrô é endógeno: linhas podem ter sido instaladas em áreas com diferentes tendências de crescimento pré-existentes, prioridades políticas ou expectativas de desenvolvimento.

### Instrumento

**Proximidade ao alinhamento de metrô planejado/descartado (projeto IMT, 1986–1987).**

- O projeto planejado prediz a exposição realizada (relevância do instrumento).
- Sendo descartado antes da implementação, é plausivamente exógeno a choques de renda não observados, condicional aos controles e efeitos fixos de RA.

### Primeiro Estágio

```
log(D_i(k) + 1) = π₀ + π₁·log(Z_i(k) + 1) + X'_{i,2000}·ϑ + α_{r(i)} + u_i
```

> **Nota:** `log(D + 1)` e `log(Z + 1)` são convenção de código para evitar `log(0)`. Como D e Z são binárias (0 ou 1), a transformação é monotônica (0→0, 1→log2) e equivalente a usar as dummies originais até uma constante.

**Diagnóstico:** F-statistic do primeiro estágio > 300 em todos os thresholds (muito acima de 10). Teste Wu-Hausman rejeita exogeneidade do tratamento na maioria das especificações.

### Segundo Estágio

```
Δy_i = β₀ + β₁·loĝ(D_i(k) + 1) + X'_{i,2000}·γ + α_{r(i)} + ε_i
```

**β₁** = efeito causal médio local (LATE) da exposição ao metrô sobre o crescimento de renda 2000–2010, usando somente a variação de exposição induzida pelo alinhamento planejado.

### Implementação em R com `{fixest}`

```r
library(fixest)

eq_iv <- feols(
  var_renda_per_capita_log ~
    log(dist_centro_brasilia_2000) +
    prop_ens_sup_completo_2000 +
    prop_analfabetos_2000 +
    prop_over_65_2000 +
    log(renda_per_capita_2000) +
    log(pop_2000)
  | ra_cira                                        # efeitos fixos de RA
  | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1), # first stage: D ~ Z
  data   = censo_var_2010 %>% filter(dummy_metro_10km == 1),
  se     = "cluster",
  cluster = ~ra_cira
)
```

---

## Controles (Baseline 2000)

Todos os controles são medidos em **2000** (pré-tratamento):

| Variável no dataset | Descrição |
|---|---|
| `dist_centro_brasilia_2000` | Distância (m) do centróide ao CBD de Brasília |
| `prop_ens_sup_completo_2000` | Proporção de residentes com ensino superior completo |
| `prop_analfabetos_2000` | Taxa de analfabetismo |
| `prop_over_65_2000` | Proporção de residentes com 65+ anos |
| `renda_per_capita_2000` | Renda per capita (R$) |
| `pop_2000` | População total do setor |

---

## Efeitos Fixos e Erros-Padrão

- **Efeitos fixos:** Região Administrativa (`ra_cira`) — absorve fatores não observados comuns a todos os setores dentro da mesma RA que afetam o crescimento de renda
- **Erros-padrão:** Clusterizados no nível da RA (`cluster = ~ra_cira`)
- **Sem efeitos fixos de tempo:** A equação já está em primeiras diferenças (Δy); efeito de tempo seria colinear
- **Amostra final:** N ≈ 1.701 setores censitários

---

## Harmonização da Malha Censitária 2000–2010

O censo de 2010 subdividiu muitos setores de 2000. Para garantir comparabilidade temporal:

1. Para cada setor `s` de 2000, identificar os fragmentos `F(s) ⊆ S2010` contidos em `s`
2. Cada fragmento é atribuído exclusivamente a um único setor de 2000
3. Agregar os valores de 2010 pelos fragmentos:

```
X²⁰¹⁰_{s,comp} = Σ_{r ∈ F(s)} X²⁰¹⁰_r
```

Resultado: cada setor de 2000 tem um par comparável `{X²⁰⁰⁰_s, X²⁰¹⁰_{s,comp}}`.

**Função responsável:** `uniao_setores()` em `3_compatibilidade.R`

---

## Pacotes R Principais

```r
library(targets)   # pipeline
library(sf)        # dados espaciais (GIS)
library(dplyr)     # manipulação de dados
library(tidyr)     # reshape
library(readxl)    # leitura de XLS
library(fixest)    # regressões com FE e IV (feols)
library(AER)       # IV alternativo (ivreg)
library(ggplot2)   # visualizações
library(units)     # unidades de medida (distâncias)
library(here)      # caminhos relativos
library(geobr)     # shapes do Brasil (IBGE)
library(magrittr)  # pipe %>%
```

---

## Resultados Principais

A exposição ao metrô tem efeito positivo e estatisticamente significativo sobre o crescimento de renda para thresholds de **1.000m, 1.500m e 2.000m**:

- Coeficientes: 0,14 a 0,19 (em log)
- Thresholds de 500m e 2.500m: positivos, mas não significativos
- Interpretação: setores expostos ao metrô cresceram mais do que setores comparáveis não expostos na mesma RA, consistente com o canal de acessibilidade ao mercado de trabalho (redução de custos de deslocamento, expansão do conjunto de empregos alcançáveis)

---

## Convenções de Nomeação de Variáveis

- `dummy_Xm` → indicador binário de exposição ao metrô construído realizado (raio X metros)
- `dummyp_Xm` → indicador binário de exposição ao **projeto** planejado (instrumento, raio X metros)
- `int_Xm` → variável de intensidade de exposição (contínua) para raio X metros
- `dummy_metro_10km` → indicador de pertencer à amostra restrita (dentro de 10 km)
- `dummy_RA_N` → indicador da Região Administrativa N
- `dummy_RA_exposta_metro` → indicador de RA atravessada pelo metrô
- `ra_cira` → código/nome da Região Administrativa do setor
- `code_tract` → código único do setor censitário
- `var_renda_per_capita_log` → variação log da renda per capita (variável dependente)
