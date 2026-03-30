# Próximos Passos — Dissertação

> **Contexto:** O resultado principal está fechado — coeficiente positivo, significativo e estável nas robustez. O que falta é a **seção de mecanismos**, que explica *por que* os setores mais expostos ao metrô cresceram de renda.

---

## Hipótese 1 — Mudança na estrutura urbana

**Lógica:** o metrô atraiu mais empreendimentos, mais apartamentos, mais pessoas. O crescimento de renda seria consequência do adensamento físico da área.

**Status:** descartada pelo orientador com base nos dados preliminares:
- Área urbana não aumentou — a região já era consolidada
- Não houve mudança no tipo de domicílio (ex: crescimento de apartamentos)
- Não houve crescimento populacional expressivo

**O que fazer:** rodar as regressões formalmente (mesmo modelo IV, trocando a variável dependente) e reportar os resultados nulos na seção de mecanismos. Um nulo bem documentado é evidência, não lacuna.

---

## Hipótese 2 — Efeito de composição (gentrificação)

**Lógica:** após o metrô, os imóveis se valorizaram e os aluguéis subiram. Isso expulsou moradores de menor renda para regiões periféricas e atraiu uma população mais rica. A renda média do setor sobe — mas não porque as pessoas ficaram mais ricas, e sim porque as pessoas mudaram.

**Exemplo:** Guará. O metrô chega, o preço dos imóveis sobe, quem não consegue pagar o novo aluguel vai embora, e quem entra tem renda mais alta.

**Status:** hipótese em aberto. **Prioridade imediata — precisa ser testada.**

**O que fazer:** rodar o mesmo modelo IV com variáveis demográficas como dependente, comparando setores expostos vs. não-expostos entre 2000 e 2010. Variáveis sugeridas pelo orientador:

- Proporção de indivíduos negros
- Proporção de mulheres
- Proporção de famílias numerosas
- Proporção de pessoas com ensino superior

**Interpretação:** se os setores mais expostos apresentarem mudança significativa nessas proporções (ex: menos negros, menos famílias numerosas, mais ensino superior), isso é evidência de efeito de composição. Se não houver mudança, o crescimento de renda provavelmente é real.

---

### ⚠️ PRÓXIMO PASSO IMEDIATO — Implementar no pipeline (R/`2_malha_censitaria.R`)

Após checar as documentações de ambos os censos, as variáveis viáveis com os dados já disponíveis são as três abaixo. As outras duas (negros e ensino superior) precisam de microdados e ficam para uma segunda rodada.

> **Contexto de dados:** Os arquivos do Censo estão em `dados/Dados setores sensitarios DF/2000/Distrito Federal/` e `dados/Dados setores sensitarios DF/2010/EXCEL/`. O pipeline carrega os dados em `R/2_malha_censitaria.R`, nas funções `unir_dados_sf_2000()` e `unir_dados_sf_2010()`.

---

#### Variável 1 — Proporção de mulheres (`prop_mulheres`)

**Lógica:** se houve gentrificação, a composição por sexo do setor pode ter mudado. Mulheres chefes de família tendem a ser mais vulneráveis a deslocamentos.

**2000 — arquivo:** `Morador_DF.XLS` (já carregado no pipeline como `censo_DF_2000_domicilio`... **atenção:** verificar — o pipeline usa `Domicilio_DF.XLS` para apartamentos; o `Morador_DF.XLS` é um arquivo separado e precisa ser carregado)

| Variável | Código | Descrição |
|----------|--------|-----------|
| Total de moradores | `V0237` | Moradores totais no setor |
| Homens moradores | `V0292` | Moradores do sexo masculino |
| **Proporção de mulheres** | `1 - V0292 / V0237` | Calcular após join |

**2010 — arquivo:** a verificar. Candidatos: `Domicilio02_DF.xls` (seção 6.3 "Domicílio, moradores") ou algum arquivo `Pessoa`. O arquivo `Basico_DF.xls` tem `V002` = população total, mas não quebra por sexo. **Antes de implementar, abrir o arquivo e confirmar qual coluna contém homens/mulheres.**

---

#### Variável 2 — Proporção de famílias numerosas (`prop_fam_numerosa`)

**Lógica:** famílias numerosas tendem a ser de baixa renda. Se o metrô causou gentrificação, a proporção dessas famílias deve cair nos setores expostos.

**Definição operacional:** domicílios com 5 ou mais moradores / total de domicílios particulares permanentes.

**2000 — arquivo:** `Domicilio_DF.XLS` (já carregado no pipeline)

| Variável | Código | Descrição |
|----------|--------|-----------|
| Dom. com 5 moradores | `V0060` | |
| Dom. com 6 moradores | `V0061` | |
| Dom. com 7 moradores | `V0062` | |
| Dom. com 8 moradores | `V0063` | |
| Dom. com 9 moradores | `V0064` | |
| Dom. com 10+ moradores | `V0065` | |
| Total de dom. part. perm. | `V0003` | Denominador |
| **Proporção** | `(V0060+V0061+V0062+V0063+V0064+V0065) / V0003` | |

**2010 — arquivo:** `Domicilio01_DF.XLS` (já carregado no pipeline)

| Variável | Código | Descrição |
|----------|--------|-----------|
| Dom. com 5 moradores | `V054` | |
| Dom. com 6 moradores | `V055` | |
| Dom. com 7 moradores | `V056` | |
| Dom. com 8 moradores | `V057` | |
| Dom. com 9 moradores | `V058` | |
| Dom. com 10+ moradores | `V059` | |
| Total de dom. part. perm. | `V001` | Denominador (já carregado como `domicilios`) |
| **Proporção** | `(V054+V055+V056+V057+V058+V059) / V001` | |

---

#### Variável 3 — Taxa de analfabetismo dos responsáveis (`prop_analfabetos_resp`)

**Lógica:** proxy de nível educacional. Regiões com maior proporção de analfabetos têm perfil socioeconômico mais baixo. Se o metrô gerou gentrificação, espera-se queda na taxa de analfabetismo nos setores expostos — não porque as pessoas ficaram mais instruídas, mas porque foram substituídas.

> **Nota:** `analfabetos` (V2249 da Instrucao1) já existe no pipeline para 2000, mas refere-se à população geral. A variável abaixo é dos *responsáveis pelo domicílio* — mais adequada porque reflete quem toma decisões de moradia. São variáveis distintas; vale usar as duas como robustez.

**2000 — arquivo:** `Responsavel1_DF.XLS` (já carregado no pipeline)

| Variável | Código | Descrição |
|----------|--------|-----------|
| Total de responsáveis | `V0402` | Responsáveis por dom. part. perm. |
| Resp. não-alfabetizados | Calcular: `V0402 - V0509` | V0509 = responsáveis alfabetizados |
| **Taxa de analfabetismo** | `(V0402 - V0509) / V0402` | |

**2010 — arquivo:** `Responsavel02_DF.xls` (seção 6.5 — **não está carregado no pipeline ainda**)

| Variável | Código | Descrição |
|----------|--------|-----------|
| Total de responsáveis | `V001` | |
| Resp. alfabetizados | `V093` | |
| **Taxa de analfabetismo** | `(V001 - V093) / V001` | |

---

#### Variáveis a deixar para depois (requerem microdados)

- **Proporção de negros:** em 2000, cor/raça não está nos arquivos do universo por setor. Precisa agregar os microdados do Censo 2000 (arquivo de pessoas, variável `V0606`).
- **Proporção com ensino superior:** em 2010, instrução não está nos arquivos do universo por setor. Precisa agregar os microdados do Censo 2010 (questionário da amostra, variável `V6400` ou similar).

---

## Hipótese 3 — Crescimento real de renda

**Lógica:** as pessoas que já moravam na área ficaram mais ricas de fato. Dois canais possíveis:

- **Canal de emprego:** o metrô aumentou a acessibilidade ao Plano Piloto, onde estão os empregos formais e melhor remunerados. As pessoas passaram a conseguir trabalhar no centro.
- **Canal de atividade local:** mais comércio e empresas se instalaram nas áreas expostas, gerando renda localmente.

**Status:** o orientador disse que ainda precisa pensar em como checar isso. **Não é prioridade imediata.**

**O que fazer quando chegar a hora:** pensar em proxies disponíveis — ex: dados de estabelecimentos formais (RAIS/CNPJ), densidade comercial, dados de commuting. Depende de disponibilidade e escopo.

---

## Hipótese auxiliar — Regulação do uso do solo

**Ideia:** verificar se o efeito de renda foi maior em setores com regulação de uso do solo mais frouxa.

**Status:** em standby. O orientador não sabe se tem viabilidade agora. **Não tocar por enquanto.**

---

## Ordem de prioridade

| # | Tarefa | Status |
|---|--------|--------|
| 1 | Testar efeito de composição (Hipótese 2) — rodar IV com variáveis demográficas como dependente | **Fazer agora** |
| 2 | Documentar formalmente o descarte da Hipótese 1 — rodar regressões com área urbana, tipo de domicílio e população | **Fazer agora** |
| 3 | Redigir a seção de mecanismos com os resultados das etapas 1 e 2 | Após os resultados |
| 4 | Pensar em como testar crescimento real de renda (Hipótese 3) | Em aberto |
| 5 | Regulação do uso do solo | Standby |

---

## Estrutura sugerida para a seção de mecanismos

1. Apresentar as três hipóteses
2. Mostrar que a Hipótese 1 é descartada pelos dados
3. Reportar o teste da Hipótese 2 e interpretar o resultado
4. Indicar a Hipótese 3 como direção para pesquisa futura
