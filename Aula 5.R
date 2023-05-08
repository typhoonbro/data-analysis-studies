### Limpando Plots, Console and Ambiente
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

library(learnr)
library(tidyverse)
library(openintro)
library(gapminder)
library(gt)
library(patchwork)
#library(emo)

library(readr)
life <- read_csv("life_exp.csv")%>% 
  mutate(
    "state = str_to_title(state),
    county = str_to_title(county)"
  )

## Medidas de Centralidade (observa��o t�pica) 
# Exemplo de estat�stica: expectativa de vida  t�pica nos EUA � de 77,6 anos
#
# De onde vem esse n�mero? 
#
# Antes de respondermos a essa pergunta, vamos tornar isso mais concreto 
# apresentando um conjunto de dados com o qual trabalharemos ao 
# longo deste algortimo.

### Dados demogr�ficos do local
# Pesquisadores em sa�de p�blica compilaram dados sobre a demografia de todos os condados dos EUA. 
# Vemos aqui que temos 4 vari�veis: 
# o nome do estado
# o nome do munic�pio
# a expectativa m�dia de vida 
# a renda mediana.

life

### Massachussets
# Vamos nos concentrar no estado de Massachusetts, que tem 14 condados. 
# Vamos filtrar os dados para os condados desse estado e 
# nomear o dataframe resultante como `life_ma`. 
# Para simplificar, tamb�m arredondaremos os valores de expectativa de vida para 
# n�meros inteiros para esse estado.

life_ma <- life %>%
  filter(state == "Massachusetts") %>%
  mutate(expectancy = round(expectancy)) %>%
  select(county, expectancy)

# Observando o resultado para Massachussets
life_ma

life_ma_summary <- life_ma %>%
  summarise(
    mean = round(mean(expectancy), 1),
    med = round(median(expectancy), 1),
    mode = 80,
    var = round(var(expectancy), 2),
    sd = round(sd(expectancy), 2),
    iqr = round(IQR(expectancy), 2)
  )

### Melhorando o entendimento dos dados
# Vamos dar uma olhada nas expectativas de vida dos locais de Massachusetts. 
# Vamos organiz�-los em ordem crescente para facilitar um pouco a resposta � pergunta 
# "Qual � um valor t�pico para a expectativa de vida nos condados de Massachusetts?"

life_ma %>%
  arrange(expectancy) %>%
  print(n = 14)

# Aqui temos uma outra forma de olhar o resulta de expectativa de vida, de forma gr�fica.
ggplot(data = life_ma, aes(x = expectancy)) +
  geom_dotplot() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

# Precisamos pensar no que significa "t�pico". 
# Uma estat�stica que costumamos usar para descrever uma observa��o t�pica � a **m�dia**, 
# ou em outras palavras, a m�dia aritm�tica.

life_ma %>%
  summarise(mean = mean(expectancy))

# Vamos adicionar esse valor como uma linha vermelha tracejada ao nosso gr�fico de pontos.
ggplot(data = life_ma, aes(x = expectancy)) +
  geom_dotplot() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = life_ma_summary$mean, color = COL[4,1], linetype = "longdash", size = 1)


### Mediana
# Outra medida "t�pica" ou de "centralidade" � a **mediana**.
# A mediana � o percentil 50, ou seja, o valor do meio nos dados classificados.
# Vamos dar outra olhada nas expectativas de vida classificadas.
life_ma %>%
  arrange(expectancy) %>%
  pull(expectancy)

# O valor que corta os dados pela metade pode ser
# calculado usando a fun��o `median()`.
life_ma %>%
  summarise(median = median(expectancy))

# Vamos adicionar esse valor como uma linha azul e s�lida ao nosso gr�fico de pontos
ggplot(data = life_ma, aes(x = expectancy)) +
  geom_dotplot() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = life_ma_summary$mean, color = COL[4,1], linetype = "longdash", size = 1) +
  geom_vline(xintercept = life_ma_summary$med, color = COL[1,1], size = 1)

# A m�dia pode ser pensada como o ponto de equil�brio dos dados e tende a ser desenhada 
# em dire��o � cauda mais longa de uma distribui��o. Isso destaca uma caracter�stica 
# importante da m�dia: sua sensibilidade a valores extremos. Por esta raz�o, ao 
# trabalhar com distribui��es assim�tricas, a mediana � frequentemente uma medida de 
# centralidade mais apropriada.


### Moda
# A **moda** � mais uma medida de centralidade. A moda � o n�mero que ocorre com mais frequ�ncia 
# no conjunto de dados. Para encontrar a moda, podemos `count()` os valores de 
# expectativa de vida e identificar o mais frequente.
life_ma %>%
  count(expectancy, sort = TRUE)

# Neste caso, a mediana e a moda s�o os mesmos, 
# mas isso geralmente n�o acontece.
# Vamos tra�ar o modo logo acima da mediana como uma linha pontilhada amarela.
ggplot(data = life_ma, aes(x = expectancy)) +
  geom_dotplot() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = life_ma_summary$mean, color = COL[4,1], linetype = "longdash", size = 1) +
  geom_vline(xintercept = life_ma_summary$med, color = COL[1,1], size = 1) +
  geom_vline(xintercept = life_ma_summary$mode, color = COL[3,1], linetype = "dashed")


### M�dia de Grupo
# Come�amos usando `mutate()` para criar uma nova vari�vel 
# que ser� TRUE se o valor do estado for "California", "Oregon" ou "Washington" e 
# FALSE caso contr�rio. Este processo usa algumas das ferramentas que vimos antes, 
# a saber, a fun��o `if_else()` e o operador `%in%`, e os coloca juntos.
# No c�digo abaixo, criamos uma nova vari�vel chamada `coast` e especificamos como 
# essa vari�vel � calculada. A condi��o que usamos dentro da fun��o `if_else()` 
# afirma que se o estado associado a uma observa��o estiver inclu�do em 
# "Calif�rnia, Oregon ou Washington, a vari�vel `coast` deve ter um valor de `"west"`. 
# Se o estado de uma observa��o n�o est� inclu�do nesta lista, ent�o a vari�vel `coast` 
# deve ter um valor de `"east"`.
# Observe a linha `life <- life %>%` no c�digo abaixo. Esta linha est� dizendo ao R
# que estamos atualizando o conjunto de dados `life` original com um novo conjunto de 
# dados que cont�m a vari�vel `coast`.
life <- life %>%
  mutate(coast = if_else(state %in% c("California", "Oregon", "Washington"), 
                         "west",
                         "east"
  ))

# Para calcular a m�dia e a mediana para os dois grupos (costa oeste e o resto do pa�s), 
# canalizamos esse conjunto de dados atualizado para a fun��o `group_by()` e indicamos 
# como gostar�amos de fazer os grupos. Ent�o podemos `resumir()` esses grupos, 
# condados da Costa Oeste e condados fora da Costa Oeste, tomando a `m�dia()` e 
# a `mediana()` de suas expectativas de vida.
life %>%
  group_by(coast) %>%
  summarize(mean(expectancy),
            median(expectancy))

# Aprendemos que olhando tanto para a m�dia quanto para a mediana, o valor t�pico 
# do condado da Costa Oeste tem um expectativa m�dia de vida mais elevada do que os 
# condados fora da Costa Oeste.


### Escolha do centro
# A escolha da medida para o centro pode ter um impacto dram�tico no que consideramos
# para ser uma observa��o t�pica. Por isso � importante que voc� considere a forma do
# distribui��o antes de decidir sobre a medida.
set.seed(38)
rskew <- rexp(1000, 1)
symm <- rnorm(1000)
d <- data.frame(x = c(rskew, symm),
                distribution = rep(c("A", "B"), c(1000, 1000)))

ggplot(d, aes(x = x, fill = distribution)) +
  geom_density(alpha = 0.3)
# "A: mean, B: mode"


### Calculando Centros
# Ao longo desta li��o, voc� usar� dados do `gapminder`, que rastreia dados demogr�ficos em
# pa�ses do mundo ao longo do tempo. Para saber mais sobre isso, voc� pode abrir o arquivo de ajuda
# `?gapminder`.

# Para este exerc�cio, vamos focar em como a expectativa de vida difere de continente para continente. Isso requer que voc� conduza sua an�lise n�o ao n�vel do pa�s, mas agregada ao n�vel do continente. Isso � poss�vel gra�as � combina��o de `group_by()` e `summarize()`, uma sintaxe muito poderosa para realizar a mesma an�lise em diferentes subconjuntos do conjunto de dados completo.
# - Criar um conjunto de dados chamado `gap2007` que contenha apenas dados do ano de 2007.
# - Usando `gap2007`, calcular a expectativa de vida m�dia e mediana para cada continente. 
#   N�o se preocupe em nomear as novas colunas produzidas por `summarize()`.
# - Confirme as tend�ncias que voc� v� nas medianas gerando boxplots lado a lado 
#   da expectativa de vida para cada continente.

## O primeiro passo � filtrar o gapminder para ter apenas o ano de 2007
gap2007 <- filter(gapminder, 
                  year == 2007)

## Em seguida, precisamos:
## 1. Usar os dados gap2007
## 2. Fazer grupos com base no continente do pa�s
## 3. Encontre a expectativa de vida m�dia e mediana de cada continente 
gap2007 %>%
  group_by(continent) %>%
  summarize(mean(lifeExp),
            median(lifeExp))

## Finalmente precisamos:
## 1. Usar os dados gap2007
## 2. Declare nossas vari�veis x e y
## 3. Adicionar boxplots ao gr�fico
gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()

# Criar dataset do ano 2007
gap2007 <- filter(gapminder, year == 2007)

# Calcular a m�dia de grupo e a vida �til mediana
gap2007 %>%
  group_by(continent) %>%
  summarize(mean(lifeExp),
            median(lifeExp))

# Gerar boxplots de lifeExp para cada continente
gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()


### Medidas de variabilidade
# Como voc� resume a variabilidade que voc� v� em um conjunto de dados?
# Vamos considerar novamente a expectativa de vida em Massachusetts. 
# Pensamos na variabilidade em um
# conjunto de dados como a distribui��o dos dados em torno de seu centro.

### Varian�a e desvio padr�o
# Primeiro vamos definir o centro como a m�dia e quantificar a dist�ncia da m�dia tomando
# a diferen�a entre cada observa��o e essa m�dia.
life_ma %>%
  mutate(deviation = expectancy - life_ma_summary$mean)  



# Gostar�amos de reduzir todas essas diferen�as a uma �nica medida de variabilidade, e uma
# op��o � adicion�-los. Mas se fizermos isso, **os positivos e os negativos se cancelar�o**.
# Para evitar isso (e tamb�m para dar maior peso aos desvios da m�dia que s�o maiores),
# elevamos cada diferen�a ao quadrado.
life_ma %>%
  mutate(
    deviation = expectancy - life_ma_summary$mean,
    deviation_sq = deviation^2
  )

### Essa nova medida � melhor, mas tem uma propriedade indesej�vel: vai ficar cada vez maior
### quanto mais dados voc� adicionar. Voc� pode corrigir esse crescimento irrestrito dividindo 
### essa medida pelo ### n�mero de observa��es, `r nrow(life_ma)`. Agora, a 
### quantidade � uma medida �til encontrada pelo seguintes passos:
# 1. Encontre o centro (m�dia) dos dados
# 2. Em seguida, encontre o quadrado da dist�ncia entre cada observa��o e a m�dia
# 3. Divida a dist�ncia total ao quadrado pelo n�mero de observa��es ($n$) no conjunto de dados
life_ma %>%
  mutate(
    deviation = expectancy - life_ma_summary$mean,
    deviation_sq = deviation^2
  ) %>%
  summarise(mean_sq_deviation = sum(deviation_sq) / nrow(life_ma))

# Se voc� alterar $n$ para $n - 1$, voc� obter� o que � chamado de *vari�ncia da amostra*,
# uma das medidas mais �teis da propaga��o de uma distribui��o.
life_ma %>%
  mutate(
    deviation = expectancy - life_ma_summary$mean,
    deviation_sq = deviation^2
  ) %>%
  summarise(mean_sq_deviation = sum(deviation_sq) / (nrow(life_ma) - 1))


# Em R, voc� pode usar a fun��o interna `var()` para calcular a varia��o da amostra
life_ma %>%
  summarise(var = var(expectancy))

# Outra medida �til � a raiz quadrada da *vari�ncia da amostra*, que � chamada de
# *desvio padr�o da amostra* ou apenas `sd()` em R. Uma informa��o conveniente sobre o
# desvio padr�o da amostra � que, uma vez calculado, est� nas mesmas unidades dos dados originais.
# Neste caso podemos dizer que o desvio padr�o da vida dos condados `r nrow(life_ma)`
# expectativas s�o `r life_ma_summary$sd` anos.
life_ma %>%
  summarise(sd = sd(expectancy))
# Em compara��o, a vari�ncia desta amostra � `r life_ma_summary$var` anos ao quadrado,
# que � uma unidade sobre a qual n�o temos intui��o real.


### Faixa interna do quartil
# H� mais duas medidas de spread que � bom conhecer.
# O **intervalo interquartil**, ou **IQR**, � a dist�ncia entre os dois n�meros que
# corta os 50% m�dios dos seus dados. Isso deve soar familiar a partir da discuss�o de
# boxplots: a altura da caixa � exatamente o IQR.
# Podemos calcular o primeiro e o terceiro quartis usando a fun��o `quantile()` e
#pegue a diferen�a deles...

life_ma %>%
  summarise(
    q1 = quantile(expectancy, 0.25),
    q3 = quantile(expectancy, 0.75),
    iqr = q3 - q1
  )

# ... ou podemos usar a fun��o interna `IQR()`.
life_ma %>%
  summarise(iqr = IQR(expectancy))

### Intervalo
# A medida final � simplesmente o **intervalo** dos dados:
# a dist�ncia entre o m�ximo e o m�nimo.
life_ma %>%
  summarise(
    min = min(expectancy),
    max = max(expectancy),
    range = max - min
  )


### Escolhendo uma medida de spread
# Para qualquer conjunto de dados, voc� pode calcular todas essas quatro estat�sticas, 
# mas quais s�o as mais significativas? O mais comumente usado na pr�tica � o desvio padr�o
# Mas o que acontece se o conjunto de dados tiver algumas observa��es extremas?
#
# Digamos que o condado de Hampden, Massachusetts, tenha expectativa de vida em torno de 78 anos,
# tinha uma expectativa de vida anterior de 97. Criaremos um novo conjunto de dados com esse valor e
# chame de `life_ma_extreme`.
life_ma_extreme <- life_ma %>%
  mutate(expectancy = if_else(county == "Hampden County", 97, expectancy))

# Abaixo temos esse valor extremo criado.
life_ma_extreme

# E abaixo temos um gr�fico de pontos dos dados com o valor extremo inventado.
ggplot(data = life_ma_extreme, aes(x = expectancy)) +
  geom_dotplot() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

###
# Se voc� recalcular a vari�ncia e o desvio padr�o, ver� que ambos
# passaram do limite. Essas medidas s�o sens�veis a valores extremos, 
# pois ambas dependem na m�dia como sua medida de centro!
life_ma_extreme %>%
  summarise(
    var = var(expectancy),
    sd  = sd(expectancy)
  )

###
# Se voc� recalcular o intervalo, ele certamente aumentar� porque est� completamente 
# determinado pelos valores extremos. Por esse motivo, o intervalo n�o � usado com frequ�ncia.
life_ma_extreme %>%
  summarise(
    min = min(expectancy),
    max = max(expectancy),
    range = max - min
  )


###
# No entanto, se voc� recalcular o IQR, ver� que ele n�o mudou.
# Como essa observa��o ainda � a maior expectativa de vida do conjunto,
# os quartis n�o se moveram. Isso revela uma boa situa��o para usar o
# IQR: quando seu conjunto de dados est� muito distorcido ou tem observa��es extremas.
life_ma_extreme %>%
  summarise(iqr = IQR(expectancy))

###
# A escolha da medida para o spread pode impactar drasticamente o qu�o vari�vel 
# consideramos nossos dados, por isso � importante que voc� considere a forma da 
# distribui��o antes de decidir sobre a medida.
rskew <- rexp(1000, 1)
symm <- rnorm(1000)
d <- data.frame(x = c(rskew, symm),
                distribution = rep(c("A", "B"), c(1000, 1000)))

ggplot(d, aes(x = x, fill = distribution)) +
  geom_density(alpha = 0.3)


### Calcular medidas de spread
# Vamos utilizar a sintaxe `group_by()` e `summarize()` para medidas de spread.
# Se voc� n�o tiver certeza se est� trabalhando com distribui��es sim�tricas ou assim�tricas, 
# � uma boa ideia considerar uma medida robusta como IQR al�m das medidas usuais 
# de vari�ncia ou desvio padr�o.
#
# O conjunto de dados `gap2007` que voc� criou em um exerc�cio anterior est� dispon�vel 
# em sua �rea de trabalho.
#
# - Para cada continente em `gap2007`, resuma as expectativas de vida usando 
#   `sd()`, `IQR()` e a contagem de pa�ses, `n()`. N�o h� necessidade de nomear 
#    as novas colunas produzidas aqui. Tenha em mente que a fun��o `n()` 
#    n�o recebe argumentos!
# - Compare graficamente a propaga��o dessas distribui��es construindo gr�ficos
#   de densidade sobrepostos de expectativa de vida divididos por 
#   continente.gap2007 <- filter(gapminder, year == 2007)

# Por fim, especifique qual vari�vel deve ser usada nos c�lculos
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())

## �ltimo passo -- criar gr�ficos de densidade com os diferentes continentes sobrepostos
## Para diferenciar os continentes, preenchemos cada um com uma cor diferente
## Usamos um alfa de 0,3 para nos permitir ver a forma de cada gr�fico de densidade
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)



## Forma e transforma��es
# Geralmente h� quatro caracter�sticas de distribui��o que s�o de interesse.
# Os dois primeiros j� cobrimos: o centro e a dispers�o ou variabilidade da distribui��o.
# A terceira � a forma da distribui��o, que pode ser descrita em termos de modalidade e
# a inclina��o.

### Modalidade (Modality)
# A modalidade de uma distribui��o � o n�mero de sali�ncias proeminentes que aparecem na distribui��o.
# Se houver um �nico modo, como em uma curva de sino, ele � chamado de "unimodal".
# Se houver dois modos proeminentes, � chamado de "bimodal". Se tiver tr�s modos ou mais, o
# conven��o � referir-se a ele como "multimodal". H� um �ltimo caso: quando n�o h� modo distinto.
# Como a distribui��o � plana em todos os valores, ela � chamada de "uniforme".

# O outro aspecto da forma da distribui��o diz respeito � sua inclina��o.

### Inclina��o / Tend�ncia (Skew)
# Se uma distribui��o tem uma cauda longa que se estende para a direita, ela � chamada de
# "inclinado � direita."

# Se essa cauda longa se estende para a esquerda, ela � chamada de "inclinada para a esquerda".
# Se voc� tiver problemas para lembrar qual � qual, apenas lembre-se de que o nome do skew est� em
# a dire��o da cauda longa.

# Se nenhuma cauda for mais longa que a outra, a distribui��o � chamada de "sim�trica".

## Renda (income)
# Vamos comparar as distribui��es de renda pessoal mediana em n�vel de condado na Costa Oeste
# e no resto do pa�s para ver que forma essas distribui��es tomam.
# Existem v�rios tipos de plotagem que podemos usar aqui. Vamos usar um gr�fico de 
# densidade sobreposta colocando a renda ao longo do eixo x, `preenchendo as duas curvas 
# com cor de acordo com se ou n�o eles est�o na Costa Oeste, ent�o adicionando 
# uma densidade mais tarde e especificando um n�vel `alpha` de 0,3.
# Isso permite que as cores sejam um pouco transparentes para que possamos ver 
# onde elas se sobrep�em.

# O gr�fico resultante mostra duas curvas, a azul representando a distribui��o 
# da Costa Oeste e o rosa que representa os condados que n�o est�o na Costa Oeste. 
# Cada distribui��o tem um �nico modo proeminente,para que possamos dizer que cada 
# distribui��o � unimodal. Voc� pode argumentar que a pequena colis�o
# cerca de 100.000 d�lares � um segundo modo, mas geralmente procuramos uma 
# estrutura de maior escala do que isso.

# � dif�cil comparar essas distribui��es porque ambas s�o fortemente assim�tricas � direita,
# ou seja, existem alguns munic�pios em cada grupo que possuem renda muito alta.
# Uma maneira de remediar isso � construir um gr�fico de uma vers�o transformada dessa vari�vel.
ggplot(life, aes(x = income, fill = coast)) +
  geom_density(alpha = 0.3)

### Transforma��es de Vari�veis
# Como a renda tem uma forte inclina��o � direita, a transforma��o da raiz quadrada ou 
# logar�tmica far� um bom trabalho de desenhar na cauda direita e espalhar 
# os valores mais baixos para que possamos ver o que est� acontecendo.
# Podemos realizar a transforma��o envolvendo a renda na fun��o `log()`, que levar� o
# log natural (ln). 
ggplot(life, aes(x = log(income), fill = coast)) +
  geom_density(alpha = 0.3)
# O resultado � uma imagem um pouco mais f�cil de interpretar: a renda t�pica nos 
# condados da Costa Oeste � de fato maior do que no resto do pa�s e o 
# segundo modo muito pequeno de alta renda
# munic�pios da Costa Oeste n�o est� presente na outra distribui��o.


### Descreva a forma
# Para construir alguma familiaridade com distribui��es de formas diferentes,
# considere as quatro que s�o plotadas
# aqui.
set.seed(12)
x1 <- density(rnorm(30, 0.6, 0.2))
x2 <- density(rnorm(30, 0.6, 0.2))
x3 <- density(rexp(30, 1))
x4 <- density(c(rnorm(15, 1, 0.1), rnorm(15, 1.7, 0.1)))
y <- c(x1$y, x2$y, x3$y, x4$y)
x <- c(x1$x, x2$x, x3$x, x4$x)

d <- data.frame(x = x,
                y = y,
                group = rep(LETTERS[1:4], each = 512))
ggplot(d, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# "A: unimodal sim�trico; B: unimodal assim�trico � direita; C: unimodal assim�trico � direita, D: bimodal sim�trico."



## Valores at�picos (outliers)
# Discutimos tr�s aspectos diferentes de uma distribui��o que � importante observar

### Caracter�sticas de uma distribui��o
# - Centro
# - Variabilidade
# - Forma
# - At�picos

# Ao realizar uma an�lise explorat�ria de dados: centro, variabilidade e forma.
# Uma quarta e �ltima coisa a procurar s�o os valores discrepantes. Estas s�o observa��es que t�m valores extremos
# longe do grosso da distribui��o. Geralmente s�o casos muito interessantes, mas tamb�m s�o bons
# para conhecer antes de prosseguir com uma an�lise mais formal.

# Vimos alguns valores extremos quando tra�amos a distribui��o de renda para munic�pios do
# Costa oeste. O que devemos fazer com este pontinho de condados? Uma coisa que podemos 
# fazer � exibir os dados usando um boxplot. Para tornar o boxplot um pouco mais 
# f�cil de comparar com um gr�fico de densidade, voc� pode especificar
# a vari�vel categ�rica (`coast`) como a vari�vel y. Esta op��o cria empilhados verticalmente
# boxplots em vez de boxplots empilhados horizontalmente. 
ggplot(life, aes(x = income, fill = coast)) +
  geom_density(alpha = 0.5)

ggplot(life, aes(x = income, y = coast, color = coast)) +
  geom_boxplot(alpha = 0.5) + 
  theme(legend.position = "hide")


p1 <- ggplot(life, aes(x = income, fill = coast)) +
  geom_density(alpha = 0.5)

p2 <- ggplot(life, aes(x = income, y = coast, color = coast)) +
  geom_boxplot(alpha = 0.5) + 
  theme(legend.position = "hide")

p1 + p2
# O que vemos � interessante: o boxplot sinaliza muitos munic�pios como outliers, 
# tanto ao longo da Costa Oeste
# mas tamb�m no resto do pa�s. Ent�o, por que o pontinho foi mais aparente na Costa Oeste?
# Tem a ver com o tamanho da amostra. H� muito menos condados no grupo da Costa Oeste, 
# ent�o esses poucos
# outliers t�m um efeito superdimensionado no gr�fico de densidade. No caso do 
# grupo n�o Costa Oeste,
# existem muitos outros condados que acabaram eliminando o efeito dos outliers na densidade
# enredo.


## Indicando valores at�picos
# Muitas vezes � considerado at�pico a fazer o resto dos dados, vamos ent�o considerar um
# nova concentra��o ou n�o deve ser determinado um valor at�pico. Isso requer que n�s `mutate()`
# uma nova coluna chamada `is_outlier` que � TRUE se a renda for maior que algum limite e FALSE
# por outro lado. Nesse caso, escolhemos um limite para valores discrepantes como munic�pios com renda superior a
#$ 75.000.
life <- life %>%
  mutate(is_outlier = income > 75000)

# Uma ferramenta �til ao inspecionar outliers � filtrar o conjunto de dados para 
# incluir apenas outliers,
# e, em seguida, organizando-os em ordem decrescente. Vamos fazer isso a seguir.
life %>%
  filter(is_outlier) %>%
  arrange(desc(income))
# Com base na sa�da, descobrimos que o condado de maior renda �, na verdade, o condado de Teton,
# no Grand Tetons de Wyoming, e que tr�s dos outros dez condados mais importantes est�o no Texas e
# dois est�o em Nebraska. Observe que n�o salvamos esse resultado em um novo objeto, 
# estamos apenas visualizando o
# sa�da para investigar os valores discrepantes, mas n�o os removemos do 
# conjunto de dados (pelo menos ainda n�o).


### Plotagem sem outliers
# Tamb�m podemos tentar reconstruir os gr�ficos de densidade sem os outliers.
#
# Para isso, formamos um pipeline onde o primeiro passo � filtrar os munic�pios que est�o
# n�o valores at�picos. Lembre-se que `is_outlier` � um vetor de `TRUE`s e `FALSE`s. 
# Podemos simplesmente afirmar
# que n�o estamos interessados em nenhum dos valores que s�o `TRUE` adicionando um 
# ponto de exclama��o (`!`)
# antes do nome da vari�vel. Podemos ent�o canalizar esse conjunto de dados filtrado para o mesmo
# c�digo `ggplot()` que usamos para os gr�ficos de densidade sobrepostos originais.

life %>%
  filter(!is_outlier) %>%  
  ggplot(aes(x = income, fill = coast)) +
  geom_density(alpha = 0.3)

ggplot(life, aes(x = income, fill = coast)) +
  geom_density(alpha = 0.3)

p1 <- life %>%
  filter(!is_outlier) %>%  
  ggplot(aes(x = income, fill = coast)) +
  geom_density(alpha = 0.3)

p2 <- ggplot(life, aes(x = income, fill = coast)) +
  geom_density(alpha = 0.3)

p1 + p2
# O resultado � um gr�fico que foca muito mais no corpo da distribui��o.
# Voc� pode contrastar isso com o gr�fico original, que foi dominado pela forte inclina��o 
# � direita causada
# pelos valores extremos. Observe que nenhum desses gr�ficos est� certo ou errado, mas eles dizem
# hist�rias da estrutura nesses dados, ambas valiosas.