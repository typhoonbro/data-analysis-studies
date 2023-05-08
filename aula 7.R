### Modelos de Regress�o
### Adapta��o (Fit)

### Limpando Plots, Console and Ambiente
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

# Carregando Pacotes -----------------------------------------------------------
library(learnr)
library(broom)
library(tidyverse)
library(openintro)
library(Lahman)
#library(emo)

# Preparando dados -------------------------------------------------------------

# Modelo: prever peso (weight) a partir da altura (height)
wgt_hgt_mod <- lm(wgt ~ hgt, data = bdims)

# Modelo: prever SLG a partir de OBP
mod_slg_obp <- lm(slg ~ obp, data = filter(mlbbat10, at_bat >= 10))


## Avaliando o ajuste do modelo (FIT)

### Qu�o bem nosso modelo de livro did�tico se encaixa?
# Agora que entendemos o que os modelos de regress�o linear *s�o* e como eles
# *trabalham*, uma pr�xima pergunta natural � considerar *qu�o bem* eles funcionam.
# Em um sentido intuitivo, parece claro que a linha de regress�o para o
# livros did�ticos se encaixa muito bem.
ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


### Qu�o bem nosso modelo se encaixa?
# Ao mesmo tempo, esta outra linha de regress�o de animais se ajusta menos bem,
# mas ainda parece �til.
ggplot(data = possum, aes(y = total_l, x = tail_l)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Podemos quantificar nossa intui��o sobre a qualidade do ajuste do modelo?

### Somas de desvios quadrados (sums of squared deviations)
# Na verdade podemos. 
# Lembre-se de que inicialmente consideramos qualquer n�mero de linhas.
# Estabelecemos a linha de regress�o �nica aplicando o crit�rio dos m�nimos quadrados
# Ou seja, encontramos a linha que minimiza a
# soma dos res�duos quadrados. Para cada observa��o - que � representada
# no gr�fico de dispers�o por um ponto - o res�duo � simplesmente a dist�ncia vertical
# entre esse ponto e a linha.
total_tail_mod <- lm(total_l ~ tail_l, data = possum)

total_tail_mod %>%
  augment() %>%
  ggplot(aes(x = tail_l, y = total_l)) +
  geom_smooth(method = "lm", se = 0) +
  geom_segment(aes(xend = tail_l, yend = .fitted),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "darkgray") +
  geom_point()


# Aqui, destacamos os res�duos do animal com setas cinzas.
# Se pud�ssemos encontrar uma linha que tornasse essas setas cinza mais curtas 
# - coletivamente, # e depois de elev�-los ao quadrado - 
# essa seria nossa linha de regress�o; mas n�o existe essa linha: esta � a melhor.

# Observe que n�o podemos simplesmente minimizar a soma dos res�duos.
# Esse n�mero � sempre zero, pois os res�duos positivos e negativos se cancelam
# um ao outro quando somados.

# A soma dos quadrados funciona bem matematicamente, mas tamb�m tem o efeito
# de penalizar grandes res�duos de forma desproporcional. 
# Isso � geralmente considerado # uma propriedade �til para 
# modelagem estat�stica, j� que normalmente preferimos um 
# modelo que erra um pouco,

## Medidas de ajuste do modelo
# Uma maneira de avaliar a for�a do ajuste � considerar o qu�o distante o modelo est�
# para um caso t�pico. Ou seja, para algumas observa��es, o valor ajustado ser�
# muito pr�ximo do valor real, enquanto para outros n�o.
# A magnitude de um res�duo t�pico pode nos dar uma sensa��o de
# qu�o pr�ximas s�o nossas estimativas.

### SSE
# Lembre-se que alguns dos res�duos s�o positivos, enquanto outros s�o negativos.
# De fato, � garantido pelo procedimento de ajuste de m�nimos quadrados que
# a m�dia dos res�duos � zero. Assim, faz mais sentido calcular
# o quadrado dos res�duos.

# Depois de usar a fun��o `augment()` para ajustar nosso modelo, a
# soma dos res�duos quadrados � facilmente calculada usando `summarize()`.
# Por conven��o, costumamos chamar essa quantidade de **SSE**,
# para "sum of squared estimated errors".

# Tenha em mente a coluna do modelo `augment()`que estamos interessados
# � chamado `.resid`. 

total_tail_mod %>%
  augment() %>%
  summarize(SSE = sum(.resid^2),
            SSE_also = (n() - 1) * var(.resid))

### RMSE
# O SSE � um �nico n�mero que captura o quanto nosso modelo falhou.
# Infelizmente, � dif�cil de interpretar, pois as unidades foram elevadas ao quadrado.
# Assim, outra maneira comum de pensar sobre a precis�o de um modelo �
# o **erro quadr�tico m�dio da raiz** ou **RMSE**.

# O RMSE � essencialmente o desvio padr�o dos res�duos.
# Voc� pode esperar que dividamos por $n$ aqui, mas em vez disso
# divide pelo n�mero de *graus de liberdade*, que neste caso � $n-2$.


# O RMSE tamb�m generaliza para qualquer tipo de modelo para uma 
# �nica resposta num�rica,
# ent�o n�o � espec�fico para modelos de regress�o.

### RSE (animais)
# Quando R exibe o `summary()` de um modelo de regress�o, ele exibe o
# "erro padr�o residual", tamb�m conhecido por RMSE. 
# Convenientemente, o RMSE est� na esposta, ent�o isso diz que nosso modelo faz um
# comprimento do corpo previsto que normalmente est� dentro de cerca de 3,57 cent�metros da verdade.
# Isso parece �til, j� que os animais em nosso conjunto de dados s�o
# entre 75 e 96 cent�metros.
summary(total_tail_mod)


### Erro padr�o residual (livros did�ticos)
# Para os livros did�ticos, o erro padr�o residual � $10,47.
# Esta informa��o n�o parece t�o �til - e ainda assim aparecia do gr�fico de dispers�o
# que o ajuste do modelo do livro did�tico foi muito melhor do que o ajuste do modelo
# dos animais. Conciliar essas duas no��es ser� o pr�ximo passo.

books_mod <- lm(ucla_new ~ amaz_new, data = textbooks)
#multiple r-squared é a porcentagem que o modelo acerta em relação aos dados reais
summary(books_mod)

### Qu�o bem nosso modelo de livro did�tico se encaixa?
# Anteriormente, voc� aprendeu sobre como poder�amos usar o
# soma dos res�duos quadrados para quantificar qu�o bem nosso 
# modelo se ajusta aos dados.
# No entanto, notamos que, embora o modelo do livro did�tico parece se encaixar 
# muito bem nos dados, o erro padr�o residual era superior a US$ 10.

ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

### Qu�o bem nosso modelo est� nosso modelo dos animais?
# Por outro lado, o erro padr�o residual para o modelo dos animais foi
# cerca de 3,5 cm, o que parece um alto grau de precis�o, para um modelo
# que n�o parece ser t�o justo.

ggplot(data = possum, aes(y = total_l, x = tail_l)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

### Modelo nulo (m�dio) (Null (average) model)
# � dif�cil comparar US$ 10 a 3,5 cent�metros. Qual � "maior"?
# O que seria bom � se tiv�ssemos uma maneira de comparar a qualidade de um
# ajuste do modelo sem unidade. Para fazer isso, � �til pensar em um benchmark.

# Se voc� tivesse que prever o comprimento do corpo de um animal, e
# voc� n�o tinha nenhuma informa��o sobre aquele gamb� em particular,
# qual seria sua previs�o? Vamos pensar...

# Uma escolha sensata seria o comprimento m�dio de todos os animais.
# E, de fato, se voc� tiver que fazer a mesma previs�o para todos os animais,
# a m�dia � o *melhor* n�mero que voc� pode escolher. 


### Visualiza��o do modelo nulo
# Esse modelo geralmente � chamado de **modelo nulo**.
# Faz sentido usar este modelo como benchmark, j� que
# n�o requer nenhum insight inicial:
null_mod <- lm(total_l ~ 1, data = possum)

null_mod %>%
  augment() %>%
  mutate(tail_l = possum$tail_l) %>%
  ggplot(aes(x = tail_l, y = total_l)) +
  geom_smooth(method = "lm", se = 0, formula = y ~ 1) +
  geom_segment(aes(xend = tail_l, yend = .fitted),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "darkgray") +
  geom_point()


### SSE do modelo nulo
# Podemos ajustar o modelo nulo em R usando `lm()`, mas incluindo apenas
# a constante 1 como nossa vari�vel explicativa. Isso resulta em um
# valor SSE de 1913,826.
possum_null <- lm(total_l ~ 1, data = possum)

null_mod %>%
  augment(possum) %>%
  summarize(SST = sum(.resid^2))


### SSE, nosso modelo
# Compare este n�mero com o SSE para nosso modelo de animais
# que usa o comprimento da cauda como vari�vel explicativa. 
# O SSE neste caso � 1301.488.
total_tail_mod %>%
  augment() %>%
  summarize(SSE = sum(.resid^2))


### Coeficiente de determina��o
# A raz�o entre o SSE do nosso modelo e o SSE do modelo nulo �
# uma quantifica��o da variabilidade explicada pelo nosso modelo.
# Mais especificamente, o SSE para o modelo nulo � frequentemente chamado de SST,
# para a soma *total* dos quadrados. Esta � uma medida da variabilidade
# na vari�vel de resposta.

# Ao construir um modelo de regress�o, esperamos explicar parte dessa variabilidade.
# A parte do SST que *n�o* � explicada pelo nosso modelo � o SSE.
# Essas ideias s�o capturadas por esta f�rmula para o
# "coeficiente de determina��o", geralmente referido como $R^2$.

# Devido a esta defini��o, interpretamos $R^2$ como
# a propor��o da variabilidade na vari�vel de resposta que � explicada
# pelo nosso modelo. � a medida mais comumente citada da
# qualidade do ajuste de um modelo de regress�o.


### Conex�o com correla��o
# J� vimos uma conex�o entre o valor da correla��o
# entre $X$ e $Y$ e a inclina��o da linha de regress�o.
# De fato, o valor do coeficiente de correla��o tamb�m est� intimamente
# relacionado ao valor de $R^2$. Para modelos de regress�o de m�nimos quadrados
# com uma �nica vari�vel explicativa, o valor de
# $R^2$ � apenas o quadrado do coeficiente de correla��o ($r_{x, y}^2$).

# Por que, ent�o, precisamos de ambos os conceitos? A correla��o � estritamente
# quantidade bivariada, s� pode estar entre uma �nica resposta e uma
# vari�vel explicativa �nica. No entanto, a regress�o � muito mais 
# modelagem flex�vel. Cada modelo de regress�o tem seu pr�prio valor de
# $R^2$.

# O $R^2$ nos d� uma medida num�rica da **for�a de ajuste** do nosso
# regress�o linear relativa a um modelo nulo baseado.

# O modelo nulo tem $R^2$ resulta em zero porque $SSE = SST$. 
# Isto �, os valores ajustadoss�o todos iguais ao
# m�dia, o res�duo para cada observa��o � a dist�ncia
# entre essa observa��o e a m�dia da resposta. J� que sempre podemos
# ajusta o modelo nulo, ele serve como uma linha de base contra a qual 
# todos os outros modelos ser�o comparados.

### Summary
# A maneira mais f�cil de ver o valor $R^2$ � aplicar a fun��o `summary()`
# ao seu objeto de modelo. 
# Nesse caso, vemos que nosso modelo baseado no comprimento da cauda explica 
# cerca de 32\% da variabilidade no comprimento do corpo para esses animais.
summary(total_tail_mod)


### E os livros did�ticos?
# Para os livros did�ticos, o valor $R^2$ � muito maior -- aqui podemos
# explicar 97% da variabilidade no pre�o da UCLA usando o pre�o na Amazon.
# De fato, a compara��o $R^2$ ajuda a confirmar nossa intui��o gr�fica de que
# o modelo do livro-texto � mais adequado aos dados do livro-texto 
# do que o modelo dos animais.
books_mod <- lm(ucla_new ~ amaz_new, data = textbooks)

summary(books_mod)

### Cuidados com o $R^2$
# Enquanto $R^2$ � certamente uma medida �til de ajuste de modelo,
# n�o � o objetivo final da modelagem estat�stica. Um alto $R^2$ sozinho
# n�o significa que voc� tenha um modelo "bom", e $R^2$ baixo n�o significa que
# voc� tem um modelo ruim. Um modelo com um $R^2$ alto pode ser overfit,
# ou pode violar as condi��es de infer�ncia que discutiremos mais tarde. 
# Um modelo com um baixo $R^2$ ainda pode fornecer informa��es substanciais
# em um problema complexo.

