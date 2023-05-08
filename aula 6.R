### Modelos de Regress�o
### Interpreta��o de modelos de regress�o

### Limpando Plots, Console and Ambiente
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

### Carregando pacotes
library(learnr)
library(tidyverse)
library(openintro)
library(grid)
library(broom)
#library(emo)


### Configura��o de dados

### Livro superfaturado?
# Vamos focar em um ponto sobre um modelo de regress�o: interpretar o valor dos coeficientes.
# Dados : 
# - 73 livros did�ticos necess�rios para alguns cursos selecionados aleatoriamente na UCLA. 
# - Para cada livro, sabemos o pre�o de varejo na livraria da UCLA e na Amazon.com. 
# - Tamb�m sabemos o departamento e o n�mero do curso para cada curso correspondente e o ISBN do livro.
library(readr)
textbooks <- read_csv("C:/Users/Usu�rio/Desktop/UFPR/01-graduacao/2023-periodo-letivo/2023.01-SIN199-metodos-quant/2023.01-Metodos-Quanti/03-modelos-regressao/textbooks.csv")

textbooks <- textbooks %>%
  mutate(course_number = readr::parse_number(course))

glimpse(textbooks)


### Comparado com o n�mero do curso?
# Pode-se supor que livros mais avan�ados custam mais. 
# Nosso melhor palpite para o n�vel do curso � extrair o n�mero do curso. 
# O gr�fico de dispers�o mostra a rela��o entre o n�mero do curso e o pre�o do 
# livro na livraria da UCLA.
textbooks %>%
  ggplot(aes(x = course_number, y = ucla_new)) +
  geom_point()
# Essa rela��o � muito fraca e, no m�nimo, parece ser negativa.

### Comparado com a Amazon?
# Em vez disso, como a Amazon.com oferece uma alternativa pronta para os alunos 
# da UCLA, vamos considerar a rela��o entre os pre�os desses livros na Amazon 
# em rela��o ao pre�o na livraria da UCLA.
ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) +
  geom_point()
# Aqui vemos evid�ncias claras de uma rela��o forte, positiva e linear.

# Importante: linha de regress�o pode ser adicionada ao gr�fico com o 
# comando `geom_smooth()`. 
# Embora isso nos forne�a uma maneira de visualizar nosso modelo, 
# na verdade n�o nos diz quais s�o os coeficientes ajustados.
ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


## Interpreta��o de inclina��o e intercepta��o (slope & intercept)
# Para obter esses valores, usaremos o comando `lm()` para realmente ajustar o modelo. 
# Especificamos dois argumentos para `lm()`:
# 1. uma 'f�rmula' que indica qual vari�vel � a resposta e qual � a explicativa
# 2. um argumento `data` que identifica o quadro de dados onde essas vari�veis est�o localizadas
lm(ucla_new ~ amaz_new, data = textbooks)
# Interpreta��o: o valor do coeficiente de intercepta��o � de $0,93, 
# enquanto o valor do coeficiente de inclina��o �  de $1,20.

# Para cada $1 dolar gasto na Amazon, o pre�o m�dio na livraria da UCLA sai por $1,20
# (pre�o m�dio da Amazon sai 20% mais barato)

### Equa��o da Regress�o
# Y= a + bX
# Y � a vari�vel dependente (de X)
# a � o coeficiente de intercepta��o (de Y)
# X � a vari�vel independente
# b � o coeficiente de inclina��o

### Unidades e escala
# Ao interpretar os coeficientes de inclina��o, deve-se prestar muita aten��o 
# �s unidades e escalas. Observe que as unidades do coeficiente de inclina��o 
# s�o as unidades da vari�vel de resposta por unidade da vari�vel explicativa. 
# Nesse caso, os pre�os nas duas livrarias s�o em d�lares, mas isso � facilmente alterado.

# Aqui, criamos uma nova vari�vel para o pre�o na Amazon em centavos e 
# reajustamos o modelo. Observe que, embora o coeficiente tenha mudado, 
# o significado subjacente n�o mudou. Aqui, dizemos que para cada centavo 
# adicional que um livro custa na Amazon, o pre�o esperado na livraria da UCLA 
# aumenta em cerca de 0,01199 d�lares, ou 1,2 centavos. Assim, em ambos os casos, 
# o pre�o de um livro na livraria da UCLA � cerca de 20% superior, em m�dia, 
# ao pre�o correspondente na Amazon.com.
textbooks <- textbooks %>%
  mutate(amaz_new_cents = amaz_new * 100) 
lm(ucla_new ~ amaz_new_cents, data = textbooks)


### Um objeto de modelo linear
# Anteriormente, aprendemos como ajustar um modelo de regress�o usando o 
# comando `lm()`. No entanto, n�o fizemos muito com isso --- apenas 
# exibimos os coeficientes ajustados no console. A sa�da de `lm()` � um objeto, 
# e h� muitas coisas �teis que voc� pode fazer com esse objeto. 
# Para come�ar, precisamos armazenar a sa�da de `lm()` como um objeto em nosso 
# ambiente, neste caso apropriadamente chamado de `books_mod`.
books_mod <- lm(ucla_new ~ amaz_new, data = textbooks)

# Observe que `mod` � da classe `lm`. Este objeto cont�m todas as informa��es 
# necess�rias para ajustar nosso modelo de regress�o, incluindo (por padr�o) 
# os dados relevantes e muitas outras informa��es que podemos extrair de 
# v�rias maneiras. Vale a pena repetir que `books_mod` � um objeto do 
# tipo `lm`---n�o � um `data.frame`, ou uma `fun��o`, ou uma `matriz`, ou 
# um `vetor`.
class(books_mod)


# Por padr�o, quando voc� tenta gerar um objeto `lm`, voc� v� a "chamada" 
# (a f�rmula usada para ajustar o modelo), bem como os coeficientes ajustados. 
books_mod


### Coeficientes ajustados
# Voc� tamb�m pode retornar apenas os coeficientes ajustados como um vetor 
# usando a fun��o `coef()`. Esta fun��o recebe um objeto `lm` como entrada e 
# gera os coeficientes do modelo. Para nossos prop�sitos, essas s�o as 
# informa��es que mais nos interessam.
coef(books_mod)

### Sumarizando o Modelo
# Tratamos a regress�o como uma t�cnica estat�stica descritiva --- explicando 
# assim nosso foco nos coeficientes. 
# H� uma s�rie de outras informa��es sobre seu modelo de regress�o que voc� 
# pode inspecionar. A fun��o `summary()` exibe isso. Praticamente todo pacote 
# de software estat�stico tem uma fun��o que exibe uma tabela semelhante de 
# sa�das para um modelo de regress�o.
summary(books_mod)


### Valores ajustados
# Como o objeto `books_mod` cont�m tudo o que R sabe sobre nosso modelo, 
# podemos pedir a R os valores ajustados, usando a fun��o `fitted.values()`. 
# Isso retorna um vetor contendo os valores y para cada observa��o em 
# nosso conjunto de dados.

# Em geral, o comprimento do vetor de valores ajustados � igual ao n�mero de 
# linhas no data frame original, pois cada observa��o corresponde a exatamente 
# um valor de $\hat{y}$. No entanto, se houver alguma observa��o com dados 
# ausentes, ela ser� automaticamente descartada por R quando o modelo for 
# ajustado e, portanto, o comprimento do vetor de valores ajustados pode n�o 
#ser t�o grande quanto o n�mero de linhas no quadro de dados original.
fitted.values(books_mod)

### Valor Residual
# Da mesma forma, cada valor ajustado gera um res�duo. Esse res�duo � a 
# diferen�a entre o valor real observado da vari�vel de resposta e o 
# valor esperado da resposta de acordo com nosso modelo. Esses res�duos 
# podem ser recuperados usando a fun��o `residuals()`, que retorna os 
# vetores dos res�duos.
residuals(books_mod)


# Executando a fun��o `augment()` em nosso objeto de modelo `lm`, 
# recuperamos um `data.frame` que cont�m nossa resposta original e 
# vari�vel explicativa, juntamente com os valores ajustados, res�duos, 
# pontua��es de alavancagem e v�rias outras informa��es relevantes para 
# cada observa��o. Trabalhar com esses quadros de dados organizados pode 
# simplificar parte do trabalho que fazemos com nossos modelos depois 
# que eles s�o ajustados.
augment(books_mod)


### Examinando res�duos
# Ao examinar os res�duos, podemos saber se determinados livros did�ticos 
# parecem estar abaixo ou acima do pre�o. Nesse caso, o livro mais caro custou 
# US$ 197 na livraria da UCLA, mas apenas US$ 131 na Amazon --- uma margem de 
# lucro de US$ 66! O modelo prev� um custo de $ 158, resultando em um 
# residual de $ 39.
augment(books_mod) %>%
  arrange(desc(.resid)) %>%
  head()

### Marca��o
# Este acaba por ser o livro de gest�o "An�lise de Demonstra��es Financeiras e
# Avalia��o de T�tulos".
textbooks %>%
  filter(ucla_new == 197)

### Fazendo previs�es
# Mas, e os livros did�ticos que n�o est�o em nosso conjunto de dados original? 
# Usar nosso modelo para fazer previs�es sobre novas observa��es --- as 
# chamadas observa��es "fora da amostra" --- � uma t�cnica poderosa fundamental 
# no aprendizado de m�quina.

# Por exemplo, o livro "Introductory Statistics with Randomization 
# and Simulation" � vendido por US$ 8,49 na Amazon. Qual seria o nosso modelo 
# predizia o pre�o de varejo na livraria da UCLA?


### Novos dados
# A fun��o `predict()`, quando aplicada a um objeto `lm`, retornar� os valores
# ajustados para as observa��es originais por padr�o. No entanto, se 
# especificarmos o argumento `newdata`, podemos usar o modelo para fazer 
# previs�es sobre quaisquer observa��es que desejarmos. Observe que o objeto 
# passado para `newdata` deve ser um `data.frame` que tenha uma vari�vel com 
# o mesmo nome da vari�vel explicativa usada para ajustar o modelo. 
# Observe tamb�m que a sa�da da fun��o `predict()` � um 
# vetor de valores ajustados.

# Link com o pre�o do novo livro
# https://www.amazon.com/Introductory-Statistics-Randomization-Simulation-David/dp/1500576697

# Aqui, criamos um `data.frame` com uma vari�vel e uma observa��o para o livro ISRS.
new_data <- data.frame(amaz_new = 10)
predict(books_mod, newdata = new_data)


# O modelo informa que o pre�o esperado na livraria da UCLA � de$12,92. 


### Visualize novas observa��es
# Alternativamente, a fun��o `augment()` receber� um argumento `newdata`. 
# No entanto, esta fun��o retornar� um `data.frame`. Isso � �til se voc� quiser 
# fazer um pouco mais com suas previs�es. Aqui, primeiro usamos `augment()` 
# para criar um novo `data.frame` de valores previstos e, em seguida, 
# usamos `geom_point()` para colocar essas observa��es no gr�fico de dispers�o 
# dos dados originais. Aqui a �nica observa��o para o 
# livro ISRS � mostrada em vermelho.
isrs <- augment(books_mod, newdata = new_data)
ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_point(data = isrs, aes(y = .fitted), size = 3, color = "red")


# E este foi o nosso tutorial de hoje, sobre 
# interpreta��o de modelos de regress�o.

