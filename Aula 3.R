### Aula 02 - Sumariza��o e Visualiza��o de Dados
### Tipos de Dados: num�rico

### Limpando Plots, Console and Ambiente
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

# Carregando pacotes
library('tidyverse')

# Carregando o dataset
library(readxl)
cars <- read_excel("cars.xls")
View(cars)

# Observando o dataset cars
glimpse(cars)

# Gr�fico com pontos
ggplot(data = cars, aes(x = horsepwr)) +
  geom_dotplot(dotsize = 0.4)

# Histogramas
ggplot(data = cars, aes(x = horsepwr)) +
  geom_bar()


# Gr�fico de densidade
ggplot(data = cars, aes(x = horsepwr)) +
  geom_density()

# Box-plot
ggplot(data = cars, aes(x = horsepwr)) +
  geom_boxplot()

# Gr�ficos coloridos
ggplot(data = cars, aes(x = horsepwr, fill = pickup)) +
  geom_density(alpha = 0.5)


# Gr�fico para 3 vari�veis
cars %>% 
  ggplot(aes(x = msrp)) +
  geom_density() +
  facet_grid(vars(pickup), vars(rear_wheel))

