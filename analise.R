## ---- REGRESSÃO LINEAR SIMPLES EM R  ---- ##

# Bibliotecas necessárias

library('tidyverse')
library('ggplot2')

# Leitura dos dados

data <- read.csv2('dados/Salary_dataset.csv',sep=',')

data <- data %>% mutate(
  YearsExperience = as.numeric(YearsExperience),
  Salary = as.integer(Salary)
  )

glimpse(data)


## ---- Análise exploratória

#* Neste modelo, temos o interesse de verificar se os anos de experiência do funcionário 
#* influenciam significamente em seu salário
#**#

# Relação entre 'Anos de experiência' e 'Salário'

ggplot(data, aes(x = YearsExperience, y = Salary)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

#* Podemos ver que os pontos parecem repousar aleatoriamente sobre a reta de regressão,
#* o que é um indício de linearidade entre "Experiência" e "Salário"
#**#

## ---- Ajuste do modelo

mod <- lm(Salary ~ YearsExperience, data)
summary(mod)

#* Interpretação dos coeficientes
#* 
#* O coeficiente "anos de experiência" é positivo e estatistísticamente significativo 
#* ao nível de 5%, ou seja, o aumento de uma unidade em 'salário' ocasiona o aumento em
#* 'Anos de experiência'
#**#

## ---- verificação das suposições (Análise de resíduos)

#* As suposições do modelo de regressão simples são:
#* 
#* Linearidade entre X (experiência) e Y (salário)
#* Sobre os erros: independência, normalidade e homocedasticidade da variância
#**#

# Linearidade - resíduos vs valores ajustados

#* Esperamos um padrão aleatório dos resíduos em torno do zero *#

plot(mod)

# Os pontos parecem se dispor aleatoriamente sobre o 0

# Resíduos







