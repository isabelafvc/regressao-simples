## ---- REGRESSÃO LINEAR SIMPLES EM R  ---- ##

# Pacotes necessários

if(!require(remotes)) install.packages("remotes")
remotes::install_github("fndemarqui/reglin", force = TRUE)

library('tidyverse')
library('ggplot2')
library('reglin')
library(car)

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

signif <- 0.05

#* As suposições do modelo de regressão simples são:
#* 
#* Linearidade entre X (experiência) e Y (salário)
#* Sobre os erros: independência, normalidade e homocedasticidade da variância
#**#

# Linearidade - resíduos vs valores ajustados

#* Esperamos um padrão aleatório dos resíduos em torno do zero *#

plot(mod)

# Normalidade dos erros - shapiro-Wilk test

normalidade <- shapiro.test(mod$residuals)
if (normalidade$p.value < signif){
  print('Há evidências de que os erros não são normais')
}else{
  print('Não há evidências de que os erros são normais')
}

# Independência dos erros - Durbin-Watson Test

independencia <- durbinWatsonTest(mod)
if (independencia$p < signif){
  print('Há evidências de que os erros não são independentes')
}else{
  print('Não há evidências de que os erros são independentes')
}

# Homocedasticidade da variância dos erros - Non constant variance test

homocedasticidade <- ncvTest(mod)
if (homocedasticidade$p < signif){
  print('Há evidências de que a variância dos erros é heterocedástica')
}else{
  print('Não há evidências de que a variância dos erros é heterocedástica')
}

  ## ---- Medidas de influência




