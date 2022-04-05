séries temporais são observações ordenadas no tempo. as séries anuais não possuem sazonalidade, uma vez que esse componente só acontece em períodos de tempo menores.

## exemplo série PIB Brasil

pib = read_excel('PIB_Brasil.xlsx')

------

# transformando a segunda coluna da tabela (valores do PIB) em uma série temporal 
pib = ts(pib[ ,2], start = 1980, freq = 1)

------

# gráfico
plot(pib, xlab = 'Ano', ylab = 'PIB (US$ de 2011)', main = 'PIB per capita - Brasil')
grid()

------
  
## modelo de tendência linear

# tamanho da amostra
n = length(pib)

# série temporal de uma sequência de 1 até n
t = ts(seq(1, n))

# modelo de tendência
modelo = lm(pib ~ t)
summary(modelo)

------

## série sem tendência

# erros do modelo de tendência
y = ts(resid(modelo))

------

## série de tendência

tend.estmimada = ts(fitted(modelo))

------

## gráfico das séries

# série original
plot(pib, xlab = 'Ano', ylab = 'PIB (US$ de 2011)', col = 'blue')
# permite inclusão de curvas no mesmo gráfico
par(new = TRUE) 
# série de tendência
plot(tend.estmimada, axes = FALSE, ann = FALSE, col = 'red')
par(new = TRUE)
# série dos resíduos (sem tendência)
plot(y, axes = F, ann = F , col = 'green')

------
  
## previsão 2019 e 2020

h = 2
prev = matrix(NA, nrow = h, ncol = 1)

for (i in 1:h){
  prev[i] = modelo$coefficients[1] + modelo$coefficients[2]*(n+i)
}

prev

------
  
# Exemplo série PIB Bélgica

pib_2 <- read_xlsx('PIB_Anual_Belgica.xlsx')

# transformando a segunda coluna da tabela em uma série temporal 
pib_2 = ts(pib_2[ ,2], start = 1953, freq = 1)

# gráfico
plot(pib_2, xlab = 'Ano', ylab = 'PIB Bélgica (Franco belga (bilhões))', main = 'PIB - Belgica')
grid()

# parece ter uma distribuição exponencial

------ 
  
## modelo de tendência não linear

# tamanho da amostra
n_2 = length(pib_2)

# série temporal de uma sequência de 1 até n
t_2 = ts(seq(1, n_2))

#modelo_2 = lm(log(pib_2) ~ t_2)
modelo_2 = lm(pib_2 ~ t_2 + I(t_2^2))
summary(modelo_2)

------
  
## série sem tendência

y_2 = ts(resid(modelo_2))

------

## série de tendência

tend.estimada_2 = ts(fitted(modelo_2))

------
  
## gráfico das séries

min.pib = min(pib, tend.estimada_2)
max.pib = max(pib, tend.estimada_2)

# série original
plot(pib_2, xlab = 'Ano', ylab = 'PIB', col = 'blue', ylim = c(min.pib, max.pib))
par(new = TRUE) 
# série de tendência
plot(tend.estimada_2, axes = FALSE, ann = FALSE, col = 'red',ylim = c(min.pib, max.pib))
par(new = TRUE)
# série dos resíduos (sem tendência)
plot(y_2, axes = F, ann = F , col = 'green', ylim = c(min.pib, max.pib))

------
  
## previsão para os próximos 10 anos

h = 10
prev = matrix(NA, nrow = h, ncol = 1)

for (i in 1:h){
  prev[i] = modelo_2$coefficients[1] + modelo_2$coefficients[2]*(n_2+i) + modelo_2$coefficients[3]*I((n_2+i)^2)
}

prev

------

min.dados1 = min(pib_2, prev, tend.estimada_2)
max.dados1 = max(pib_2, prev, tend.estimada_2)

plot(window(pib_2, start = 1953), ylim = c(min.dados1, max.dados1), ylab = 'PIB', xlim = c(1953, 2021))
lines(ts(tend.estimada_2, start = 1953), col = 'red')
abline(v = end(pib_2), col = 'blue', lty = 2)
lines(ts(c(tend.estimada_2[length(tend.estimada_2)], prev),  start = 2011, freq =1), col = 'red', lty = 2)