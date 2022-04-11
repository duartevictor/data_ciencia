## transformando em uma série temporal

desemp <- read_excel('TaxaDesemprego.xlsx')

desemp <- ts(desemp[,2], start = c(1985, 1), freq = 12)

plot(desemp, ylab = 'taxa desemprego', xlab = 'mês/ano', main = 'taxa de desemprego RMSP')

# função de auto correlação
acf(desemp)

------
  
## variável binária

library(forecast)

# dummies da série temporal
dummies <- seasonaldummy(desemp)
View(dummies)

modelo <- lm(desemp ~ dummies)
summary(modelo)

# componente sazonal
sazonal <- ts(modelo$fitted.values)
plot(sazonal)

# série dessazonalizada
dessaz <- ts(modelo$residuals)
plot(dessaz)

# gráficos
plot(desemp, ylab = 'taxa desemprego', xlab = 'mês/ano', main = 'taxa de desemprego RMSP')
par(new = T)
plot(dessaz, col = 'blue', ann = F, axes = F)

------

# média móvel

# multiplicativa pois existe variabilidade crescente nos erros (os erros não são homocedásticos)
mm <- decompose(desemp, type = 'multiplicative')

# componente sazonal
sazonal.mm <- mm$seasonal
plot.ts(sazonal.mm)

------
  
# média móvel