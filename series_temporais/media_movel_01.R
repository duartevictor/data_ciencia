

## média móvel simples

pib <- read_excel('PIB_Anual_Belgica.xlsx')
View(pib)

pib <- ts(pib[,2], start = 1953, freq = 1)
plot(pib, ylab = 'PIB', main = 'PIB Bélgica', xlab = 'Anos')

------
  
# tamanho janela
r <- 4

# simple moving average
pib.sma <- SMA(pib, r)
View(pib.sma)

pib.sma = ts(pib.sma)

------
  
# gráfico de média móveis (como r = 4, não está disponível para os 3 primeiros anos) 
plot(pib, ylab = 'PIB', main = 'PIB Bélgica', xlab = 'Anos', col = 'blue')
par(new = T)
plot(pib.sma, axes = F, ann = F, col = 'red')

------

# previsão da média mível para os próximos 10 anos

n = length(pib)

h = 10

prev = matrix(NA, nrow=h, ncol=1)

for (i in 1:h){
  prev[i] = pib.sma[n]
}

------

# gráfico com as previsões
min.dados = min(pib, prev)
max.dados = max(pib, prev)

plot(pib, ylim = c(min.dados, max.dados), xlim = c(1953, 2021), ylab = 'PIB', main = 'PIB Bélgica', xlab = 'Anos', col = 'blue')
lines(ts(pib.sma, start = 1953), col = 'red', lty = 2)
abline(v = end(pib), lty = 2)
lines(ts(c(pib.sma[length(pib.sma)], prev), start = 2011, freq = 1), col = 'red', lty = 2)

------

## média móvel centrada

r = 4

pib.mmc = ma(pib, order = r)
View(pib.mmc)

------

# 3 gráficos (original, média móvel central e média móvel aritmética)

plot(pib, ylab = 'PIB', xlab = 'Anos')
par(new = T)
plot(pib.mmc, axes = F, ann = F, col = 'red')
par(new = T)
plot(pib.sma, axes = F, ann = F, col = 'darkgreen')