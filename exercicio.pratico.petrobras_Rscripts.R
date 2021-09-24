  # Exercicio pratico com dados de uma empresa brasileira
  
#No caso Petrobras

source("/cloud/project/install_and_load_packages.R")


# Dados da ação do Bradesco desde 01/01/2008
price_day <- quantmod::getSymbols("BBDC4.SA", src = "yahoo", from = '2008-01-01')
#Usando a função quant.mod para pegar os dados no yahoo finance do Bradesco->Vide material  Hudson

day_return <- na.omit(PerformanceAnalytics::Return.calculate(BBDC4.SA$BBDC4.SA.Close, method = "discrete"))
#fazendo o retorno diario para a serie e omitindo dados faltantes

log_day_return <- na.omit(PerformanceAnalytics::Return.calculate(BBDC4.SA$BBDC4.SA.Close, method = "log"))
#fazendo o log retorno diario para a serie e omitindo dados faltantes

# Gráfico do preço BBDC4
plot.xts(BBDC4.SA$BBDC4.SA.Close, main = "Preços BBDC4.SA", xlab = "tempo", ylab = "preços")

#Gráfico do retorno diario do preço BBDC4
plot.xts(day_return, main = "Retorno diario BBDC4.SA", xlab = "tempo", ylab = "preços")

#Gráfico do log retorno diario do BBDC4
plot.xts(log_day_return, main = "Log Retorno diario BBDC4.SA", xlab = "tempo", ylab = "preços")

#Vou usar um lag de 30 para simular um mes, mesmo que um mes tenha menos dias corridos(21~22)

# ACF plots do  retorno diario  X log retorno diario
par (mfcol = c(2, 2))
acf (day_return, lag = 30) 							
acf (day_return ^ 2, lag = 30) 
acf (log_day_return, lag = 30) 							
acf (log_day_return ^ 2, lag = 30) 

#Estatistica basicas
basicStats(day_return)
basicStats(log_day_return)
#Stdev             0.024969
#Skewness         -0.423296
#Kurtosis         21.229812

# Aqui, usamos a função adfTest do pacote fUnitRoots para testar se há raiz unitária ->Ctrl+c Ctrl+v Hudson
# As hipóteses do teste são:
# - H0: raiz unitária (passeio aleatório)
# - H1: sem raiz unitária (não é um passeio aleatório)
#Dickey-Fuller Test

  #log retorno diario
#passeio aleatório
fUnitRoots::adfTest(log_day_return, lags = 30, type=c("nc"))
  #Dickey-Fuller: -16.6777
  #p-valor >0,01
#passeio aleatório com drift
fUnitRoots::adfTest(log_day_return, lags = 30, type=c("c"))
  # Dickey-Fuller: -16.6764
  #p-valor>0,01
#passeio aleatório com constante e tendência
fUnitRoots::adfTest(log_day_return, lags = 30, type=c("ct"))
  #Dickey-Fuller: -16.6739
  #p-valor>0,01
#retorno diario

  #day return
#passeio aleatório
fUnitRoots::adfTest(day_return, lags = 2, type=c("nc"))
  #p-valor>0,01
#passeio aleatório com drift
fUnitRoots::adfTest(day_return, lags = 2, type=c("c"))
  #p-valor>0,01
#passeio aleatório com constante e tendência
fUnitRoots::adfTest(day_return, lags = 2, type=c("ct"))
  #p-valor>0,01
 
  # Ljung Box Test
# Teste de autocorrelação dos resíduos
#  - H0: residuos não são  autocorrelacionados
#  - H1: residuos são autocorrelacionados
Box.test (day_return, lag = 30, type = 'Ljung')
  #p-value = 6.426e-05
Box.test (log_day_return, lag = 30, type = 'Ljung')
  #p-value = 0.0001105

# Teste de Heterocedasticidade condicional
#  - H0: quadrado dos residuos não são  autocorrelacionados
#  - H1: quadrado dos residuos sao autocorrelacionados
Box.test (day_return^2, lag = 30, type = 'Ljung')
  #p-value < 2.2e-16
Box.test (log_day_return^2, lag = 30, type = 'Ljung')
  #p-value < 2.2e-16

  #Modelando

#day_return
# Ordem do Arima (retorno diario)
par(mfcol=c(1,2))
acf(day_return,lag=30)
pacf(day_return,lag=30)
#arimA day_return
a2=arima(day_return,order=c(3,1,3),seasonal=list(order=c(0,0,0),period=5), include.mean=T)
a2 
coeftest(a2)
#retirando o ma(3)
c2=c(NA,NA,NA,NA,NA,0)
a2=arima(day_return,order=c(3,1,3),seasonal=list(order=c(0,0,0),period=5),fixed=c2,include.mean=T)
a2
coeftest(a2)
#retirando ar(2)
c2=c(NA,0,NA,NA,NA,0)
a2=arima(day_return,order=c(3,1,3),seasonal=list(order=c(0,0,0),period=5),fixed=c2,include.mean=T)
a2
coeftest(a2)
#retirando ar(1)
c2=c(0,0,NA,NA,NA,0)
a2=arima(day_return,order=c(3,1,3),seasonal=list(order=c(0,0,0),period=5),fixed=c2,include.mean=T)
a2
coeftest(a2)
tsdiag(a2,gof=30)
#Testando os resíduos
res2 = a2$residuals
t.test(res2)

  # Ordem do modelo (log retorno diario) <-Usei log do retorno
par(mfcol=c(2,2))
acf(log_day_return,lag=30)
pacf(log_day_return,lag=30)
acf(log_day_return^2,lag=30)
pacf(log_day_return^2,lag=30)
#comportamento da serie senoidal pode ser indicação de sazonalidade 


  #Modelo ARIMA Log retorno diario <-Usado no trabalho
c1=c(NA,NA,NA,NA,NA,NA,NA,NA)
a1=arima(log_day_return,order=c(3,1,3),seasonal=list(order=c(1,0,1),period=5),fixed=c1,include.mean=T)
a1 
coeftest(a1)
#retirando o ar(1)
c1=c(0,NA,NA,NA,NA,NA,NA,NA)
a1=arima(log_day_return,order=c(3,1,3),seasonal=list(order=c(1,0,1),period=5),fixed=c1,include.mean=T)
a1
coeftest(a1) 
# ao tirarmos a(1) tornamos  a(2),ma(2) e m(3) insignificantes a principio.
#Será que compensa retirar ar(1) então? <-tirei na duvida
#retirando ar(2)
c1=c(0,0,NA,NA,NA,NA,NA,NA)
a1=arima(log_day_return,order=c(3,1,3),seasonal=list(order=c(1,0,1),period=5),fixed=c1,include.mean=T)
a1
coeftest(a1)
#retirando ma(3)
c1=c(0,0,NA,NA,NA,0,NA,NA)
a1=arima(log_day_return,order=c(3,1,3),seasonal=list(order=c(1,0,1),period=5),fixed=c1,include.mean=T)
a1
coeftest(a1)
tsdiag(a1,gof=30)
#não retirei a média 
#Testando os resíduos
resi = a1$residuals
t.test(resi)
#Ainda há informações nos resíduos

# Ordem do GARCH
par(mfcol=c(2,2))
acf(resi,lag=30)
pacf(resi,lag=30)
acf(resi^2,lag=30)
pacf(resi^2,lag=30)
#Usar um Garch(1,1)- normal 
#Quando rodei outros como: garch(2,1) só foi relevante 1,1
  
  
# Modelo 1: GARCH 

m1 = garchFit (~ garch (1, 1), data = resi, trace = F,include.mean=T) 
summary (m1)
AB1= 8.279e-02+ 9.038e-01
#Alpha + Beta 0,98659

#plot(m1) para ver serie,residuos,etc

resi1 = residuals (m1, standardize = T)  
# Resíduos padronizados
archTest (resi1, 30)                  

# Obtendo volatilidade para o modelo m1
v1 = volatility (m1)  						
vol1 = ts (v1, frequency = 252, start = c(2008, 1))
res1 = ts(resi1, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol1, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do Modelo 1')
plot (res1, xlab = 'year', ylab = 'st. resi', type = 'l',main='Resíduos padronizados do Modelo 1') 

# Obtain ACF & PACF
par (mfcol = c(2, 2)) 							
acf (resi1, lag = 30)
pacf (resi1, lag = 30)
acf (resi1^2, lag = 30)
pacf (resi1^2, lag = 30) 
#há informação no resíduo 22

# Plot of predictive intervals for m1
par (mfcol = c(1, 1))
mu1= 1.175e-04
upp1 = mu1 +  2 * v1
low1 = mu1 - 2 * v1
plot (time(log_day_return),log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.3, 0.3),
      main = 'Intervalo de confiança para Log retorno diario usando Modelo 1')
lines (time(log_day_return), upp1, lty = 2, col = 'blue')
lines (time(log_day_return), low1, lty = 2, col = 'blue')

  # Modelo 2: GARCH- t-student

m2 = garchFit (~ garch (1, 1), data = resi, trace = F,include.mean=T,cond.dist = 'std')
summary (m2)
AB2= 1.092e-01 +8.179e-01
#Alpha+Beta=0,9271
#significância de mu(média) aumentou em relação a m1

#plot(m2)

resi2 = residuals (m2, standardize = T)
#resíduos padronizados
archTest (resi2, 30)                  

# Volatilidade para o modelo m2
v2 = volatility (m2)  							
vol2 = ts (v2, frequency = 252, start = c(2008, 1))
res2 = ts(resi2, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol2, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do Modelo 2')
plot (res2, xlab = 'year', ylab = 'st. resi', type = 'l',main='Resíduos padronizados do Modelo 2') 

# Obtain ACF & PACF
par (mfcol = c(1, 2)) 							
acf (resi2, lag = 30)
pacf (resi2, lag = 30)
acf (resi2^2, lag = 30)
pacf (resi2^2, lag = 30) 
#há informação no resíduo 22

# Plot of predictive intervals for m2
par (mfcol = c(1, 1))
mu2=  5.665e-04
upp2 = mu2 +  2 * v2
low2 = mu2 - 2 * v2
plot (time(log_day_return),log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.3, 0.3),
      main = 'Intervalo de confiança para Log retorno diario usando Modelo 2')
lines (time(log_day_return), upp2, lty = 2, col = 'blue')
lines (time(log_day_return), low2, lty = 2, col = 'blue')



#Modelo 3: GARCH - Skew Student-t

m3 = garchFit (~ garch (1, 1), data = resi, trace = F,include.mean=T,cond.dist = 'sstd')
summary (m3)
AB3=1.091e-01+ 8.180e-01
#Alpha+Beta=0,9286
#mu continua com baixa significancia igual m1
  
#plot(m3)

resi3 = residuals (m3, standardize = T) 
#resíduos normalizados
archTest (resi3, 30)                  

# Volatilidade para o modelo m3
v3 = volatility (m3)  							
vol3 = ts (v3, frequency = 252, start = c(2008, 1))
res3 = ts(resi3, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol3, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do modelo 3')
plot (res3, xlab = 'year', ylab = 'st. resi', type = 'l',main='residuos padronizados do modelo 3') 

# Obtain ACF & PACF
par (mfcol = c(1, 2)) 							
acf (resi3, lag = 30)
pacf (resi3, lag = 30)
acf (resi3^2, lag = 30)
pacf (resi3^2, lag = 30) 
#há informação no resíduo 22

# Plot of predictive intervals for m3
par (mfcol = c(1, 1))
mu3=  5.539e-04 
upp3 = mu2 +  2 * v3
low3 = mu2 - 2 * v3
plot (time(log_day_return),log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.3, 0.3),
      main = 'Intervalo de confiança para Log retorno diario usando Modelo 3')
lines (time(log_day_return), upp3, lty = 2, col = 'blue')
lines (time(log_day_return), low3, lty = 2, col = 'blue')

  # Modelo 4:IGARCH 
source ("Igarch.R.txt")
m4 = Igarch(day_return,include.mean = T,volcnt=F)
m4 = Igarch(resi,include.mean = T,volcnt=F)
names(m4)
m4$par
resi4 = resi / m4$volatility 
#Resíduo padronizado
archTest (resi4, 30)     
par (mfcol = c(1, 1))
plot.ts(time(log_day_return),m4$volatility,xlab = 'year', ylab = 'volatility',type='l',main='Volatilidade do modelo 4')

# Obtain volatility
v4 = m4$volatility  							
vol4 = ts (v4, frequency = 252, start = c(2008, 1))
res4 = ts(resi4, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol4, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do modelo 4')
plot (res4, xlab = 'year', ylab = 'st. resi', type = 'l',main='Residuos padronizados do modelo 4') 

# Obtain ACF & PACF
par (mfcol = c(2, 2)) 							
acf (resi4, lag = 30)
pacf (resi4, lag = 30)
acf (resi4^2, lag = 30)
pacf (resi4^2, lag = 30) 
#tem informação no periodo 22 no residuo ao quadrado

# Obtain plot of predictive intervals
par (mfcol = c(1, 1))
mu4 = 6.823087e-05 # valor de mu calculado       
upp4 = mu4 + 2 * v4
low4 = mu4 - 2 * v4
plot (time(log_day_return), log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.3, 0.3),main = 'Intervalo de confiança para  Modelo 4')
lines (time(log_day_return), upp4, lty = 2, col = 'blue')
lines (time(log_day_return), low4, lty = 2, col = 'blue')


  # Modelo 6: EGARCH(1,1)


source ("Egarch.R.txt")
m6 = Egarch (resi)
names(m6)
summary (m6)
resi6 = m6$residuals
archTest (resi6, 30)     
resi6 = m6$residuals / m6$volatility 
# Os residuos padronizados
archTest (resi6, 30)     

# Obtain volatility
v6 = m6$volatility  							
vol6 = ts (v6, frequency = 252, start = c(2008, 1))
res6 = ts(resi6, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol6, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do Modelo 6')
plot (res6, xlab = 'year', ylab = 'st. resi', type = 'l',main='Resíduos padronizados do Modelo 6') 

# Obtain ACF & PACF
par (mfcol = c(2, 2)) 							
acf (resi6, lag = 30)
pacf (resi6, lag = 30)
acf (resi6^2, lag = 30)
pacf (resi6^2, lag = 30) 
#há informação no resíduo 25 e residuo^2 22

# Obtain plot of predictive intervals
par (mfcol = c(1, 1))
mu6 = 0  #Considerando média zero
upp6 = mu6 + 2 * v6
low6 = mu6 - 2 * v6
plot (time(log_day_return), log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.4, 0.4),main='Intervalo de confiança para a média usando EGARCH(1,1)')
lines (time(log_day_return), upp6, lty = 2, col = 'blue')
lines (time(log_day_return), low6, lty = 2, col = 'blue')


  # Modelo 7 - TGARCH(1,1) 

source ('Tgarch11.R.txt')
m7 = Tgarch11 (resi)
names (m7)
resi7 = m7$residuals
archTest (resi7, 30)     
resi7 = m7$residuals / m7$volatility ## Os residuos padronizados
archTest (resi7, 30)   

# Obtain volatility
v7 = m7$volatility  							
vol7 = ts (v7, frequency = 252, start = c(2008, 1))
res7 = ts(resi7, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol7, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do modelo 7')
plot (res7, xlab = 'year', ylab = 'st. resi', type = 'l',main='Residuos padronizados do modelo 7') 

# Obtain ACF & PACF
par (mfcol = c(1, 2)) 							
acf (resi7, lag = 30)
pacf (resi7, lag = 30)
acf (resi7^2, lag = 30)
pacf (resi7^2, lag = 30) 
#problem no residuo ao quadrado(significancia no residuo 22)

# Obtain plot of predictive intervals
par (mfcol = c(1, 1))
mu7 = 0 #Substituir o valor de mu encontrado no Modelo
upp7 = mu7 + 2 * v7
low7 = mu7 - 2 * v7
plot (time(log_day_return), log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.3, 0.3),main='Intervalo de confianca para MODELO 7')
lines (time(log_day_return), upp7, lty = 2, col = 'blue')
lines (time(log_day_return), low7, lty = 2, col = 'blue')


# Modelo 8 -ARMA+APARCH model with Skew Student-t innovations 

m8 = garchFit (~ aparch (2, 2), data = resi, trace = F,include.mean=T,cond.dist = 'sged',algorithm = c("lbfgsb"))
summary (m8)
#beta(1)irrelevante e gamma(1)pouco significante

#plot(m8)

resi8 = residuals (m8, standardize = T)  
archTest (resi8, 30)                  

# Obtain volatility para o modelo m8
v8 = volatility (m8)  							
vol8 = ts (v8, frequency = 252, start = c(2008, 1))
res8 = ts(resi8, frequency = 252, start = c(2008, 1))
par (mfcol = c(2, 1))  							
plot (vol8, xlab = 'year', ylab = 'volatility', type = 'l',main='Volatilidade do modelo 8')
plot (res8, xlab = 'year', ylab = 'st. resi', type = 'l',main='residuos padronizados do modelo 8') 

# Obtain ACF & PACF
par (mfcol = c(2, 2)) 							
acf (resi8, lag = 30)
pacf (resi8, lag = 30)
acf (resi8^2, lag = 30)
pacf (resi8^2, lag = 30) 
#há informação no primeiro resíduo

# Obtain plot of predictive intervals for mu
par (mfcol = c(1, 1))
mu8=  4.875e-05
upp8 = mu8 + 2 * v3
low8 = mu8 - 2 * v3
plot (time(log_day_return), log_day_return, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.3, 0.3),
      main = 'Intervalo de confianca para Modelo 8')
lines (time(log_day_return), upp8, lty = 2, col = 'blue')
lines (time(log_day_return), low8, lty = 2, col = 'blue')

  #Comparação da volatilidade dos modelos

  #m1,m2,m3
par (mfcol = c(3,1))
plot (time(log_day_return), v1, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = 'GARCH+(norm)')
plot (time(log_day_return), v2, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = 'GARCH- t-student')
plot (time(log_day_return), v3, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = 'GARCH+(skewt)')
  #m4,m6
par (mfcol = c(3,1))
plot (time(log_day_return), v4, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = 'IGarch+(norm)')
plot (time(log_day_return), v6, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = ' EGARCH(1,1)') 
  #m7,m8
par (mfcol = c(3,1))
plot (time(log_day_return), v7, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = 'TGarch+(norm)')
plot (time(log_day_return), v8, xlab = 'year', ylab = 'volatility', type = 'l', ylim = c(0, 0.1))
title (main = ' APARCH+(skew GED)')


# Os valore de AIC e BIC de cada modelo para volatilidade ARMA+GARCH :
m1@fit$ics 
#      AIC       BIC       SIC      HQIC 
#-4.750597 -4.742498 -4.750600 -4.747682 
m2@fit$ics
#    AIC       BIC       SIC      HQIC 
#-4.937958 -4.927834 -4.937963 -4.934314 
m3@fit$ics
#    AIC       BIC       SIC      HQIC 
#-4.937283 -4.925135 -4.937291 -4.932911 
m8@fit$ics
#    AIC       BIC       SIC      HQIC 
#-4.850200 -4.827929 -4.850228 -4.842184   

#Melhor modelo segundo AIC,BIC e HQIC é o Modelo(2);Garch-t-student

fGarch::predict(m2, n.ahead = 5) 
#  meanForecast  meanError  standardDeviation
#1 0.0005751518 0.04121256        0.04121256
#2 0.0005751518 0.04013811        0.04013811
#3 0.0005751518 0.03911590        0.03911590
#4 0.0005751518 0.03814398        0.03814398
#5 0.0005751518 0.03722048        0.03722048


  

