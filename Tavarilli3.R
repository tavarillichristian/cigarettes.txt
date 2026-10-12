cigarettes = read.table("cigarettes.txt", header=T)
str(cigarettes)
head(cigarettes)

pairs(cigarettes[, 2:5], pch=16, col="blue")

#La matrice di dispersione evidenzia una forte 
#relazione lineare positiva tra il monossido di 
#carbonio (co) e le altre componenti del tabacco 
#(cat, nic e peso). In particolare coni il catrame
#e la nicotina evidenziando che l'aumetare di uno 
#questi elementi favorisce l'aumentare del monossido
#di carbonio.

coplot(cigarettes$co ~ cigarettes$cat | cut(cigarettes$nic, 3) * cut(cigarettes$peso, 3), rows=1, col="green")

#il grafico evidenzia la tendenza positiva tra catrame 
#e monossido di carbonio si mantiene stabile anche 
#all'interno dei diversi sottoinsiemi condizionati.

co.lm = lm(co ~ cat + nic + peso, data=cigarettes)
summary(co.lm)
model.matrix(co.lm)
formula(co.lm)
coefficients(co.lm)

#si rifiuta l'ipotesi nulla H0 solo per il catrame a causa di 
#un coefficiente stimato pari a 0.9626 e un p-value di 0.000692
#che lo rende altamente significativo. i coefficienti relativi 
#alla nicotina e al peso presentano p-value elevati, indicando 
#che non sono statisticamente significativi, per questo non si rifiuta H0.

#L'indice R^2 è pari a 0.9186, il che indica che circa il 91.86% 
#della variabilità della variabile risposta, il monossido di carbonio, 
#è spiegata dalle tre covariate inserite nel modello (cat, nic e peso).

attributes(co.lm)
co.lm$coefficients
co.lm$residuals
co.lm$fitted

par(mfrow=c(1,2))
plot(co.lm, which=1, pch=16, sub="") 
plot(co.lm, which=2, pch=16, sub="") 
shapiro.test(co.lm$residuals)

#Il primo grafico consente di verificare l'ipotesi di linearità e di omoschedasticità. 
#Il secondo grafico (Q-Q plot) permette di verificare che i residui del modello
#seguono una distribuzione normale.

fit1 = step(co.lm)
summary(fit1)

plot(fit1, which=1, pch=16, sub="")
plot(fit1, which=2, pch=16, sub="")
shapiro.test(fit1$residuals)

#conferma che la relazione tra le variabili è adeguatamente catturata dal modello ridotto. 
#Nel grafico Q-Q Residuals i punti si dispongono in prossimità della bisettrice teorica, 
#indicando che gli errori si distribuiscono in modo approssimativamente normale.

rstandard(co.lm)
plot(rstandard(co.lm), pch=16, ylab="Residui standardizzati")
abline(h=c(2, -2), col="purple", lty=2)
identify(rstandard(co.lm))

plot(cigarettes$cat, cigarettes$co, pch=16, xlab="catrame", ylab="co")
abline(lm(co ~ cat, data=cigarettes), col="red", lwd=2)

cigarettes_c = cigarettes[-c(3, 16), ]
co.lm_c = lm(co ~ cat, data = cigarettes_c)
summary(co.lm_c)
plot(cigarettes_c$cat, cigarettes_c$co, pch=16, xlab="catrame", ylab="monossido di carbonio")
abline(co.lm_c, col="skyblue", lwd=2, lty=2)
abline(lm(co ~ cat, data = cigarettes), col="orange", lwd=2)
legend("bottomright", legend=c("Con Outlier", "Senza Outlier"), lwd=2, col=c("orange", "skyblue"), lty=c(1, 2))

#si evidenzia dal grafico come gli outlayer influiscano, anche se di poco, sulla pendenza della retta di regressione.
#infatti il modello senza outlier ha una pendenza leggermente maggiore rispetto al modello completo.
