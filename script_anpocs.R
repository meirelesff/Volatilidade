## R SCRIPT DO PAPER PARA A ANPOCS (volatilidade eleitoral no Brasil, 1982-2014)

# Carrega as libs e os dados.

library(lmtest)
library(sandwich)
library(Cairo)
library(effects)
bd1 <- readRDS("bd_eleitoral.Rda")

# Roda as correlacoes de pearson e as regressoes.

anos <- c("1982", "1986")
FHC <- c("1982", "1986", "1990")
Lula <- c("2002", "2006", "2010", "2014")
cor(bd1$NEP_FED,bd1$NEP_EST,use="complete.obs") # para todo o periodo.
cor(bd1$NEP_FED[bd1$ANO %in% anos],bd1$NEP_EST[bd1$ANO %in% anos], use="complete.obs") # antes de 1990.
cor(bd1$NEP_FED[!bd1$ANO %in% anos],bd1$NEP_EST[!bd1$ANO %in% anos], use="complete.obs") # a partir de 1990.
cor(bd1$NEP_FED[!bd1$ANO %in% FHC],bd1$NEP_EST[!bd1$ANO %in% FHC], use="complete.obs") # a partir de 1994.
cor(bd1$NEP_FED[bd1$ANO %in% Lula],bd1$NEP_EST[bd1$ANO %in% Lula], use="complete.obs") # a partir de 2002.

cor(bd1$VOLAT_F,bd1$VOLAT_EST,use="complete.obs") # volatilidade para todo o periodo.
cor(bd1$VOLAT_F[bd1$ANO %in% anos],bd1$VOLAT_EST[bd1$ANO %in% anos], use="complete.obs") # antes de 1990.
cor(bd1$VOLAT_F[!bd1$ANO %in% anos],bd1$VOLAT_EST[!bd1$ANO %in% anos], use="complete.obs") # a partir de 1990.
cor(bd1$VOLAT_F[!bd1$ANO %in% FHC],bd1$VOLAT_EST[!bd1$ANO %in% FHC], use="complete.obs") # a partir de 1994.
cor(bd1$VOLAT_F[bd1$ANO %in% Lula],bd1$VOLAT_EST[bd1$ANO %in% Lula], use="complete.obs") # a partir de 2002.


m1 <- lm(NEP_FED ~ NEP_EST, bd1)
m2 <- lm(VOLAT_F ~ VOLAT_EST, bd1)
m3 <- lm(VOLAT_F ~ VOLAT_EST + NEP_FED + ANO, bd1) # controla pelo NEP e pelo ANO.
m4 <- lm(VOLAT_F ~ VOLAT_EST + NEP_FED + ANO + UF, bd1) # idem + controle para UF.
m5 <- lm(VOLAT_F ~ VOLAT_EST*ANO + NEP_FED + UF, bd1) # interacao entre volatilidade est e ano.
m6 <- lm(VOLAT_F ~ VOLAT_EST*UF + NEP_FED + ANO, bd1) # interacao com UF.

# Corrige a heterocedasticidade dos modelos.

m1W <- coeftest(m1, vcov. = vcovHC)
m2W <- coeftest(m2, vcov. = vcovHC)
m3W <- coeftest(m3, vcov. = vcovHC)
m4W <- coeftest(m4, vcov. = vcovHC)
m5W <- coeftest(m5, vcov. = vcovHC)
m6W <- coeftest(m6, vcov. = vcovHC)

# Gera alguns graficos.

par(mfrow=c(1,1))
CairoPNG('NEP.png',width = 640, height = 580) # Prepara para gerar o JPEG com o Cairo.
plot( # NEP federal vs. NEP estadual todo o periodo com reg. line.
  bd1$NEP_EST,
  bd1$NEP_FED,
  pch = 20,
  cex = 1.3,
  xlab = "NEP Federal",
  ylab = "NEP Estadual",
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1982-2014")

abline(m1, lwd=2, col="#303030")
legend("topright", bty="n", cex=0.7, col="#303030", legend=paste("R² =", format(summary(m1)$adj.r.squared, digits=2)))
dev.off()

CairoPNG('Volat.png',width = 640, height = 580) # Prepara para gerar o JPEG com o Cairo.
plot( # Volatilidade federal vs. volatilidaed estadual todo o periodo com reg. line.
  bd1$VOLAT_EST,
  bd1$VOLAT_F,
  pch = 20,
  cex = 1.3,
  xlab = "Volatilidade Estadual",
  ylab = "Volatilidade Federal",
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1982-2014")

abline(m2, lwd=2, col="#303030")
legend("topright", bty="n", cex=0.7, col="#303030", legend=paste("R² =", format(summary(m2)$adj.r.squared, digits=2)))
dev.off()


# Mosaico de graficos do NEP.

CairoPNG('NEP_mosaico.png',width = 680, height = 480)
par(mfrow=c(2,2))

plot( # NEP estadual vs. NEP federal antes de 1990
  bd1$NEP_EST[bd1$ANO %in% anos],
  bd1$NEP_FED[bd1$ANO %in% anos],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Federal",
  ylab = "NEP Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1982-1986")

plot( # NEP estadual vs. NEP federal 1990
  bd1$NEP_EST[!bd1$ANO %in% anos],
  bd1$NEP_FED[!bd1$ANO %in% anos],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Federal",
  ylab = "NEP Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1990-2014")

plot( # NEP estadual vs. NEP federal 1994
  bd1$NEP_EST[!bd1$ANO %in% FHC],
  bd1$NEP_FED[!bd1$ANO %in% FHC],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Federal",
  ylab = "NEP Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1994-2014")

plot( # NEP estadual vs. NEP federal 2002
  bd1$NEP_EST[bd1$ANO %in% Lula],
  bd1$NEP_FED[bd1$ANO %in% Lula],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Federal",
  ylab = "NEP Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "2002-2014")

dev.off()


# Mosaico de graficos da volatilidade.

CairoPNG('Volat_mosaico.png',width = 680, height = 480)
par(mfrow=c(2,2))

plot( # volatilidade estadual vs. volatilidade federal todo o periodo.
  bd1$VOLAT_EST,
  bd1$VOLAT_F,
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Federal",
  ylab = "Volatilidade Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1982-2014")

plot( # Volatilidade estadual vs. Volatilidade federal 1990
  bd1$VOLAT_EST[!bd1$ANO %in% anos],
  bd1$VOLAT_F[!bd1$ANO %in% anos],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Federal",
  ylab = "Volatilidade Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1990-2014")

plot( # Volatilidade estadual vs. Volatilidade federal 1994
  bd1$VOLAT_EST[!bd1$ANO %in% FHC],
  bd1$VOLAT_F[!bd1$ANO %in% FHC],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Federal",
  ylab = "Volatilidade Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "1994-2014")

plot( # Volatilidade estadual vs. Volatilidade federal 2002
  bd1$VOLAT_EST[bd1$ANO %in% Lula],
  bd1$VOLAT_F[bd1$ANO %in% Lula],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Federal",
  ylab = "Volatilidade Estadual",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "2002-2014")

dev.off()

# Plota o efeito marginal da volatilidade estadual com as mudancas no ano.

CairoPNG('Volat_marginal.png', width = 680, height = 480)
plot(effect(term="VOLAT_EST:ANO",mod=m5,default.levels=9),multiline=TRUE,xlab="",ylab="",main="")
dev.off()


# Para exibir os coeficientes das reg. com SE's corrigidos, digite: m1W, m2W...