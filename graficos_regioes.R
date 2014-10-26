## Graficos de volatilidade e NEP por regiao.

library(Cairo)
anos <- c("1982", "1986")
FHC <- c("1982", "1986", "1990")
Lula <- c("2002", "2006", "2010", "2014")
sul <- c("RS","SC","PR")
sud <- c("SP","RJ","ES","MG")
centro <- c("MT","MS","GO","DF")
nord <- c("PE","BA","SE","AL","PB","RN","CE","PI","MA")
norte <- c("PA","TO","RR","RO","AC","AP","AM")


# NEP regiao SUL

CairoPNG('NEP_sul.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # NEP estadual vs. NEP federal antes de 1990
  bd1$NEP_EST[bd1$UF %in% sul],
  bd1$NEP_FED[bd1$UF %in% sul],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sul (1982-2014)")

plot( # NEP estadual vs. NEP federal 1994
  bd1$NEP_EST[!bd1$ANO %in% FHC & bd1$UF %in% sul],
  bd1$NEP_FED[!bd1$ANO %in% FHC & bd1$UF %in% sul],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sul (1994-2014)")

dev.off()


# NEP regiao SUDESTE

CairoPNG('NEP_sudeste.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # NEP estadual vs. NEP federal antes de 1990
  bd1$NEP_EST[bd1$UF %in% sud],
  bd1$NEP_FED[bd1$UF %in% sud],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sudeste (1982-2014)")

plot( # NEP estadual vs. NEP federal 1994
  bd1$NEP_EST[!bd1$ANO %in% FHC & bd1$UF %in% sud],
  bd1$NEP_FED[!bd1$ANO %in% FHC & bd1$UF %in% sud],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sudeste (1994-2014)")

dev.off()


# NEP regiao Centro-Oeste

CairoPNG('NEP_centrooeste.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # NEP estadual vs. NEP federal antes de 1990
  bd1$NEP_EST[bd1$UF %in% centro],
  bd1$NEP_FED[bd1$UF %in% centro],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Centro-Oeste (1982-2014)")

plot( # NEP estadual vs. NEP federal 1994
  bd1$NEP_EST[!bd1$ANO %in% FHC & bd1$UF %in% centro],
  bd1$NEP_FED[!bd1$ANO %in% FHC & bd1$UF %in% centro],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Centro-Oeste (1994-2014)")

dev.off()


# NEP regiao NORDESTE

CairoPNG('NEP_nordeste.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # NEP estadual vs. NEP federal antes de 1990
  bd1$NEP_EST[bd1$UF %in% nord],
  bd1$NEP_FED[bd1$UF %in% nord],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Nordeste (1982-2014)")

plot( # NEP estadual vs. NEP federal 1994
  bd1$NEP_EST[!bd1$ANO %in% FHC & bd1$UF %in% nord],
  bd1$NEP_FED[!bd1$ANO %in% FHC & bd1$UF %in% nord],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Nordeste (1994-2014)")

dev.off()


# NEP regiao Norte

CairoPNG('NEP_norte.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # NEP estadual vs. NEP federal antes de 1990
  bd1$NEP_EST[bd1$UF %in% norte],
  bd1$NEP_FED[bd1$UF %in% norte],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sudeste (1982-2014)")

plot( # NEP estadual vs. NEP federal 1994
  bd1$NEP_EST[!bd1$ANO %in% FHC & bd1$UF %in% norte],
  bd1$NEP_FED[!bd1$ANO %in% FHC & bd1$UF %in% norte],
  pch = 20,
  cex = 1.1,
  xlab = "NEP Câmara",
  ylab = "NEP Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sudeste (1994-2014)")

dev.off()


# Volatilidade regiao SUL

CairoPNG('VOLAT_sul.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # Volatilidade estadual vs. Volatilidade federal antes de 1990
  bd1$VOLAT_EST[bd1$UF %in% sul],
  bd1$VOLAT_F[bd1$UF %in% sul],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sul (1982-2014)")

plot( # Volatilidade estadual vs. Volatilidade federal 1994
  bd1$VOLAT_EST[!bd1$ANO %in% FHC & bd1$UF %in% sul],
  bd1$VOLAT_F[!bd1$ANO %in% FHC & bd1$UF %in% sul],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sul (1994-2014)")

dev.off()



# Volatilidade regiao SUDESTE

CairoPNG('VOLAT_sudeste.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # Volatilidade estadual vs. Volatilidade federal antes de 1990
  bd1$VOLAT_EST[bd1$UF %in% sud],
  bd1$VOLAT_F[bd1$UF %in% sud],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sudeste (1982-2014)")

plot( # Volatilidade estadual vs. Volatilidade federal 1994
  bd1$VOLAT_EST[!bd1$ANO %in% FHC & bd1$UF %in% sud],
  bd1$VOLAT_F[!bd1$ANO %in% FHC & bd1$UF %in% sud],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Sudeste (1994-2014)")

dev.off()


# Volatilidade regiao CENTRO-OESTE

CairoPNG('VOLAT_centrooeste.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # Volatilidade estadual vs. Volatilidade federal antes de 1990
  bd1$VOLAT_EST[bd1$UF %in% centro],
  bd1$VOLAT_F[bd1$UF %in% centro],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Centro-Oeste (1982-2014)")

plot( # Volatilidade estadual vs. Volatilidade federal 1994
  bd1$VOLAT_EST[!bd1$ANO %in% FHC & bd1$UF %in% centro],
  bd1$VOLAT_F[!bd1$ANO %in% FHC & bd1$UF %in% centro],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Centro-Oeste (1994-2014)")

dev.off()



# Volatilidade regiao Nordeste

CairoPNG('VOLAT_nordeste.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # Volatilidade estadual vs. Volatilidade federal antes de 1990
  bd1$VOLAT_EST[bd1$UF %in% nord],
  bd1$VOLAT_F[bd1$UF %in% nord],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Nordeste (1982-2014)")

plot( # Volatilidade estadual vs. Volatilidade federal 1994
  bd1$VOLAT_EST[!bd1$ANO %in% FHC & bd1$UF %in% nord],
  bd1$VOLAT_F[!bd1$ANO %in% FHC & bd1$UF %in% nord],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Nordeste (1994-2014)")

dev.off()


# Volatilidade regiao NORTE

CairoPNG('VOLAT_norte.png',width = 680, height = 340)
par(mfrow=c(1,2))

plot( # Volatilidade estadual vs. Volatilidade federal antes de 1990
  bd1$VOLAT_EST[bd1$UF %in% norte],
  bd1$VOLAT_F[bd1$UF %in% norte],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Norte (1982-2014)")

plot( # Volatilidade estadual vs. Volatilidade federal 1994
  bd1$VOLAT_EST[!bd1$ANO %in% FHC & bd1$UF %in% norte],
  bd1$VOLAT_F[!bd1$ANO %in% FHC & bd1$UF %in% norte],
  pch = 20,
  cex = 1.1,
  xlab = "Volatilidade Câmara",
  ylab = "Volatilidade Assembleia",
  cex.lab=0.9,
  cex.axis = 0.8,
  col = "#1E90FF",
  bty = "n",
  main = "Norte (1994-2014)")

dev.off()