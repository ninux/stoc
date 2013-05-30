# Serie 12

# Aufgabe 1

# 1a) 
forbes <- read.table("http://stat.ethz.ch/Teaching/Datasets/forbes.dat",header=TRUE)
par(mfrow = c(3,1))
plot(forbes[,"Temp"], forbes[,"Press"])
# ja, denn Temperatur zu Luftdruck dürfte sich nicht all zu "wild" verhalten

# 1b)
forbes.fit <- lm(Press ~ Temp, data = forbes)
summary(forbes.fit)
abline(forbes.fit)

# 1c) 
plot(fitted(forbes.fit), resid(forbes.fit), main="Tukey-Anscombe Plot")
abline(h=0)
qqnorm(resid(forbes.fit))
# der Tukey-Anscombe-Plot zeigt auf, dass es einen systematischen Fehler gibt, denn die Punkte verlaufen in einer Parabelform
# der QQ-Norm-Plot zeigt: kann normalverteilt sein, es ist keine spezielle Abweichung ersichtlich
qqline(resid(forbes.fit))
# 'qqline' zeigt es ist'qqline' zeigt es ist ziemlich gerade

# 1d)
# Druck logarithmieren und in Datensatz hinzufügen
forbes[,"Logpress"] <- log(forbes[,"Press"])
# Streudiagramm Temperatur zu logarithmiertem Druck
plot(forbes[,"Temp"], forbes[,"Logpress"])
# Regressionsgerade erstellen
forbes.logfit <- lm(Logpress~Temp, data=forbes)
# Regressionsgerade einzeichnen
abline(forbes.logfit)

# 1e)
# Tukey-Anscombe-Plot erstellen
plot(fitted(forbes.logfit), resid(forbes.logfit), main="Tukey-Anscombe Plot")
# Normalplot erstellen
qqnorm(resid(forbes.logfit))
# Ja es gibt Ausreisser, gut sichbar wenn 'qqline' gezogen wird
qqline(resid(forbes.logfit))
# Es gibt einen Ausreisser, die Modellannahme stimmt weil der Fehler normalverteilt ist???

# 1f)
# Ausreisser mittels 'identify' lokalisieren
identify(fitted(forbes.logfit), resid(forbes.logfit))
# es zeigt an, dass die Beobachtung die Nummer 12 hat
# die Beobachtung wird nun aus dem Datensatz entfernt
forbes <- forbes[-12,]

# plots nochmals erstellen
# ich erkenne lediglich, dass der Tukey-Anscombe-Plot eine wildere Streuung aufzeigt

# Benötigte Zeit um die Aufgabe 1 zu lösen: ca. 60 Minuten zum lösen, ca. 45 korrigieren und besprechen
# Alles Verstanden: NEIN
# Offene Fragen: 
# * Die Modellannahme stimmt, wenn der Fehler normalverteilt ist???

# Aufgabe 2

# 2a)
# daten erfassen
runner.distance <- c(100, 200, 400, 800, 1000, 1500, 2000, 3000, 5000, 10000, 20000, 25000, 30000)
runner.time <- c(9.9, 19.8, 43.8, 103.7, 136, 213.1, 296.2, 457.6, 793.0, 1650.8, 3464.4, 4495.6, 5490.4)
runner  <- data.frame(runner.distance, runner.time)

# lineare Regression "walten lassen"
runner.reg <- lm(runner[,"runner.time"] ~ runner[,"runner.distance"])
summary(runner.reg)

# Der P-Wert ist sehr sehr nahe an Null, d.h. es wäre sehr sehr unwahrscheinlich, dass heisst es muss einen Zusammenhang geben bzw. Beta_1 kann nicht Null sein

# 2b)
# Formel anwenden zur Berechnung des Vertrauensinterwalls
konf.low <- 0.18170-(0.00173*(qt(0.975, df=11)))
konf.high <- 0.18170+(0.00173*(qt(0.975, df=11)))
konf.low
konf.high
# also ist es die Variante ii)

# 2c)
# Formel anwenden ohne die Störgrösse (Epsilon_i)
# also Messung - b_0 + b_1*x 
res.5 <- 136-(-62.59296+(0.1817*1000))

# 2d)
# Nein sollte man nicht können, denn über eine so lange Strecke werden Menschen anderst laufen (und andere Bedürfnisse haben die Zeit-relevant sind...)

# 2e)
# Der geschätzte Standardfehler wird im summary angegeben als sogenannter 'Residual standard error' und beträgt hier 62.68
# wichtig hierbei ist, dass man sieht, dass es für kleine Distanzen keinen Sinn macht, denn die Zeiten sind da wesentlich kürzer als diese Abweichung bzw. Genauigkeit

# 2f)
# es müsste homogen verteilt sein (schön gestreut) aber man sieht eine strukturierte Abweichung, das heisst, dass das erstellte bzw. gewählte Modell systematisch falsch ist

# 2g)
# da der Plot zu 2f) irgendwie quadratisch aussieht, könnte man eine quadratische Formel formulieren

# Aufgabe 3

# 3a)
# Datensatz von Hang eintippen. Ernsthaft???
hubble.speed <- c(170, 290, -130, -70, -185, -220, 200, 290, 270, 200, 300, -30, 650, 150, 500, 920, 450, 500, 500, 960, 500, 850, 800, 1090)
hubble.distance <- c(0.032, 0.034, 0.214, 0.263, 0.275, 0.275, 0.450, 0.500, 0.500, 0.630, 0.800, 0.900, 0.900, 0.900, 0.900, 1.000, 1.100, 1.100, 1.400, 1.700, 2.000, 2.000, 2.000, 2.000)
# kontrolle ob wenigstens gleichviele Einträge vorhanden sind da alles von Hand eingegeben
length(hubble.speed)-length(hubble.distance)

# einfach mal gegeneinander plotten
plot(hubble.speed, hubble.distance)

# Parameter ermitteln 
hubble.fit <- lm(hubble.distance ~ hubble.speed)
summary(hubble.fit)
# Regressionsgerade einzeichnen
abline(hubble.fit)

# wir zwingen 'lm()' uns die Parameter so zu berechnen, dass die Regressionsgerade durch den Punkt (0;0) geht mit
hubbe.zerofit <- lm(hubble.distance ~ 0+hubble.speed)
abline(hubble.zerofit, col='blue')
summary(hubble.zerofit)
# wir können noch farblich hervorheben, dass es wirklich bei (0;0) durchgeht
abline(h=0, col='red')
abline(v=0, col='red')

# Zeit ist Distanz dividiert durch die Geschwindigkeit, was der Steigung der Regressionsgerade entspricht, also rechnen wird die megaparsec-Sekunden * Steigung
spaceage <- 979.8*0.001922
spaceage

# 

# Konfidenzintervall = 95%
alpha <- 0.05
# Vertrauensintervall bestimmen
# ACHTUNG: Wir wollen den Schnittpunkt (intercept) zu y=0 also zur x-Achse ermitteln bzw. das Vertrauensintervall dazu. Wir müssen also mit den Werten aus der '(Intercept)' Zeile arbeiten
# intervall = Estimate +/- (Std. Error * qt(1-(alpha/2), Degree-of-Freedom))
konf.low <- (0.3990982-(0.1184697*qt(1-(alpha/2), df=22)))
konf.high <- (0.3990982+(0.1184697*qt(1-(alpha/2), df=22)))
konf.low
konf.high

# Benötigte Zeit für Aufgabe 3: ca. 60 Minuten Blut schwitzen und Skript konsolutieren ohne erfolg und dann noch ca. 40 Minuten zu zweit besprechen und korrigieren
