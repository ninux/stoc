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
# der Tukey-Annscombe-Plot zeigt auf, dass es keinen systematischen Fehler gibt (?)
# WAS IST EIN TUKEY-ANSCOMBE-PLOT???
# der QQ-Norm-Plot zeigt: Fehler ist linear?
qqline(resid(forbes.fit))
# einigermassen gerade???

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
# Modellannahmen? KEINE AHNUNG WAS GEMEINT IST!!!

# 1f)
# Ausreisser mittels 'identify' lokalisieren
identify(fitted(forbes.logfit), resid(forbes.logfit))
# es zeigt an, dass die Beobachtung die Nummer 12 hat
# die Beobachtung wird nun aus dem Datensatz entfernt
forbes <- forbes[-12,]

# plots nochmals erstellen
# ich erkenne lediglich, dass der Tukey-Anscombe-Plot eine wildere Streuung aufzeigt

# Benötigte Zeit um die Aufgabe 1 zu lösen: ca. 60 Minuten
# Alles Verstanden: NEIN
# Offene Fragen: 
# * Was Zeigt ein Tukey-Anscombe-Plot?
# * Was macht 'fitted()'?


# Aufgabe 2

# 2a)

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

# 

# Konfidenzintervall = 95%
alpha <- 0.95
# Vertrauensintervall bestimmen
# intervall = Estimate +/- (Std. Error * qt(1-(alpha/2), Degree-of-Freedom))
konf.low <- (0.0013729-(0.0002274*qt(1-(alpha/2), df=22)))
konf.high  <- (0.0013729+(0.0002274*qt(1-(alpha/2), df=22)))


