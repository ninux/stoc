# lesson 11

# Aufgabe 1a)
# Es ist ein gepaarter Test weil es zwei mal die selbe Messung ist.

# 1b) 
messung.A <- c(120, 265, 157, 187, 219, 288, 156, 205, 163)
messung.B <- c(127, 281, 160, 185, 220, 298, 167, 203, 171)

# Modellannahme: t-Test, denn es halndelt sich um eine kontinuierliche
# Messgrösse

# Nullhypothese: Es gibt keinen Unterschied der Messreihen

# Alternative: Messreihe B ist systematisch höher

# Teststatistik: (beobachtet - erwartet)/(geschätzter Standardfehler)

# Signifikanzniveau: alpha=0.05

# Vertrauensbereich: Wir vermuten, dass 

# Testergebnis
t.test(messung.A-messung.B, mu=0, conf.level=0.95)


# lösung vom dozenten
d <- messung.A - messung.B

mean.d <- mean(d)
sd.d <- sd(d)
n <- length(d)
testst.d <- mean.d/(sd.d/sqrt(n))
testst.d
qqnorm(messung.A) # mit qqnorm prüfen wir ob die messreihen normalverteilt sind
qqnorm(messung.B)
t.test(messung.A, messung.B, mu=0, paired=TRUE, alternative="less")
t.test(d)


# Aufgabe 2

# 2a) 
# Es ist ein ungepaarter Test, weil es sich nicht um die selben Messungen handelt.

# 2b) NICHT MACHEN!

# 2c)
jackals <- read.table("http://stat.ethz.ch/Teaching/Datasets/jackals.dat", header=TRUE)
jackals
qqnorm(jackals[,"M"]) # prüfen ob es normalverteilt ist
qqnorm(jackals[,"W"])
# falls die daten normalverteilt sind machen wir den t-Test
t.test(jackals[,"M"], jackals[,"W"], var.equal=TRUE)
# falls die daten nicht normalverteilt sind machen wir den wilcox.test
# dieser setzt voraus, dass beide datenreihen eine ähnliche verteilung haben
# diese können verschoben sein, müssen aber die gleiche Form haben

# 2d)
wilcox.test(jackals[,"M"], jackals[,"W"])

# 3a)
# Es handelt sich un einen gepaarten Test, da der selbe Test zwei mal durchgeführt wird.

# 3b)
ghhg	

# Aufgabe 4

# 4a)
tiefe <- c(0, 0.2, 0.5, 0.6, 0.8, 0.9, 1.2, 6.0)
temperatur <- c(6, 4.2, 0.6, -2.1, -5.2, -7.3, -8.9, 15)
plot(tiefe, temperatur, main="Streudiagramm")
# die messreihe ist etwas ungünstig da eine unschöne streuung vorhanden ist
# dort wo sich der charakter der reihe ändert gibt es keine datenpunkte
# oder es gibt einen zufälligen ausreisser 

# 4b)
# wir entfernen den ausreisser aus den daten
tiefeoA <- tiefe[-8]
temperaturoA <- temperatur[-8]
plot(tiefeoA, temperaturoA)
abline(lm(temperaturoA~tiefeoA))
