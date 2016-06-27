# Paul GOUJON, Stéphane LOUIS
# UTC - SY09 - TP2
# Exercice1

# IRIS =====================================================
# load data
data(iris);
i = iris # on aime les noms de variables simples 
pir = princomp(iris[,1:4]) # ACP
pdf("plots/E1Q1ACPiris.pdf")
biplot(pir, main="ACP sur le jeu Iris (1er plan factoriel)", xlab="Component 1", ylab="Component 2")
dev.off()


# Nouvelle représentation, en utilisant les couleurs pour différencier les
# espèces
pdf("plots/E1Q1_2_ACPiris.pdf")
plot(pir$scores[,1], pir$scores[,2], col=iris$Species, main="ACP sur le dataset Iris (1er Plan Factoriel) - Différenciation des espèces", xlab="Component 1", ylab="Component 2")
dev.off()


# Crabs ====================================================
c = read.csv("crabs2.csv", header=T)
prc = princomp(c[,1:4])
pdf("E1Q2_ACPcrabs.pdf")
biplot(prc, main="ACP sur le dataset crabs2 (1er plan factoriel)", xlab="Component 1", ylab="Component 2")
dev.off()


# colored crabs ACP
spsex = interaction(c$sp, c$sex)
pdf("plots/E1Q2_2_ACPcrabs.pdf")
plot(prc$scores[,1], prc$scores[,2], col=spsex, main="ACP sur le dataset crabs2 (1er plan factoriel)", xlab="Component 1", ylab="Component 2")
dev.off()

# MUTATIONS ================================================
m = read.csv("mutations2.csv", header=T, row.names=1)
m = as.dist(m, diag=T, upper=T)

# mds
mds = cmdscale(m, k=2)
pdf("plots/E1Q3_MDSm.pdf")
plot(mds[,1], mds[,2], main="MDS (k=2) du dataset Mutations (Tableau de dissimilarités)", xlab="Dimension 1", ylab="Dimension 2")
abline(h=0)
abline(v=0)
abline(0,1)
dev.off()

# shepard
help(Shepard)
shep = Shepard(m, cmdscale(m, k=2))
plot(shep)
pdf("plots/E1Q3_SHEPm.pdf")
plot(shep, main="Diagramme de Shepard pour le MDS du dataset Mutations, avec k = 2", xlab="Dimension 1", ylab="Dimension 2")
abline(h=0)
abline(v=0)
abline(0,1)
dev.off()

          1
shep3 = Shepard(m, cmdscale(m, k=3))
shep4 = Shepard(m, cmdscale(m, k=4))
shep4 = Shepard(m, cmdscale(m, k=5))
shep4 = Shepard(m, cmdscale(m, k=4))
shep5 = Shepard(m, cmdscale(m, k=5))
pdf("plots/E1Q3_SHEP3m.pdf")
plot(shep3, main="Diagramme de Shepard pour le MDS du dataset Mutations, avec k = 3", xlab="Dimension 1", ylab="Dimension 2")
abline(h=0)
abline(v=0)
abline(0,1)
dev.off()

 
pdf("plots/E1Q3_SHEP5m.pdf")
plot(shep5, main="Diagramme de Shepard pour le MDS du dataset Mutations, avec k = 5", xlab="Dimension 1", ylab="Dimension 2")
abline(h=0)
abline(v=0)
abline(0,1)
dev.off()

