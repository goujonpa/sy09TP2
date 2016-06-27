# Paul GOUJON, Stéphane LOUIS
# UTC - SY09 - TP2
# Exercice 2

# Mutations =============================================

m = read.csv("mutations2.csv", header=T, row.names=1)
m = as.dist(m, diag=T, upper=T)

# clustering
clustm = hclust(m, method="ward.D2")
pdf("plots/E2Q1_CLUSTm.pdf")
plot(clustm, main="Classification hiérachique ascendante a partir du dataset Mutations")
dev.off()

# Iris ==================================================

data(iris)
i = iris
di = dist(iris[,1:4]) # calcul la distance entre les individus
cdi = hclust(m, method="ward.D2")
pdf("plots/E2Q2_cdi.pdf")
plot(cdi, main="Classification du tableau de dissimilarités issu du dataset Iris")
dev.off()

# with diana
didi = diana(di)
pdf("plots/E2Q3_diani.pdf")
plot(didi, main="Classification hiérarchique descendante du tableau de dissimilarités issu du dataset Iris")
dev.off()



