# Paul GOUJON, Stéphane LOUIS
# UTC - SY09 - TP2
# Exercice 3

# Iris =====================================
data(iris)
i = iris
pri = princomp(iris[,1:4])
di = dist(i[,1:4])

ki2 = kmeans(di, 2)
ki3 = kmeans(di, 3)
ki4 =kmeans(di, 4)

pdf("E3Q1_ki2.pdf")
plot(pri$scores[,1], pri$scores[,2], col=ki2$cluster, main="Partition du dataset Iris via Kmeans en 2 classes", xlab="Component 1", ylab="Component 2")
dev.off()

pdf("E3Q1_ki3.pdf")
plot(pri$scores[,1], pri$scores[,2], col=ki3$cluster, main="Partition du dataset Iris via Kmeans en 3 classes", xlab="Component 1", ylab="Component 2")
dev.off()

pdf("E3Q1_ki4.pdf")
plot(pri$scores[,1], pri$scores[,2], col=ki4$cluster, main="Partition du dataset Iris via Kmeans en 4 classes", xlab="Component 1", ylab="Component 2")
dev.off()

pdf("E3Q1_ki2_2.pdf")
plot(i, col=ki2$cluster, main="Partition du dataset Iris via Kmeans en 2 classes")
dev.off()

pdf("E3Q1_ki3_2.pdf")
plot(i, col=ki3$cluster, main="Partition du dataset Iris via Kmeans en 3 classes")
dev.off()

pdf("E3Q1_ki4_2.pdf")
plot(i, col=ki4$cluster, main="Partition du dataset Iris via Kmeans en 4 classes")
dev.off()


# Q2 : répétition de la même méthode jusqu'à obtenir des résultats différents


# Q3 : Détermination nombre optimal de classes
mat = matrix(nrow=100, ncol=10)
for (i in 1:100) { for (j in 1:10) { ki = kmeans(di, j); mat[i,j] = ki$tot.withinss; }}
mins = apply(mat,2,min)


# Crabs =======================================================================

c = read.csv("crabs2.csv", header=T)
dc = dist(c[,1:4])

# K = 2
kc = kmeans(dc, 2) # répété un grand nombre de fois
pdf("plots/E3Q5_1.pdf")
plot(prc$scores[,1], prc$scores[,2], col=kc$cluster, main="Classification du dataset crabs2 en K = 2 classes (kmeans)", xlab="Component 1", ylab="Component 2")
dev.off()

pdf("plots/E3Q5_2.pdf")
plot(c, col=kc$cluster, main="Classification du dataset crabs2 en K = 2 classes (kmeans)")
dev.off()

# K = 4
kc = kmeans(dc, 4)
c$spsex = interaction(c$sp, c$sex)

pdf("plots/E3Q6_1.pdf")
plot(c, col=kc$cluster, main="Classification du dataset crabs2 en K = 4 classes (kmeans)")
dev.off()

pdf("plots/E3Q6_2.pdf")
plot(prc$scores[,1], prc$scores[,2], col=kc$cluster, main="Classification du dataset crabs2 en K = 4 classes (kmeans)", xlab="Component 1", ylab="Component 2")
dev.off()

# Mutations ===================================================================

m = read.csv("mutations2.csv", header=T, row.names=1)
m = as.dist(m, diag=T, upper=T)
cm = cmdscale(m, k=5)

km = kmeans(m, 3) # several times
pdf("plots/E3Q7_1.pdf")
plot(cm[,1], cm[,2], col=km$cluster, main="Classification du dataset Mutations en K = 3 classes (kmeans)", xlab="Component 1", ylab="Component 2")
dev.off()









