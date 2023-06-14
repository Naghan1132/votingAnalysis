# après on se fixe un n_candidat 3 => 2 candiidats unif et 1 beta
# (faire varier param alpha et beta)
# et voir si le résultats des méthodes donnent toujours le même résultat

# Modifier popularité d'un candidat

library(voteSim)
library(stats)

#define range
p = seq(0,1, length=100)
#create plot of Beta distribution with shape parameters 2 and 10
plot(p, dbeta(p, 6,3), type='l')


echantillon1 <- rbeta(100, 10, 3)
echantillon2 <- rbeta(100, 2, 2)
print(mean(echantillon1)) # => 0.75
print(mean(echantillon2)) # => 0.52


# ==== LOADING DATA FROM EXPERIMENTS votingExperiments::evolving_alpha_beta() ====

#test avec 4 random fichier

load("experiments_output_data/beta_unif/0.2_alpha_0.1_beta_10_simus.RData")
matrix_0.2_alpha_0.1_beta <- dissimilarity_matrix
load("experiments_output_data/beta_unif/0.6_alpha_1.5_beta_10_simus.RData")
matrix_0.6_alpha_1.5_beta <- dissimilarity_matrix
load("experiments_output_data/beta_unif/1_alpha_10_beta_10_simus.RData")
matrix_1_alpha_10_beta_10_simus <- dissimilarity_matrix
load("experiments_output_data/beta_unif/5_alpha_0.9_beta_10_simus.RData")
matrix_5_alpha_0.9_beta_10_simus <- dissimilarity_matrix



# ==== MDS ====

alpha <- c("0.2","0.6","1","5")
beta <- c("0.1","1.5","10","0.9")
couples <- expand.grid(alpha = alpha, beta = beta)
print(couples)
for (c in couples) {
  print(c)
}

#créer tous les couple de alpha/beta => les col et les row auront les noms de tout les couples

evolution_matrix <- matrix(0,length(alpha),length(beta))
colnames(evolution_matrix) <- alpha
rownames(evolution_matrix) <- beta
View(evolution_matrix)

for (i in 1:(length(alpha) - 1)) {
  for (j in (i + 1):length(beta)) {
    # fonction distance
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_", alpha[i]))) - as.vector(get(paste0("matrix_", beta[j]))))^2))
    evolution_matrix[i, j] <- similarity
    evolution_matrix[j, i] <- similarity
  }
}

mds <- cmdscale(evolution_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(evolution_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5) +
  geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed")
p <- ggplotly(p)
print(p)










