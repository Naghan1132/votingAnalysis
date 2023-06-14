# après on se fixe un n_candidat 3 => 2 candiidats unif et 1 beta
# (faire varier param alpha et beta)
# et voir si le résultats des méthodes donnent toujours le même résultat

# Modifier popularité d'un candidat


library(ggplot2)
library(plotly)
library(gridExtra)

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
load("experiments_output_data/beta_unif/0.2_alpha_0.3_beta_10_simus.RData")
matrix_0.2_alpha_0.3_beta <- dissimilarity_matrix
load("experiments_output_data/beta_unif/0.6_alpha_0.1_beta_10_simus.RData")
matrix_0.6_alpha_0.1_beta <- dissimilarity_matrix
load("experiments_output_data/beta_unif/0.6_alpha_0.3_beta_10_simus.RData")
matrix_0.6_alpha_0.3_beta <- dissimilarity_matrix



# ==== MDS ====

alpha <- c("0.2_alpha","0.6_alpha")
beta <- c("0.1_beta","0.3_beta")
couples <- expand.grid(alpha = alpha, beta = beta)
print(couples)
chaine_couples <- paste(couples$alpha, couples$beta, sep = "_")
# Affichage de la chaîne de caractères
print(chaine_couples)


#créer tous les couple de alpha/beta => les col et les row auront les noms de tout les couples

evolution_matrix <- matrix(0,length(chaine_couples),length(chaine_couples))
colnames(evolution_matrix) <- chaine_couples
rownames(evolution_matrix) <- chaine_couples
View(evolution_matrix)

for (i in 1:(length(chaine_couples) - 1)) {
  for (j in (i + 1):length(chaine_couples)) {
    # fonction distance
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_", chaine_couples[i]))) - as.vector(get(paste0("matrix_", chaine_couples[j]))))^2))
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










