# après on se fixe un n_candidat 3 => 2 candiidats unif et 1 beta
# (faire varier param alpha et beta)
# et voir si le résultats des méthodes donnent toujours le même résultat

# Modifier la popularité d'un candidat

library(ggplot2)
library(plotly)
library(gridExtra)

# ==== LOADING DATA FROM EXPERIMENTS votingExperiments::evolving_alpha_beta() ====
alpha <- c(0.5,0.6,0.7,0.8,0.9,1)
beta <- c(0.5,0.6,0.7,0.8,0.9,1)


matrix_list <- list()
for (a in alpha) {
  for (b in beta) {
    # Loading files
    file_path <- paste0("experiments_output_data/alpha_beta_evolving/",a, "_alpha_",b, "_beta_50_simus.RData")
    load(file_path)
    matrix_name <- paste0("matrix_", a, "_alpha_", b, "_beta")
    matrix_list[[matrix_name]] <- dissimilarity_matrix
  }
}
for (a in alpha) {
  for (b in beta) {
    # Construire le nom de la variable avec un nom dynamique
    var_name <- paste0("matrix_",a,"_alpha_",b,"_beta")
    # Assigner la matrice de dissimilarité correspondante à la variable
    assign(var_name, matrix_list[[var_name]])
  }
}

# ==== Calculate All MDS ====

# Créer une liste pour stocker les résultats de MDS
mds_list <- list()
for (a in alpha) {
  for (b in beta) {
    matrix_name <- paste0("matrix_", a, "_alpha_", b, "_beta")
    dissimilarity_matrix <- matrix_list[[matrix_name]]
    mds_result <- cmdscale(dissimilarity_matrix, k = 2)
    mds_list[[matrix_name]] <- mds_result
  }
}


# ==== Generate All PLOTS ====

plot_list <- list()
for (a in alpha) {
  for (b in beta) {
    matrix_name <- paste0("matrix_", a, "_alpha_", b, "_beta")
    mds_name <- paste0("mds_", a, "_alpha_", b, "_beta")

    dissimilarity_matrix <- matrix_list[[matrix_name]]
    mds_result <- mds_list[[matrix_name]]

    # Créer le graphique MDS
    plot <- ggplot(as.data.frame(mds_result), aes(x = V1, y = V2, label = rownames(dissimilarity_matrix))) +
      geom_point(color = "blue", size = 3) +
      geom_text(hjust = 0.5, vjust = -0.5) +
      labs(x = "Dimension 1", y = "Dimension 2", title = paste0("MDS - ", a, " alpha, ", b, " beta"))

    plot_list[[paste0("plot_", a, "_alpha_", b, "_beta")]] <- plot
  }
}
# Convertir la liste de graphiques en une liste de grobs
grobs <- lapply(plot_list, ggplotGrob)

# ==== GRIDs ====
grid.arrange(grobs$plot_0.5_alpha_0.5_beta,grobs$plot_0.6_alpha_0.7_beta,grobs$plot_0.8_alpha_0.9_beta,grobs$plot_1_alpha_1_beta,ncol=2,nrow=2)



# ==== Construire les couples alpha/beta pour analyse ====
couples <- expand.grid(alpha = alpha, beta = beta)
chaine_couples <- paste(couples$alpha, couples$beta, sep = "_")

for (i in 1:length(chaine_couples)) {
  values <- strsplit(chaine_couples[i],"_")
  chaine_couples[i] <- paste0(values[[1]][1],"_alpha_",values[[1]][2],"_beta")
}

# ==== Matrice d'évolution ====
evolution_matrix <- matrix(0,length(chaine_couples),length(chaine_couples))
colnames(evolution_matrix) <- chaine_couples
rownames(evolution_matrix) <- chaine_couples

# ==== Distance ====
for (i in 1:(length(chaine_couples) - 1)) {
  for (j in (i + 1):length(chaine_couples)) {
    # fonction distance
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_",chaine_couples[i]))) - as.vector(get(paste0("matrix_", chaine_couples[j]))))^2))
    evolution_matrix[i, j] <- similarity
    evolution_matrix[j, i] <- similarity
  }
}

# ==== MDS ====
mds <- cmdscale(evolution_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(evolution_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5) +
  geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed")
p <- ggplotly(p)
print(p)


View(evolution_matrix)







