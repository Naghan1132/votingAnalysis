library(ggplot2)
library(plotly)
library(gridExtra)

# ==== LOADING DATA FROM votingExperiments::nc_nv_evolving() ====

num_candidates <- c(3, 4, 5, 7, 9, 14)
num_voters <- c(9, 15, 21, 51, 101, 1001)

matrix_list <- list()
for (num_c in num_candidates) {
  for (num_v in num_voters) {
    # Loading files
    #file_path <- paste0("experiments_output_data/nc_nv_evolving/beta/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    file_path <- paste0("experiments_output_data/nc_nv_evolving/unif/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    #file_path <- paste0("experiments_output_data/nc_nv_evolving/norm/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    load(file_path)
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")

    #elimine <- list("anti_plularity","infinity")
    #indices <- which(colnames(dissimilarity_matrix) %in% elimine)
    #dissimilarity_matrix <- dissimilarity_matrix[-indices,-indices]

    matrix_list[[matrix_name]] <- dissimilarity_matrix
  }
}

for (num_v in num_voters) {
  for (num_c in num_candidates) {
    # Construire le nom de la variable avec un nom dynamique
    var_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    # Assigner la matrice de dissimilarité correspondante à la variable
    assign(var_name, matrix_list[[var_name]])
  }
}

# ==== Calculate All MDS ====

# Créer une liste pour stocker les résultats de MDS
mds_list <- list()
for (num_c in num_candidates) {
  for (num_v in num_voters) {
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    dissimilarity_matrix <- matrix_list[[matrix_name]]
    mds_result <- cmdscale(dissimilarity_matrix, k = 2)
    mds_list[[matrix_name]] <- mds_result
  }
}


# ==== Generate All PLOTS ====

plot_list <- list()
for (num_v in num_voters) {
  for (num_c in num_candidates) {
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    mds_name <- paste0("mds_", num_v, "_voters_", num_c, "_candidates")

    dissimilarity_matrix <- matrix_list[[matrix_name]]
    mds_result <- mds_list[[matrix_name]]

    # Créer le graphique MDS
    plot <- ggplot(as.data.frame(mds_result), aes(x = V1, y = V2, label = rownames(dissimilarity_matrix))) +
      geom_point(color = "blue", size = 3) +
      geom_text(hjust = 0.5, vjust = -0.5) +
      xlim(-400, 400) +
      ylim(-400, 400) +
      labs(x = "Dimension 1", y = "Dimension 2", title = paste0("MDS - ", num_v, " voters, ", num_c, " candidates"))

    plot_list[[paste0("plot_", num_v, "_voters_", num_c, "_candidates")]] <- plot
  }
}

# Convertir la liste de graphiques en une liste de grobs
grobs <- lapply(plot_list, ggplotGrob)

# ==== GRIDs ====
grid.arrange(grobs$plot_9_voters_3_candidates,grobs$plot_9_voters_5_candidates,grobs$plot_9_voters_7_candidates,grobs$plot_9_voters_14_candidates,ncol=2,nrow=2)

grid.arrange(grobs$plot_9_voters_3_candidates,grobs$plot_51_voters_3_candidates,grobs$plot_101_voters_3_candidates,grobs$plot_1001_voters_3_candidates,ncol=2,nrow=2)

grid.arrange(grobs$plot_9_voters_5_candidates,grobs$plot_51_voters_5_candidates,grobs$plot_101_voters_5_candidates,grobs$plot_1001_voters_5_candidates,ncol=2,nrow=2)

grid.arrange(grobs$plot_9_voters_14_candidates,grobs$plot_51_voters_14_candidates,grobs$plot_101_voters_14_candidates,grobs$plot_1001_voters_14_candidates,ncol=2,nrow=2)


# Afficher les graphiques côte à côte
grid.arrange(grobs = grobs, ncol = length(num_candidates), nrow = length(num_voters))

# Sauvegarde des grobs
for (i in seq_along(grobs)) {
  name <- names(as.list(plot_list))[i]
  ggsave(paste0("plot_img_without_somes_methods/plot_",name,".png"), plot = grobs[[i]], width = 8, height = 8, dpi = 300)
}

# ==== Distance Analysis by voters and candidates ====

# TRÈS intéressant !
voters <- c(15,101)
candidates <- c(3,4,5)
couples <- expand.grid(voters = voters, candidates = candidates)
chaine_couples <- paste(couples$voters, couples$candidates, sep = "_")


for (i in 1:length(chaine_couples)) {
  values <- strsplit(chaine_couples[i],"_")
  chaine_couples[i] <- paste0(values[[1]][1],"_voters_",values[[1]][2],"_candidates")
}


# ==== Matrice d'évolution ====
evolution_matrix <- matrix(0,length(chaine_couples),length(chaine_couples))
colnames(evolution_matrix) <- chaine_couples
rownames(evolution_matrix) <- chaine_couples

# ==== Distance ====
for (i in 1:(length(chaine_couples) - 1)) {
  for (j in (i + 1):length(chaine_couples)) {
    # distance betwee, all couples
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


# ==== Analysis (line by line) ====

# faire distance sur chaque ligne, puis faire MDS sur toutes ces matrices de distances
# Pour chaque ligne : calculer les distances entre matrices de dissimilarirtés à n_votants fixés
# , et faire le MDS de ces situations on obtient 6 points reliés (un pour chaque situations),
# et donc 7 graphes MDS


# === Addionner les matrices de dissimilarité de chaque lignes entres elles ===
methods_names <- c("uninominal1T","uninominal2T","successif_elimination","bucklin","borda","nanson","minimax","copeland","condorcet","range_voting","approval","JM","infinity","star","anti_plularity")
for(v in num_voters){
  var_name <- paste0("matrix_",v,"_voters")
  line_matrix <- matrix(0, length(methods_names),length(methods_names))
  colnames(line_matrix) <- methods_names
  rownames(line_matrix) <- methods_names
  for (c in num_candidates) {
    m1 <-  paste0("matrix_",v,"_voters_",c,"_candidates")
    line_matrix <- line_matrix + get(m1)
  }
  assign(var_name, line_matrix)
}


# === Puis faire la distances entres toutes les lignes ===

lines_voters <- c("9_voters","15_voters","21_voters","51_voters","101_voters","1001_voters")

evolution_matrix <- matrix(0,length(lines_voters),length(lines_voters))
colnames(evolution_matrix) <- lines_voters
rownames(evolution_matrix) <- lines_voters

for (i in 1:(length(lines_voters) - 1)) {
  for (j in (i + 1):length(lines_voters)) {
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_",lines_voters[i]))) - as.vector(get(paste0("matrix_", lines_voters[j]))))^2))
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


# ==== Analysis (column by column) ====


# == Addionner les matrices de dissimilarité de chaque colonnes entres elles ==
methods_names <- c("uninominal1T","uninominal2T","successif_elimination","bucklin","borda","nanson","minimax","copeland","condorcet","range_voting","approval","JM","infinity","star","anti_plularity")
for(c in num_candidates){
  var_name <- paste0("matrix_",c,"_candidates")
  col_matrix <- matrix(0, length(methods_names),length(methods_names))
  colnames(col_matrix) <- methods_names
  rownames(col_matrix) <- methods_names
  for (v in num_voters) {
    m1 <-  paste0("matrix_",v,"_voters_",c,"_candidates")
    col_matrix <- col_matrix + get(m1)
  }
  assign(var_name, col_matrix)
}


# == Puis faire la distances entres toutes les colonnes ==

columns_candidates <- c("3_candidates","4_candidates","5_candidates","7_candidates","9_candidates","14_candidates")

evolution_matrix <- matrix(0,length(columns_candidates),length(columns_candidates))
colnames(evolution_matrix) <- columns_candidates
rownames(evolution_matrix) <- columns_candidates

for (i in 1:(length(columns_candidates) - 1)) {
  for (j in (i + 1):length(columns_candidates)) {
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_",columns_candidates[i]))) - as.vector(get(paste0("matrix_", columns_candidates[j]))))^2))
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


