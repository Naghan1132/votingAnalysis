library(ggplot2)
library(plotly)
library(gridExtra)

# ====

# focus sur 2 méthodes, faire varier nC et nV et voir dans quels cas les méthodes se reprochent ou s'éloignent

methods_choosen <- c("uninominal1T","approval")
#methods_choosen <- c("approval","range_voting")
# methods_choosen <- c("borda","range_voting")

num_candidates <- c(3, 4, 5, 7, 9, 14)
num_voters <- c(9, 15, 21, 51, 101, 1001)

name_list <- list()
matrix_list <- list()
for (num_c in num_candidates) {
  for (num_v in num_voters) {
    # Loading files
    #file_path <- paste0("experiments_output_data/nc_nv_evolving/beta/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    file_path <- paste0("experiments_output_data/nc_nv_evolving/unif/", num_v, "_voters_", num_c, "_candidates_100_simus.RData")
    #file_path <- paste0("experiments_output_data/nc_nv_evolving/norm/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    load(file_path)
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    matrix_list[[matrix_name]] <- dissimilarity_matrix
    name_list <- append(name_list,matrix_name)
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


# select only methods we focus on
for(n in name_list){
  m <- get(n)[methods_choosen,methods_choosen]
  assign(n, m) # sauvegarde
}

# faire varier nC et nV => et MDS avec des points de 2 différentes couleurs


voters <- c("9_voters","15_voters","21_voters","51_voters","101_voters")
candidates <- c("3_candidates","4_candidates","5_candidates","7_candidates","9_candidates","14_candidates")
couples <- expand.grid(methods = methods_choosen,voters = voters, candidates = candidates)
chaine_couples <- paste(couples$methods,couples$voters, couples$candidates, sep = "_")

print(chaine_couples)

# ==== Matrice d'évolution ====
evolution_matrix <- matrix(0,length(chaine_couples),length(chaine_couples))
colnames(evolution_matrix) <- chaine_couples
rownames(evolution_matrix) <- chaine_couples
View(evolution_matrix)
cpt = 0
# ==== Distance ====
for (i in 1:(length(chaine_couples) - 1)) {
  for (j in (i + 1):length(chaine_couples)) {
    print(chaine_couples[i])
    print(chaine_couples[j])
    print("__")

    name1 <- strsplit(chaine_couples[i],"_")[[1]][1]
    v1 <- strsplit(chaine_couples[i],"_")[[1]][2]
    c1 <- strsplit(chaine_couples[i],"_")[[1]][4]

    name2 <- strsplit(chaine_couples[j],"_")[[1]][1]
    v2 <- strsplit(chaine_couples[j],"_")[[1]][2]
    c2 <- strsplit(chaine_couples[j],"_")[[1]][4]

    m1 <- get(paste0("matrix_",v1,"_voters_",c1,"_candidates"))
    m2 <- get(paste0("matrix_",v2,"_voters_",c2,"_candidates"))

    print(m1)
    print(m1[name1,name2])
    print(m2)
    print(m2[name1,name2])

    if(c1 == c2 & v1 == v2){
      # si même matrice (m1 == m2)
      similarity <- m1[name1,name2]
    }else{
      similarity <- sqrt(sum((m1[name1,name2] - m2[name1,name2])^2))
    }
    print(similarity)
    evolution_matrix[i, j] <- similarity
    evolution_matrix[j, i] <- similarity
    cpt = cpt +1
    if(cpt == 2){
      stop()
    }

  }
}
View(evolution_matrix)
print(isSymmetric(evolution_matrix))

# ==== MDS ====
mds <- cmdscale(evolution_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(evolution_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5)
p <- ggplotly(p)
print(p)
