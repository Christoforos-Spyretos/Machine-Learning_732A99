data <- read.csv("Labs/Lab02/communities.csv")

# Task 1

data_scale <- data.frame(scale(data[,-101]))

data_scale$ViolentCrimesPerPop <- data$ViolentCrimesPerPop

cov_matrix <- cov(data_scale)

eig <- eigen(cov_matrix)

counter <- which(cumsum(eig$values/sum(eig$values) * 100) >= 95)
counter[1]

prop_var <- sprintf("%2.3f", eig$values/sum(eig$values)*100)
prop_var[c(1:2)]

# Task 2

pca <- princomp(data_scale)

plot(pca$loadings[,1], main = "Score Plot", pch = 16, col = "navy", ylab = "First Principle Component" )

df_scores <- data.frame(pca$scores[,1:2])

df_scores$ViolentCrimesPerPop <- data$ViolentCrimesPerPop

ggplot(data = df_scores, aes(x=Comp.1, y=Comp.2, color = ViolentCrimesPerPop)) + geom_point() +
  labs(title("PC Scores of PC1 & PC2")) + xlab("PC1") + ylab("PC2")
  #guides(color=guide_legend("Violent Crimes Per 100K Population"))







