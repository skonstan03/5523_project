require(data.table)
require(ggplot2)
require(magrittr)
require(gridExtra)
require(dplyr)
require(tidyr)

spotify <- fread("songs_normalize.csv")

names <- c("song")
dup_helper <- spotify %>% select(all_of(names))

spotify <- spotify[!(duplicated(dup_helper))] %>% filter(genre != "set()")


attributes <- c("duration_ms", "popularity", "danceability", "energy", "loudness",
                "speechiness", "acousticness", "instrumentalness", "liveness", "valence")

spotify.a <- spotify %>% select(all_of(attributes))

k <- 5

clst <- hclust(dist(spotify.a), method = "single") %>% cutree(k)

hclst_single <- clst

hclst_comp <- hclust(dist(spotify.a), method = "complete") %>% cutree(k)

c_genre <- as.data.frame(cbind(spotify$genre, clst))

colnames(c_genre) <- c("Genre", "Cluster")

genres_with_combos <- unique(spotify$genre)

genres <- genres_with_combos[!grepl(",", genres_with_combos)]

calc_matrix <- matrix(0, nrow = k, ncol = length(genres))

colnames(calc_matrix) <- genres

rownames(calc_matrix) <- paste("Cluster", seq(1,k))

for (i in 1:k) {
  for (j in 1:length(genres)) {
    calc_matrix[i,j] <- dim(c_genre %>% filter(grepl(genres[j], Genre)) %>% filter(Cluster == i))[1]
  }
}

calc_matrix <- as.data.frame(calc_matrix)


sums <- apply(calc_matrix, 1, sum)

calc_matrix <- calc_matrix / sums

print(calc_matrix)

calc_matrix$genre <- seq(1,k)

df_long <- pivot_longer(calc_matrix, cols = -genre, names_to = 
                          "Genre", values_to = "Value")


df_long$Genre <- factor(df_long$Genre)
df_long$genre <- factor(df_long$genre)
ggplot(df_long, aes(x = genre, y = Value, fill = Genre)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Genre Mapping to Existing Genres",
       x = "New Genre ID",
       y = "PCT Songs by Old Genre",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

