require(magrittr)

### CALCULATING ORIGINAL GENRE METRICS

w_genres <- cbind(spotify.a, spotify$genre)

### WITHINSS

total_withinss <- 0

for (g in genres) {
  filtered <- w_genres[grepl(g, spotify$genre),][,1:10]
  centers <- apply(filtered, 2, mean)
  sq_diff <- apply(filtered, 1, function(x) (x-centers)^2)
  withinss <- sum(sq_diff)
  total_withinss <- total_withinss + withinss
}


original_withinss <- total_withinss
k5_withinss <- k5clst$tot.withinss

### H SINGLE

sh_genres <- cbind(spotify.a, hclst_single)

total_withinss <- 0

for (g in 1:5) {
  filtered <- (sh_genres %>% filter(hclst_single == g))[,1:10]
  centers <- apply(filtered, 2, mean)
  sq_diff <- apply(filtered, 1, function(x) (x-centers)^2)
  withinss <- sum(sq_diff)
  total_withinss <- total_withinss + withinss
}

h_single_withinss <- total_withinss


### H COMPLETE

sh_genres <- cbind(spotify.a, hclst_comp)

total_withinss <- 0

for (g in 1:5) {
  filtered <- (sh_genres %>% filter(hclst_comp == g))[,1:10]
  centers <- apply(filtered, 2, mean)
  sq_diff <- apply(filtered, 1, function(x) (x-centers)^2)
  withinss <- sum(sq_diff)
  total_withinss <- total_withinss + withinss
}

h_comp_withinss <- total_withinss

d <- data.frame(Genre_System = 
                  c("Original", "K-means", "H S-Linkage",
                    "H C-Linkage"),
                Within_SS = 
                  c(original_withinss, k5_withinss,  h_single_withinss,
                    h_comp_withinss))


barplot(d$Within_SS, xlab = "Genre System", ylab = "", names.arg = d$Genre_System, col = "blue", main = "Total Within-Cluster Sum of Squares")
