test_calc_matrix <- calc_matrix

test_calc_matrix$genre <- seq(1,5)

df_long <- pivot_longer(test_calc_matrix, cols = -genre, names_to = 
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
