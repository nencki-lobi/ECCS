# Correlations - means between studies

##Preparing data

cor = story_mean_ratings_study %>%
  pivot_wider(id_cols = c("ord","category", "part"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean","n"))

get.correlations = function(title, result) {
  cat("\n", title, "\n", "\n")
  print(result)
}

sink(file = file.path(psubdir,"Cor_means_studies.txt"), type ="output")
get.correlations("Normality tests for means in study 1",

##Normality tests

for (i in 0:6) {
  df = cor %>%
    filter(part == i)
  
  normality_m1 = shapiro.test(df$mean.1)
  normality_m2 = shapiro.test(df$mean.2)
  print(normality_m1)
  print(normality_m2)
}
)

## Correlation of means ratings on each scale regardless of category

get.correlations("Correlation of means ratings on each scale regardless of category",
                 
### more parameters (p values, conf intervals)

# for (i in 0:6) {
#   df = cor %>%
#     filter(part == i)
#   
#   res1 = cor.test(df$mean.1, df$mean.2, method = "pearson")
#   print(res1)
# }

###OR shorter

for (i in 0:6) {
  df = cor %>% 
    filter(part == i)
  res1 = cor(df$mean.1, df$mean.2)
  print(res1)
}
)
## Correlation of means ratings for each scale regardless of scale

get.correlations("Correlation of means ratings for each scale regardless of scale",
                 
### more parameters (p values, conf intervals)

# for (j in labels_categories) {
#   df = cor %>%
#     filter(category == j)
#   
#   res2 = cor.test(df$mean.1, df$mean.2, method = "pearson")
#   print(res2)
# }

### OR shorter

for (j in labels_categories) {
  df = cor %>% 
    filter(category == j)
  res2 = cor(df$mean.1, df$mean.2)
  print(res2)
}
)
## Correlation of mean ratings for each category on each scale

get.correlations("Correlation of mean ratings for each category on each scale",
                 
### more parameters (p values, conf intervals)

# for (j in labels_categories) {
#   for (i in 0:6) {
#     df = cor %>%
#       filter(part == i & category == j)
#     res3 = cor.test(df$mean.1, df$mean.2, method = "pearson")
#     print(res3)
#   }
# }

### OR shorter

for (j in labels_categories) {
  for (i in 0:6) {
    df = cor %>% 
      filter(part == i & category == j)
    res3 = cor(df$mean.1, df$mean.2)
    print(res3)
  }
}
)
sink(file = NULL)