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


##Normality tests
get.correlations("Normality tests for means in study 1",
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

# Correlations - n ratings, presentation and evaluation time


cor = mean_times %>%
  pivot_wider(id_cols = c("ord", "code", "category"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean_pres","mean_eval", "n"))

sink(file = file.path(psubdir,"Cor_times_studies.txt"), type ="output")

## Mean no of ratings per study
cat("\n","Mean number of ratings per study, mean presentation times, mean evaluation times", "\n", "\n")
mean(cor$n.1)
mean(cor$n.2)

# Mean presentation times per study
cat("\n","Mean presentation times per study", "\n", "\n")
mean(cor$mean_pres.1)
mean(cor$mean_pres.2)

# Mean presentation times per study
cat("\n","Mean evaluation times per study", "\n", "\n")
mean(cor$mean_eval.1)
mean(cor$mean_eval.2)

## Normality tests
cat("\n","Normality of presentation and evaluation time distribution", "\n", "\n")
normality_pt = shapiro.test(mean_times$mean_pres)
normality_et = shapiro.test(mean_times$mean_eval)
print(normality_pt)
print(normality_et)

## Presentation time correlations
cat("\n","Presentation time correlations", "\n", "\n")

res4 = cor.test(cor$mean_pres.1, cor$mean_pres.2, method = "pearson")
print(res4)
  
## Evaluation time correlations
cat("\n","Evaluation time correlations", "\n", "\n")
res5 = cor.test(cor$mean_eval.1, cor$mean_eval.2, method = "kendall")
print(res5)

sink(file = NULL)

ggplot(cor, aes(mean_pres.1, mean_pres.2)) +
  geom_point()

ggplot(cor, aes(mean_eval.1, mean_eval.2)) +
  geom_point()
