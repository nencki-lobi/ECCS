# Plots

pdir = "./output"
if (!dir.exists(pdir)) {dir.create(pdir)}

psubdir = file.path(pdir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required)))
if (!dir.exists(psubdir)) {dir.create(psubdir)}

## Plot comparison of mean ratings between studies

df = story_mean_ratings_study %>%
  pivot_wider(id_cols = c("ord","code","category","part"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean","n"))

plot.fig7 = function(data) {
  ggplot(data, aes(mean.1, mean.2, label = code, colour=factor(category))) +
    geom_point() +
    xlim(c(-1,100)) + ylim(c(-1,100)) +
    xlab("Mean ratings in Study 1 in Poland (convenience sampling)") + ylab("Mean ratings in Study 2 in Poland (purposive sampling)") +
    scale_color_manual(values = colors_categories, name = "Story type") +
    geom_abline(aes(intercept = 0, slope = 1)) +
    geom_label(data = subset(data, abs(mean.1 - mean.2) > 25), show.legend = FALSE) +
    beauty
}

### Separate plots
for (i in 0:6) {
  subdf = filter(df, part == i)
  p = plot.fig7(subdf) +
    labs(title = paste("Differences in mean ratings of stories on", tolower(labels_scales[i+1]) , "scale"))
  ggsave(paste("Fig 7 -", part_to_scale[i+1], "- ratings by study.png"), p, path = psubdir)
}

### Wrapped plots
p = plot.fig7(df) +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Differences in mean ratings of stories on each scale")
ggsave("Fig 7 - Wrap - ratings by study.png", p, width = 15, height = 10, path = psubdir)
