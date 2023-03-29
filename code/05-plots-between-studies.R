# Plots

odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

osubdir = file.path(odir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required)))
if (!dir.exists(osubdir)) {dir.create(osubdir)}

fdir.create = function(name) {
  fdir = file.path(osubdir, name)
  if (!dir.exists(fdir)) {dir.create(fdir)}
  fdir
}

## Plot comparison of mean ratings between studies

fdir = fdir.create("Fig 9 - Comparison of story ratings between studies")

df = story_mean_ratings_study %>%
  pivot_wider(id_cols = c("ord","code","category","part"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean","n"))

plot.fig9 = function(data) {
  ggplot(data, aes(mean.1, mean.2, label = code, colour=category)) +
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
  p = plot.fig9(subdf) +
    labs(title = paste("Differences in mean ratings of stories on", tolower(labels_scales[i+1]) , "scale"))
  ggsave(paste0(part_to_scale[i+1], ".png"), p, path = fdir)
}

### Wrapped plots
p = plot.fig9(df) +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Differences in mean ratings of stories on each scale")
ggsave("Fig 9 - Comparison of story ratings between studies - Facet by scale.png", p, width = 15, height = 10, path = osubdir)
