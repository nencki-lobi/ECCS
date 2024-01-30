# Load helpers and derivatives

## Compute helpers & derivatives

rmarkdown::render("ECCS.Rmd", params = list(studies = "c(13,14,15)",
                                            recompute = TRUE,
                                            figures = FALSE,
                                            classification = FALSE))

## Load precomputed helpers & derivatives

helpers = file.path("./output/helpers.RData")
derivatives = file.path("./output/derivatives/studies-13-14-15/derivatives.RData")

# Run classification analysis for different class sizes (k = 9, k = 12, k = 15)

classification1 = "ANG-HOP-NEU-k-9"
classification2 = "ANG-HOP-NEU-k-12"
classification3 = "ANG-HOP-NEU-k-15"

df = data.frame(k = c(9, 12, 15))
df %>% pwalk( ~ rmarkdown::render("ECCS.Rmd", params = list(studies = "c(13,14,15)",
                                                            recompute = FALSE,
                                                            figures = FALSE,
                                                            classification = TRUE,
                                                            l = "ANG,HOP,NEU", 
                                                            k = ..1)))
# Prepare data

prepare.data = function(classification) {

  fname = paste0("./output/classification/studies-13-14-15-l-", classification, "/classification-results.csv")
  
  c = read.table(fname, header = T, sep = ",", encoding = "UTF-8", check.names=FALSE)
  
  c = c[,-1] # drop first column
  
  c = c %>% 
    select(!starts_with("D.")) %>% # drop unnecessary columns
    mutate(name = classification) # record origin
}

c1 = prepare.data(classification1)
c2 = prepare.data(classification2)
c3 = prepare.data(classification3)

# Create "dummy" classification for benchmarking

dummy = bind_cols(select(stories, "ord", "code", "category"),
            class = "", 
            select(transposed_story_mean_ratings, starts_with("mean.")))

dummy = dummy %>% mutate(class = category,
                         name = "ECCS-k-30")

colnames(dummy) = c("ord", "code", "category", "class",
                      paste("M", labels_scales, sep = "."), "name")

# Compare different classification results

classes = c("ANG", "HOP", "NEU")
order_scales = c("Valence", "Arousal", "Anger", "Hope")
order_names = c(classification1, classification2, classification3, "ECCS-k-30")
mycolors = c("#6C4472", "#89698E", "#B8A5BB", "#e9e2eb")

df = rbind(c1,c2,c3,dummy) %>%
  select(c("name", "code", "category", starts_with("M."))) %>%
  rename_with(~ sub("M.", "", .x), everything()) %>%
  pivot_longer(all_of(labels_scales), names_to = "scale", values_to = "val") %>%
  filter(scale %in% order_scales) %>%
  filter(category %in% classes) %>%
  mutate(scale = factor(scale, levels = order_scales),
         name = factor(name, levels = order_names)) 

p = ggplot(df, aes(x=category, y=val, fill=name)) +
  geom_boxplot() + 
  xlab("Category") + ylab("Mean") +
  labs(title = "Comparison of classification results") +
  scale_fill_manual(values = mycolors) +
  beauty + theme(legend.title=element_blank(), legend.position = "bottom")

p = p + facet_wrap(~scale, ncol = 4)
ggsave("Comparison of classification results.png", p, width = 10, height = 6, 
       path = "./output/classification")

# Inspect stories that get assigned to each class with each classification solution 

show.stories = function(stories, c1, c2) {
  
  diff = setdiff(c1$code, c2$code)
  
  stories %>% 
    filter(code %in% diff) %>%
    select(-EN, -NO) %>%
    group_by(category) %>%
    group_split
}

show.stories(stories, c1, c2) # all stories in classification 1 that aren't in classification 2
show.stories(stories, c2, c1) # all stories in classification 2 that aren't in classification 1
