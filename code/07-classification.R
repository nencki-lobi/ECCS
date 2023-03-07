# Classification

odir = "./classification"
if (!dir.exists(odir)) {dir.create(odir)}

osubdir = file.path(odir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required),
  "-l-", str_replace_all(params$l, ",", "-"),
  "-k-", as.character(params$k)))
if (!dir.exists(osubdir)) {dir.create(osubdir)}

## Define functions

get.distances = function(data, centroids) {
  D = rdist(data, centroids) # calculate distance
  avgD = colMeans(D) # average over the sample
}

get.classes = function(category, distances, thr, labels) {
  
  # define criteria
  
  # The following conditions have to be satisfied in order to classify a story 
  # to one of the classes: 
  # (1) The story’s distance to the respective class must be smaller than a certain threshold.
  # (2) The story must meet the first condition for one class only.
  # (3) If the story falls within an area of intersection of two (or more) classes, it remains unclassified.
  # (4) If the story does not meet the first condition for any of the classes, it remains unclassified.
  # (5) Assigned class must match initial category label.
  
  criterion1 = distances < thr  # distance of at least this threshold
  criterion2 = sum(criterion1) == 1
  
  label = labels[criterion1]
  
  class = ifelse(criterion2, 
                 ifelse(label == category, label, "unclassified"),"unclassified")
}

mutate.individual = function(x, f) {
  x + (runif(1) - 0.5) * f
}

run.genetic = function(initial_thr, expected_class_size, categories, distances, labels, nbest, noffspring, factor, niter, logfile) {
  
  nlabels = length(labels)
  npopulation = nbest * noffspring
  current_class_size = matrix(0, npopulation, nlabels)
  fitness = matrix(0, npopulation, 1)
  
  nstories = nrow(distances)
  classification = matrix(0, nstories, 1)
  
  M = t(replicate(npopulation, initial_thr))
  initial_population = apply(M, c(1,2), mutate.individual, f = factor)
  
  iter = 0
  population = initial_population
  
  solution = matrix(NA, nrow=nlabels, ncol=1)
  
  sink(file = logfile)
  
  while (iter < niter) {
    for (individual in 1:npopulation) {
      
      thr = population[individual,]
      
      for (story in 1:nstories) {
        classification[story] = get.classes(categories[story], distances[story,], thr, labels)
      }
      
      for (lab in 1:nlabels) {
        current_class_size[individual,lab] = sum(classification == labels[lab])
      }
      
      f = expected_class_size - current_class_size[individual,]
      f[f<0] = 0
      fitness[individual] = sum(f^2)
    }
    
    ranking = sort(fitness, index.return = TRUE)
    I = head(ranking$ix, nbest)
    best = population[I,]
    best_fitness = fitness[I]
    best_class_size = current_class_size[I,]
    
    cat("Iteration:", as.character(iter), 
        "| Best fitness:", as.character(best_fitness[1]), 
        "| Mean fitness:", as.character(mean(best_fitness)), "\n")
    
    if (best_fitness[1] == 0) {
      solution = best[1,]
      break
    } 
    else {
      offspring = best
      
      for (individual in 1:nbest) {
        
        M = t(replicate(noffspring-1, best[individual,]))
        offspring = rbind(offspring, apply(M, c(1,2), mutate.individual, f = factor))
      }
      
      population = offspring
      iter = iter + 1
    }
    
  }
  
  #cat("Solution not found in:", as.character(niter), "iterations. \n")
  
  sink(file = NULL)
  
  out = list(solution = solution, best = best, best_fitness = best_fitness, best_class_size = best_class_size)
  return(out)
  
}

## Visual inspection of the data distribution

df = ratings
df$part = factor(df$part)

p = ggplot(df, aes(x=opt)) +
  geom_histogram(bins=100, color = "grey", fill = "grey") +
  xlab("Value") + ylab("Count") +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) +
  beauty

ggsave(paste("data-distribution.png"), 
       width = 2000, height = 1200, units = "px",
       p, path = osubdir)

## Initial setup

centroids = rbind(diag(5)*99, 
                  rep(0, 5), # NEU
                  rep(49, 5)) # MID

rownames(centroids) = c(labels_categories, "MID")
colnames(centroids) = labels_scales[-c(1,2)]

nstories = length(ords)
ncentroids = nrow(centroids)

## For each story get its initial category label

categories = stories$category

## For each story get its distance from each of the classes

distances = matrix(NA, nrow=nstories, ncol=ncentroids)

for(i in ords) {
  
  # get ratings for the ith story
  df = ratings %>% filter(ord == i) %>%
    pivot_wider(id_cols = c("sid", "code", "stid"),
                names_from = c("name", "ord", "part"),
                names_sep = ".",
                names_sort = T,
                values_from = "opt") %>%
    select(contains("rateme")) %>%
    select(-(1:2)) # skip valence and arousal
  
  distances[i+1,] = get.distances(df, centroids)
}

colnames(distances) = c(labels_categories, "MID")

distances_from_corners = distances[,labels_categories]
distance_from_the_middle = distances[,"MID"]

## Best and worst stories

df = bind_cols(items, distances)

best_stories = df %>% 
  arrange(desc(MID)) %>%
  slice_head(n = 30)

worst_stories = df %>% 
  arrange(MID) %>%
  slice_head(n = 30)

## Inspect how different set of thresholds impact class sizes

df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) <- c('class', 'n', 'thr')

thresholds = seq(0, 140.0071, by=0.1) # max possible distance between centroids is 140.0071

for(thr in thresholds) {
  
  classes = matrix(NA, nrow=nstories, ncol=1)  
  
  for(i in ords) {
    classes[i+1] = get.classes(categories[i+1], distances_from_corners[i+1,], thr, labels_categories)
  }
  
  subdf = as.data.frame(classes)
  colnames(subdf) = "class"
  
  subdf = subdf %>% count(class)
  subdf$thr = thr
  
  df = rbind(df, subdf)
}

transposed_df = df %>%
  pivot_wider(id_cols = "thr",
              names_from = "class",
              names_sort = T,
              values_from = "n",
              values_fill = list(n = 0))

labels = df %>% 
  group_by(class) %>% 
  summarise(max = max(n)) %>% 
  arrange(desc(max)) %>% 
  select(class)

labels = labels$class
colors = colors_classes[labels]

df$class = factor(df$class, levels = labels)

p = ggplot(df, aes(x=thr, y=n, fill=class)) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/30,
    alpha = 0.5) +
  xlab("Threshold") + ylab("Class size") +
  scale_x_continuous(breaks=seq(0, 140, by=10)) +
  scale_y_continuous(breaks=seq(0, 180, by=30)) +
  scale_fill_manual(values = colors, name = "") +
  beauty

h = ggplotly(p) %>%
  layout(font = list(family = 'Arial'),
         legend = list(orientation = 'h', xanchor = "center", x = 0.5))

setwd(osubdir)
saveWidget(as_widget(h), "classification-plotly.html")
setwd('../..')

## Find optimal set of thresholds for classification with genetic algorithm

l = unlist(strsplit(params$l, split = ","))
k = params$k
n = length(l)

initial_thr = apply(distances[,l], 2, sort)[k,]
expected_class_size = as.integer(rep(k, n))

nbest = 10
noffspring = 10
factor = 50
niter = 100000

logfile = file.path(osubdir, "genetic.log")

genetic = run.genetic(initial_thr, expected_class_size,
                  categories, distances[,l], l,
                  nbest, noffspring, factor, niter, logfile)

## Visual inspection of the genetic algorithm's performance

df = read.table(text = gsub("Iteration: ", "",
                            gsub(" \\| Best fitness: ", ",",
                                 gsub(" \\| Mean fitness: ", ",", 
                                      readLines(logfile)))), 
                sep = ",", strip.white = T)

colnames(df) = c("iter", "bfit", "mfit")

p = ggplot(df, aes(x=iter, y=bfit)) +
  geom_line() +
  xlab("Iteration") + ylab("Fitness") +
  scale_x_continuous(limits = c(0, 100000),
                     breaks=seq(0, 100000, by=25000),
                     labels = c("0", "25K", "50K", "75K", "100K")) +
  scale_y_continuous(limits = c(0, 50),
                     breaks=seq(0, 50, by=10)) +
  beauty

ggsave(paste("genetic-algorithm-performance.png"), 
       width = 1000, height = 1000, units = "px",
       p, path = osubdir)

## For each story return a classification label

classes = matrix(NA, nrow=nstories, ncol=1)

#thr = initial_thr
thr = genetic$solution

for(i in ords) {
   classes[i+1] = get.classes(categories[i+1], distances[i+1,l], thr, l)
}

classes = data.frame(class = classes)

## Preview the classification results

results = bind_cols(select(stories, "ord", "code", "category"),
               classes, 
               select(transposed_story_mean_ratings, starts_with("mean.")), 
               distances)

colnames(results) = c("ord", "code", "category", "class",
                 paste("M", labels_scales, sep = "."),
                 paste("D", c(labels_categories, "MID"), sep = "."))

results_sliced = results %>%
  filter(class != "unclassified") %>%
  group_by(class) %>%
  slice_max(D.MID, n = k) 

results_diff = setdiff(results, results_sliced) %>% 
  mutate(class = "unclassified")

## How to use:
# results %>% filter(class == category)
# results %>% filter(class == category & category == "NEU")
# results %>% filter(class != category & category == "NEU")
# results %>% filter(class != category & category == "NEU") %>% group_by(class) %>% summarise(n = n())

write.csv(results, file = file.path(osubdir, "classification-results.csv"))
write.csv(results_sliced, file = file.path(osubdir, "classification-results_sliced.csv"))

save(best_stories, worst_stories, genetic, results, 
     file = file.path(osubdir, "classification-results.RData"))

## Create a martix of scatterplots to visualise classes 

labels = c("unclassified", l)
colors = colors_classes[labels]

df = rbind(results_sliced, results_diff) %>%
  select("ord", "category", "class", starts_with("M.")) %>%
  mutate(class = factor(class, levels = labels)) %>%
  arrange(class)

colnames(df) = c("ord", "category", "class", labels_scales)

png(file.path(osubdir, "scatterplot-matrix.png"),
    width=12, height=12, units="in", res=300)

par(mar=c(4,4,1,1))

pairs(df[,4:10], pch = 19, cex = 1,
      col = colors[df$class],
      cex.labels = 2, 
      cex.axis = 2,
      xlim = c(0,100),
      ylim = c(0,100),
      upper.panel=NULL)

dev.off()
