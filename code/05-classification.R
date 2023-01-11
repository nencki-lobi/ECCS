# Classification

odir = "./classification"
if (!dir.exists(odir)) {dir.create(odir)}

## Define functions

get.distances = function(data, centroids) {
  D = rdist(data, centroids) # calculate distance
  avgD = colMeans(D) # average over the sample
}

get.classes = function(distances, thr, labels) {
  
  # define criteria
  
  # Two conditions have to be satisfied in order to classify a word to one of
  # the classes: (1) the word's distance to the emotion or neutral
  # state must be smaller than a certain threshold; (2) the word must meet
  # the first condition for one category only; in other words, if it falls
  # within an area of intersection of two categories, it remains
  # unclassified. Thus, words more distant from all the classes than the
  # respective thresholds remain unclassified, and so do words that are close
  # (in this sense) to two or more classes.
  
  criterion1 = distances < thr  # distance of at least this threshold
  criterion2 = sum(criterion1) == 1
  
  class = ifelse(criterion2, labels[criterion1], "unclassified")
}

mutate.individual = function(x, f) {
  x + (runif(1) - 0.5) * f
}

run.genetic = function(initial_thr, expected_class_size, distances, labels, nbest, noffspring, factor, niter, logfile) {
  
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
        classification[story] = get.classes(distances[story,], thr, labels)
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

## Visual inspection of the data

df = ratings
df$part = factor(df$part)

p = ggplot(df, aes(x=opt)) +
  geom_histogram(bins=100, color = "gray") +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) +
  beauty

## Initial setup

centroids = rbind(diag(5)*99, 
                  rep(0, 5), # NEU
                  rep(49, 5)) # MID

rownames(centroids) = c(labels_categories, "MID")
colnames(centroids) = labels_scales[-c(1,2)]

nstories = length(ords)
ncentroids = nrow(centroids)

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

distances_from_classes = distances[,labels_categories]
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
    classes[i+1] = get.classes(distances_from_classes[i+1,], thr, labels_categories)
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

labels = c("unclassified", "NEU", "HOP", "COM", "ANG", "ANX", "GUI")
colors = c("gray", "#96949B", "#69A2B0", "#FEAEA5", "#E05263", "#659157", "#6C5670")
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

setwd(odir)
saveWidget(as_widget(h), "classification-plotly.html")
setwd('..')

## Find optimal set of thresholds for classification with genetic algorithm

k = params$k
n = ncentroids-1

initial_thr = apply(distances_from_classes, 2, sort)[k,]
expected_class_size = as.integer(rep(k, n))

nbest = 10
noffspring = 10
factor = 50
niter = 100000

logfile = file.path(odir, "genetic.log")

out = run.genetic(initial_thr, expected_class_size, 
                  distances_from_classes, labels_categories, 
                  nbest, noffspring, factor, niter, logfile)
save(out, file = file.path(odir, "genetic-out.RData"))

## For each story return a classification label

classes = matrix(NA, nrow=nstories, ncol=1)

#thr = initial_thr
thr = out$solution

for(i in ords) {
   classes[i+1] = get.classes(distances_from_classes[i+1,], thr, labels_categories)
}
