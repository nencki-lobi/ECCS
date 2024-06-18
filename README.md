# ECCS

This repository contains supplementary materials (data and code) associated with the development of the Emotional Climate Change Stories (ECCS) dataset. The remaining supplementary materials can be found on the accompanying [OSF website](https://osf.io/v8hts/).

Please cite the corresponding publication when using these materials:

> Zaremba, D., Michałowski, J.M., Klöckner, C.A., Marchewka A., & Wierzba, M. (2024) Development and validation of the Emotional Climate Change Stories (ECCS) stimuli set. Behavior Research Methods, 56, 3330–3345. [https://doi.org/10.3758/s13428-024-02408-1](https://doi.org/10.3758/s13428-024-02408-1)

## How to use

To download the ECCS dataset, use the following links:
- [ECCS stories](https://github.com/nencki-lobi/ECCS/blob/main/ECCS-stories.tsv)
- [ECCS ratings - Study 1](https://github.com/nencki-lobi/ECCS/blob/main/ECCS-ratings-S1.tsv)
- [ECCS ratings - Study 2](https://github.com/nencki-lobi/ECCS/blob/main/ECCS-ratings-S2.tsv)
- [ECCS ratings - Study 3](https://github.com/nencki-lobi/ECCS/blob/main/ECCS-ratings-S3.tsv)

To reproduce the results described in the article, run:

```
rmarkdown::render("ECCS.Rmd")
```

To perform the classification analysis, run:

```
rmarkdown::render("ECCS.Rmd", params = list(studies = "c(13,14,15)", 
                                            figures = FALSE, 
                                            classification = TRUE))
```

Alternatively, you can use `pwalk` to iterate over different sets of parameters, for instance:

```
df = data.frame(k = c(9, 12, 15))
df %>% pwalk( ~ rmarkdown::render("ECCS.Rmd", params = list(studies = "c(13,14,15)",
                                                            figures = FALSE,
                                                            classification = TRUE,
                                                            l = "ANG,HOP,NEU", 
                                                            k = ..1)))
```

## Obtaining data from the SQL database

Data can be obtained directly from the SQL database by running the following command:

```
psql -U grieg -d grieg --file rtask.sql
```

## Requirements

The following R packages are required: `broom`, `fields`, `htmlwidgets`, `plotly`, `psych`, `rmarkdown`, `rstatix`, `scales`, `tidyverse`.
