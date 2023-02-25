# ECCS

This repository contains supplementary materials (data and code) associated with the development of the Emotional Climate Change Stories (ECCS) dataset.

## How to use

You can reproduce the analysis by specifying parameters for the report:

```
rmarkdown::render("ECCS.Rmd", params = list(studies = "c(13,14,15)"))
```

Alternatively, you can use `pwalk` to iterate over multiple values of parameters:

```
df = data.frame(studies = c("13", "14", "15", "c(13,14)", "c(13,14,15)"))
df %>% pwalk( ~ rmarkdown::render("ECCS.Rmd", params = list(studies = ..1)))
```

## Obtaining data from the SQL database

Data can be obtained directly from the SQL database by running the following command:

```
psql -U grieg -d grieg --file rtask.sql
```

## Requirements

The following R packages are required: `broom`, `fields`, `htmlwidgets`, `plotly`, `psych`, `rmarkdown`, `scales`, `tidyverse`.
