# ECCS

This repository contains supplementary materials (data and code) associated with the development of the Emotional Climate Change Stories (ECCS) dataset.

## Obtaining data from the SQL database 

Data can be obtained directly from the SQL database by running the following command:

```
psql -U grieg -d grieg --file rtask.sql
```

## Requirements

The following R packages are required: `dplyr`, `ggplot2`, `stringr`, `tidyr`.