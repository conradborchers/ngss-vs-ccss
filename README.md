# Sentiment Comparison of Two Large Curricular Reforms: NGSS and CCSS

## Citation

TBD

```
TBD
```

## Folder structure

### Twitter/X API

Tweets were collected using the `GET /2/tweets/search/all` endpoint which is documented [here](https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all). Individual API responses were stored as `.json` files in the `json/` folder

### Sentiment Software

Raw text returned by the Twitter/X API were analyzed using VADER using its Python library as performed in `run-vader.py` which sources files from the `json/` folder.

### Data Analysis Code

The repository follows standard practices of the `{targets}` data analysis pipelines. To learn more about the `{targets}` package, please refer to its [documentation](https://books.ropensci.org/targets/).

`run_targets.R` runs the `{targets}` start to finish. Checkpoints are specified in `_targets.R` while subprocesses are defined in `R/functions.R`.

Finally, `analysis.R` takes preprocessed output from the targets pipeline for downstream analyses reported in the manuscript.

## Data availability

To learn more about the project data set and its availability, please send an email to ANON at [ANON@DOMAIN.COM](mailto:ANON@DOMAIN.COM). 

