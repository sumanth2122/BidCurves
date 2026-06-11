# Bid Curve Shenanigans

California consumers are paying double the amount for electricity compared to other regions in the United States. The high electricity rates are a relatively recent occurrence, with rates increasingly growing within the past decade. Other regions’ electricity rates have not grown nearly as much as California. Our goal is to look into why California has such a large gap. We are using data from the California Independent System Operator (CAISO) to analyze trends and anomalous behavior in California’s energy market with the goal of identifying and de-anonymizing participating sellers. Our work increases transparency in California’s energy market through three analyses: exploratory investigation for anomaly detection in the market, ID matching across reassignment, and de-anonymization of anonymous market participants. Our work does not identify a single cause for California’s high energy prices or determine which participants contribute most to abnormal market behavior, but we provide software to make these kinds of questions more measurable and answerable.

This repository contains some of our results and tools designed to analyze the California energy market.

## Repository Structure

You need to bring your own data: save a single Parquet of all the DAM data you want to analyze as `CAISO_dam_big.parquet` in the repository root, and similarly for `CAISO_rtm_big.parquet`.

### `CAISOPEA`

The `CAISOPEA` directory houses a R package which exposes a few functions useful for analyzing bid data. Read the [package vignette](CAISOPEA\vignettes\package-overview.qmd) for more details and a short walkthrough of the functions. You can install the package from the repo root using `pak`:

```r
# install.packages("pak")
pak::local_install("CAISOPEA")
```

### `dashboard`

The `dashboard` directory contains the source for our interactive dashboards which provide similar tools as our package but in a user friendly way, eliding some detail but being more accessible. Our dashboards are split by the market data they work on:

DAM Dashboard: <https://nicoleeyee-bid-curve-shenanigans.share.connect.posit.cloud/>

RTM Dashboard: <https://visruth-rtm-bid-curve-shenanigans.share.connect.posit.cloud>

### `final_outputs`

The `final_outputs` directory has our de-anonymization results which are used to power the dashboards and were generated using the package (see `curtailments_run.qmd` and `outage_run.qmd`).
