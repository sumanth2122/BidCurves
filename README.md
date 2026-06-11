# Bid Curve Shenanigans

By: Nicole, Visruth, and Sumanth

California consumers are paying double the amount for electricity compared to other regions in the United States. The high electricity rates are a relatively recent occurrence, with rates increasingly growing within the past decade. Other regions' electricity rates have not grown nearly as much as California. Our goal is to look into why California has such a large gap. We are using data from the California Independent System Operator (CAISO) to analyze trends and anomalous behavior in California's energy market with the goal of identifying and de-anonymizing participating sellers. Our work increases transparency in California's energy market through three analyses: exploratory investigation for anomaly detection in the market, ID matching across reassignment, and de-anonymization of anonymous market participants. Our work does not identify a single cause for California's high energy prices or determine which participants contribute most to abnormal market behavior, but we provide software to make these kinds of questions more measurable and answerable.

This repository contains our results and tools designed to analyze the California energy market.

For more information about matching logic and feature engineering, check out our report at:
https://docs.google.com/document/d/1v4Ee1ASTs7Jmy0X7iRGIEIJggm3n2c5NnD6fr2QP81o/edit?tab=t.0

## Dashboards

Interactive dashboards built on our outputs are hosted externally:

- DAM Dashboard: <https://nicoleeyee-bid-curve-shenanigans.share.connect.posit.cloud/>
- RTM Dashboard: <https://visruth-rtm-bid-curve-shenanigans.share.connect.posit.cloud>

## Repository Structure

```
BidCurves/
├── CAISOPEA/                        # R package
├── curtailments_run.qmd             # DAM curtailment deanonymization script
├── outage_run.qmd                   # RTM outage deanonymization script
├── final_outputs/                   # CSV results written by the run scripts
├── CAISO_dam_big.parquet            # (user upload/use default) DAM bid data
├── CAISO_rtm_big.parquet            # (user upload/use default) RTM bid data
├── prior_trade_day_outages.parquet  # (user upload/use default) outage/curtailment report
└── ID_attributes.parquet            # (user upload/use default) per-seq-ID feature attributes
```

---

## Data Requirements

You must supply four Parquet files in the repository root before running anything. We have provided data for prior trade day and ID_attributes spanning 2023-2025. DAM and RTM data can be downloaded at the following link for 2023-2025, along with Outage/Curtailment data:
https://www.dropbox.com/scl/fo/pa5jluzek9kut8em3h80f/ACKYCC8-eqs3i4Sp-3FPLRc?rlkey=ullr689mkjdzbgwxzj3di14qf&e=4&st=82zdqs1r&dl=0


### `CAISO_dam_big.parquet`

A single Parquet containing all Day-Ahead Market (DAM) bid data you want to analyze. Required columns:

| `RESOURCEBID_SEQ` | integer/string | Anonymous sequence identifier |
| `STARTTIME` | timestamp | Bid date/time |
| `SCH_BID_XAXISDATA` | numeric | MW quantity point on the bid curve |
| `SELFSCHEDMW` | numeric | Self-schedule MW (0 if none) |

### `CAISO_rtm_big.parquet`

A single Parquet containing all Real-Time Market (RTM) bid data. Required columns:

| `RESOURCEBID_SEQ` | integer/string | Anonymous sequence identifier |
| `MARKETPRODUCTTYPE` | string | Market product type (filtered to `"EN"` internally) |
| `SCH_BID_XAXISDATA` | numeric | MW quantity point on the bid curve |
| `SCH_BID_TIMEINTERVALSTART` | timestamp | Bid interval start |
| `SCH_BID_TIMEINTERVALSTOP` | timestamp | Bid interval stop |
| `TIMEINTERVALSTART` | timestamp | Dispatch interval start |
| `TIMEINTERVALEND` | timestamp | Dispatch interval end |

### `prior_trade_day_outages.parquet`

CAISO's Prior Trade Day Outage Report (PTDO). Required columns:

| `OUTAGE TYPE` | string | `"PLANNED"`, `"FORCED"`, etc. |
| `RESOURCE ID` | string | Named resource identifier |
| `RESOURCE NAME` | string | Human-readable resource name |
| `RESOURCE PMAX MW` | numeric | Nameplate capacity |
| `CURTAILMENT MW` | numeric | Amount curtailed |
| `CURTAILMENT START DATE TIME` | timestamp | When the curtailment begins |
| `CURTAILMENT END DATE TIME` | timestamp | When the curtailment ends |

### `ID_attributes.parquet`

Pre-computed per-`RESOURCEBID_SEQ` attributes. Required columns:

| `RESOURCEBID_SEQ` | integer/string | Sequence identifier |
| `max_mw` | numeric | All-time observed maximum MW bid |

This file is used to pre-filter candidate seq IDs by nameplate size before the expensive join. You can generate it from `CAISO_dam_big.parquet` with:

```r
library(arrow)
library(dplyr)
read_parquet("CAISO_dam_big.parquet") |>
  group_by(RESOURCEBID_SEQ) |>
  summarise(max_mw = max(SCH_BID_XAXISDATA, na.rm = TRUE)) |>
  write_parquet("ID_attributes.parquet")
```

---

## Installation

Install the `CAISOPEA` package from the repository root:

```r
# install.packages("pak")
pak::local_install("CAISOPEA")
```

Or with base R tools:

```bash
R CMD INSTALL CAISOPEA
```

Dependencies (`arrow`, `dplyr`, `duckplyr`, `ggplot2`, `lubridate`, `readr`, `stringr`, `tibble`) are declared in `CAISOPEA/DESCRIPTION` and will be installed automatically by `pak`. After making any changes to the package source, reinstall before re-running the scripts.

---

## Package Functions

### `get_curtailment_matches()` — DAM curtailment deanonymization

Matches named resources in the outage report to anonymous `RESOURCEBID_SEQ` identifiers in the DAM data using planned partial curtailment events as a signal.

```r
results <- get_curtailment_matches(
  dam_data           = dam_data,   # collected R data.frame with DAM bids
  outages            = outages,    # Arrow Table or data.frame (PTDO)
  id_attrs           = id_attrs,   # Arrow Table or data.frame (ID_attributes)
  tolerance          = 0.05,       # MW match tolerance (5% of expected_available_mw)
  pmax_tolerance     = 0.10,       # nameplate size pre-filter tolerance (10%)
  min_matches        = 2L,         # minimum matching curtailment days required
  resolve_collisions = TRUE,       # remove seq ID collisions across resources
  top_n              = 5L,         # candidate seq IDs to retain per resource
  return_detail      = TRUE        # TRUE returns full list; FALSE returns just final_matches
)
```

When `return_detail = TRUE` the function returns a named list:

| `final_matches` | One row per named resource — best seq ID after collision resolution, with score and confidence |
| `top_candidates` | Up to `top_n` ranked candidates per resource (no collision resolution applied) |
| `match_detail` | Per-day MW values for every match in `final_matches` |
| `scores` | Raw score table for all candidates before ranking |
| `raw_matches` | Unfiltered candidate rows before scoring |
| `planned_partial` | All planned partial curtailment events (useful for computing total event counts vs. matched events) |

### `get_outage_rtm_matches()` — RTM outage deanonymization

Matches named generators in the outage report to anonymous `RESOURCEBID_SEQ` identifiers in the RTM data using full-outage bid gaps as a signal.

```r
rtm_results <- get_outage_rtm_matches(
  rtm_data           = rtm_data,   # lazy DuckDB tbl or collected data.frame
  outages            = outages,    # Arrow Table or data.frame (PTDO)
  generators         = NULL,       # optional character vector to restrict to specific generators
  edge_minutes       = 5,          # minutes of allowed slack at bid gap edges
  min_matches        = 1L,         # minimum matching intervals required
  resolve_collisions = TRUE,
  top_n              = 5L,
  return_detail      = TRUE
)
```

Returns the same list structure as above: `final_matches`, `top_candidates`, `match_detail`, `scores`, `raw_matches`, and `outages`.

### `find_similar_ids()` — cosine similarity ID matching

Finds `RESOURCEBID_SEQ` identifiers with similar bid curve feature profiles using a precomputed weighted cosine similarity matrix. Useful for tracking resources across ID reassignments.

```r
find_similar_ids(
  id           = 123456,     # RESOURCEBID_SEQ to query
  n            = 10L,        # number of similar IDs to return
  start_filter = "after"     # "none" | "after" | "before"
)
# Returns a tibble with RESOURCEBID_SEQ and cosine_similarity columns
```

Feature vectors include bid curve shape statistics, peak-hour activity patterns, self-schedule fraction, and more. The `start_filter` argument restricts candidates by when they first appeared:

- `"none"` — all candidates
- `"after"` — only IDs that appeared *after* the query ID was last active (successor search)
- `"before"` — only IDs that appeared *before* the query ID became active (predecessor search)

---

## Running the Example Scripts

Both scripts are [Quarto](https://quarto.org/) documents. They load the package, run the matching pipeline, display results, and write CSVs to `final_outputs/`. Render from the terminal or from within RStudio/VS Code.

### Curtailment deanonymization (`curtailments_run.qmd`)

```bash
quarto render curtailments_run.qmd
```

What the script does:

1. Opens a DuckDB connection, selects the four needed columns from `CAISO_dam_big.parquet`, and collects into an R data frame.
2. Reads `prior_trade_day_outages.parquet` and `ID_attributes.parquet`.
3. Calls `get_curtailment_matches(..., return_detail = TRUE)`.
4. Displays `final_matches` and `top_candidates` as tables.
5. Plots the distribution of confidence ratios on a log scale.
6. Writes four CSVs to `final_outputs/`.

### RTM outage deanonymization (`outage_run.qmd`)

```bash
quarto render outage_run.qmd
```

What the script does:

1. Opens a DuckDB connection and registers `CAISO_rtm_big.parquet`
2. Reads `prior_trade_day_outages.parquet` as an Arrow Table.
3. Calls `get_outage_rtm_matches(..., return_detail = TRUE)`.
4. Displays results and plots confidence distribution.
5. Writes three CSVs to `final_outputs/`.

---

## Output Files

All outputs are written to `final_outputs/`:

| `curtailment_matches_final.csv` | One row per named resource — collision-resolved best seq ID with score and confidence |
| `curtailment_top_candidates.csv` | Up to 5 ranked candidates per resource with PMAX and score details |
| `curtailment_match_detail.csv` | Per-day MW values for every final curtailment match |
| `curtailment_event_counts.csv` | Total curtailment events per resource vs. how many matched |
| `outage_rtm_matches_final.csv` | One row per named generator — collision-resolved best seq ID with score and confidence |
| `outage_rtm_top_candidates.csv` | Up to 5 ranked candidates per generator with score details |
| `outage_rtm_match_detail.csv` | Per-interval detail for every final RTM outage match |

---

