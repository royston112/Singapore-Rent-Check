# Singapore Rent Check

**Explore Singapore rental evidence before deciding what to pay or charge.**

[**Try the live app →**](https://royston-wong.shinyapps.io/sg-rent-checker/)

An R Shiny group project developed for DBA3702 at the National University of Singapore. The app helps tenants and landlords compare asking rents with historical rental records and explore properties, neighbourhoods and nearby amenities.

## About this repository

This is a **source-code portfolio**. It presents the application, benchmarking logic, data preparation and validation scripts for review.

Raw datasets, processed runtime files, credentials and deployment configuration are excluded. **This repository cannot run as-is.** Use the live app above; no R installation or local setup is required.

## What the app does

- **Rent check:** compare an asking rent with the median and middle 50% of comparable historical rents.
- **Property search:** search for HDB towns, blocks and private properties, with the selected location displayed in the search field.
- **Explore map:** browse properties and rental patterns geographically, including an area or specific home selected in Rent check.
- **Nearby amenities:** view schools, hawker centres, MRT/LRT access points and other available amenities.
- **Rental trends:** inspect changes in the rents represented by the selected segment.
- **Favourites:** save properties for comparison, with persistence in the same browser when local storage is available.
- **Share results:** copy a bookmarked link that restores the saved search state.

## How to try it

1. Open the [live application](https://royston-wong.shinyapps.io/sg-rent-checker/).
2. Choose tenant or landlord and the type of home.
3. Search for a town, block or private property.
4. Select the relevant home characteristics and optionally enter an asking rent.
5. Review the benchmark, comparable record count and period used.
6. Explore the map and nearby amenities, save favourites or share the search.

The hosted app may take a little longer to open when it has been inactive.

## Benchmarking approach

The app selects rental records according to the chosen market, location and available home characteristics. It summarises comparable rents using the median and interquartile range (25th–75th percentiles).

When a narrow search has too few records, the benchmark may widen its comparison scope or time window. Users should read the result's description and record count to understand the evidence used.

The typical range describes the middle half of historical comparable rents. It is not a guaranteed price range or a prediction interval. A selected private-bedroom category is retained where applicable; choosing “Any” can combine categories.

## Source-code guide

| File | Purpose |
| --- | --- |
| `app.R` | Shiny interface, reactive interactions, search, maps, favourites and sharing |
| `02_benchmark.R` | Comparable selection and rental benchmark calculations |
| `amenities.R` | Supporting amenities functionality |
| `00_geocode_hdb_blocks.R` | Geocoding HDB block locations |
| `01_clean_data.R` | Initial rental and index data cleaning |
| `01b_clean_new_data.R` | Preparation and integration of supplementary spatial data |
| `01c_amenities.R` | Preparation of amenities data |
| `01d_locations.R` | Preparation of private-property locations |
| `03_backtest.R` | Historical validation of the benchmarking method |

The preparation scripts document the workflow; they require external inputs that are not bundled here.

## Data and technologies

The project combines HDB and private rental records with planning-area boundaries, property locations and amenities data. Source providers include:

- [HDB](https://www.hdb.gov.sg/) and [data.gov.sg](https://data.gov.sg/) — HDB rental and supporting public datasets.
- [Urban Redevelopment Authority](https://www.ura.gov.sg/) — private rental records, rental index and planning-area information.
- [OneMap](https://www.onemap.gov.sg/) — geocoding and map services.
- [Land Transport Authority DataMall](https://datamall.lta.gov.sg/) — MRT/LRT spatial information.
- [Ministry of Education](https://www.moe.gov.sg/) and data.gov.sg — school information.
- [OpenStreetMap](https://www.openstreetmap.org/copyright) — supplementary places data.

These links identify providers; individual dataset definitions, dates and usage terms should be consulted when reproducing the analysis. The app's **Data & limits** page provides further context.

The project uses **R, Shiny, bslib, tidyverse, sf, Leaflet and Plotly**, with deployment on **shinyapps.io**.

## Interpretation and limitations

- Historical rental evidence does not establish a definitive “fair” rent or guarantee a future agreement.
- HDB dates refer to rental approval months; private rental dates refer to lease commencement months.
- The rental datasets used do not capture all factors affecting rent, such as furnishing, condition, view and floor level.
- HDB rental records do not provide unit floor area; private floor areas are reported in bands.
- A missing map marker can result from filters, time windows, minimum record requirements or missing coordinates.
- Nearby-amenity distances are approximate straight-line distances, not walking routes or verified distances to school gates.
- MRT/LRT exit records should not be interpreted as counts of unique stations.
- Favourites stored in a browser do not automatically synchronise across devices and can be lost when browser storage is cleared.
- Shared links restore search inputs; they are not permanent snapshots of results if the underlying data or app changes.

## Project credits

Project completed for the NUS module **DBA3702 — *Descriptive Analytics with R***.

**Done by:** Royston Wong, Sarah Chew, Joyce Wu, Joshua Böer, Sherman Ng
**Supervised by:** Professor Liu Qizhang

## Repository scope

The repository is intended for code inspection and project discussion. It excludes raw and processed data, API credentials, local R session files and shinyapps.io deployment metadata. Third-party data and services remain subject to their respective terms.

