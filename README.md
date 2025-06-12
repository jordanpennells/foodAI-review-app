# Food AI Dashboard

This repository contains a Shiny application for interactively exploring bibliometric trends in the use of artificial intelligence across the food sector. The dashboard was developed as a companion to the Food AI systematic review, allowing readers to visualise publication statistics, keyword networks and topic clusters derived from review articles.

## Dependencies

The app relies on several R packages:

- shiny, shinythemes, shinycssloaders
- memoise
- dplyr, tidyr, widyr
- ggplot2, plotly, ggwordcloud
- DT
- bibliometrix
- tidytext, topicmodels
- stringr, tm, proxy
- igraph, ggraph, tidygraph
- wordcloud2

Ensure these packages are installed before running the app.

## Running the App

Clone this repository and run one of the following commands from the repository root:
Before launching, run `renv::restore()` to install the packages recorded in `renv.lock`.

```R
# Option 1
Rscript app.R

# Option 2
shiny::runApp()
```

Both commands launch the local Shiny server and open the dashboard in your browser.

## Downloading Data and Plots

Several tabs now feature download buttons. In the **Food AI Review Database** tab you can export the table of review articles as a CSV file. The **Publication Years** visualisation also includes a button to save the bar chart as a PNG image.

## Data Files

Obtained from a February 2025 search of the Web of Science Core Collection and complementary OpenAlex queries, the app expects the following files in the repository root:

- `FoodAI_Feb2025.bib` – bibliographic records for the 128 review articles analysed in the dashboard. `app.R` now reads this file directly instead of relying on an external CSV.
- `openalex_minimal.csv` – metadata retrieved from the OpenAlex API for research articles cited by the reviews.
- `titles_and_abstracts.csv` – matching titles and abstracts from the same OpenAlex query.

These files were collected as part of the systematic review described in the accompanying paper and should be kept in the repository root so that `app.R` can load them correctly.

## Linked Paper

The associated paper provides a systematic bibliometric analysis of artificial intelligence research in food science and engineering. Using Web of Science data, the authors review how research topics and industry adoption have evolved, highlight barriers to wider implementation and identify gaps for future study. The dashboard in this repository offers an interactive way to explore that bibliometric dataset and complements the narrative findings presented in the paper.

## Citation

Citation metadata for the accompanying paper is provided in [CITATION.cff](CITATION.cff).

## License

This project is licensed under the [MIT License](LICENSE).

