[README.md](https://github.com/user-attachments/files/35360234/README.md)
# AnyData Insights



AnyData Insights is a web application for statistical analysis, data visualization, and geographic information systems (GIS).

Built with R and Shiny, it helps students, researchers, and data analysts explore, clean, analyze, and visualize data without writing code.



## Features

|Workspace|Capabilities|
|-|-|
|Dashboard and Data|Dataset overview, interactive tables, filtering and filtered data export|
|Cleaning|Missing-value imputation, complete cases, duplicate removal, variable transformations, scaling and outlier removal using IQR or Z-scores|
|Descriptive|Numeric summaries, grouped statistics and categorical frequency tables|
|Tests|One-sample, independent Welch and paired t-tests; one-way and Welch ANOVA; Mann–Whitney, Wilcoxon signed-rank, Kruskal–Wallis and Friedman tests|
|Categories|Contingency tables and categorical association tests|
|Correlation|Pearson, Spearman and Kendall correlation|
|Regression|Linear regression with selectable predictors|
|Logistic|Binary logistic regression|
|Visualise|Customizable charts for distributions, relationships, statistical analysis and model evaluation; PNG, SVG and HTML export where available|
|Test Finder|Guided statistical test selection based on research questions and data characteristics|
|GIS \& Spatial Analytics|Coordinate-based and spatial-file imports, layer management, interactive maps, coordinate reference systems, geometry tools, distance, area, length and buffer calculations|
|Report|HTML reports, methods and limitations, and CSV analysis history export|
|Visitor Analytics|Administrator dashboard with local CSV or Google Sheets event storage|

Statistical results include interpretation and assumption guidance where implemented. The application also provides light and dark interface styling.



## Getting started

### Prerequisites



* [R](https://cran.r-project.org/)
* [RStudio](https://posit.co/download/rstudio-desktop/) (recommended, not required)
* [Git](https://git-scm.com/) (optional for downloading the project)



### 1\. Clone the repository

```bash
git clone https://github.com/O-Ahmed-Yus/Any-Data-Analyzer.git
cd Any-Data-Analyzer
```

Alternatively, download the repository as a ZIP archive and extract it.



### 2\. Install R packages

Open `AnyData\_Insights.Rproj` in RStudio and run the following in the R Console:

```r
install.packages(c("shiny", "bslib", "DT", "readxl", "ggplot2", "thematic"))
```

The following packages support additional features such as statistical file imports, maps, chart exports and visitor analytics:

```r
install.packages(c(
  "haven", "sf", "leaflet", "leaflet.extras", "classInt",
  "plotly", "dplyr", "htmltools", "htmlwidgets", "svglite",
  "rmarkdown", "uuid", "filelock", "googlesheets4"
))
```

These are the main dependencies identified in the source code, not a version-locked environment. Some export features require Pandoc or other system utilities. On Linux, `sf` may require GDAL, GEOS and PROJ system libraries.



### 3\. Launch the application

From the repository root, run:

```r
shiny::runApp(".")
```

Or open `app.R` and click **Run App** in RStudio.



## Typical workflow

1. Upload a dataset or select a built-in example.
2. Inspect the data and apply filters or cleaning operations.
3. Explore descriptive statistics and visualizations.
4. Choose a statistical test or regression model using the relevant workspace or Test Finder.
5. Review the results, assumptions and interpretations.
6. Export the relevant chart, dataset, map or report.

The main application supports CSV, TSV, text, Excel and statistical data imports through its file reader. The upload limit is configured at 50 MiB. Demo datasets include Iris, mtcars, ToothGrowth, mpg, a simulated student survey, employee salaries and Pima diabetes data.



## GIS workspace

The GIS workspace uses `sf` and `leaflet` to work with spatial data. Users can create point layers from coordinate columns, import spatial files, manage layers, inspect attributes and customize map styling. Spatial operations include measurements and buffers. Layer exports include GeoJSON, GeoPackage and CSV; map export helpers support HTML and PNG subject to available dependencies.



## Project structure

```text
Any-Data-Analyzer/
├── app.R                    # Main Shiny UI, server and statistical workflows
├── AnyData\_Insights.Rproj   # RStudio project
├── R/
│   ├── gis\_\*.R              # Spatial helpers, storage, demo layers and exports
│   ├── mod\_gis\_\*.R          # GIS interface and server modules
│   ├── viz\_\*.R              # Chart builders, data profiling and themes
│   ├── mod\_viz\_\*.R          # Visualization studio modules
│   ├── track\_store.R        # Visitor event storage
│   └── mod\_track\_\*.R        # Visitor tracking and administrator interface
├── demo/
│   ├── pima.csv
│   └── salaries.csv
├── www/                     # Application logos and favicon
├── .gitignore
└── README.md
```

## Technology stack

|Technology|Role|
|-|-|
|R|Statistical computing and data processing|
|Shiny|Reactive web application|
|bslib|Bootstrap-based UI and themes|
|DT|Interactive data tables|
|ggplot2 / plotly|Static and interactive visualizations|
|readxl / haven|Data import|
|sf / leaflet|Spatial data processing and mapping|
|Google Sheets (optional)|Persistent visitor analytics backend|

## 

## Visitor analytics configuration

The tracking module supports local CSV storage by default and an optional Google Sheets backend. Configure settings using environment variables in a private `.Renviron` file or server environment:

|Variable|Purpose|
|-|-|
|`TRACKING\_BACKEND`|`local` or `gsheets`|
|`TRACKING\_DIR`|Local event storage directory|
|`TRACKING\_SHEET`|Google Sheet ID or URL|
|`TRACKING\_SHEET\_TAB`|Worksheet name (default: `events`)|
|`TRACKING\_ADMIN\_TOKEN`|Administrator passphrase|
|`TRACKING\_TZ`|Display timezone (default: UTC)|
|`GOOGLE\_APPLICATION\_CREDENTIALS`|Service account credential file path|
|`GCP\_SERVICE\_ACCOUNT\_JSON`|Alternative service account credential JSON|

Never commit credentials or administrator tokens. Local analytics storage may not survive restarts on ephemeral hosting such as shinyapps.io.



## Data privacy and analytical limitations

Uploaded files are handled through Shiny's temporary upload mechanism. The tracking module can record dataset names and usage metadata, including browser, device and visitor/session identifiers; it is not intended to record dataset contents. Review the tracking configuration, hosting environment and applicable privacy requirements before accepting sensitive data. Online map tiles and externally hosted resources may require an internet connection.

Statistical outputs are intended to support analysis, not replace professional judgment. Check assumptions, data quality and research design before drawing conclusions.



## Deployment

The application can be prepared for deployment on a Shiny-compatible hosting service such as [shinyapps.io](https://www.shinyapps.io/) or a self-hosted Shiny Server. Ensure dependencies and any required environment variables are configured. No live deployment URL is specified here.



## Feedback and contributions

Report bugs or suggest improvements through [GitHub Issues](https://github.com/O-Ahmed-Yus/Any-Data-Analyzer/issues). Include reproduction steps, expected behavior and a non-sensitive example dataset when relevant.



## License

No license file is currently included in the repository. Public visibility alone does not grant permission to redistribute or modify the code. Contact the repository owner regarding reuse.



## Maintainer

[O-Ahmed-Yus](https://github.com/O-Ahmed-Yus)

