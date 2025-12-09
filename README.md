# **Final Project: Ridership & On-Time Performance (OTP) Analysis for Simualted RIPTA Data**

This project performs exploratory, research-driven, and visualization-focused 
analysis of simulated **public transit ridership** and **on-time performance (OTP)** data. 
It includes functions for data cleaning, aggregation, merging, priority categorization, 
and visualization.

The project is organized into two major analysis pipelines:

1. **Exploratory Data Analysis (EDA)**
2. **Research-Driven Analysis (RDA)**, 
including aggregation, mapping, priority assignment, and visual reporting for 
route prioritization.

---

## 📁 **Project Structure**

```
final_project/
│
├── data/
│   ├── otp_simulated.csv
│   └── ridership_simulated.csv
│
├── exploratory_analysis/
│   └── scripts/
│       ├── data_completeness.R
│       ├── univariate_analysis.R
│       └── demographic_analysis.R
│
├── research_driven_analysis/
│   └── scripts/
│       ├── aggregate.R
│       ├── mapping.R
│       ├── analysis.R
│       ├── utilities.R
│       ├── visualization_pt1.R
│       └── visualization_pt2.R
│
└── run_eda.qmd
└── run_research_driven_analysis.qmd
└── README.md
```

---

#**1. Exploratory Data Analysis (EDA)**

The file **`run_eda.qmd`** loads libraries, imports data, and sources 
scripts that produce exploratory summaries.

#**2. Research-Driven Analysis**

The file **`run_research_driven_analysis.qmd`** 
provides the project’s full analysis pipeline.

## **Functions (from `/scripts/`)**

### ### **`aggregate_ridership()`**

Aggregates simulated ridership data by **Route × Hour**

### **`aggregate_otp()`**

Aggregates OTP data by **Route × Hour**

### **`map_ridership()`**

Merges aggregated ridership + OTP datasets 
and fills missing values with zero.

### **`assign_and_split_priority()`**

Categorizes each Route × Hour as:

* **Early** (avg delay < −5 min)
* **Late** (avg delay > +5 min)
* **On_Time**

### **`filter_routes()`**

Extracts rows for specific route IDs.

### **`common_routes()`**

Returns routes present in both dataframes.

---

#**3. Visualization**

Two complementary visualization modules:

* **visualization_pt1.R** — route-specific deep dives, faceted views, and 
performance bands

* **visualization_pt2.R** — heatmaps, temporal plots, delay distributions

Both are automatically sourced in the run documents.

---

#**Required Libraries**
```r
tidyverse
dplyr
readr
tidyr
lubridate
ggplot2
```

---

#**Data**

Please place your datasets into:

```
final_project/data/
│── otp_simulated.csv
│── ridership_simulated.csv
```

Both datasets are loaded in the Quarto documents automatically.

---