# Weather Station Analysis — Hemp Field Trials
**University of Florida | Tropical Research and Education Center (TREC)**

This project contains R scripts and data used to analyze multi-site weather station data collected during hemp field trials at UF/TREC. The analysis separates irrigation, rainfall, and fertilization events, calculates Volumetric Water Content (VWC) across soil depths, and computes Growing Degree Days (GDD) to support agronomic decision-making.

---

## Project Overview

Weather data was collected from three station types deployed across multiple field trial blocks:

- **HoboLink** — 3 soil moisture sensors at surface, shallow, and root zone depths
- **FieldClimate (FC)** — Up to 6 sensors at 10 cm intervals (surface through deep subsoil)
- **FAWN** (Florida Automated Weather Network) — Official UF rainfall records used as ground truth

---

## Key Algorithms

### 1. Volumetric Water Content (VWC)
Raw sensor readings from HoboLink and FieldClimate stations were cleaned by removing negative values (sensor glitches), then averaged across depths using `colMeans()`. A separate root-level average was calculated using only the top 3 sensors — the zone most relevant to hemp water uptake.

### 2. Irrigation vs. Rainfall Separation
The algorithm uses two independent data sources to classify each day's water input:

| Water Type | Logic |
|---|---|
| **Rainfall** | FAWN recorded precipitation that day |
| **Irrigation** | FC stations recorded water, but FAWN recorded none |
| **No Water** | Both FAWN and FC stations recorded no water |

Because FAWN is an independent certified weather network, any water the FC stations detected that FAWN did not record must have come from the irrigation system — not the sky.

### 3. Fertilization Event Tagging
Fertilizer application dates for Block 1 and Block 10 were recorded from field trial logs and hardcoded into the script. Each day in the dataset was then flagged as a fertilization or non-fertilization day, allowing analysis of whether nutrient applications coincided with irrigation or rainfall events — a key indicator of leaching risk.

### 4. Growing Degree Days (GDD)
GDD was calculated using a base temperature of **3.4°C** (the emergence threshold for hemp):

```
GDD = Average Daily Temp − Base Temp (floored at 0)
```

---

## Repository Structure

```
Weather_station/
├── water_analysis.R                  # Main analysis script
├── Weather_station.Rproj             # RStudio project file
├── average_daily_sum.csv             # Aggregated daily averages
├── daily_sum.csv                     # Daily precipitation sums
├── FAWN_report rainfall_summary.csv  # FAWN rainfall data
├── Data_summary/                     # Raw data exports by station
├── Daily/                            # Daily aggregated data
├── Hourly/                           # Hourly aggregated data
├── Field Climate/                    # FieldClimate station data
└── R_code/                           # Supporting R scripts
```

---

## Dependencies

```r
library(tidyverse)
library(ggplot2)
library(reshape2)
```

---

## Visualizations

The script produces the following plots using `ggplot2`:

- **Daily GDD over time** — line and histogram by station
- **Rainfall events with fertilization overlay** — scatter plot colored by fertilization status
- **Water type classification over time** — irrigation vs. rainfall vs. no water, faceted by fertilization
- **Multi-station precipitation** — all FC and FAWN stations plotted together

---

## Field Trial Notes

**Planting Dates:** May 9, June 6, July 11–12, 2023
**Harvest Dates:** August–October 2023

**Fertilizer Applications:**
- Block 1 (Timing & Rate Study): May–July 2023
- Block 10 (Source Study — CRF & Polymer Coated Urea): July–September 2023

---

## Author

**Gabriel Zaldivar**
Intern, UF/IFAS Tropical Research and Education Center
Data collection, station maintenance, and analysis — Summer/Fall 2023
