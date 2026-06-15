## 📖 Overview

`DoveFun` is a collection of helper functions designed to streamline R workflows for researchers at **Dove Marine Laboratory**. The package provides utilities for:

- **Time series visualization** with date-based shading
- **Synthetic data generation** for environmental simulations (e.g., temperature, tides)
- **Oceanographic calculations** for oxygen saturation analysis

These functions support everyday marine research tasks, particularly for intertidal chamber experiments and dissolved oxygen analysis.

---

## 🚀 Installation

### From GitHub

```r
install.packages("devtools")
devtools::install_github("noctilucass/DoveFun")
```

### Load the package

```r
library(DoveFun)
```

---

## 📦 Functions

### 1. `shadow()` - Date-based Shading for ggplot2

Creates a `geom_rect` layer to shade a specific date on a time series plot. Useful for highlighting experimental periods, events, or conditions.

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `date` | Date vector | Date in `YYYY-MM-DD` format |
| `filling` | Character | Fill color for the rectangle |

**Returns:** A `ggplot2` layer (`geom_rect`)

**Example:**

```r
library(ggplot2)
library(DoveFun)

# Create a time series plot
ggplot(data = your_data, aes(x = Time, y = Temperature)) +
  geom_line() +
  shadow(date = "2024-03-15", filling = "red") +  # Shade specific date
  theme_minimal()
```

---

### 2. `control_IC()` - Synthetic Fluctuation Time Series

Generates a cyclic sequence of values that fluctuate daily around an average value. Ideal for simulating environmental sensor data (temperature, tides, etc.) with regular daily cycles.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n_rows` | Integer | — | Total number of values to generate (positive integer) |
| `amplitude` | Numeric | `5` | Magnitude of fluctuation around average (non-negative) |
| `avg_value` | Numeric | `20` | Central mean value around which fluctuation occurs |
| `rows_per_day` | Integer | `24` | Observations per day (e.g., 24 for hourly) |
| `random` | Logical | `FALSE` | Add uniform random noise if `TRUE` |
| `seed` | Integer | `NULL` | Random seed for reproducibility (when `random = TRUE`) |

**Returns:** Character vector of length `n_rows` with formatted fluctuation sequence (rounded to 1 decimal, multiplied by 10, zero-padded to 3 digits)

**Details:**

The function creates a daily fluctuation pattern:
1. Increasing sequence from `avg_value - amplitude` to `avg_value + amplitude`
2. Decreasing sequence back to `avg_value - amplitude`

These sequences form a full daily cycle and repeat until `n_rows` values are generated.

**Examples:**

```r
# Generate deterministic daily fluctuation (48 hourly values)
seq1 <- control_IC(n_rows = 48)

# Generate noisy fluctuation with custom parameters
seq2 <- control_IC(
  n_rows = 100,
  amplitude = 3,
  avg_value = 15,
  random = TRUE,
  seed = 123
)

# Different temporal resolution (12 observations/day)
seq3 <- control_IC(
  n_rows = 60,
  rows_per_day = 12
)
```

---

### 3. `calcular_saturacion_O2()` - Oxygen Saturation Percentage

Computes oxygen saturation (%) from dissolved oxygen measurements using **TEOS-10 solubility equations** and seawater density. Essential for oceanographic and marine biology research.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `data` | Data frame | — | Contains temperature, DO, and time columns |
| `salinidad` | Numeric | `33` | Practical salinity (PSU) |
| `temp_col` | Character | `"Temp"` | Column name for temperature (°C) |
| `do_col` | Character | `"DO"` | Column name for dissolved oxygen (mg/L) |
| `time_col` | Character | `"Time"` | Column name for datetime object |
| `day_col` | Character | `"Exp_Day"` | Column name for experimental day variable |

**Returns:** List containing:

| Component | Description |
|-----------|-------------|
| `O2_umol_kg` | Oxygen solubility (μmol/kg) |
| `O2_mg_L` | Oxygen solubility (mg/L) |
| `data_mod` | Original data with added `porc_saturacion` column |
| `resumen` | Summary by day and hour (mean ± SD) |

**Method:**

1. Oxygen solubility using TEOS-10 (μmol/kg) via `gsw_O2sol_SP_pt()`
2. Seawater density calculation via `oce::swRho()`
3. Conversion to mg/L using molecular weight of O₂ (31.998 g/mol)
4. Saturation percentage: `100 × DO / O2_sat_mgL`
5. Aggregation by day and hour (mean and standard deviation)

**Dependencies:**

- `gsw` - TEOS-10 thermodynamic equations
- `oce` - Oceanographic data analysis
- `dplyr` - Data manipulation
- `lubridate` - DateTime handling
- `magrittr` - Pipe operator

**Example:**

```r
library(DoveFun)

# Calculate oxygen saturation
result <- calcular_saturacion_O2(
  data = df,
  salinidad = 33,
  temp_col = "Temp",
  do_col = "DO",
  time_col = "Time",
  day_col = "Exp_Day"
)

# Access results
result$O2_umol_kg      # Oxygen solubility (μmol/kg)
result$O2_mg_L         # Oxygen solubility (mg/L)
result$data_mod        # Data with saturation column
result$resumen         # Summary by day/hour
```

---
## 🔗 References

- **TEOS-10**: Thermodynamic Equation of Seawater 2010 (IO-PSOG)
- **gsw package**: Gross, T., et al. (2014). `gsw`: Gibbs Scientific Utilities for seawater
- **oce package**: Kirchwood, D., & Delrue, R. (2012). `oce`: Analysis of Oceanographic Data
- **heatwaveR**; Hobday, A.J. et al. (2016). A hierarchical approach to defining marine heatwaves
