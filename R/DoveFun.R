#' Shadow for ggplot
#'
#' @param date Date vector (YYYY-MM-DD)
#' @param filling Fill color for the rectangle
#'
#' @return A ggplot2 layer (geom_rect)
#' @export
#'
#' @examples
shadow <-  function(date, filling){
  geom_rect(aes(xmin=ymd_hms(paste(date, "00:00:00")),
                xmax = ymd_hms(paste(date, "24:00:00")),
                ymin = -Inf,
                ymax = Inf), fill = filling, alpha = 0.05)

}

# Function to create a fluctuation sequence


#' Create a Synthetic Fluctuation Time Series for the INTERTIDAL CHAMBER
#'
#' Generates a cyclic sequence of values that fluctuate daily around an average value,
#' optionally adding random noise. The output is formatted as a character vector
#' representing scaled and padded numeric values.
#'
#' This function is useful for simulating environmental or sensor-like data
#' (e.g., temperature, tides) with a regular daily cycle.
#'
#' @param n_rows Integer. Total number of values (rows) to generate.
#'   Must be a positive integer.
#' @param amplitude Numeric. The magnitude of fluctuation around the average value.
#'   Must be non-negative. Default is 5.
#' @param avg_value Numeric. The central (mean) value around which the fluctuation occurs.
#'   Default is 20.
#' @param rows_per_day Integer. Number of observations per day (e.g., 24 for hourly data).
#'   Must be a positive integer. Default is 24.
#' @param random Logical. If `TRUE`, adds uniform random noise to the sequence.
#'   Default is `FALSE`.
#' @param seed Integer or NULL. Optional random seed for reproducibility when
#'   `random = TRUE`. Default is `NULL`.
#'
#' @details
#' The function constructs a daily fluctuation pattern consisting of:
#' \itemize{
#'   \item An increasing sequence from \code{avg_value - amplitude} to \code{avg_value + amplitude}
#'   \item A decreasing sequence back to \code{avg_value - amplitude}
#' }
#' These sequences are concatenated to form a full daily cycle and repeated
#' until \code{n_rows} values are generated.
#'
#' If \code{random = TRUE}, a small random noise component is added using a uniform
#' distribution with bounds \code{± amplitude/4}.
#'
#' The resulting values are:
#' \enumerate{
#'   \item Rounded to one decimal place
#'   \item Multiplied by 10
#'   \item Converted to character format
#'   \item Left-padded with zeros to 3 digits (if less than 100)
#' }
#'
#' @return
#' A character vector of length \code{n_rows}, containing the formatted fluctuation sequence.
#'
#' @examples
#' # Generate a deterministic daily fluctuation
#' seq1 <- control_IC(n_rows = 48)
#'
#' # Generate a noisy fluctuation sequence
#' seq2 <- control_IC(
#'   n_rows = 100,
#'   amplitude = 3,
#'   avg_value = 15,
#'   random = TRUE,
#'   seed = 123
#' )
#'
#' # Example with different temporal resolution (e.g., 12 observations/day)
#' seq3 <- control_IC(
#'   n_rows = 60,
#'   rows_per_day = 12
#' )
#'
#' @seealso
#' \code{\link{seq}}, \code{\link{runif}}
#'
#' @export
control_IC <- function(n_rows, amplitude = 5, avg_value = 20, rows_per_day = 24, random = FALSE, seed = NULL) {
  # Validate inputs
  if (!is.numeric(n_rows) || n_rows <= 0 || n_rows != as.integer(n_rows)) stop("'n_rows' must be a positive integer.")
  if (!is.numeric(amplitude) || amplitude < 0) stop("'amplitude' must be non-negative.")
  if (!is.numeric(avg_value)) stop("'avg_value' must be numeric.")
  if (!is.numeric(rows_per_day) || rows_per_day <= 0 || rows_per_day != as.integer(rows_per_day)) stop("'rows_per_day' must be a positive integer.")

  if (!is.null(seed)) set.seed(seed)

  # Calculate number of days
  n_days <- ceiling(n_rows / rows_per_day)

  # Build one day fluctuation (up then down)
  up <- seq(from = avg_value - amplitude, to = avg_value + amplitude, length.out = rows_per_day / 2)
  down <- seq(from = avg_value + amplitude, to = avg_value - amplitude, length.out = rows_per_day / 2)
  daily_pattern <- c(up, down)

  # Repeat pattern for all rows
  fluct_seq <- rep(daily_pattern, length.out = n_rows)

  # Add randomness if requested
  if (random) {
    fluct_seq <- fluct_seq + runif(n_rows, -amplitude/4, amplitude/4)
  }

  # Format: round to 1 decimal, multiply by 10, convert to character
  formatted_seq <- sapply(round(fluct_seq, 1) * 10, function(x) {
    if (x < 100) {
      sprintf("%03d", as.integer(x))  # adds leading zero only if <100
    } else {
      as.character(as.integer(x))
    }
  })

  return(formatted_seq)
}

#' Calculate Oxygen Saturation Percentage from DO (mg/L)
#'
#' Computes oxygen saturation (%) based on dissolved oxygen measurements,
#' temperature, and salinity. Uses TEOS‑10 solubility equations and seawater density.
#'
#' @param data Data frame containing temperature, dissolved oxygen, and time columns.
#' @param salinidad Numeric. Practical salinity (PSU). Default = 33.
#' @param temp_col Character. Column name for temperature (°C).
#' @param do_col Character. Column name for dissolved oxygen (mg/L).
#' @param time_col Character. Column name for datetime object.
#' @param day_col Character. Column name for experimental day variable.
#'
#' @details
#' The function performs:
#' \enumerate{
#'   \item Oxygen solubility using TEOS‑10 (μmol/kg)
#'   \item Conversion to mg/L using seawater density
#'   \item Calculation of % saturation
#'   \item Aggregation by day and hour (mean and SD)
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{O2_umol_kg}{Oxygen solubility (μmol/kg)}
#'   \item{O2_mg_L}{Oxygen solubility (mg/L)}
#'   \item{data_mod}{Original data with added saturation column}
#'   \item{resumen}{Summary by day and hour}
#' }
#'
#' @examples
#' result <- calcular_saturacion_O2(data = df)
#'
#' @importFrom gsw gsw_O2sol_SP_pt
#' @importFrom oce swRho
#' @importFrom dplyr group_by summarise
#' @importFrom lubridate hour
#' @importFrom magrittr %>%

calcular_saturacion_O2 <- function(
    data,
    salinidad = 33,
    temp_col = "Temp",
    do_col = "DO",
    time_col = "Time",
    day_col = "Exp_Day"
) {

  # Extraer variables con nombres flexibles
  Temp <- data[[temp_col]]
  DO   <- data[[do_col]]
  Time <- data[[time_col]]

  # 1) Solubilidad de O2 (μmol/kg)
  O2_umol_per_kg <- gsw_O2sol_SP_pt(
    SP = rep(salinidad, nrow(data)),
    pt = Temp
  )

  # 2) Densidad (kg/L)
  rho_kg_per_L <- swRho(
    salinity = rep(salinidad, nrow(data)),
    temperature = Temp,
    pressure = 0
  ) / 1000

  # 3) Conversión a mg/L
  O2_sat_mgL <- O2_umol_per_kg * 31.998e-3 * rho_kg_per_L

  # 4) Porcentaje de saturación
  data$porc_saturacion <- 100 * DO / O2_sat_mgL

  # 5) Resumen por día y hora
  resumen <- data %>%
    dplyr::group_by(
      .data[[day_col]],
      hour = lubridate::hour(.data[[time_col]])
    ) %>%
    dplyr::summarise(
      meanSaturationOxygen = mean(porc_saturacion, na.rm = TRUE),
      SaturationOxygenSD   = sd(porc_saturacion, na.rm = TRUE),
      .groups = "drop"
    )

  # 6) Salida
  return(list(
    O2_umol_kg   = O2_umol_per_kg,
    O2_mg_L      = O2_sat_mgL,
    data_mod     = data,
    resumen      = resumen
  ))
}




#' Process NetCDF SST Data and Export to Excel
#'
#' Reads a NetCDF file containing sea surface temperature data,
#' computes the spatial mean over time, and exports results to Excel.
#'
#' @param nc_path Character. Path to input NetCDF file.
#' @param excel_path Character. Path to output Excel file.
#'
#' @details
#' Steps performed:
#' \enumerate{
#'   \item Reads longitude, latitude, time, and temperature (thetao)
#'   \item Converts time to datetime
#'   \item Reshapes SST array into matrix
#'   \item Computes mean SST per timestep
#'   \item Saves output to Excel
#' }
#'
#' @return Data frame with:
#' \itemize{
#'   \item Date
#'   \item Datetime
#'   \item Mean SST
#' }
#'
#' @examples
#' tab <- procesar_nc_a_excel("data.nc", "output.xlsx")
#'
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom lubridate as_datetime ymd_hms hour yday
#' @importFrom writexl write_xlsx
#' @export
procesar_nc_a_excel <- function(nc_path, excel_path) {

  # Abrir archivo NetCDF
  ncin <- nc_open(nc_path)
  print(ncin)

  ##### extraer variables
  lon <- ncvar_get(ncin, "longitude")
  lat <- ncvar_get(ncin, "latitude")
  t <- ncvar_get(ncin, "time")

  # Convertir tiempo
  datetime <- as_datetime(t)

  # Temperatura
  sst <- ncvar_get(ncin, "thetao")  # quitar comentario si necesitas conversión de K a °C

  ##### cerrar archivo
  nc_close(ncin)

  ##### reorganizar datos
  sstvec.long <- as.vector(sst)
  sstmat <- matrix(sstvec.long,
                   nrow = length(lon) * length(lat),
                   ncol = length(t))

  sstmean <- colMeans(sstmat, na.rm = TRUE)

  t_date <- as.Date(datetime)

  tab <- data.frame(
    t = t_date,
    sstmean = sstmean,
    datetime = datetime
  )

  ##### guardar a Excel
  write_xlsx(tab, excel_path)

  return(tab)
}

#' Run Marine Heatwave Analysis from SST Time Series
#'
#' Performs a full marine heatwave (MHW) analysis using the heatwaveR package,
#' including climatology, event detection, seasonal classification, and plotting.
#' This function automatically adapts to both hourly and daily time series.
#'
#' @param tab Data frame containing date/time (`t`) and temperature (`sstmean`).
#'            `t` can be Date or POSIXct.
#' @param clim_start Character. Start date for climatology period (YYYY-MM-DD).
#' @param clim_end Character. End date for climatology period (YYYY-MM-DD).
#'
#' @details
#' Workflow:
#' \enumerate{
#'   \item Prepare daily time series
#'   \item Automatically detect time resolution (daily or hourly)
#'   \item Compute climatology (ts2clm)
#'   \item Detect marine heatwaves (detect_event)
#'   \item Identify top events
#'   \item Expand events to daily resolution
#'   \item Merge with original time series to compute anomalies
#'   \item Compute time since event start (in days)
#'   \item Generate plots:
#'     \itemize{
#'       \item Event timeline
#'       \item Lollipop plot
#'       \item Scatter plots (seasonal/anomaly)
#'       \item Seasonal averages with SD
#'     }
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{mhw}{Full heatwaveR output}
#'   \item{top_events}{Top intensity events}
#'   \item{mhw_ts}{Expanded dataset with anomalies}
#'   \item{mhw_season}{Seasonal summaries}
#'   \item{plots}{List of ggplot objects}
#' }
#'
#' @examples
#' results <- run_mhw_analysis(tab)
#' print(results$plots$events)
#'
#' @import dplyr lubridate ggplot2 ggpubr heatwaveR
#' @export
run_mhw_analysis <- function(tab,
                             clim_start,
                             clim_end) {

  #' @importFrom dplyr group_by summarise mutate select arrange filter left_join distinct count ungroup
  #' @importFrom lubridate month yday
  #' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs facet_grid facet_wrap scale_colour_gradient2 geom_ribbon theme element_text
  #' @importFrom ggpubr ggarrange
  #' @importFrom heatwaveR lolli_plot event_line ts2clm detect_event
  #' @importFrom magrittr %>%

  # ---------------------------
  # 0. Preparación inicial
  # ---------------------------
  tab$t <- as.POSIXct(tab$t)

  # Detectar resolución temporal automáticamente
  dt <- median(diff(as.numeric(tab$t)), na.rm = TRUE)
  is_daily <- dt >= 86400
  time_scale <- ifelse(is_daily, 1, 24)

  # ---------------------------
  # 1. Preparación de datos
  # ---------------------------
  ntab <- tab %>%
    group_by(t) %>%
    summarise(
      max = max(sstmean),
      min = min(sstmean),
      temp = mean(sstmean),
      range = max - min,
      .groups = "drop"
    )

  ntab$t <- as.Date(ntab$t)

  # ---------------------------
  # 2. Climatología y eventos
  # ---------------------------
  ts  <- ts2clm(ntab, climatologyPeriod = c(clim_start, clim_end))
  mhw <- detect_event(ts, categories = TRUE, climatology = TRUE)

  top_events <- mhw$event %>%
    ungroup() %>%
    select(event_no, duration, date_start, date_peak,
           intensity_max, intensity_cumulative) %>%
    arrange(-intensity_max) %>%
    head(5)

  # ---------------------------
  # 3. Plots iniciales
  # ---------------------------
  event_plot <- event_line(
    mhw, spread = 2000, metric = intensity_max,
    start_date = clim_start, end_date = clim_end
  ) + theme(legend.position = "top")

  lolli <- lolli_plot(mhw, metric = intensity_max)

  combined_events_plot <- ggarrange(event_plot, lolli, ncol = 1)

  # ---------------------------
  # 4. Expandir eventos
  # ---------------------------
  mhw[["event"]] <- mhw[["event"]] %>%
    select(-season) %>%   # remove old season completely
    mutate(
      season = case_when(
        month(date_peak) %in% c(12, 1, 2) ~ "Winter",
        month(date_peak) %in% c(3, 4, 5) ~ "Spring",
        month(date_peak) %in% c(6, 7, 8) ~ "Summer",
        TRUE ~ "Autumn"
      )
    )

  mhw_events <- mhw[["event"]]

  mhw_days <- mhw_events %>%
    rowwise() %>%
    do(data.frame(
      date = seq(.$date_start, .$date_end, by = "day"),
      event_no = .$event_no
    ))

  # ---------------------------
  # 5. Merge con serie original
  # ---------------------------
  mhw_ts <- tab %>%
    left_join(mhw_days, by = c("t" = "date")) %>%
    filter(!is.na(event_no))

  mhw_ts$date <- mhw_ts$t

  clim <- mhw[["climatology"]]

  mhw_ts <- mhw_ts %>%
    left_join(clim[, c("t", "seas")], by = "t") %>%
    mutate(temp_anom = sstmean - seas)

  # tiempo desde inicio (en días)
  mhw_ts <- mhw_ts %>%
    group_by(event_no) %>%
    arrange(t) %>%
    mutate(
      step_from_start = row_number() - 1,
      time_from_start_days = step_from_start / time_scale
    ) %>%
    ungroup()

  # estación
  mhw_ts <- mhw_ts %>%
    left_join(
      mhw_events %>% select(event_no, season),
      by = "event_no"
    )

  mhw_ts$season <- factor(
    mhw_ts$season,
    levels = c("Summer","Autumn","Winter","Spring")
  )

  # estacionalidad continua
  mhw_ts <- mhw_ts %>%
    mutate(
      doy = yday(t),
      season_cont = cos(2 * pi * (doy - 173) / 365)
    )

  # ---------------------------
  # 6. Plots scatter
  # ---------------------------
  p1 <- ggplot(mhw_ts,
               aes(time_from_start_days, temp_anom, color = season_cont)) +
    geom_point() +
    theme_minimal() +
    labs(y = "Temperature anomaly (ºC)", x = "Days", color = "Season") +
    scale_colour_gradient2(
      low = "#2c7bb6", mid = "#ffd6a5", high = "#d7191c",
      midpoint = 0,
      breaks = c(-0.9999, 0, 0.9999),
      labels = c("Winter", "", "Summer")
    )

  p2 <- ggplot(mhw_ts,
               aes(time_from_start_days, temp_anom, color = temp_anom)) +
    geom_point() +
    theme_minimal() +
    labs(y = "Temperature anomaly (ºC)", x = "", color = "ºC") +
    facet_grid(rows = vars(season)) +
    scale_colour_gradient2(low = "yellow", mid = "blue", high = "#d7191c", midpoint = 0)

  combined_scatter <- ggarrange(p1, p2, ncol = 1, legend = "right")

  # ---------------------------
  # 7. Promedios por estación
  # ---------------------------
  mhw_season <- mhw_ts %>%
    group_by(season, step_from_start) %>%
    summarise(
      temp = mean(temp_anom),
      sd   = sd(temp_anom),
      .groups = "drop"
    )

  counts <- mhw_ts %>%
    distinct(event_no, season) %>%
    count(season)

  labels_season <- counts %>%
    mutate(label = paste0(season, " (n = ", n, ")")) %>%
    select(season, label) %>%
    deframe()

  p3 <- ggplot(mhw_season,
               aes(step_from_start / time_scale, temp, color = season)) +
    geom_point() +
    facet_wrap(~season, ncol = 1,
               labeller = labeller(season = labels_season)) +
    geom_ribbon(aes(y = temp,
                    ymin = temp - sd,
                    ymax = temp + sd,
                    fill = season),
                alpha = .2) +
    theme_minimal() +
    labs(y = "Temperature anomaly (ºC)", x = "Days") +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  # ---------------------------
  # OUTPUT
  # ---------------------------
  return(list(
    mhw = mhw,
    top_events = top_events,
    mhw_ts = mhw_ts,
    mhw_season = mhw_season,
    plots = list(
      events = combined_events_plot,
      scatter = combined_scatter,
      seasonal = p3
    )
  ))
}

#' Create a Synthetic Daily Fluctuation Time Series for OXYGEN CONTROLLER
#'
#' Generates a time series with daily fluctuations over a specified number of days.
#' Each day contains a fixed number of observations (e.g., hourly data), and values
#' vary between a specified minimum and maximum.
#'
#' Three types of fluctuation patterns are available:
#' \itemize{
#'   \item \code{"lineal"}: linear increase and decrease (sawtooth-like)
#'   \item \code{"seno"}: smooth sinusoidal pattern
#'   \item \code{"random"}: fully random values within bounds, ensuring daily min and max
#' }
#'
#' @param dias Integer. Number of days to simulate. Default is 21.
#' @param horas_por_dia Integer. Number of observations per day (e.g., 24 for hourly data).
#'   Default is 24.
#' @param valor_min Numeric. Minimum value within each day. Default is 20.
#' @param valor_max Numeric. Maximum value within each day. Default is 180.
#' @param inicio Character. Start datetime in format \code{"YYYY-MM-DD HH:MM:SS"}.
#'   Default is \code{"2023-01-01 07:00:00"}.
#' @param tipo Character. Type of fluctuation. Must be one of:
#'   \code{"lineal"}, \code{"seno"}, or \code{"random"}.
#' @param seed Integer or NULL. Optional random seed for reproducibility.
#'
#' @details
#' The function builds a daily pattern and repeats it for the specified number
#' of days. For the \code{"random"} mode, values are sampled uniformly within
#' the specified range for each day, and the minimum and maximum values are
#' explicitly enforced to ensure they are always present.
#'
#' Time is generated as a continuous sequence of timestamps, starting at the
#' specified datetime and incremented by one hour (or the implied unit) per row.
#'
#' @return
#' A data frame with two columns:
#' \itemize{
#'   \item \code{datetime}: POSIXct timestamp sequence
#'   \item \code{valor}: numeric values representing the fluctuation
#' }
#'
#' @examples
#' # Default example (21 days, hourly, linear pattern)
#' df1 <- oxygen_controller()
#'
#' # Random fluctuation with fixed bounds
#' df2 <- oxygen_controller(tipo = "random", seed = 123)
#'
#' # Custom start date and value range
#' df3 <- oxygen_controller(
#'   dias = 10,
#'   valor_min = 50,
#'   valor_max = 200,
#'   inicio = "2024-06-01 09:00:00"
#' )
#'
#' @importFrom lubridate ymd_hms hours
#' @export
oxygen_controller <- function(
    dias = 21,
    horas_por_dia = 24,
    valor_min = 20,
    valor_max = 180,
    inicio = "2023-01-01 07:00:00",
    tipo = c("lineal", "seno", "random"),
    seed = NULL
) {

  tipo <- match.arg(tipo)

  if (!is.null(seed)) set.seed(seed)

  total_filas <- dias * horas_por_dia
  valores <- vector(length = total_filas)

  for (d in 1:dias) {

    if (tipo == "lineal") {
      subida <- seq(valor_min, valor_max, length.out = horas_por_dia / 2)
      bajada <- seq(valor_max, valor_min, length.out = horas_por_dia / 2)
      patron_dia <- c(subida, bajada)

    } else if (tipo == "seno") {
      t <- seq(0, 2*pi, length.out = horas_por_dia)
      media <- (valor_max + valor_min) / 2
      amplitud <- (valor_max - valor_min) / 2
      patron_dia <- media + amplitud * sin(t - pi/2)

    } else if (tipo == "random") {
      patron_dia <- runif(horas_por_dia, min = valor_min, max = valor_max)

      # asegurar min y max por día
      idx <- sample(1:horas_por_dia, 2)
      patron_dia[idx[1]] <- valor_min
      patron_dia[idx[2]] <- valor_max
    }

    inicio_idx <- (d - 1) * horas_por_dia + 1
    fin_idx <- d * horas_por_dia

    valores[inicio_idx:fin_idx] <- patron_dia
  }

  tiempo_inicio <- lubridate::ymd_hms(inicio)
  tiempo <- tiempo_inicio + lubridate::hours(0:(total_filas - 1))

  df <- data.frame(
    datetime = tiempo,
    valor = valores
  )

  return(df)
}


