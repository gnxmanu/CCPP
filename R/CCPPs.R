#' An R Concentration Calculation and Plotting Package (CCPPs) for Los Gatos Gas Analyzer (LGGA).
#'
#' Built for 3 LGGA day-runs with control in triplicates and four sample replicates.
#'
#' @param path_main Export path. If NULL, it asks the user in GUI.
#' @param path_day1 Import path of 'LGGA log.txt' for day 1. If NULL, it asks the user in GUI.
#' @param path_day2 Import path of 'LGGA log.txt' for day 2. If NULL, it asks the user in GUI.
#' @param path_day3 Import path of 'LGGA log.txt' for day 3. If NULL, it asks the user in GUI.
#' @param interval Length of interval to average on. If NULL, 10 seconds.
#' @param base_interval Offset before injection time input by the user
#' @param peak_interval Offset before injection time input by the user
#' @param V_sample Injected sample volume. Litres, V_s.
#' @param V_loop Loop Volume. Litres, V_s.. Equivalent to 92 mL.
#' @param offset_initial_final Offset after injection at which the raw data will be considered. Seconds.
#'
#' @return Summary of calculated concentrations and produces plots and csv in set directory.
#'
#' @examples
#' CCPPs()

CCPPs <- function() {
# Install and load packages --------
  # install.packages("pacman")
  # library(pacman)
  # pacman::p_load(readxl, tcltk, ggplot2, deSolve, data.table, plotly, tidyr, dplyr, stringr, lubridate, readr, Rcpp)

  # Since pacman does not seem to work properly inside the function (prompt opens and disrupts loading)
  if (!require(readxl)) install.packages('readxl')
  library(readxl)

  if (!require(tcltk)) install.packages('tcltk')
  library(tcltk)

  if (!require(ggplot2)) install.packages('ggplot2')
  library(ggplot2)

  if (!require(Rcpp)) install.packages('Rcpp')
  library(Rcpp)

  if (!require(data.table)) install.packages('data.table')
  library(data.table)

  if (!require(plotly)) install.packages('plotly')
  library(plotly)

  if (!require(tidyr)) install.packages('tidyr')
  library(tidyr)

  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)

  if (!require(stringr)) install.packages('stringr')
  library(stringr)

  if (!require(lubridate)) install.packages('lubridate')
  library(lubridate)

  if (!require(readr)) install.packages('readr')
  library(readr)

# User prompt -----------------
  # Prompt user for folder
  mainDir <- tk_choose.dir(caption = "Choose the folder where plots and csv are to be exported.") # Ask with caption in GUI

  # Establish path to excel with all days within
  LGGA_Excel <- tk_choose.files(caption = "Choose the Excel File with the injection times for all days.") # Ask with caption in GUI

# Define 'UKL_LGGA' function ------------
UKL_LGGA <- function() {
  # Parameters -----------------
  interval_seconds <- 10 # length of interval to average on
  base_interval_begin_seconds <- 20 # offset before injection time input by the user
  peak_interval_begin_seconds <- 20 # offset after injection time input by the user
  V_sample = 100E-6 # [L] volume units # V_sample = V_s =  Is the injected sample volume
  V_loop = 92E-3 # [L] equivalent to 92 mL # V_loop = V_l = Loop Volume
  offset_initial_final <- 60
  # identifier <- "XXX%" # Default

  # Times ----------------------
  # Setting the injection of standards and replicates' time
  Ctrl_R1 <- InjectionTimes[1]
  Ctrl_R2 <- InjectionTimes[2]
  Ctrl_R3 <- InjectionTimes[3]

  SampleProbe_R1 <- InjectionTimes[4]
  SampleProbe_R2 <- InjectionTimes[5]
  SampleProbe_R3 <- InjectionTimes[6]
  SampleProbe_R4 <- InjectionTimes[7]

  # Transforming data set ----------
  # Rename columns as first row and erase first row
  colnames(LGGA_log) <- LGGA_log[1,]
  LGGA_log <- LGGA_log[-1,]

  # Reset row index
  row.names(LGGA_log) <- NULL

  # Save column names
  column_names <- colnames(LGGA_log)

  # Remove empty spaces preceding all values
  LGGA_log <- data.frame(lapply(LGGA_log, function(x) trimws(x, which = c("left"))))

  # Reset column names
  colnames(LGGA_log) <-  trimws(column_names, which = c("left"))

  # Subset gases peak values corresponding to the humid and dry fraction
  LGGA_log <- LGGA_log[,c("Time",
                          "[CH4]d_ppm",
                          "[CO2]d_ppm")]

  # Find the exact string in "Time" whenever the day and hour is input as XX/XX/XXXX XX:XX.
  Ctrl_R1string <- toString(LGGA_log[grep(Ctrl_R1, LGGA_log$Time), ][1,1])
  Ctrl_R2string <- toString(LGGA_log[grep(Ctrl_R2, LGGA_log$Time), ][1,1])
  Ctrl_R3string <- toString(LGGA_log[grep(Ctrl_R3, LGGA_log$Time), ][1,1])

  SampleProbe_R1_string <- toString(LGGA_log[grep(SampleProbe_R1, LGGA_log$Time), ][1,1])
  SampleProbe_R2_string <- toString(LGGA_log[grep(SampleProbe_R2, LGGA_log$Time), ][1,1])
  SampleProbe_R3_string <- toString(LGGA_log[grep(SampleProbe_R3, LGGA_log$Time), ][1,1])
  SampleProbe_R4_string <- toString(LGGA_log[grep(SampleProbe_R4, LGGA_log$Time), ][1,1])

  # Define initial and final times as one minute before and after the first and last measurement, respectively.
  t_initial <- dmy_hms(Ctrl_R1string) - dseconds(offset_initial_final) # For Day1
  t_final <- dmy_hms(SampleProbe_R4_string) + dseconds(offset_initial_final) # For Day1

  # Reorder data in date
  t_initial <- format(t_initial, format='%d/%m/%Y %H:%M:%S')
  t_final <- format(t_final, format='%d/%m/%Y %H:%M:%S')

  # Find the exact string in "Time" whenever the day and hour is input as XX/XX/XXXX XX:XX.
  t_initial_string <- toString(LGGA_log[grep(t_initial, LGGA_log$Time), ][1,1])
  t_final_string <- toString(LGGA_log[grep(t_final, LGGA_log$Time), ][1,1])

  # Find rows corresponding to time stamps
  t_initial_row <- which(LGGA_log == t_initial_string)
  t_final_row <- which(LGGA_log == t_final_string)

  Ctrl_R1row <- which(LGGA_log == Ctrl_R1string)
  Ctrl_R2row <- which(LGGA_log == Ctrl_R2string)
  Ctrl_R3row <- which(LGGA_log == Ctrl_R3string)

  SampleProbe_R1_row <- which(LGGA_log == SampleProbe_R1_string)
  SampleProbe_R2_row <- which(LGGA_log == SampleProbe_R2_string)
  SampleProbe_R3_row <- which(LGGA_log == SampleProbe_R3_string)
  SampleProbe_R4_row <- which(LGGA_log == SampleProbe_R4_string)

  # Subset only indicated time span by the user
  LGGA_log <- LGGA_log[c(t_initial_row:t_final_row),]

  # Reset row index
  row.names(LGGA_log) <- NULL

  # Create a secondary data set that neglects controls
  t_initial_wc <- mdy_hms(SampleProbe_R1_string) - dseconds(offset_initial_final)
  t_initial_wc <- format(t_initial_wc, format='%m/%d/%Y %H:%M:%S')
  t_initial_string_wc <- toString(LGGA_log[grep(t_initial_wc, LGGA_log$Time), ][1,1])
  t_initial_row_wc <- which(LGGA_log == t_initial_string_wc)
  LGGA_log_wc <- LGGA_log[c(t_initial_row_wc:t_final_row),]

  # Create Data Set to plot --------
  # Add time series values to the data frame
  LGGA_log_plot <- as.data.table(LGGA_log)

  # Obtain only the second with fractional second and start at t-initial
  LGGA_log_plot$TimeSeconds <- mdy_hms(LGGA_log_plot$Time)
  LGGA_log_plot$TimeSeconds <- LGGA_log_plot$TimeSeconds - LGGA_log_plot$TimeSeconds[1]

  # Do the same for the Without Controls data set
  LGGA_log_plot_wc <- as.data.table(LGGA_log_wc)
  LGGA_log_plot_wc$TimeSeconds <- mdy_hms(LGGA_log_plot_wc$Time)
  LGGA_log_plot_wc$TimeSeconds <- LGGA_log_plot_wc$TimeSeconds - LGGA_log_plot_wc$TimeSeconds[1]

  ## CO2 with controls ------------------------
  LGGA_log_plot_CO2 <- LGGA_log_plot %>%
    select("TimeSeconds",
           "[CO2]d_ppm") %>%
    gather(key = "variable", value = "value", -"TimeSeconds")

  LGGA_log_plot_CO2$value <- as.numeric(LGGA_log_plot_CO2$value)
  nrows_plot_CO2 <- nrow(LGGA_log_plot_CO2)

  ## Without controls -----------
  LGGA_log_plot_CO2_wc <- LGGA_log_plot_wc %>%
    select("TimeSeconds",
           "[CO2]d_ppm") %>%
    gather(key = "variable", value = "value", -"TimeSeconds")

  LGGA_log_plot_CO2_wc$value <- as.numeric(LGGA_log_plot_CO2_wc$value)
  nrows_plot_CO2_wc <- nrow(LGGA_log_plot_CO2_wc)

  ## CH4 with controls ------------------------
  LGGA_log_plot_CH4 <- LGGA_log_plot %>%
    select("TimeSeconds",
           "[CH4]d_ppm") %>%
    gather(key = "variable", value = "value", -"TimeSeconds")

  LGGA_log_plot_CH4$value <- as.numeric(LGGA_log_plot_CH4$value)
  nrows_plot_CH4 <- nrow(LGGA_log_plot_CH4)

  ## Without controls -----------
  LGGA_log_plot_CH4_wc <- LGGA_log_plot_wc %>%
    select("TimeSeconds",
           "[CH4]d_ppm") %>%
    gather(key = "variable", value = "value", -"TimeSeconds")

  LGGA_log_plot_CH4_wc$value <- as.numeric(LGGA_log_plot_CH4_wc$value)
  nrows_plot_CH4_wc <- nrow(LGGA_log_plot_CH4_wc)

  # First Plots: All treatmens and replicateswith and without controls ----------------------
  ## CO2 with controls ----------
  plot_CO2_LGGA_log_plot <- ggplot(LGGA_log_plot_CO2, aes(x =c(1:nrows_plot_CO2), y = value)) +
    geom_line(aes(color = variable), size = 0.25) +
    theme_classic() +
    ylim(min(LGGA_log_plot_CO2$value), max(LGGA_log_plot_CO2$value)) +
    ggtitle(paste("Concentration Evolution of Carbon Dioxide")) +
    labs(subtitle=paste(format(date, format="%B %d %Y")),
         y="CO2 (dry) Concentration [ppm]",
         x="Time [s]",
         caption="Manuel Ayala -Velazquez\nIntMetOx",
         col="Compartment") +
    scale_color_manual(name = "Species", values = c("steelblue3")) +
    geom_vline(xintercept = Ctrl_R1row-t_initial_row, linetype="dotted",
               color = "darkred", size=0.5) +
    geom_vline(xintercept = Ctrl_R2row-t_initial_row, linetype="dotted",
               color = "darkred", size=0.5) +
    geom_vline(xintercept = Ctrl_R3row-t_initial_row, linetype="dotted",
               color = "darkred", size=0.5) +
    geom_vline(xintercept = SampleProbe_R1_row-t_initial_row, linetype="dotted",
               color = "darkgreen", size=0.5) +
    geom_vline(xintercept = SampleProbe_R2_row-t_initial_row, linetype="dotted",
               color = "darkgreen", size=0.5) +
    geom_vline(xintercept = SampleProbe_R3_row-t_initial_row, linetype="dotted",
               color = "darkgreen", size=0.5) +
    geom_vline(xintercept = SampleProbe_R4_row-t_initial_row, linetype="dotted",
               color = "darkgreen", size=0.5)

  ## CO2 without controls ----------
  # Subset only values without NAs
  LGGA_log_plot_CO2_wc <- LGGA_log_plot_CO2_wc[1:nrow(na.omit(LGGA_log_plot_CO2_wc)),]
  nrows_plot_CO2_wc <- nrow(LGGA_log_plot_CO2_wc)

  plot_CO2_LGGA_log_plot_wc <- ggplot(LGGA_log_plot_CO2_wc, aes(x =c(1:nrows_plot_CO2_wc), y = value)) +
    geom_line(aes(color = variable), size = 0.25) +
    theme_classic() +
    ylim(min(LGGA_log_plot_CO2_wc$value), max(LGGA_log_plot_CO2_wc$value)) +
    ggtitle(paste("Concentration Evolution of Carbon Dioxide")) +
    labs(subtitle=paste(format(date, format="%B %d %Y")),
         y="CO2 (dry) Concentration [ppm]",
         x="Time [s]",
         caption="Manuel Ayala -Velazquez\nIntMetOx",
         col="Compartment") +
    scale_color_manual(name = "Species", values = c("steelblue3")) +
    geom_vline(xintercept = SampleProbe_R1_row-SampleProbe_R1_row, linetype="dotted",
               color = "darkgreen", size=0.5) +
    geom_vline(xintercept = SampleProbe_R2_row-SampleProbe_R1_row, linetype="dotted",
               color = "darkgreen", size=0.5) +
    geom_vline(xintercept = SampleProbe_R3_row-SampleProbe_R1_row, linetype="dotted",
               color = "darkgreen", size=0.5) +
    geom_vline(xintercept = SampleProbe_R4_row-SampleProbe_R1_row, linetype="dotted",
               color = "darkgreen", size=0.5)

  ## CH4 with controls ----------
  plot_CH4_LGGA_log_plot <- ggplot(LGGA_log_plot_CH4, aes(x =c(1:nrows_plot_CH4), y = value)) +
    geom_line(aes(color = variable), size = 0.25) +
    theme_classic() +
    ylim(min(LGGA_log_plot_CH4$value), max(LGGA_log_plot_CH4$value)) +
    ggtitle(paste("Concentration Evolution of Methane")) +
    labs(subtitle=paste(format(date, format="%B %d %Y")),
         y="CH4 (dry) Concentration [ppm]",
         x="Time [s]",
         caption="Manuel Ayala -Velazquez\nIntMetOx",
         col="Compartment") +
    scale_color_manual(name = "Species", values = c("steelblue3")) +
    geom_vline(xintercept = Ctrl_R1row-t_initial_row, linetype="dotted",
               color = "red", size=0.5) +
    geom_vline(xintercept = Ctrl_R2row-t_initial_row, linetype="dotted",
               color = "red", size=0.5) +
    geom_vline(xintercept = Ctrl_R3row-t_initial_row, linetype="dotted",
               color = "red", size=0.5) +
    geom_vline(xintercept = SampleProbe_R1_row-t_initial_row, linetype="dotted",
               color = "blue", size=0.5) +
    geom_vline(xintercept = SampleProbe_R2_row-t_initial_row, linetype="dotted",
               color = "blue", size=0.5) +
    geom_vline(xintercept = SampleProbe_R3_row-t_initial_row, linetype="dotted",
               color = "blue", size=0.5) +
    geom_vline(xintercept = SampleProbe_R4_row-t_initial_row, linetype="dotted",
               color = "blue", size=0.5)

  ## CH4 without controls ----------
  LGGA_log_plot_CH4_wc <- LGGA_log_plot_CH4_wc[1:nrow(na.omit(LGGA_log_plot_CH4_wc)),]
  nrows_plot_CH4_wc <- nrow(LGGA_log_plot_CH4_wc)

  plot_CH4_LGGA_log_plot_wc <- ggplot(LGGA_log_plot_CH4_wc, aes(x =c(1:nrows_plot_CH4_wc), y = value)) +
    geom_line(aes(color = variable), size = 0.25) +
    theme_classic() +
    ylim(min(LGGA_log_plot_CH4_wc$value), max(LGGA_log_plot_CH4_wc$value)) +
    ggtitle(paste("Concentration Evolution of Methane")) +
    labs(subtitle=paste(format(date, format="%B %d %Y")),
         y="CH4 (dry) Concentration [ppm]",
         x="Time [s]",
         caption="Manuel Ayala -Velazquez\nIntMetOx",
         col="Compartment") +
    scale_color_manual(name = "Species", values = c("steelblue3")) +
    geom_vline(xintercept = SampleProbe_R1_row-SampleProbe_R1_row, linetype="dotted",
               color = "turquoise4", size=0.5) +
    geom_vline(xintercept = SampleProbe_R2_row-SampleProbe_R1_row, linetype="dotted",
               color = "turquoise4", size=0.5) +
    geom_vline(xintercept = SampleProbe_R3_row-SampleProbe_R1_row, linetype="dotted",
               color = "turquoise4", size=0.5) +
    geom_vline(xintercept = SampleProbe_R4_row-SampleProbe_R1_row, linetype="dotted",
               color = "turquoise4", size=0.5)

  # Calculate Plateaus ---------
  ## S_R1 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_S_R1_base_plateau_interval_initial <- mdy_hms(Ctrl_R1string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_S_R1_peak_plateau_interval_initial <- mdy_hms(Ctrl_R1string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_S_R1_peak_plateau_interval_final <- datetime_S_R1_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_S_R1_base_plateau_interval_initial <- str_remove(datetime_S_R1_base_plateau_interval_initial, " CET")
  datetime_S_R1_peak_plateau_interval_initial <- str_remove(datetime_S_R1_peak_plateau_interval_initial, " CET")
  datetime_S_R1_peak_plateau_interval_final <- str_remove(datetime_S_R1_peak_plateau_interval_final, " CET")

  # Substitute dash by forward slash
  datetime_S_R1_base_plateau_interval_initial <- gsub("-", "/", datetime_S_R1_base_plateau_interval_initial)
  datetime_S_R1_peak_plateau_interval_initial <- gsub("-", "/", datetime_S_R1_peak_plateau_interval_initial)
  datetime_S_R1_peak_plateau_interval_final <- gsub("-", "/", datetime_S_R1_peak_plateau_interval_final)

  # Invert order of date
  datetime_S_R1_peak_plateau_interval_initial <- as.POSIXct(datetime_S_R1_peak_plateau_interval_initial)
  datetime_S_R1_base_plateau_interval_initial <- as.POSIXct(datetime_S_R1_base_plateau_interval_initial)
  datetime_S_R1_peak_plateau_interval_final <- as.POSIXct(datetime_S_R1_peak_plateau_interval_final)

  datetime_S_R1_base_plateau_interval_initial <- format(datetime_S_R1_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_S_R1_peak_plateau_interval_initial <- format(datetime_S_R1_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_S_R1_peak_plateau_interval_final <- format(datetime_S_R1_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_S_R1_peak_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_S_R1_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  S_R1_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_S_R1_peak_plateau_interval_initial_string) #-t_initial_row

  datetime_S_R1_base_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_S_R1_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  S_R1_base_plateau_interval_initial_row <- which(LGGA_log == datetime_S_R1_base_plateau_interval_initial_string) #-t_initial_row

  datetime_S_R1_peak_plateau_interval_final_string <- toString(LGGA_log[grep(datetime_S_R1_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  S_R1_peak_plateau_interval_final_row <- which(LGGA_log == datetime_S_R1_peak_plateau_interval_final_string) #-t_initial_row

  # Obtain plateaus
  # Obtain the measured values in such interval
  CO2_interval_base_S_R1 <- LGGA_log$`[CO2]d_ppm`[c(S_R1_base_plateau_interval_initial_row:c(S_R1_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_S_R1 <- LGGA_log$`[CO2]d_ppm`[c(S_R1_peak_plateau_interval_initial_row:S_R1_peak_plateau_interval_final_row)]

  CH4_interval_base_S_R1 <-LGGA_log$`[CH4]d_ppm`[c(S_R1_base_plateau_interval_initial_row:c(S_R1_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_S_R1 <- LGGA_log$`[CH4]d_ppm`[c(S_R1_peak_plateau_interval_initial_row:S_R1_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_S_R1 <- as.numeric(CO2_interval_base_S_R1)
  CH4_interval_base_S_R1 <- as.numeric(CH4_interval_base_S_R1)

  CO2_interval_peak_S_R1 <- as.numeric(CO2_interval_peak_S_R1)
  CH4_interval_peak_S_R1 <- as.numeric(CH4_interval_peak_S_R1)

  ## S_R2 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_S_R2_base_plateau_interval_initial <- mdy_hms(Ctrl_R2string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_S_R2_peak_plateau_interval_initial <- mdy_hms(Ctrl_R2string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_S_R2_peak_plateau_interval_final <- datetime_S_R2_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_S_R2_base_plateau_interval_initial <- str_remove(datetime_S_R2_base_plateau_interval_initial, " CET")
  datetime_S_R2_peak_plateau_interval_initial <- str_remove(datetime_S_R2_peak_plateau_interval_initial, " CET")
  datetime_S_R2_peak_plateau_interval_final <- str_remove(datetime_S_R2_peak_plateau_interval_final, " CET")
  # Substitute dash by forward slash
  datetime_S_R2_base_plateau_interval_initial <- gsub("-", "/", datetime_S_R2_base_plateau_interval_initial)
  datetime_S_R2_peak_plateau_interval_initial <- gsub("-", "/", datetime_S_R2_peak_plateau_interval_initial)
  datetime_S_R2_peak_plateau_interval_final <- gsub("-", "/", datetime_S_R2_peak_plateau_interval_final)
  # # Invert order of date
  datetime_S_R2_peak_plateau_interval_initial <- as.POSIXct(datetime_S_R2_peak_plateau_interval_initial)
  datetime_S_R2_base_plateau_interval_initial <- as.POSIXct(datetime_S_R2_base_plateau_interval_initial)
  datetime_S_R2_peak_plateau_interval_final <- as.POSIXct(datetime_S_R2_peak_plateau_interval_final)

  datetime_S_R2_base_plateau_interval_initial <- format(datetime_S_R2_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_S_R2_peak_plateau_interval_initial <- format(datetime_S_R2_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_S_R2_peak_plateau_interval_final <- format(datetime_S_R2_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_S_R2_peak_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_S_R2_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  S_R2_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_S_R2_peak_plateau_interval_initial_string) #-t_initial_row

  datetime_S_R2_base_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_S_R2_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  S_R2_base_plateau_interval_initial_row <- which(LGGA_log == datetime_S_R2_base_plateau_interval_initial_string) #-t_initial_row

  datetime_S_R2_peak_plateau_interval_final_string <- toString(LGGA_log[grep(datetime_S_R2_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  S_R2_peak_plateau_interval_final_row <- which(LGGA_log == datetime_S_R2_peak_plateau_interval_final_string) #-t_initial_row

  # Compute average of both plateaus
  # Obtain the measured values in such interval
  # CO2_interval_base_S_R2 <- LGGA_log$`[CO2]d_ppm`[c(S_R2_base_plateau_interval_initial_row:S_R2_base_plateau_interval_initial_row+interval_seconds)]
  CO2_interval_base_S_R2 <- LGGA_log$`[CO2]d_ppm`[c(S_R2_base_plateau_interval_initial_row:c(S_R2_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_S_R2 <- LGGA_log$`[CO2]d_ppm`[c(S_R2_peak_plateau_interval_initial_row:S_R2_peak_plateau_interval_final_row)]

  # CH4_interval_base_S_R2 <-LGGA_log$`[CH4]d_ppm`[c(S_R2_base_plateau_interval_initial_row:S_R2_base_plateau_interval_initial_row+interval_seconds)]
  CH4_interval_base_S_R2 <-LGGA_log$`[CH4]d_ppm`[c(S_R2_base_plateau_interval_initial_row:c(S_R2_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_S_R2 <- LGGA_log$`[CH4]d_ppm`[c(S_R2_peak_plateau_interval_initial_row:S_R2_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_S_R2 <- as.numeric(CO2_interval_base_S_R2)
  CH4_interval_base_S_R2 <- as.numeric(CH4_interval_base_S_R2)

  CO2_interval_peak_S_R2 <- as.numeric(CO2_interval_peak_S_R2)
  CH4_interval_peak_S_R2 <- as.numeric(CH4_interval_peak_S_R2)
  ## S_R3 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_S_R3_base_plateau_interval_initial <- mdy_hms(Ctrl_R3string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_S_R3_peak_plateau_interval_initial <- mdy_hms(Ctrl_R3string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_S_R3_peak_plateau_interval_final <- datetime_S_R3_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_S_R3_base_plateau_interval_initial <- str_remove(datetime_S_R3_base_plateau_interval_initial, " CET")
  datetime_S_R3_peak_plateau_interval_initial <- str_remove(datetime_S_R3_peak_plateau_interval_initial, " CET")
  datetime_S_R3_peak_plateau_interval_final <- str_remove(datetime_S_R3_peak_plateau_interval_final, " CET")
  # Substitute dash by forward slash
  datetime_S_R3_base_plateau_interval_initial <- gsub("-", "/", datetime_S_R3_base_plateau_interval_initial)
  datetime_S_R3_peak_plateau_interval_initial <- gsub("-", "/", datetime_S_R3_peak_plateau_interval_initial)
  datetime_S_R3_peak_plateau_interval_final <- gsub("-", "/", datetime_S_R3_peak_plateau_interval_final)
  # # Invert order of date
  datetime_S_R3_peak_plateau_interval_initial <- as.POSIXct(datetime_S_R3_peak_plateau_interval_initial)
  datetime_S_R3_base_plateau_interval_initial <- as.POSIXct(datetime_S_R3_base_plateau_interval_initial)
  datetime_S_R3_peak_plateau_interval_final <- as.POSIXct(datetime_S_R3_peak_plateau_interval_final)

  datetime_S_R3_base_plateau_interval_initial <- format(datetime_S_R3_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_S_R3_peak_plateau_interval_initial <- format(datetime_S_R3_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_S_R3_peak_plateau_interval_final <- format(datetime_S_R3_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_S_R3_peak_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_S_R3_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  S_R3_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_S_R3_peak_plateau_interval_initial_string) #-t_initial_row

  datetime_S_R3_base_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_S_R3_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  S_R3_base_plateau_interval_initial_row <- which(LGGA_log == datetime_S_R3_base_plateau_interval_initial_string) #-t_initial_row

  datetime_S_R3_peak_plateau_interval_final_string <- toString(LGGA_log[grep(datetime_S_R3_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  S_R3_peak_plateau_interval_final_row <- which(LGGA_log == datetime_S_R3_peak_plateau_interval_final_string) #-t_initial_row

  # Compute average of both plateaus
  # Obtain the measured values in such interval
  # CO2_interval_base_S_R3 <- LGGA_log$`[CO2]d_ppm`[c(S_R3_base_plateau_interval_initial_row:S_R3_base_plateau_interval_initial_row+interval_seconds)]
  CO2_interval_base_S_R3 <- LGGA_log$`[CO2]d_ppm`[c(S_R3_base_plateau_interval_initial_row:c(S_R3_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_S_R3 <- LGGA_log$`[CO2]d_ppm`[c(S_R3_peak_plateau_interval_initial_row:S_R3_peak_plateau_interval_final_row)]

  # CH4_interval_base_S_R3 <-LGGA_log$`[CH4]d_ppm`[c(S_R3_base_plateau_interval_initial_row:S_R3_base_plateau_interval_initial_row+interval_seconds)]
  CH4_interval_base_S_R3 <-LGGA_log$`[CH4]d_ppm`[c(S_R3_base_plateau_interval_initial_row:c(S_R3_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_S_R3 <- LGGA_log$`[CH4]d_ppm`[c(S_R3_peak_plateau_interval_initial_row:S_R3_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_S_R3 <- as.numeric(CO2_interval_base_S_R3)
  CH4_interval_base_S_R3 <- as.numeric(CH4_interval_base_S_R3)

  CO2_interval_peak_S_R3 <- as.numeric(CO2_interval_peak_S_R3)
  CH4_interval_peak_S_R3 <- as.numeric(CH4_interval_peak_S_R3)
  ## Sample_R1 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_Sample_R1_base_plateau_interval_initial <- mdy_hms(SampleProbe_R1_string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_Sample_R1_peak_plateau_interval_initial <- mdy_hms(SampleProbe_R1_string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_Sample_R1_peak_plateau_interval_final <- datetime_Sample_R1_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_Sample_R1_base_plateau_interval_initial <- str_remove(datetime_Sample_R1_base_plateau_interval_initial, " CET")
  datetime_Sample_R1_peak_plateau_interval_initial <- str_remove(datetime_Sample_R1_peak_plateau_interval_initial, " CET")
  datetime_Sample_R1_peak_plateau_interval_final <- str_remove(datetime_Sample_R1_peak_plateau_interval_final, " CET")
  # Substitute dash by forward slash
  datetime_Sample_R1_base_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R1_base_plateau_interval_initial)
  datetime_Sample_R1_peak_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R1_peak_plateau_interval_initial)
  datetime_Sample_R1_peak_plateau_interval_final <- gsub("-", "/", datetime_Sample_R1_peak_plateau_interval_final)
  # # Invert order of date
  datetime_Sample_R1_peak_plateau_interval_initial <- as.POSIXct(datetime_Sample_R1_peak_plateau_interval_initial)
  datetime_Sample_R1_base_plateau_interval_initial <- as.POSIXct(datetime_Sample_R1_base_plateau_interval_initial)
  datetime_Sample_R1_peak_plateau_interval_final <- as.POSIXct(datetime_Sample_R1_peak_plateau_interval_final)

  datetime_Sample_R1_base_plateau_interval_initial <- format(datetime_Sample_R1_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R1_peak_plateau_interval_initial <- format(datetime_Sample_R1_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R1_peak_plateau_interval_final <- format(datetime_Sample_R1_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_Sample_R1_peak_plateau_interval_initial_MBtring <- toString(LGGA_log[grep(datetime_Sample_R1_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R1_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R1_peak_plateau_interval_initial_MBtring) #-t_initial_row

  datetime_Sample_R1_base_plateau_interval_initial_MBtring <- toString(LGGA_log[grep(datetime_Sample_R1_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R1_base_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R1_base_plateau_interval_initial_MBtring) #-t_initial_row

  datetime_Sample_R1_peak_plateau_interval_final_MBtring <- toString(LGGA_log[grep(datetime_Sample_R1_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  Sample_R1_peak_plateau_interval_final_row <- which(LGGA_log == datetime_Sample_R1_peak_plateau_interval_final_MBtring) #-t_initial_row

  # Compute average of both plateaus
  # Obtain the measured values in such interval
  # CO2_interval_base_Sample_R1 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R1_base_plateau_interval_initial_row:Sample_R1_base_plateau_interval_initial_row+interval_seconds)]
  CO2_interval_base_Sample_R1 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R1_base_plateau_interval_initial_row:c(Sample_R1_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_Sample_R1 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R1_peak_plateau_interval_initial_row:Sample_R1_peak_plateau_interval_final_row)]

  # CH4_interval_base_Sample_R1 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R1_base_plateau_interval_initial_row:Sample_R1_base_plateau_interval_initial_row+interval_seconds)]
  CH4_interval_base_Sample_R1 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R1_base_plateau_interval_initial_row:c(Sample_R1_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_Sample_R1 <- LGGA_log$`[CH4]d_ppm`[c(Sample_R1_peak_plateau_interval_initial_row:Sample_R1_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_Sample_R1 <- as.numeric(CO2_interval_base_Sample_R1)
  CH4_interval_base_Sample_R1 <- as.numeric(CH4_interval_base_Sample_R1)

  CO2_interval_peak_Sample_R1 <- as.numeric(CO2_interval_peak_Sample_R1)
  CH4_interval_peak_Sample_R1 <- as.numeric(CH4_interval_peak_Sample_R1)

  ## Sample_R2 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_Sample_R2_base_plateau_interval_initial <- mdy_hms(SampleProbe_R2_string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_Sample_R2_peak_plateau_interval_initial <- mdy_hms(SampleProbe_R2_string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_Sample_R2_peak_plateau_interval_final <- datetime_Sample_R2_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_Sample_R2_base_plateau_interval_initial <- str_remove(datetime_Sample_R2_base_plateau_interval_initial, " CET")
  datetime_Sample_R2_peak_plateau_interval_initial <- str_remove(datetime_Sample_R2_peak_plateau_interval_initial, " CET")
  datetime_Sample_R2_peak_plateau_interval_final <- str_remove(datetime_Sample_R2_peak_plateau_interval_final, " CET")
  # Substitute dash by forward slash
  datetime_Sample_R2_base_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R2_base_plateau_interval_initial)
  datetime_Sample_R2_peak_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R2_peak_plateau_interval_initial)
  datetime_Sample_R2_peak_plateau_interval_final <- gsub("-", "/", datetime_Sample_R2_peak_plateau_interval_final)
  # # Invert order of date
  datetime_Sample_R2_peak_plateau_interval_initial <- as.POSIXct(datetime_Sample_R2_peak_plateau_interval_initial)
  datetime_Sample_R2_base_plateau_interval_initial <- as.POSIXct(datetime_Sample_R2_base_plateau_interval_initial)
  datetime_Sample_R2_peak_plateau_interval_final <- as.POSIXct(datetime_Sample_R2_peak_plateau_interval_final)

  datetime_Sample_R2_base_plateau_interval_initial <- format(datetime_Sample_R2_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R2_peak_plateau_interval_initial <- format(datetime_Sample_R2_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R2_peak_plateau_interval_final <- format(datetime_Sample_R2_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_Sample_R2_peak_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_Sample_R2_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R2_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R2_peak_plateau_interval_initial_string) #-t_initial_row

  datetime_Sample_R2_base_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_Sample_R2_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R2_base_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R2_base_plateau_interval_initial_string) #-t_initial_row

  datetime_Sample_R2_peak_plateau_interval_final_string <- toString(LGGA_log[grep(datetime_Sample_R2_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  Sample_R2_peak_plateau_interval_final_row <- which(LGGA_log == datetime_Sample_R2_peak_plateau_interval_final_string) #-t_initial_row

  # Compute average of both plateaus
  # Obtain the measured values in such interval
  # CO2_interval_base_Sample_R2 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R2_base_plateau_interval_initial_row:Sample_R2_base_plateau_interval_initial_row+interval_seconds)]
  CO2_interval_base_Sample_R2 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R2_base_plateau_interval_initial_row:c(Sample_R2_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_Sample_R2 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R2_peak_plateau_interval_initial_row:Sample_R2_peak_plateau_interval_final_row)]

  # CH4_interval_base_Sample_R2 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R2_base_plateau_interval_initial_row:Sample_R2_base_plateau_interval_initial_row+interval_seconds)]
  CH4_interval_base_Sample_R2 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R2_base_plateau_interval_initial_row:c(Sample_R2_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_Sample_R2 <- LGGA_log$`[CH4]d_ppm`[c(Sample_R2_peak_plateau_interval_initial_row:Sample_R2_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_Sample_R2 <- as.numeric(CO2_interval_base_Sample_R2)
  CH4_interval_base_Sample_R2 <- as.numeric(CH4_interval_base_Sample_R2)

  CO2_interval_peak_Sample_R2 <- as.numeric(CO2_interval_peak_Sample_R2)
  CH4_interval_peak_Sample_R2 <- as.numeric(CH4_interval_peak_Sample_R2)

  ## Sample_R3 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_Sample_R3_base_plateau_interval_initial <- mdy_hms(SampleProbe_R3_string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_Sample_R3_peak_plateau_interval_initial <- mdy_hms(SampleProbe_R3_string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_Sample_R3_peak_plateau_interval_final <- datetime_Sample_R3_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_Sample_R3_base_plateau_interval_initial <- str_remove(datetime_Sample_R3_base_plateau_interval_initial, " CET")
  datetime_Sample_R3_peak_plateau_interval_initial <- str_remove(datetime_Sample_R3_peak_plateau_interval_initial, " CET")
  datetime_Sample_R3_peak_plateau_interval_final <- str_remove(datetime_Sample_R3_peak_plateau_interval_final, " CET")
  # Substitute dash by forward slash
  datetime_Sample_R3_base_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R3_base_plateau_interval_initial)
  datetime_Sample_R3_peak_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R3_peak_plateau_interval_initial)
  datetime_Sample_R3_peak_plateau_interval_final <- gsub("-", "/", datetime_Sample_R3_peak_plateau_interval_final)
  # # Invert order of date
  datetime_Sample_R3_peak_plateau_interval_initial <- as.POSIXct(datetime_Sample_R3_peak_plateau_interval_initial)
  datetime_Sample_R3_base_plateau_interval_initial <- as.POSIXct(datetime_Sample_R3_base_plateau_interval_initial)
  datetime_Sample_R3_peak_plateau_interval_final <- as.POSIXct(datetime_Sample_R3_peak_plateau_interval_final)

  datetime_Sample_R3_base_plateau_interval_initial <- format(datetime_Sample_R3_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R3_peak_plateau_interval_initial <- format(datetime_Sample_R3_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R3_peak_plateau_interval_final <- format(datetime_Sample_R3_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_Sample_R3_peak_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_Sample_R3_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R3_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R3_peak_plateau_interval_initial_string) #-t_initial_row

  datetime_Sample_R3_base_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_Sample_R3_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R3_base_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R3_base_plateau_interval_initial_string) #-t_initial_row

  datetime_Sample_R3_peak_plateau_interval_final_string <- toString(LGGA_log[grep(datetime_Sample_R3_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  Sample_R3_peak_plateau_interval_final_row <- which(LGGA_log == datetime_Sample_R3_peak_plateau_interval_final_string) #-t_initial_row

  # Compute average of both plateaus
  # Obtain the measured values in such interval
  # CO2_interval_base_Sample_R3 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R3_base_plateau_interval_initial_row:Sample_R3_base_plateau_interval_initial_row+interval_seconds)]
  CO2_interval_base_Sample_R3 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R3_base_plateau_interval_initial_row:c(Sample_R3_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_Sample_R3 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R3_peak_plateau_interval_initial_row:Sample_R3_peak_plateau_interval_final_row)]

  # CH4_interval_base_Sample_R3 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R3_base_plateau_interval_initial_row:Sample_R3_base_plateau_interval_initial_row+interval_seconds)]
  CH4_interval_base_Sample_R3 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R3_base_plateau_interval_initial_row:c(Sample_R3_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_Sample_R3 <- LGGA_log$`[CH4]d_ppm`[c(Sample_R3_peak_plateau_interval_initial_row:Sample_R3_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_Sample_R3 <- as.numeric(CO2_interval_base_Sample_R3)
  CH4_interval_base_Sample_R3 <- as.numeric(CH4_interval_base_Sample_R3)

  CO2_interval_peak_Sample_R3 <- as.numeric(CO2_interval_peak_Sample_R3)
  CH4_interval_peak_Sample_R3 <- as.numeric(CH4_interval_peak_Sample_R3)

  ## Sample_R4 -----------------------
  # Subtract 'base_interval_begin_seconds' seconds to the injection time to create the base interval of the current sample
  datetime_Sample_R4_base_plateau_interval_initial <- mdy_hms(SampleProbe_R4_string) - dseconds(base_interval_begin_seconds)

  # Add 'peak_interval_begin_seconds' second to the injection time to create the initial peak interval of the current sample
  datetime_Sample_R4_peak_plateau_interval_initial <- mdy_hms(SampleProbe_R4_string) + dseconds(peak_interval_begin_seconds)

  # Add 'interval_seconds' second to the initial peak time stamp to create the final time stamp for the peak
  datetime_Sample_R4_peak_plateau_interval_final <- datetime_Sample_R4_peak_plateau_interval_initial + dseconds(interval_seconds)

  # Remove time zone
  datetime_Sample_R4_base_plateau_interval_initial <- str_remove(datetime_Sample_R4_base_plateau_interval_initial, " CET")
  datetime_Sample_R4_peak_plateau_interval_initial <- str_remove(datetime_Sample_R4_peak_plateau_interval_initial, " CET")
  datetime_Sample_R4_peak_plateau_interval_final <- str_remove(datetime_Sample_R4_peak_plateau_interval_final, " CET")
  # Substitute dash by forward slash
  datetime_Sample_R4_base_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R4_base_plateau_interval_initial)
  datetime_Sample_R4_peak_plateau_interval_initial <- gsub("-", "/", datetime_Sample_R4_peak_plateau_interval_initial)
  datetime_Sample_R4_peak_plateau_interval_final <- gsub("-", "/", datetime_Sample_R4_peak_plateau_interval_final)
  # # Invert order of date
  datetime_Sample_R4_peak_plateau_interval_initial <- as.POSIXct(datetime_Sample_R4_peak_plateau_interval_initial)
  datetime_Sample_R4_base_plateau_interval_initial <- as.POSIXct(datetime_Sample_R4_base_plateau_interval_initial)
  datetime_Sample_R4_peak_plateau_interval_final <- as.POSIXct(datetime_Sample_R4_peak_plateau_interval_final)

  datetime_Sample_R4_base_plateau_interval_initial <- format(datetime_Sample_R4_base_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R4_peak_plateau_interval_initial <- format(datetime_Sample_R4_peak_plateau_interval_initial,format='%m/%d/%Y %H:%M:%S')
  datetime_Sample_R4_peak_plateau_interval_final <- format(datetime_Sample_R4_peak_plateau_interval_final,format='%m/%d/%Y %H:%M:%S')

  # Retrieve row where the previous times are located
  datetime_Sample_R4_peak_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_Sample_R4_peak_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R4_peak_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R4_peak_plateau_interval_initial_string) #-t_initial_row

  datetime_Sample_R4_base_plateau_interval_initial_string <- toString(LGGA_log[grep(datetime_Sample_R4_base_plateau_interval_initial, LGGA_log$Time), ][1,1])
  Sample_R4_base_plateau_interval_initial_row <- which(LGGA_log == datetime_Sample_R4_base_plateau_interval_initial_string) #-t_initial_row

  datetime_Sample_R4_peak_plateau_interval_final_string <- toString(LGGA_log[grep(datetime_Sample_R4_peak_plateau_interval_final, LGGA_log$Time), ][1,1])
  Sample_R4_peak_plateau_interval_final_row <- which(LGGA_log == datetime_Sample_R4_peak_plateau_interval_final_string) #-t_initial_row

  # Compute average of both plateaus
  # Obtain the measured values in such interval
  # CO2_interval_base_Sample_R4 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R4_base_plateau_interval_initial_row:Sample_R4_base_plateau_interval_initial_row+interval_seconds)]
  CO2_interval_base_Sample_R4 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R4_base_plateau_interval_initial_row:c(Sample_R4_base_plateau_interval_initial_row+interval_seconds))]
  CO2_interval_peak_Sample_R4 <- LGGA_log$`[CO2]d_ppm`[c(Sample_R4_peak_plateau_interval_initial_row:Sample_R4_peak_plateau_interval_final_row)]

  # CH4_interval_base_Sample_R4 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R4_base_plateau_interval_initial_row:Sample_R4_base_plateau_interval_initial_row+interval_seconds)]
  CH4_interval_base_Sample_R4 <-LGGA_log$`[CH4]d_ppm`[c(Sample_R4_base_plateau_interval_initial_row:c(Sample_R4_base_plateau_interval_initial_row+interval_seconds))]
  CH4_interval_peak_Sample_R4 <- LGGA_log$`[CH4]d_ppm`[c(Sample_R4_peak_plateau_interval_initial_row:Sample_R4_peak_plateau_interval_final_row)]

  # Convert string to numerics
  CO2_interval_base_Sample_R4 <- as.numeric(CO2_interval_base_Sample_R4)
  CH4_interval_base_Sample_R4 <- as.numeric(CH4_interval_base_Sample_R4)

  CO2_interval_peak_Sample_R4 <- as.numeric(CO2_interval_peak_Sample_R4)
  CH4_interval_peak_Sample_R4 <- as.numeric(CH4_interval_peak_Sample_R4)

  # Compute averages -----------------------
  ## Standards ------------------
  X_infty_S_R1_CO2 <- mean(CO2_interval_peak_S_R1)
  X_0_S_R1_CO2 <- mean(CO2_interval_base_S_R1)
  X_infty_S_R1_CH4 <-mean(CH4_interval_peak_S_R1)
  X_0_S_R1_CH4 <- mean(CH4_interval_base_S_R1)

  X_infty_S_R2_CO2 <- mean(CO2_interval_peak_S_R2)
  X_0_S_R2_CO2 <- mean(CO2_interval_base_S_R2)
  X_infty_S_R2_CH4 <-mean(CH4_interval_peak_S_R2)
  X_0_S_R2_CH4 <- mean(CH4_interval_base_S_R2)

  X_infty_S_R3_CO2 <- mean(CO2_interval_peak_S_R3)
  X_0_S_R3_CO2 <- mean(CO2_interval_base_S_R3)
  X_infty_S_R3_CH4 <-mean(CH4_interval_peak_S_R3)
  X_0_S_R3_CH4 <- mean(CH4_interval_base_S_R3)

  ## Samples --------------------
  X_infty_Sample_R1_CO2 <- mean(CO2_interval_peak_Sample_R1)
  X_0_Sample_R1_CO2 <- mean(CO2_interval_base_Sample_R1)
  X_infty_Sample_R1_CH4 <-mean(CH4_interval_peak_Sample_R1)
  X_0_Sample_R1_CH4 <- mean(CH4_interval_base_Sample_R1)

  X_infty_Sample_R2_CO2 <- mean(CO2_interval_peak_Sample_R2)
  X_0_Sample_R2_CO2 <- mean(CO2_interval_base_Sample_R2)
  X_infty_Sample_R2_CH4 <-mean(CH4_interval_peak_Sample_R2)
  X_0_Sample_R2_CH4 <- mean(CH4_interval_base_Sample_R2)

  X_infty_Sample_R3_CO2 <- mean(CO2_interval_peak_Sample_R3)
  X_0_Sample_R3_CO2 <- mean(CO2_interval_base_Sample_R3)
  X_infty_Sample_R3_CH4 <-mean(CH4_interval_peak_Sample_R3)
  X_0_Sample_R3_CH4 <- mean(CH4_interval_base_Sample_R3)

  X_infty_Sample_R4_CO2 <- mean(CO2_interval_peak_Sample_R4)
  X_0_Sample_R4_CO2 <- mean(CO2_interval_base_Sample_R4)
  X_infty_Sample_R4_CH4 <-mean(CH4_interval_peak_Sample_R4)
  X_0_Sample_R4_CH4 <- mean(CH4_interval_base_Sample_R4)

  # Second Plots: Plateaus  ----------------------
  ## CO2 ------------------------
  # Add the plateau ranges to the plots
  plot_CO2_LGGA_log_plot_base_peak <- plot_CO2_LGGA_log_plot +
    # Standards
    # R1
    # Peak
    geom_segment(aes(x=S_R1_peak_plateau_interval_initial_row,
                     xend=S_R1_peak_plateau_interval_final_row,
                     y=X_infty_S_R1_CO2,
                     yend=X_infty_S_R1_CO2)) +
    geom_point(aes(x=(S_R1_peak_plateau_interval_initial_row+S_R1_peak_plateau_interval_final_row)/2,
                   y=X_infty_S_R1_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=S_R1_base_plateau_interval_initial_row,
                     xend=S_R1_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_S_R1_CO2,
                     yend=X_0_S_R1_CO2)) +
    geom_point(aes(x=(S_R1_base_plateau_interval_initial_row+(((S_R1_base_plateau_interval_initial_row+interval_seconds)-S_R1_base_plateau_interval_initial_row)/2)),
                   y=X_0_S_R1_CO2), colour="red") +
    # R2
    # Peak
    geom_segment(aes(x=S_R2_peak_plateau_interval_initial_row,
                     xend=S_R2_peak_plateau_interval_final_row,
                     y=X_infty_S_R2_CO2,
                     yend=X_infty_S_R2_CO2)) +
    geom_point(aes(x=(S_R2_peak_plateau_interval_initial_row+S_R2_peak_plateau_interval_final_row)/2,
                   y=X_infty_S_R2_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=S_R2_base_plateau_interval_initial_row,
                     xend=S_R2_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_S_R2_CO2,
                     yend=X_0_S_R2_CO2)) +
    geom_point(aes(x=(S_R2_base_plateau_interval_initial_row+(((S_R2_base_plateau_interval_initial_row+interval_seconds)-S_R2_base_plateau_interval_initial_row)/2)),
                   y=X_0_S_R2_CO2), colour="red") +
    # R3
    # Peak
    geom_segment(aes(x=S_R3_peak_plateau_interval_initial_row,
                     xend=S_R3_peak_plateau_interval_final_row,
                     y=X_infty_S_R3_CO2,
                     yend=X_infty_S_R3_CO2)) +
    geom_point(aes(x=(S_R3_peak_plateau_interval_initial_row+S_R3_peak_plateau_interval_final_row)/2,
                   y=X_infty_S_R3_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=S_R3_base_plateau_interval_initial_row,
                     xend=S_R3_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_S_R3_CO2,
                     yend=X_0_S_R3_CO2)) +
    geom_point(aes(x=(S_R3_base_plateau_interval_initial_row+(((S_R3_base_plateau_interval_initial_row+interval_seconds)-S_R3_base_plateau_interval_initial_row)/2)),
                   y=X_0_S_R3_CO2), colour="red") +
    # Samples
    # R1
    # Peak
    geom_segment(aes(x=Sample_R1_peak_plateau_interval_initial_row,
                     xend=Sample_R1_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R1_CO2,
                     yend=X_infty_Sample_R1_CO2)) +
    geom_point(aes(x=(Sample_R1_peak_plateau_interval_initial_row+Sample_R1_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R1_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R1_base_plateau_interval_initial_row,
                     xend=Sample_R1_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R1_CO2,
                     yend=X_0_Sample_R1_CO2)) +
    geom_point(aes(x=(Sample_R1_base_plateau_interval_initial_row+(((Sample_R1_base_plateau_interval_initial_row+interval_seconds)-Sample_R1_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R1_CO2), colour="red") +
    # R2
    # Peak
    geom_segment(aes(x=Sample_R2_peak_plateau_interval_initial_row,
                     xend=Sample_R2_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R2_CO2,
                     yend=X_infty_Sample_R2_CO2)) +
    geom_point(aes(x=(Sample_R2_peak_plateau_interval_initial_row+Sample_R2_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R2_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R2_base_plateau_interval_initial_row,
                     xend=Sample_R2_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R2_CO2,
                     yend=X_0_Sample_R2_CO2)) +
    geom_point(aes(x=(Sample_R2_base_plateau_interval_initial_row+(((Sample_R2_base_plateau_interval_initial_row+interval_seconds)-Sample_R2_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R2_CO2), colour="red") +
    # R3
    # Peak
    geom_segment(aes(x=Sample_R3_peak_plateau_interval_initial_row,
                     xend=Sample_R3_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R3_CO2,
                     yend=X_infty_Sample_R3_CO2)) +
    geom_point(aes(x=(Sample_R3_peak_plateau_interval_initial_row+Sample_R3_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R3_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R3_base_plateau_interval_initial_row,
                     xend=Sample_R3_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R3_CO2,
                     yend=X_0_Sample_R3_CO2)) +
    geom_point(aes(x=(Sample_R3_base_plateau_interval_initial_row+(((Sample_R3_base_plateau_interval_initial_row+interval_seconds)-Sample_R3_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R3_CO2), colour="red") +
    # R4
    # Peak
    geom_segment(aes(x=Sample_R4_peak_plateau_interval_initial_row,
                     xend=Sample_R4_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R4_CO2,
                     yend=X_infty_Sample_R4_CO2)) +
    geom_point(aes(x=(Sample_R4_peak_plateau_interval_initial_row+Sample_R4_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R4_CO2), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R4_base_plateau_interval_initial_row,
                     xend=Sample_R4_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R4_CO2,
                     yend=X_0_Sample_R4_CO2)) +
    geom_point(aes(x=(Sample_R4_base_plateau_interval_initial_row+(((Sample_R4_base_plateau_interval_initial_row+interval_seconds)-Sample_R4_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R4_CO2), colour="red")

  ## CH4 ---------
  plot_CH4_LGGA_log_plot_base_peak <- plot_CH4_LGGA_log_plot +
    # Standards
    # R1
    # Peak
    geom_segment(aes(x=S_R1_peak_plateau_interval_initial_row,
                     xend=S_R1_peak_plateau_interval_final_row,
                     y=X_infty_S_R1_CH4,
                     yend=X_infty_S_R1_CH4)) +
    geom_point(aes(x=(S_R1_peak_plateau_interval_initial_row+S_R1_peak_plateau_interval_final_row)/2,
                   y=X_infty_S_R1_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=S_R1_base_plateau_interval_initial_row,
                     xend=S_R1_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_S_R1_CH4,
                     yend=X_0_S_R1_CH4)) +
    geom_point(aes(x=(S_R1_base_plateau_interval_initial_row+(((S_R1_base_plateau_interval_initial_row+interval_seconds)-S_R1_base_plateau_interval_initial_row)/2)),
                   y=X_0_S_R1_CH4), colour="red") +
    # R2
    # Peak
    geom_segment(aes(x=S_R2_peak_plateau_interval_initial_row,
                     xend=S_R2_peak_plateau_interval_final_row,
                     y=X_infty_S_R2_CH4,
                     yend=X_infty_S_R2_CH4)) +
    geom_point(aes(x=(S_R2_peak_plateau_interval_initial_row+S_R2_peak_plateau_interval_final_row)/2,
                   y=X_infty_S_R2_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=S_R2_base_plateau_interval_initial_row,
                     xend=S_R2_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_S_R2_CH4,
                     yend=X_0_S_R2_CH4)) +
    geom_point(aes(x=(S_R2_base_plateau_interval_initial_row+(((S_R2_base_plateau_interval_initial_row+interval_seconds)-S_R2_base_plateau_interval_initial_row)/2)),
                   y=X_0_S_R2_CH4), colour="red") +
    # R3
    # Peak
    geom_segment(aes(x=S_R3_peak_plateau_interval_initial_row,
                     xend=S_R3_peak_plateau_interval_final_row,
                     y=X_infty_S_R3_CH4,
                     yend=X_infty_S_R3_CH4)) +
    geom_point(aes(x=(S_R3_peak_plateau_interval_initial_row+S_R3_peak_plateau_interval_final_row)/2,
                   y=X_infty_S_R3_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=S_R3_base_plateau_interval_initial_row,
                     xend=S_R3_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_S_R3_CH4,
                     yend=X_0_S_R3_CH4)) +
    geom_point(aes(x=(S_R3_base_plateau_interval_initial_row+(((S_R3_base_plateau_interval_initial_row+interval_seconds)-S_R3_base_plateau_interval_initial_row)/2)),
                   y=X_0_S_R3_CH4), colour="red") +
    # Samples
    # MB1
    # Peak
    geom_segment(aes(x=Sample_R1_peak_plateau_interval_initial_row,
                     xend=Sample_R1_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R1_CH4,
                     yend=X_infty_Sample_R1_CH4)) +
    geom_point(aes(x=(Sample_R1_peak_plateau_interval_initial_row+Sample_R1_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R1_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R1_base_plateau_interval_initial_row,
                     xend=Sample_R1_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R1_CH4,
                     yend=X_0_Sample_R1_CH4)) +
    geom_point(aes(x=(Sample_R1_base_plateau_interval_initial_row+(((Sample_R1_base_plateau_interval_initial_row+interval_seconds)-Sample_R1_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R1_CH4), colour="red") +
    # MB2
    # Peak
    geom_segment(aes(x=Sample_R2_peak_plateau_interval_initial_row,
                     xend=Sample_R2_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R2_CH4,
                     yend=X_infty_Sample_R2_CH4)) +
    geom_point(aes(x=(Sample_R2_peak_plateau_interval_initial_row+Sample_R2_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R2_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R2_base_plateau_interval_initial_row,
                     xend=Sample_R2_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R2_CH4,
                     yend=X_0_Sample_R2_CH4)) +
    geom_point(aes(x=(Sample_R2_base_plateau_interval_initial_row+(((Sample_R2_base_plateau_interval_initial_row+interval_seconds)-Sample_R2_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R2_CH4), colour="red") +
    # MB3
    # Peak
    geom_segment(aes(x=Sample_R3_peak_plateau_interval_initial_row,
                     xend=Sample_R3_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R3_CH4,
                     yend=X_infty_Sample_R3_CH4)) +
    geom_point(aes(x=(Sample_R3_peak_plateau_interval_initial_row+Sample_R3_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R3_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R3_base_plateau_interval_initial_row,
                     xend=Sample_R3_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R3_CH4,
                     yend=X_0_Sample_R3_CH4)) +
    geom_point(aes(x=(Sample_R3_base_plateau_interval_initial_row+(((Sample_R3_base_plateau_interval_initial_row+interval_seconds)-Sample_R3_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R3_CH4), colour="red") +
    # MB4
    # Peak
    geom_segment(aes(x=Sample_R4_peak_plateau_interval_initial_row,
                     xend=Sample_R4_peak_plateau_interval_final_row,
                     y=X_infty_Sample_R4_CH4,
                     yend=X_infty_Sample_R4_CH4)) +
    geom_point(aes(x=(Sample_R4_peak_plateau_interval_initial_row+Sample_R4_peak_plateau_interval_final_row)/2,
                   y=X_infty_Sample_R4_CH4), colour="blue") +
    # Base
    geom_segment(aes(x=Sample_R4_base_plateau_interval_initial_row,
                     xend=Sample_R4_base_plateau_interval_initial_row+interval_seconds,
                     y=X_0_Sample_R4_CH4,
                     yend=X_0_Sample_R4_CH4)) +
    geom_point(aes(x=(Sample_R4_base_plateau_interval_initial_row+(((Sample_R4_base_plateau_interval_initial_row+interval_seconds)-Sample_R4_base_plateau_interval_initial_row)/2)),
                   y=X_0_Sample_R4_CH4), colour="red")

  # Third Plots: Revise (zoom in) plateaus --------
  ## Controls -------------------
  S_R1_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak + xlim(S_R1_base_plateau_interval_initial_row*0.95, S_R1_peak_plateau_interval_final_row*1.05)
  S_R1_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak + xlim(S_R1_base_plateau_interval_initial_row*0.95, S_R1_peak_plateau_interval_final_row*1.05)

  S_R2_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak + xlim(S_R2_base_plateau_interval_initial_row*0.95, S_R2_peak_plateau_interval_final_row*1.05)
  S_R2_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak + xlim(S_R2_base_plateau_interval_initial_row*0.95, S_R2_peak_plateau_interval_final_row*1.05)

  S_R3_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak + xlim(S_R3_base_plateau_interval_initial_row*0.95, S_R3_peak_plateau_interval_final_row*1.05)
  S_R3_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak + xlim(S_R3_base_plateau_interval_initial_row*0.95, S_R3_peak_plateau_interval_final_row*1.05)

  ## Treatments --------
  Sample_R1_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak +
    xlim(Sample_R1_base_plateau_interval_initial_row*0.95, Sample_R1_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CO2$value[Sample_R1_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CO2$value[Sample_R1_peak_plateau_interval_initial_row]*1.05)

  Sample_R1_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak +
    xlim(Sample_R1_base_plateau_interval_initial_row*0.95, Sample_R1_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CH4$value[Sample_R1_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CH4$value[Sample_R1_peak_plateau_interval_initial_row]*1.1)

  Sample_R2_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak +
    xlim(Sample_R2_base_plateau_interval_initial_row*0.95, Sample_R2_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CO2$value[Sample_R2_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CO2$value[Sample_R2_peak_plateau_interval_initial_row]*1.05)

  Sample_R2_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak +
    xlim(Sample_R2_base_plateau_interval_initial_row*0.95, Sample_R2_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CH4$value[Sample_R2_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CH4$value[Sample_R2_peak_plateau_interval_initial_row]*1.1)

  Sample_R3_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak +
    xlim(Sample_R3_base_plateau_interval_initial_row*0.95, Sample_R3_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CO2$value[Sample_R3_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CO2$value[Sample_R3_peak_plateau_interval_initial_row]*1.05)

  Sample_R3_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak +
    xlim(Sample_R3_base_plateau_interval_initial_row*0.95, Sample_R3_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CH4$value[Sample_R3_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CH4$value[Sample_R3_peak_plateau_interval_initial_row]*1.1)

  Sample_R4_plot_zoomin_CO2 <- plot_CO2_LGGA_log_plot_base_peak +
    xlim(Sample_R4_base_plateau_interval_initial_row*0.95, Sample_R4_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CO2$value[Sample_R4_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CO2$value[Sample_R4_peak_plateau_interval_initial_row]*1.05)

  Sample_R4_plot_zoomin_CH4 <- plot_CH4_LGGA_log_plot_base_peak +
    xlim(Sample_R4_base_plateau_interval_initial_row*0.95, Sample_R4_peak_plateau_interval_final_row*1.05) +
    ylim(LGGA_log_plot_CH4$value[Sample_R4_base_plateau_interval_initial_row]*0.95, LGGA_log_plot_CH4$value[Sample_R4_peak_plateau_interval_initial_row]*1.1)

  # Calculations of Concentration -----------
  ## S_R1 -----------------------
  DeltaX_S_R1_CO2 <- X_infty_S_R1_CO2 - X_0_S_R1_CO2
  DeltaX_S_R1_CH4 <- X_infty_S_R1_CH4 - X_0_S_R1_CH4

  c_S_R1_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_S_R1_CO2 + X_0_S_R1_CO2
  c_S_R1_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_S_R1_CH4 + X_0_S_R1_CH4

  ## S_R2 -----------------------
  DeltaX_S_R2_CO2 <- X_infty_S_R2_CO2 - X_0_S_R2_CO2
  DeltaX_S_R2_CH4 <- X_infty_S_R2_CH4 - X_0_S_R2_CH4

  c_S_R2_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_S_R2_CO2 + X_0_S_R2_CO2
  c_S_R2_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_S_R2_CH4 + X_0_S_R2_CH4

  ## S_R3 -----------------------
  DeltaX_S_R3_CO2 <- X_infty_S_R3_CO2 - X_0_S_R3_CO2
  DeltaX_S_R3_CH4 <- X_infty_S_R3_CH4 - X_0_S_R3_CH4

  c_S_R3_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_S_R3_CO2 + X_0_S_R3_CO2
  c_S_R3_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_S_R3_CH4 + X_0_S_R3_CH4

  ## Sample_R1 -----------------------
  DeltaX_Sample_R1_CO2 <- X_infty_Sample_R1_CO2 - X_0_Sample_R1_CO2
  DeltaX_Sample_R1_CH4 <- X_infty_Sample_R1_CH4 - X_0_Sample_R1_CH4

  c_Sample_R1_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R1_CO2 + X_0_Sample_R1_CO2
  c_Sample_R1_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R1_CH4 + X_0_Sample_R1_CH4

  ## Sample_R2 -----------------------
  DeltaX_Sample_R2_CO2 <- X_infty_Sample_R2_CO2 - X_0_Sample_R2_CO2
  DeltaX_Sample_R2_CH4 <- X_infty_Sample_R2_CH4 - X_0_Sample_R2_CH4

  c_Sample_R2_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R2_CO2 + X_0_Sample_R2_CO2
  c_Sample_R2_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R2_CH4 + X_0_Sample_R2_CH4

  ## Sample_R3 -----------------------
  DeltaX_Sample_R3_CO2 <- X_infty_Sample_R3_CO2 - X_0_Sample_R3_CO2
  DeltaX_Sample_R3_CH4 <- X_infty_Sample_R3_CH4 - X_0_Sample_R3_CH4

  c_Sample_R3_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R3_CO2 + X_0_Sample_R3_CO2
  c_Sample_R3_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R3_CH4 + X_0_Sample_R3_CH4

  ## Sample_R4 -----------------------
  DeltaX_Sample_R4_CO2 <- X_infty_Sample_R4_CO2 - X_0_Sample_R4_CO2
  DeltaX_Sample_R4_CH4 <- X_infty_Sample_R4_CH4 - X_0_Sample_R4_CH4

  c_Sample_R4_CO2 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R4_CO2 + X_0_Sample_R4_CO2
  c_Sample_R4_CH4 <- ((V_loop + V_sample)/V_sample) * DeltaX_Sample_R4_CH4 + X_0_Sample_R4_CH4

  # Gather the base and peak concentrations in individual data frames ---------------------
  ## Gather all X_0 of CO2 in one data frame --------------------------
  X_0_CO2 <- rbind(
    X_0_S_R1_CO2,
    X_0_S_R2_CO2,
    X_0_S_R3_CO2,
    X_0_Sample_R1_CO2,
    X_0_Sample_R2_CO2,
    X_0_Sample_R3_CO2,
    X_0_Sample_R4_CO2)

  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), X_0_CO2)

  ## Gather all X_infty of CO2 in one data frame --------------------------
  X_infty_CO2 <- rbind(
    X_infty_S_R1_CO2,
    X_infty_S_R2_CO2,
    X_infty_S_R3_CO2,
    X_infty_Sample_R1_CO2,
    X_infty_Sample_R2_CO2,
    X_infty_Sample_R3_CO2,
    X_infty_Sample_R4_CO2)

  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), X_infty_CO2)

  ## Gather all X_0 of CH4 in one data frame --------------------------
  X_0_CH4 <- rbind(
    X_0_S_R1_CH4,
    X_0_S_R2_CH4,
    X_0_S_R3_CH4,
    X_0_Sample_R1_CH4,
    X_0_Sample_R2_CH4,
    X_0_Sample_R3_CH4,
    X_0_Sample_R4_CH4)

  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), X_0_CH4)

  ## Gather all X_infty of CH4 in one data frame --------------------------
  X_infty_CH4 <- rbind(
    X_infty_S_R1_CH4,
    X_infty_S_R2_CH4,
    X_infty_S_R3_CH4,
    X_infty_Sample_R1_CH4,
    X_infty_Sample_R2_CH4,
    X_infty_Sample_R3_CH4,
    X_infty_Sample_R4_CH4)

  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), X_infty_CH4)

  ## Gather all deltas of CO2 in one data frame --------------------------
  Deltas_CO2 <- rbind(
    DeltaX_S_R1_CO2,
    DeltaX_S_R2_CO2,
    DeltaX_S_R3_CO2,
    DeltaX_Sample_R1_CO2,
    DeltaX_Sample_R2_CO2,
    DeltaX_Sample_R3_CO2,
    DeltaX_Sample_R4_CO2)

  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), Deltas_CO2)

  ## Gather all deltas of CH4 in one data frame --------------------------
  Deltas_CH4 <- rbind(
    DeltaX_S_R1_CH4,
    DeltaX_S_R2_CH4,
    DeltaX_S_R3_CH4,
    DeltaX_Sample_R1_CH4,
    DeltaX_Sample_R2_CH4,
    DeltaX_Sample_R3_CH4,
    DeltaX_Sample_R4_CH4)

  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), Deltas_CH4)

  # Calculate concentrations and deltas --------------------------
  ## Concentrations -------------
  Concentration_CO2_vector <- c(c_S_R1_CO2, c_S_R2_CO2, c_S_R3_CO2,
                                c_Sample_R1_CO2, c_Sample_R2_CO2, c_Sample_R3_CO2, c_Sample_R4_CO2)

  Concentration_CH4_vector <- c(c_S_R1_CH4, c_S_R2_CH4, c_S_R3_CH4,
                                c_Sample_R1_CH4, c_Sample_R2_CH4, c_Sample_R3_CH4, c_Sample_R4_CH4)

  Names <- c("Standard_R1", "Standard_R2", "Standard_R3",
             "Sample_R1", "Sample_R2", "Sample_R3", "Sample_R4")

  Concentration_df <- data.frame(Names, Concentration_CO2_vector, Concentration_CH4_vector)

  colnames(Concentration_df) <- c("Sample","Concentration CO2 [ppm]", "Concentration CH4 [ppm]")

  assign(paste("Concentrations", gsub("/", "_", date), sep = "_", collapse = NULL), Concentration_df)
  Concentration_df

  ## Averages of replicates --------
  CarbonDioxide <- c(mean(Concentration_df[1:3,2]), mean(Concentration_df[4:7,2]))
  Methane <- c(mean(Concentration_df[1:3,3]), mean(Concentration_df[4:7,3]))

  Names2 <- c("Standards","Sample")
  Concentration_means <- data.frame(Names2, CarbonDioxide, Methane)
  colnames(Concentration_means) <- c("Sample", "Concentration CO2 [ppm]", "Concentration CH4 [ppm]")
  assign(paste("Concentrations_means", gsub("/", "_", date), sep = "_", collapse = NULL), Concentration_df)
  Concentration_means

  # Export Plots ------------------------------------------------------------
  # Create 'Figures' folder
  setwd(mainDir)
  dir.create(file.path(mainDir, "Figures"))

  # Enter Figures folder
  setwd(paste(mainDir, "/Figures" , sep = ""))

  date2 <- dmy(date)
  date2 <- format(date2, format = "%B_%d_%Y")

  # Create 'date' folder
  dir.create(paste(date2))

  # Enter date folder
  setwd(paste(mainDir, "/Figures/", date2, sep = ""))

  subfolder_names <- c("CO2","CH4")
  for (j in 1:length(subfolder_names)){
    folder<-dir.create(subfolder_names[j])
  }

  # Enter CO2 folder and create folders
  setwd(paste(mainDir, "/Figures/", date2, "/CO2", sep = ""))

  subfolder_names <- c("All", "All_base_peaks", "All_WithoutControls", "ZoomInToPlateaus")
  for (j in 1:length(subfolder_names)){
    folder<-dir.create(subfolder_names[j])
  }

  # Enter CH4 folder and create folders
  setwd(paste(mainDir, "/Figures/", date2, "/CH4", sep = ""))

  subfolder_names <- c("All", "All_base_peaks", "All_WithoutControls", "ZoomInToPlateaus")
  for (j in 1:length(subfolder_names)){
    folder<-dir.create(subfolder_names[j])
  }

  # Enter CO2 folder and create folders
  setwd(paste(mainDir, "/Figures/", date2, "/CO2/ZoomInToPlateaus", sep = ""))

  # subfolder_names <- c("S_R3", "Sample_R3")
  # for (j in 1:length(subfolder_names)){
  #   folder<-dir.create(subfolder_names[j])
  # }

  # Enter CH4 folder and create folders
  setwd(paste(mainDir, "/Figures/", date2, "/CH4/ZoomInToPlateaus", sep = ""))

  # All (without markings) ------------------------
  # Enter CH4/All folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/All/", sep = "") , sep = ""))

  ggsave(plot = plot_CH4_LGGA_log_plot,
         filename = paste(paste("CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Enter CO2/All folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/All/", sep = "") , sep = ""))

  ggsave(plot = plot_CO2_LGGA_log_plot,
         filename = paste(paste("CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # All + Base and Peaks ------------------------
  # Enter CH4/All_base_peaks folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/All_base_peaks", sep = "") , sep = ""))

  ggsave(plot = plot_CH4_LGGA_log_plot_base_peak,
         filename = paste(paste("CH4_base_peak", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Enter CO2/All folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/All_base_peaks", sep = "") , sep = ""))

  ggsave(plot = plot_CO2_LGGA_log_plot_base_peak,
         filename = paste(paste("CO2_base_peak", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # All Without Controls ------------
  # Enter CH4/All_WithoutControls folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/All_WithoutControls/", sep = "") , sep = ""))

  ggsave(plot = plot_CH4_LGGA_log_plot_wc,
         filename = paste(paste("CH4_WithoutControls", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Enter CO2/All_WithoutControls folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/All_WithoutControls/", sep = "") , sep = ""))

  ggsave(plot = plot_CO2_LGGA_log_plot_wc,
         filename = paste(paste("CO2_WithoutControls", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Zoom In --------------------
  # S_R1 -----------------------
  # Enter CH4/ZoomInToPlateaus folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = S_R1_plot_zoomin_CH4,
         filename = paste(paste("Standard_R1_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = S_R1_plot_zoomin_CO2,
         filename = paste(paste("Standard_R1_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # S_R2 -----------------------
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = S_R2_plot_zoomin_CH4,
         filename = paste(paste("Standard_R2_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = S_R2_plot_zoomin_CO2,
         filename = paste(paste("Standard_R2_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # S_R3 -----------------------
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = S_R3_plot_zoomin_CH4,
         filename = paste(paste("Standard_R3_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = S_R3_plot_zoomin_CO2,
         filename = paste(paste("Standard_R3_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)


  # Sample R1 ---------------
  # Enter CH4/ZoomInToPlateaus folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R1_plot_zoomin_CH4,
         filename = paste(paste("Sample_R1_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R1_plot_zoomin_CO2,
         filename = paste(paste("Sample_R1_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Sample R2 ---------------
  # Enter CH4/ZoomInToPlateaus folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R2_plot_zoomin_CH4,
         filename = paste(paste("Sample_R2_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R2_plot_zoomin_CO2,
         filename = paste(paste("Sample_R2_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Sample R3 ---------------
  # Enter CH4/ZoomInToPlateaus folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R3_plot_zoomin_CH4,
         filename = paste(paste("Sample_R3_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R3_plot_zoomin_CO2,
         filename = paste(paste("Sample_R3_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Sample R4 ---------------
  # Enter CH4/ZoomInToPlateaus folder and print plot
  setwd(paste(mainDir, paste("/Figures/", date2, "/CH4/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R4_plot_zoomin_CH4,
         filename = paste(paste("Sample_R4_ZoomIn_CH4", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  setwd(paste(mainDir, paste("/Figures/", date2, "/CO2/ZoomInToPlateaus/", sep = "") , sep = ""))

  ggsave(plot = Sample_R4_plot_zoomin_CO2,
         filename = paste(paste("Sample_R4_ZoomIn_CO2", gsub("/", "_", date), sep = "_"), ".png", sep = ""),
         dpi = 300,
         width = 11,
         height = 8)

  # Return ---------------------
  # summary <- list(Concentration_means, Concentration_df, Deltas_CO2, Deltas_CH4, X_infty_CO2, X_0_CO2, X_infty_CH4, X_0_CH4)
  return(list(Concentration_means, Concentration_df, Deltas_CO2, Deltas_CH4, X_infty_CO2, X_0_CO2, X_infty_CH4, X_0_CH4))
}

# Define implementation of 'UKL_LGGA' for 1 day --------
  ## Preamble -------------------
  FunctionDir <- mainDir
  ScriptsDir <- FunctionDir

  # Assign different worksheets to each day
  day1_excel <- read_xlsx(LGGA_Excel, range="F14:F22", col_names = FALSE, sheet = "Day1")

  # Extract dates from different worksheets to each day
  date_day1 <- read_xlsx(LGGA_Excel, range="D8", col_names = FALSE, sheet = "Day1")
  date_day1 <- format(date_day1[[1]][1], format='%d/%m/%y')

  ## Run function for each day --------
    ### Day 1 ------------
    Ctrl_R1_Day1 <- format(day1_excel[[1]][1], format='%H:%M:%S')
    Ctrl_R2_Day1 <- format(day1_excel[[1]][2], format='%H:%M:%S')
    Ctrl_R3_Day1 <- format(day1_excel[[1]][3], format='%H:%M:%S')

    SampleProbe_R1_Day1 <- format(day1_excel[[1]][4], format='%H:%M:%S')
    SampleProbe_R2_Day1 <- format(day1_excel[[1]][5], format='%H:%M:%S')
    SampleProbe_R3_Day1 <- format(day1_excel[[1]][6], format='%H:%M:%S')
    SampleProbe_R4_Day1 <- format(day1_excel[[1]][7], format='%H:%M:%S')

    date <- date_day1
    identifier <- "Day 1"

    # Rename LGGA_log files and/or assign  here the full path, do not modify those raw data files
    LGGA_log <- read.table(tk_choose.files(caption = "Choose the LGGA log file (.txt)."),
                           header = FALSE, sep = ",", dec = ".", skip=1, fill = TRUE)

    InjectionTimes <- c(Ctrl_R1_Day1, Ctrl_R2_Day1, Ctrl_R3_Day1,
                        SampleProbe_R1_Day1, SampleProbe_R2_Day1, SampleProbe_R3_Day1, SampleProbe_R4_Day1)

    Summary_Day1 <- UKL_LGGA()

    ConcentrationMeans_Day1 <- Summary_Day1[[1]]
    ConcentrationAll_Day1 <- Summary_Day1[[2]]
    Deltas_CO2_Day1 <- Summary_Day1[[3]]
    Deltas_CH4_Day1 <- Summary_Day1[[4]]
    X_infty_CO2_Day1 <- Summary_Day1[[5]]
    X_0_CO2_Day1 <- Summary_Day1[[6]]
    X_infty_CH4_Day1 <- Summary_Day1[[7]]
    X_0_CH4_Day1 <- Summary_Day1[[8]]

    SD_CO2_Day1 <- c(sd(ConcentrationAll_Day1[1:3,2]), sd(ConcentrationAll_Day1[4:7,2]))
    SD_CH4_Day1 <- c(sd(ConcentrationAll_Day1[1:3,3]), sd(ConcentrationAll_Day1[4:7,3]))

    ConcentrationMeans_SD_Day1 <- ConcentrationMeans_Day1
    ConcentrationMeans_SD_Day1$SD_CO2 <- SD_CO2_Day1
    ConcentrationMeans_SD_Day1$SD_CH4 <- SD_CH4_Day1

    col_order <- c("Sample", "Concentration CO2 [ppm]", "SD_CO2", "Concentration CH4 [ppm]", "SD_CH4")
    ConcentrationMeans_SD_Day1 <- ConcentrationMeans_SD_Day1[, col_order]

  # Gather results -------------
  AllCalculatedConcentrations <- rbind(ConcentrationMeans_Day1)

  AllCalculatedConcentrations <- as.data.frame(AllCalculatedConcentrations)

  # Add label with date
  AllCalculatedConcentrations$Date <- c(rep(date_day1, each = 1)) # was 3

  # Subset by removing Standards
  AllCalculatedConcentrations <- AllCalculatedConcentrations[-c(which(AllCalculatedConcentrations$Sample == "Standards")),]

  # Reset row index
  row.names(AllCalculatedConcentrations) <- NULL

  # Subset by Treatment
  CalculatedConcentrations_Sample <- dplyr::filter(AllCalculatedConcentrations, Sample %in% c("Sample"))
  CalculatedConcentrations_NonSample <- dplyr::filter(AllCalculatedConcentrations, Sample %in% c("NonSample"))

    ### All Mean Values with SD --------
    AllCalculatedConcentrations_Mean_SD <- rbind(
      ConcentrationMeans_SD_Day1)

    # Add date
    AllCalculatedConcentrations_Mean_SD$Date <- c(rep(date_day1, each = 1))

    ### All Individual Values --------
    AllCalculatedConcentrations_Ind <- rbind(
      ConcentrationAll_Day1)

    # Add label with date
    AllCalculatedConcentrations_Ind$Date <- c(rep(date_day1, each = 7))

    ### Gather all deltas_CO2 in one data frame --------------------------
    AllCalculatedConcentrations_Ind_Deltas_CO2<- rbind(
      ConcentrationAll_Day1$Deltas_CO2_Day1 <- Deltas_CO2_Day1)

    AllCalculatedConcentrations_Ind$Delta_CO2 <- AllCalculatedConcentrations_Ind_Deltas_CO2

    AllCalculatedConcentrations_Ind_Deltas <- AllCalculatedConcentrations_Ind

    ### Gather all deltas_CH4 in one data frame --------------------------
    AllCalculatedConcentrations_Ind_Deltas_CH4<- rbind(
      ConcentrationAll_Day1$Deltas_CH4_Day1 <- Deltas_CH4_Day1)

    AllCalculatedConcentrations_Ind$Delta_CH4 <- AllCalculatedConcentrations_Ind_Deltas_CH4

    AllCalculatedConcentrations_Ind_Deltas <- AllCalculatedConcentrations_Ind

    ### Gather all X_0_CO2 in one data frame --------------------------
    AllCalculatedConcentrations_Ind_X_0_CO2<- rbind(
      ConcentrationAll_Day1$X_0_Day1 <- X_0_CO2_Day1)

    AllCalculatedConcentrations_Ind_Deltas$X_0_CO2 <- AllCalculatedConcentrations_Ind_X_0_CO2

    ### Gather all X_infty_CO2 in one data frame --------------------------
    AllCalculatedConcentrations_Ind_X_infty_CO2<- rbind(
      ConcentrationAll_Day1$X_infty_Day1 <- X_infty_CO2_Day1)

    AllCalculatedConcentrations_Ind_Deltas$X_infty_CO2 <- AllCalculatedConcentrations_Ind_X_infty_CO2

    AllCalculatedConcentrations_Ind_AllVariables <- AllCalculatedConcentrations_Ind_Deltas

    ### Gather all X_0_CH4 in one data frame --------------------------
    AllCalculatedConcentrations_Ind_X_0_CH4<- rbind(
      ConcentrationAll_Day1$X_0_Day1 <- X_0_CH4_Day1)

    AllCalculatedConcentrations_Ind_AllVariables$X_0_CH4 <- AllCalculatedConcentrations_Ind_X_0_CH4

    ### Gather all X_infty_CH4 in one data frame --------------------------
    AllCalculatedConcentrations_Ind_X_infty_CH4<- rbind(
      ConcentrationAll_Day1$X_infty_Day1 <- X_infty_CH4_Day1)

    AllCalculatedConcentrations_Ind_AllVariables$X_infty_CH4 <- AllCalculatedConcentrations_Ind_X_infty_CH4

    col_order <- c("Sample", "Date", "Concentration CO2 [ppm]", "X_infty_CO2", "X_0_CO2", "Delta_CO2", "Concentration CH4 [ppm]","X_infty_CH4", "X_0_CH4", "Delta_CH4")
    AllCalculatedConcentrations_Ind_AllVariables <- AllCalculatedConcentrations_Ind_AllVariables[, col_order]

  ## Print CSV ------------------
  AllCalculatedConcentrations_Ind_AllVariables
  setwd(mainDir)
  write_excel_csv(AllCalculatedConcentrations_Ind_AllVariables, paste("CalculatedConcentrations", sep=".", "csv"), del = ";")
  return(AllCalculatedConcentrations_Ind_AllVariables)
}
