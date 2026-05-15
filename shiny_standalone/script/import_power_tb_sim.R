library(dplyr)
library(gt)

# Try to load the power table from RDS file
power_tb <- NULL
load_success <- FALSE

# Load from expected path
tryCatch({
  rds_path <- file.path(project_root, "outputs/ana_3/result_3.2_20260223_5k.rds")
  if (!file.exists(rds_path)) {
    warning(paste("File not found:", rds_path))
  } else {
    result <- readRDS(rds_path)
    if (!is.null(result$power_tb) && nrow(result$power_tb) > 0) {
      power_tb <- result$power_tb
      load_success <- TRUE
      cat("[INFO] Successfully loaded power_tb with", nrow(power_tb), "rows and", ncol(power_tb), "columns\n")
    }
  }
}, error = function(e) {
  warning(paste("[ERROR] Failed to load power_tb:", e$message))
})

# Create placeholder table if loading failed
if (!load_success || is.null(power_tb)) {
  warning("Using placeholder power table")
  power_tb <- data.frame(
    Design = c("Parallel Design (w/bl)", "Parallel RM Design (w/bl)", "Crossover Design", 
               "Ascending Design", "Sequential Design", "Parallel Design (w/o bl)", 
               "Parallel RM Design (w/o bl)", "Modified Ascending Design", "Modified Parallel Design"),
    pwr_5 = c(0.1144, 0.1280, 0.3168, 0.3872, 0.5232, 0.0718, 0.0692, 0.3834, 0.4808),
    pwr_6 = c(0.1418, 0.1574, 0.4246, 0.4936, 0.6418, 0.0780, 0.0794, 0.4892, 0.5916),
    pwr_7 = c(0.1772, 0.2032, 0.5440, 0.6062, 0.7406, 0.0874, 0.0882, 0.6012, 0.6842),
    pwr_8 = c(0.2146, 0.2442, 0.6582, 0.7082, 0.8194, 0.0972, 0.1016, 0.7030, 0.7714),
    pwr_9 = c(0.2586, 0.2940, 0.7458, 0.7914, 0.8818, 0.1086, 0.1154, 0.7858, 0.8320),
    pwr_10 = c(0.3070, 0.3576, 0.8274, 0.8582, 0.9256, 0.1234, 0.1296, 0.8528, 0.8748),
    pwr_11 = c(0.3676, 0.4216, 0.8916, 0.9054, 0.9596, 0.1438, 0.1470, 0.8976, 0.9136),
    pwr_12 = c(0.4210, 0.4844, 0.9372, 0.9460, 0.9784, 0.1590, 0.1604, 0.9376, 0.9350),
    pwr_13 = c(0.4796, 0.5478, 0.9662, 0.9720, 0.9870, 0.1756, 0.1886, 0.9634, 0.9476),
    pwr_14 = c(0.5420, 0.6128, 0.9808, 0.9878, 0.9934, 0.1948, 0.2114, 0.9804, 0.9590),
    pwr_15 = c(0.6014, 0.6690, 0.9928, 0.9936, 0.9964, 0.2190, 0.2268, 0.9872, 0.9668),
    pwr_16 = c(0.6474, 0.7308, 0.9952, 0.9984, 0.9990, 0.2364, 0.2472, 0.9926, 0.9762),
    pwr_17 = c(0.7010, 0.7850, 0.9990, 0.9996, 0.9994, 0.2558, 0.2686, 0.9938, 0.9794),
    pwr_18 = c(0.7480, 0.8230, 0.9998, 0.9996, 1.0000, 0.2784, 0.2944, 0.9944, 0.9820),
    pwr_19 = c(0.7900, 0.8540, 0.9998, 1.0000, 1.0000, 0.3062, 0.3208, 0.9956, 0.9834),
    pwr_20 = c(0.8252, 0.8882, 1.0000, 1.0000, 1.0000, 0.3328, 0.3488, 0.9974, 0.9862)
  )
}

# Format the power table
# 1. Rename columns: remove "pwr_" prefix
names(power_tb) <- gsub("pwr_", "", names(power_tb))

# 2. Replace design names with descriptive labels
power_tb$Design <- case_when(
  power_tb$Design == "xov"          ~ "Crossover Design",
  power_tb$Design == "par_bl"       ~ "Parallel Design (w/bl)",
  power_tb$Design == "par_rm_bl"    ~ "Parallel RM Design (w/bl)",
  power_tb$Design == "asc"          ~ "Ascending Design",
  power_tb$Design == "seq"          ~ "Sequential Design",
  power_tb$Design == "par"          ~ "Parallel Design (w/o bl)",
  power_tb$Design == "par_rm"       ~ "Parallel RM Design (w/o bl)",
  power_tb$Design == "modified_asc" ~ "Modified Ascending Design",
  power_tb$Design == "modified_par" ~ "Modified Parallel Design",
  TRUE ~ power_tb$Design
)

colnames(power_tb)[1]<- "Effect Size "

# Safely create tables with row bounds checking
safe_subset <- function(df, rows) {
  valid_rows <- rows[rows <= nrow(df) & rows > 0]
  if (length(valid_rows) == 0) {
    return(df[1, , drop = FALSE])
  }
  return(df[valid_rows, , drop = FALSE])
}

# Assign to global environment using <<-
p1_power_tb_sim <<- safe_subset(power_tb, c(1, 6)) %>% gt()
p2_power_tb_sim <<- safe_subset(power_tb, c(2, 7)) %>% gt()
p3_power_tb_sim <<- safe_subset(power_tb, 3) %>% gt()
p4_power_tb_sim <<- safe_subset(power_tb, 5) %>% gt()
p5_power_tb_sim <<- safe_subset(power_tb, 4) %>% gt()
