library(dplyr)
library(readxl)
library(survival)
library(ggsurvfit)

# Read the Excel file
data <- read_excel("Survival_ISS.xlsx")

# Prepare survival objects
data <- data %>%
  mutate(
    os_surv = Surv(time = os_time / 365, event = death),
    pfs_surv = Surv(time = pfs_time / 365, event = progression)
  )

# Column names for KM stratification
strat_col <- c("ISS", "RISS", "R2ISS")

# Create a list to store plots
plots <- list()

# Create a list of plot names for the PNG files
plot_names <- c("ISS_OS", "ISS_PFS", "RISS_OS", "RISS_PFS",
                "R2ISS_OS", "R2ISS_PFS")

# Loop through each stratification column and generate plots
for (i in 1:3) {
  j <- strat_col[i]  # Get the stratification column name
  # For overall survival
  plots[[2 * i - 1]] <- survfit2(os_surv ~ data[[j]], data = data) %>% # nolint
    ggsurvfit() +
    add_confidence_interval() +
    add_risktable() +
    add_legend_title(strat_col[i]) +
    add_pvalue() +
    labs(
      x = "Time (Years)",
      y = "OS",
    )
  # For progression-free survival
  plots[[2 * i]] <- survfit2(pfs_surv ~ data[[j]], data = data) %>%
    ggsurvfit() +
    add_confidence_interval() +
    add_risktable() +
    add_legend_title(strat_col[i]) +
    add_pvalue() +
    labs(
      x = "Time (Years)",
      y = "PFS"
    )
}

# Save each plot into its own PNG file
for (i in 1:6) {
  png(paste0(plot_names[i], ".png"), width = 2200, height = 2400, res = 300)
  print(plots[[i]])  # Print the individual plot
  dev.off()
}