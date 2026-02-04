# basic_plots_from_weekly_counts.R
# Minimal script to make basic plots from:
# case_count_weekly_cp_clues_age.txt
#
# Expected columns:
# epi_week, age_range, state, municipality, CP, CLUES, n

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---------
# Input
# ---------
IN_FILE <- Sys.getenv("IN_FILE", "data/public_min/case_count_weekly_cp_clues_age.txt")
OUT_DIR <- Sys.getenv("OUT_DIR", "outputs/figures_basic")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# auto-detect delimiter (tab or space)
read_weekly_counts <- function(path) {
  # try tab first
  x <- tryCatch(
    readr::read_tsv(path, show_col_types = FALSE),
    error = function(e) NULL
  )
  if (!is.null(x) && ncol(x) >= 3) return(x)

  # fallback: whitespace-delimited
  readr::read_table(path, show_col_types = FALSE)
}

df <- read_weekly_counts(IN_FILE) %>%
  mutate(
    epi_week = as.character(epi_week),
    age_range = as.character(age_range),
    state = as.character(state),
    municipality = as.character(municipality),
    CP = str_pad(as.character(CP), 5, "left", "0"),
    CLUES = as.character(CLUES),
    n = as.numeric(n)
  )

stopifnot(all(c("epi_week","age_range","state","municipality","CP","CLUES","n") %in% names(df)))

theme_basic <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
}

# ---------------------------
# Plot 1: Total cases per week
# ---------------------------
p_week <- df %>%
  group_by(epi_week) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = epi_week, y = n, group = 1)) +
  geom_line() +
  labs(x = "Epi week", y = "Cases (weekly total)") +
  theme_basic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(file.path(OUT_DIR, "01_cases_total_by_week.png"), p_week, width = 9, height = 4.5, dpi = 300)

# --------------------------------------
# Plot 2: Total cases per week by age band
# --------------------------------------
p_week_age <- df %>%
  group_by(epi_week, age_range) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = epi_week, y = n, color = age_range, group = age_range)) +
  geom_line(linewidth = 0.7) +
  labs(x = "Epi week", y = "Cases", color = "Age range") +
  theme_basic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(file.path(OUT_DIR, "02_cases_by_week_and_age.png"), p_week_age, width = 9, height = 4.8, dpi = 300)

# -----------------------------------
# Plot 3: Top hospitals overall (CLUES)
# -----------------------------------
top_k <- as.integer(Sys.getenv("TOP_K", "20"))

p_top_hosp <- df %>%
  group_by(CLUES) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  slice_max(n, n = top_k) %>%
  mutate(CLUES = fct_reorder(CLUES, n)) %>%
  ggplot(aes(x = CLUES, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Hospital (CLUES)", y = "Cases (total)") +
  theme_basic()

ggsave(file.path(OUT_DIR, "03_top_hospitals_bar.png"), p_top_hosp, width = 7.5, height = 6.5, dpi = 300)

# ----------------------------------------------------
# Plot 4: Weekly dynamics for top hospitals (small mult)
# ----------------------------------------------------
top_ids <- df %>%
  group_by(CLUES) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  slice_max(n, n = min(12, top_k)) %>%
  pull(CLUES)

p_top_hosp_week <- df %>%
  filter(CLUES %in% top_ids) %>%
  group_by(epi_week, CLUES) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = epi_week, y = n, group = 1)) +
  geom_line() +
  facet_wrap(~ CLUES, scales = "free_y") +
  labs(x = "Epi week", y = "Cases") +
  theme_basic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(file.path(OUT_DIR, "04_top_hospitals_weekly_facets.png"), p_top_hosp_week, width = 10, height = 7.5, dpi = 300)

# ------------------------------------------
# Plot 5: Heatmap CP x week (aggregated CP)
# ------------------------------------------
# Warning: can be large; this uses top CPs only.
top_cp <- as.integer(Sys.getenv("TOP_CP", "60"))

p_heat <- df %>%
  group_by(CP) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  slice_max(n, n = top_cp) %>%
  pull(CP) %>%
  (\(keep_cp) df %>% filter(CP %in% keep_cp))() %>%
  group_by(epi_week, CP) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = epi_week, y = fct_rev(factor(CP)), fill = n)) +
  geom_tile() +
  labs(x = "Epi week", y = "CP", fill = "Cases") +
  theme_basic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(file.path(OUT_DIR, "05_heatmap_cp_by_week.png"), p_heat, width = 12, height = 8, dpi = 300)

message("Wrote basic plots to: ", OUT_DIR)
