#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Ann-Sophie Loock                                                                


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(lme4, stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq, psych, ggcorrplot, gt, 
               data.table)


# load data ----------------------------------------------------------------

load(file="./04_data_analysis/analysis.data.rda")
colnames(analysis.data) # Get all column names


# Calculate descriptive stats ---------------------------------------------
dt <- as.data.table(analysis.data) # convert to data table format

# Filter out numeric variables
num_cols <- names(dt)[vapply(dt, is.numeric, FUN.VALUE = logical(1))]
num_cols <- setdiff(num_cols, c("record_id")) # exclude record id column

# obtain data table with only numeric variables
dt_num <- dt[ , ..num_cols] 

# calculate descriptive stats
desc <- as.data.table(psych :: describe(dt_num), keep.rownames = "variable") 

# keep selected descriptive stats columns
keep <- c("variable", "n", "mean","sd","median", "min","max","range")  
desc <- desc[ , ..keep]

# round numeric values
col_to_round <- setdiff(names(desc), c("variable", "n"))
desc[ , (col_to_round) := lapply(.SD, round, 1), .SDcols = col_to_round]

# make a pretty table
desc %>%
  gt() %>%
  fmt_number(columns = -c(variable, n), decimals = 1) %>% 
  tab_header(title = "Descriptive Stats...", subtitle = paste0("N = ", nrow(dt_num), " rows; ", length(num_cols), " numeric variables")) %>%
  cols_label(n = "N", sd = "SD") %>%
  tab_options(table.font.size = px(12))


# Descriptive stats table -------------------------------------------------
# --- 1) choose variables ----
tab1_df_cont <- analysis.data %>%
  mutate(
    #msf_num = as.numeric(msf) / 3600,
    PDS_score = dplyr::coalesce(PDS_score_m, PDS_score_f)) %>%
  transmute(
    age = slypos_demographics_age,
    
    # LEBA predictors
    F2_leba, F3_leba, F4_leba, F5_leba,
    
    # Outcomes
    msf_num, msf_sc_num,
    Promis_sd_sum, Promis_sri_sum,
    
    # Additional measures
    le_week_num,
    ASE_score,
    Photophila_score, Photophobia_score,
    PDS_score
  )


# Labels 
var_labels <- c(
  age = "Age (years)",
  sex = "Sex",
  work = "Work or school status",
  school = "School factor",
  
  F2_leba = "LEBA F2, spending time outdoors (sum score)",
  F3_leba = "LEBA F3, device use in bed (sum score)",
  F4_leba = "LEBA F4, evening light control (sum score)",
  F5_leba = "LEBA F5, morning/daytime light (sum score)",
  
  msf_num = "MSF (numeric, hours)",
  msf_sc_num = "MSFsc (numeric, hours)",
  Promis_sd_sum = "PROMIS Sleep Disturbance 4a (sum score)",
  Promis_sri_sum = "PROMIS Sleep-Related Impairment 4a (sum score)",
  
  F1_leba = "LEBA F1, wearing blue light filters (sum score)",
  le_week_num = "Weekly outdoor light exposure (hours/week)",
  ASE_score = "Sleep environment (ASE score)",
  Photophila_score = "Photophilia score",
  Photophobia_score = "Photophobia score",
  PDS_score = "Pubertal development (PDS score)")

# --- 2) helper summaries ----
summ_cont <- function(df, var, group) {
  x <- df[[var]]
  tibble(
    group = group,
    variable = var_labels[[var]],
    level = "",
    N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

summ_cat <- function(df, var, group) {
  tmp <- df %>%
    filter(!is.na(.data[[var]])) %>%
    count(.data[[var]], name = "n") %>%
    mutate(p = 100 * n / sum(n))
  
  # header row for the variable
  header <- tibble(
    group = group,
    variable = var_labels[[var]],
    level = "",
    N = sum(tmp$n),
    Mean = NA_real_, SD = NA_real_, Median = NA_real_, Min = NA_real_, Max = NA_real_
  )
  
  # category rows
  rows <- tmp %>%
    transmute(
      group = group,
      variable = "",
      level = as.character(.data[[var]]),
      N = n,
      Mean = NA_real_, SD = NA_real_, Median = NA_real_, Min = NA_real_, Max = NA_real_
    )
  
  bind_rows(header, rows)
}

# --- 3) build Table 1 data ----
tab1_long <- bind_rows(
  summ_cont(tab1_df_cont, "age", "Demographics"),
  
  summ_cont(tab1_df_cont, "F2_leba", "LEBA factors"),
  summ_cont(tab1_df_cont, "F3_leba", "LEBA factors"),
  summ_cont(tab1_df_cont, "F4_leba", "LEBA factors"),
  summ_cont(tab1_df_cont, "F5_leba", "LEBA factors"),
  
  summ_cont(tab1_df_cont, "msf_num", "Sleep outcomes"),
  summ_cont(tab1_df_cont, "msf_sc_num", "Sleep outcomes"),
  summ_cont(tab1_df_cont, "Promis_sd_sum", "Sleep outcomes"),
  summ_cont(tab1_df_cont, "Promis_sri_sum", "Sleep outcomes"),
  
  summ_cont(tab1_df_cont, "le_week_num", "Additional measures"),
  summ_cont(tab1_df_cont, "ASE_score", "Additional measures"),
  summ_cont(tab1_df_cont, "Photophila_score", "Additional measures"),
  summ_cont(tab1_df_cont, "Photophobia_score", "Additional measures"),
  summ_cont(tab1_df_cont, "PDS_score", "Additional measures")
)

tab1_long <- tab1_long %>%
  mutate(across(any_of(c("Mean","SD","Median","Min","Max")), ~ round(.x, 2)))

library(flextable)
library(officer)

ft <- flextable(tab1_long) %>%
  set_header_labels(
    group = "",
    variable = "Characteristic",
    N = "N",
    Mean = "Mean",
    SD = "SD",
    Median = "Median",
    Min = "Min",
    Max = "Max"
  ) %>%
  theme_booktabs() %>%
  autofit() %>%
  align(j = 3:ncol(tab1_long), align = "center", part = "all") %>%
  align(j = 1:2, align = "left", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  colformat_num(j = c("Mean","SD","Median","Min","Max"), digits = 2, na_str = "")

ft <- colformat_num(
  ft,j = c("Mean", "SD", "Median", "Min", "Max"),
  digits = 1, na_str = "")

doc <- read_docx() %>%
  body_add_par("Table 1. Descriptive characteristics of the study sample.", style = "Normal") %>%
  body_add_flextable(ft)

# Save
print(doc, target = "Table1_descriptives.docx")

# Categorical variables
tab_cat <- analysis.data %>%
  transmute(
    Sex = slypos_demographics_sex.factor,
    `Work or school status` = slypos_demographics_work_or_school.factor,
    `School factor` = slypos_demographics_school.factor
  ) %>%
  pivot_longer(everything(), names_to = "Characteristic", values_to = "Level") %>%
  filter(!is.na(Level)) %>%
  count(Characteristic, Level, name = "n") %>%
  group_by(Characteristic) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

ft_cat <- flextable(tab_cat) %>%
  colformat_num(j = "pct", digits = 1) %>%
  set_header_labels(n = "n", pct = "%") %>%
  theme_booktabs() %>%
  autofit()

# Average Mid-sleep Time --------------------------------------------------

# Deal with MSF and MSFsc hms objects
dt[ , msf_num := as.numeric(msf)/3600] # convert MSF to numeric (already done for MSF sc)

# Calculate mean and sd on numeric level
# for MSF
mean_msf <- dt[ , mean(msf_num, na.rm = TRUE)]
sd_msf <- dt[ , sd(msf_num, na.rm = TRUE)]
hms :: hms(seconds = mean_msf * 3600)
hms :: hms(seconds = sd_msf * 3600)

# for MSFsc
mean_msfsc <- dt[ , mean(msf_sc_num, na.rm = TRUE)]
sd_msfsc <- dt[ , sd(msf_sc_num, na.rm = TRUE)]
hms :: hms(seconds = mean_msfsc * 3600)
hms :: hms(seconds = sd_msfsc * 3600)



# Plots -------------------------------------------------------------------
# Forest plot 

load(file="./04_data_analysis/results.rda")

# Mutate format
coef_df <- all_results %>% 
  mutate(
    outcome = dplyr :: recode(
      outcome,
      "msf_num"        = "MSF (mid-sleep on free days)",
      "msf_sc_num"     = "MSFsc (corrected for oversleep)",
      "Promis_sd_sum"  = "Sleep disturbances (PROMIS)",
      "Promis_sri_sum" = "Sleep-related impairment (PROMIS)"
    ),
    leba = dplyr :: recode(
      predictor, 
      "F2_leba" = "F2 – spending time outdoors",
      "F3_leba" = "F3 – device use in bed",
      "F4_leba" = "F4 – evening light control",
      "F5_leba" = "F5 – morning/daytime light"
    ),
    leba = factor(
      leba, 
      levels = rev(c(
        "F2 – spending time outdoors",
        "F3 – device use in bed",
        "F4 – evening light control",   
        "F5 – morning/daytime light"
      ))
    ) 
  )

# Create forest plots of all LEBA factors and sleep timing, sleep disturbances, and sleep-related impairment
ggplot(coef_df, 
       aes(x = beta_mean, 
           y = leba, 
           xmin = ci_lower, 
           xmax = ci_upper)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ outcome, scales = "free_x") +
  labs(
    x = "Posterior mean β (95% credible interval)",
    y = "LEBA factor",
    title = "Bayesian regression coefficients: LEBA factors predicting sleep outcomes"
  ) +
  theme_minimal() +
  theme( strip.text = element_text(face = "bold"),
         axis.title.y = element_blank())


# Forest plots for manuscript ---------------------------------------------
# libraries needed
library(grid)
library(ggpubr)

# Create nicer forest plots for inclusion in manuscript
# Select F2 and F3 (main results)
coef_df2 <- coef_df %>%
  filter(
    predictor %in% c("F2_leba", "F3_leba"),
    outcome %in% c("MSF (mid-sleep on free days)",
                   "Sleep disturbances (PROMIS)",
                   "Sleep-related impairment (PROMIS)")) %>%
  mutate(
    leba = factor(dplyr::recode(predictor, "F2_leba" = "F2", "F3_leba" = "F3"),
      levels = c("F3", "F2"))) # use short LEBA factor names


# Forest plot function
make_forest <- function(df, outcome_name, show_y = FALSE) {
  ggplot(df %>% dplyr::filter(outcome == outcome_name),
         aes(x = beta_mean, y = leba)) + 
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                   height = 0.10, linewidth = 0.4, colour = "grey20") +
    geom_point(shape = 23, size = 3.25, stroke = 0.4, #diamond point
               fill = "#9BAFB5", colour = "grey10") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
                       expand = expansion(mult = 0.03)) +
    scale_y_discrete(expand = expansion(add = 0.3),
                     labels = if (show_y) waiver() else function(x) rep("", length(x))) +
    coord_cartesian(xlim = c(-0.25, 0.35)) +
    #labs(title = outcome_name) +
    theme_classic(base_size = 12) +
    theme(
      text = element_text(family = "Helvetica"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 11, colour = "grey10"),
      axis.text.y = if (show_y) element_text(size = 11, colour = "grey10") else element_blank(),
      axis.ticks.y = if (show_y) element_line(colour = "grey10") else element_blank(),
      axis.line = element_line(linewidth = 0.5, colour = "grey10"),
      plot.margin = margin(6, 8, 6, 8)
    )
}

# Manage A, B, C tags
tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 14),
  plot.tag.position = c(0.1, 0.98))  # inside panel, top-left

# Tagged plots
p_msf <- make_forest(coef_df2, "MSF (mid-sleep on free days)", show_y = TRUE) + labs(tag = "A") + tag_theme
p_sd  <- make_forest(coef_df2, "Sleep disturbances (PROMIS)", show_y = FALSE) + labs(tag = "B") + tag_theme
p_sri <- make_forest(coef_df2, "Sleep-related impairment (PROMIS)", show_y = FALSE) + labs(tag = "C") + tag_theme

# Arrange three plots
fig_main <- ggarrange(p_msf, p_sd, p_sri, 
                      ncol = 3,
                      align = "hv", 
                      font.label = list(size = 14, face = "bold", family = "Helvetica"))
  
fig_main <- annotate_figure(fig_main,
  bottom = text_grob(expression("Posterior mean " * beta * " (95% credible interval)"),
                     size = 13, family = "Helvetica"))

ggsave("Fig_forest_F2F3.png", fig_main, width = 12, height = 3, units = "in", dpi = 300, bg = "white")
# Save as vector graphic
# ggsave("Fig1_forest_F2F3.svg", fig_main, width = 7, height = 4, units = "in", bg = "white")



# Scatter plots (main results) --------------------------------------------
# Function to create MSF associations
make_scatter_time <- function(df, x, y, x_lab, clock_breaks = 4) {
  df <- df %>% mutate(y_plot = .data[[y]] %% 24)
  
  ggplot(df, aes(x = .data[[x]], y = y_plot)) +
    geom_point(
      position = position_jitter(width = 0.20, height = 0),
      alpha = 0.15,      # visibility of points
      size = 1.6,
      colour = "grey20") +
    geom_smooth(
      method = "lm",
      se = TRUE,
      linewidth = 1.0,   # regression line thickness
      colour = "#9BAFB5",
      fill = "#9BAFB5",
      alpha = 0.25) +
    geom_hline(       # horizontal line for reference
      yintercept = mean(df$msf_num, na.rm = TRUE),
      linetype = "dashed",
      linewidth = 0.3,
      colour = "grey40") +
    scale_y_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, by = clock_breaks),
      labels = function(x) sprintf("%02d:%02d", floor(x), round((x %% 1) * 60))) +
    labs(
      x = x_lab,
      y = "Mid-sleep on free days \n[hh:mm]") +
    theme_classic(base_size = 13) +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(colour = "grey10"),
      axis.title = element_blank())}

# MSF scatter plots
# F2: Time spent outdoors
scatter_f2_msf <- make_scatter_time(MSF, "F2_leba", "msf_num",
                         "Time spent outdoors [sum score]",
                         clock_breaks = 6)

# F3: Device use in bed
scatter_f3_msf <- make_scatter_time(MSF, "F3_leba", "msf_num",
                               "Device use in bed [sum score]",
                               clock_breaks = 6)

# Function to create PROMIS sleep outcomes scatter plots
make_scatter_promis <- function(df, x, y, x_lab, y_lab) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(
      position = position_jitter(width = 0.15, height = 0.15),
      alpha = 0.18, size = 1.8, colour = "grey55") +
    geom_smooth(
      method = "lm", se = TRUE,
      linewidth = 1.0,
      colour = "#9BAFB5",
      fill = "#9BAFB5",
      alpha = 0.25) +
    geom_hline(
      yintercept = mean(df[[y]], na.rm = TRUE),
      linetype = "dashed",
      linewidth = 0.3,
      colour = "grey50") +
    labs(x = x_lab, y = y_lab) +
    theme_classic(base_size = 13) +
    theme(text = element_text(family = "Helvetica"),
          axis.text = element_text(colour = "grey10"),
          axis.title = element_blank())}

# Create some themes for arranging plots together
# manage tags
add_panel_tag <- function(p, tag) {
  p +  
    labs(tag = NULL) + # remove existing tags for plots that exist alreadys
    theme(plot.tag = element_blank()) +  
    annotate("text",
               x = -Inf, y = Inf, label = tag,
               hjust = -0.18, vjust = 1.15,
               size = 5, fontface = "bold", family = "Helvetica") +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(8, 8, 8, 8))}

# Scatter plots sleep disturbance
scatter_f2_sd  <- make_scatter_promis(PROMIS_clean, "F2_leba", "Promis_sd_sum",
                         "Time spent outdoors\n[sum score]", "Sleep disturbances\n[sum score]")

scatter_f3_sd  <- make_scatter_promis(PROMIS_clean, "F3_leba", "Promis_sd_sum",
                         "Device use in bed\n[sum score]", "Sleep disturbances\n[sum score]") 

# Scatter plots sleep-related impairment
scatter_f2_sri <- make_scatter_promis(PROMIS_clean, "F2_leba", "Promis_sri_sum",
                         "Time spent outdoors\n[sum score]", "Sleep-related impairment\n[sum score]") 

scatter_f3_sri <- make_scatter_promis(PROMIS_clean, "F3_leba", "Promis_sri_sum",
                         "Device use in bed\n[sum score]", "Sleep-related impairment\n[sum score]") 

## Arrange scatter plots in one figure
# Add tags
a <- add_panel_tag(scatter_f2_msf, "A")
b <- add_panel_tag(scatter_f2_sd, "B")
c <- add_panel_tag(scatter_f2_sri, "C")
d <- add_panel_tag(scatter_f3_msf, "D")
e <- add_panel_tag(scatter_f3_sd, "E")
f <- add_panel_tag(scatter_f3_sri, "F")

fig_scatter <- ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2, align ="hv") 

row_labels <- gridExtra::arrangeGrob(
  grid::textGrob("F2: Time spent outdoors", rot = 90,
                 x = unit(0.7, "npc"),  # push text right
                 gp = grid::gpar(fontsize = 12, fontfamily = "Helvetica")),
  grid::textGrob("F3: Device use in bed", rot = 90,
                 x = unit(0.7, "npc"),  # push text right
                 gp = grid::gpar(fontsize = 12, fontfamily = "Helvetica")),
  ncol = 1)

fig_scatter <- ggpubr::annotate_figure(fig_scatter, 
                                       left = row_labels)
fig_scatter

ggsave("Fig_scatter_all.png", fig_scatter, width = 12, height = 7, units = "in", dpi = 300, bg = "white")




# Correlation matrix ------------------------------------------------------
cor_mat_data <- cor_data %>% mutate(PDS = coalesce(PDS_score_m, PDS_score_f))
# trying to simply use one PDS score - as there is so very few anyways.


# --- order + nicer labels  ----
ord <- c("F1_leba","F2_leba","F3_leba","F4_leba","F5_leba",
         "msf_num","msf_sc_num","le_week_num",
         "Photophila_score","Photophobia_score","ASE_score",
         "Promis_sd_sum","Promis_sri_sum","PDS")

lab <- c(
  F1_leba="F1", F2_leba="F2", F3_leba="F3", F4_leba="F4", F5_leba="F5",
  msf_num="MSF", msf_sc_num="MSFsc",
  le_week_num="Weekly LE",
  Photophila_score="Photophil.", Photophobia_score="Photophob.",
  ASE_score="ASE",
  Promis_sd_sum="SD", Promis_sri_sum="SRI", PDS="PDS"
)

# --- build correlation matrix ----
cor <- corr.test(cor_mat_data[ , ord], use = "pairwise", method = "spearman", adjust= "fdr")

# --- reorder matrices ----
# Distinguish rows vs. columns
row_ord <- ord
col_ord <- ord

# Subset matrices 
R <- cor$r[row_ord, col_ord]
P <- cor$p[row_ord, col_ord]

# Readable labels
colnames(R) <- lab[col_ord]
rownames(R) <- lab[row_ord]
colnames(P) <- lab[col_ord]
rownames(P) <- lab[row_ord]

# --- Plot ----
# Palette using Chrono turquoise
chrono_turq <- "#9BAFB5"
chrono_warm <- "#C65B5B"

# Base plot
p <- ggcorrplot(
  R, type = "upper",
  p.mat = P, sig.level = 0.05, insig = "blank",
  lab = TRUE,
  lab_size = 3,
  colors = c(chrono_warm, "white", chrono_turq),
  outline.col = "grey85") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(margin = margin(r = 6))) 
  
p <- p + scale_y_discrete(limits = rev(rownames(R)))
p <- p + scale_fill_gradient2(
      name = "Spearman \u03c1",
      low = "#C65B5B",
      mid = "white",
      high = "#9BAFB5",
      midpoint = 0,
      limits = c(-1, 1))

ggsave("cor_matrix(2).png", p, width = 10, height = 6, units = "in", dpi = 300, bg = "white")


