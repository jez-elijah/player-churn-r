# =============================================================================
# Player Churn & Retention Analysis in R  |  Cookie Cats mobile game
# -----------------------------------------------------------------------------
# Business question: Moving the first progression gate from level 30 to level 40
# was expected to keep players engaged longer. Did it? Who churns, when, and can
# we flag at-risk players early?
#
# Run from the project root:   Rscript R/churn_analysis.R
# Outputs: results/*.csv, figures/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
  library(ggplot2); library(pROC)
})
set.seed(42)
dir.create("results", showWarnings = FALSE); dir.create("figures", showWarnings = FALSE)

# ---- 0. Load data -----------------------------------------------------------
# Source: "Mobile Games A/B Testing - Cookie Cats" (Kaggle; Tactile Entertainment).
# If data/cookie_cats.csv is missing, fall back to a public GitHub mirror.
data_path <- "data/cookie_cats.csv"
if (!file.exists(data_path)) {
  dir.create("data", showWarnings = FALSE)
  download.file(
    "https://raw.githubusercontent.com/nsy0079/cookie-cats-ab-test/main/data/cookie_cats.csv",
    data_path, quiet = TRUE)
}
raw <- read_csv(data_path, show_col_types = FALSE,
                col_types = cols(userid = col_double(), version = col_character(),
                                 sum_gamerounds = col_double(),
                                 retention_1 = col_logical(), retention_7 = col_logical()))

# ---- 1. Data validation -----------------------------------------------------
srm <- chisq.test(table(raw$version))     # sample-ratio-mismatch check (expect 50/50)
validation <- tibble(
  check = c("rows", "distinct userid", "duplicate userid", "missing values",
            "negative game rounds", "players with 0 rounds",
            "max game rounds (14 days)", "rounds 99.9th percentile",
            "group split gate_30", "group split gate_40", "SRM chi-square p-value"),
  value = c(nrow(raw), n_distinct(raw$userid), sum(duplicated(raw$userid)), sum(is.na(raw)),
            sum(raw$sum_gamerounds < 0), sum(raw$sum_gamerounds == 0),
            max(raw$sum_gamerounds), quantile(raw$sum_gamerounds, .999),
            sum(raw$version == "gate_30"), sum(raw$version == "gate_40"), srm$p.value))
write_csv(validation, "results/01_data_validation.csv")

# One player logged 49,854 rounds in 14 days (implausible / bot-like). It is kept for
# retention metrics (a binary outcome is unaffected) but excluded from rounds summaries.
df <- raw %>%
  mutate(churn_d1 = !retention_1,          # did NOT come back on day 1
         churn_d7 = !retention_7,          # did NOT come back on day 7 (churn proxy)
         version  = factor(version, levels = c("gate_30", "gate_40")),
         is_outlier = sum_gamerounds > quantile(sum_gamerounds, .9999))

# ---- 2. Retention funnel ----------------------------------------------------
funnel <- tibble(
  stage = c("Installed", "Returned day 1", "Returned day 7"),
  players = c(nrow(df), sum(df$retention_1), sum(df$retention_7)),
  pct_of_installs = players / nrow(df) * 100,
  pct_of_previous = c(NA, players[2] / players[1], players[3] / players[2]) * 100)
write_csv(funnel, "results/02_retention_funnel.csv")

# ---- 3. A/B test: gate_30 (control) vs gate_40 (treatment) ------------------
ab_test <- function(d, flag, label = flag) {
  t <- d %>% group_by(version) %>%
    summarise(n = n(), x = sum(.data[[flag]]), .groups = "drop")
  g30 <- t[t$version == "gate_30", ]; g40 <- t[t$version == "gate_40", ]
  pt <- prop.test(c(g40$x, g30$x), c(g40$n, g30$n))   # difference = gate_40 - gate_30
  tibble(metric = label, n_gate30 = g30$n, n_gate40 = g40$n,
         rate_gate30 = g30$x / g30$n * 100, rate_gate40 = g40$x / g40$n * 100,
         diff_pp = (g40$x / g40$n - g30$x / g30$n) * 100,
         ci_low_pp = pt$conf.int[1] * 100, ci_high_pp = pt$conf.int[2] * 100,
         p_value = pt$p.value)
}
ab <- bind_rows(ab_test(df, "retention_1", "Day-1 retention"),
                ab_test(df, "retention_7", "Day-7 retention")) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm"))
write_csv(ab, "results/03_ab_test.csv")

# Bootstrap probability that gate_30 has higher day-7 retention (binary outcome, so
# resampling within each arm is equivalent to drawing binomial counts).
B <- 20000
d7 <- df %>% group_by(version) %>% summarise(n = n(), p = mean(retention_7), .groups = "drop")
boot_diff <- rbinom(B, d7$n[2], d7$p[2]) / d7$n[2] - rbinom(B, d7$n[1], d7$p[1]) / d7$n[1]
boot <- tibble(boot_ci_low_pp = quantile(boot_diff, .025) * 100,
               boot_ci_high_pp = quantile(boot_diff, .975) * 100,
               prob_gate30_better = mean(boot_diff < 0))
write_csv(boot, "results/03b_bootstrap_day7.csv")

# Power: smallest day-7 effect this experiment could reliably (80%) detect
mde <- power.prop.test(n = min(d7$n), p1 = d7$p[1], power = 0.8, sig.level = 0.05)
power_tbl <- tibble(group_size = min(d7$n), baseline_d7 = d7$p[1] * 100,
                    detectable_rate_at_80pct_power = mde$p2 * 100,
                    minimum_detectable_effect_pp = abs(mde$p2 - d7$p[1]) * 100)
write_csv(power_tbl, "results/03c_power.csv")

# ---- 4. Segmentation by early engagement ------------------------------------
# Players are grouped by rounds played in the first 14 days. NOTE: rounds are measured
# AFTER randomisation, so segment-level treatment effects are exploratory (the gate
# itself can change how many rounds a player logs).
seg_levels <- c("0 rounds", "1-9", "10-49", "50-199", "200+")
df <- df %>% mutate(segment = cut(sum_gamerounds, c(-Inf, 0, 9, 49, 199, Inf),
                                  labels = seg_levels, ordered_result = TRUE))

seg_profile <- df %>% group_by(segment) %>%
  summarise(players = n(), pct_of_players = n() / nrow(df) * 100,
            day1_retention_pct = mean(retention_1) * 100,
            day7_retention_pct = mean(retention_7) * 100,
            median_rounds = median(sum_gamerounds), .groups = "drop")
write_csv(seg_profile, "results/04_segment_profile.csv")

seg_ab <- bind_rows(lapply(seg_levels, function(s)
  ab_test(filter(df, segment == s), "retention_7", s))) %>%
  rename(segment = metric) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm"))
write_csv(seg_ab, "results/04b_segment_ab_day7.csv")

# Lifecycle stage from retention flags (a simple customer-value-cycle view)
lifecycle <- df %>%
  mutate(stage = case_when(retention_1 & retention_7 ~ "Retained (day 1 + day 7)",
                           retention_1 & !retention_7 ~ "Day-1 only (lapsed by day 7)",
                           !retention_1 & retention_7 ~ "Day-7 only (returned late)",
                           TRUE ~ "Never returned")) %>%
  group_by(stage) %>%
  summarise(players = n(), pct = n() / nrow(df) * 100,
            median_rounds = median(sum_gamerounds),
            mean_rounds_trimmed = mean(sum_gamerounds[!is_outlier]), .groups = "drop") %>%
  arrange(desc(players))
write_csv(lifecycle, "results/04c_lifecycle_stages.csv")

# ---- 5. Churn prediction ----------------------------------------------------
# Target: churn_d7 (did not return on day 7).
#  Model A - early-warning model using only information known by day 1
#            (version + day-1 return). No leakage.
#  Model B - adds rounds played. Rounds cover 14 days, i.e. they include days 8-14, so
#            they partly encode the outcome (target leakage). Shown to quantify how
#            much a naive model would over-state performance.
df <- df %>% mutate(log_rounds = log1p(sum_gamerounds),
                    y = as.integer(churn_d7),
                    d1_return = as.integer(retention_1))

k <- 5; df$fold <- sample(rep(1:k, length.out = nrow(df)))
cv_auc <- function(formula) {
  oof <- numeric(nrow(df))
  for (f in 1:k) {
    m <- glm(formula, data = df[df$fold != f, ], family = binomial())
    oof[df$fold == f] <- predict(m, df[df$fold == f, ], type = "response")
  }
  fold_auc <- sapply(1:k, function(f) as.numeric(auc(roc(df$y[df$fold == f], oof[df$fold == f], quiet = TRUE))))
  list(oof = oof, fold_auc = fold_auc, roc = roc(df$y, oof, quiet = TRUE))
}
A <- cv_auc(y ~ d1_return + version)
Bm <- cv_auc(y ~ log_rounds + d1_return + version)

models <- tibble(model = c("A: day-1 return + version (no leakage)",
                           "B: + 14-day rounds (leaky)"),
                 cv_auc_mean = c(mean(A$fold_auc), mean(Bm$fold_auc)),
                 cv_auc_sd = c(sd(A$fold_auc), sd(Bm$fold_auc)))
write_csv(models, "results/05_churn_models.csv")

final_A <- glm(y ~ d1_return + version, data = df, family = binomial())
or_A <- tibble(term = names(coef(final_A)), odds_ratio = exp(coef(final_A)),
               ci_low = exp(confint.default(final_A))[, 1],
               ci_high = exp(confint.default(final_A))[, 2])
write_csv(or_A, "results/05b_model_A_odds_ratios.csv")

# Practical early-warning rule: flag every player who did not return on day 1
rule <- df %>% group_by(day1_returned = retention_1) %>%
  summarise(players = n(), day7_churn_pct = mean(churn_d7) * 100, .groups = "drop")
write_csv(rule, "results/05c_day1_rule.csv")
flag <- df$churn_d1
rule_perf <- tibble(precision_pct = mean(df$churn_d7[flag]) * 100,
                    recall_pct = sum(df$churn_d7 & flag) / sum(df$churn_d7) * 100,
                    players_flagged_pct = mean(flag) * 100)
write_csv(rule_perf, "results/05d_day1_rule_performance.csv")

# ---- 6. Figures -------------------------------------------------------------
theme_set(theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold")))
col <- c(gate_30 = "#1f4e79", gate_40 = "#c55a11")

ci_df <- df %>% group_by(version) %>%
  summarise(d1 = mean(retention_1), d7 = mean(retention_7), n = n(), .groups = "drop") %>%
  pivot_longer(c(d1, d7), names_to = "metric", values_to = "rate") %>%
  mutate(se = sqrt(rate * (1 - rate) / n), metric = recode(metric, d1 = "Day 1", d7 = "Day 7"))
p1 <- ggplot(ci_df, aes(metric, rate * 100, fill = version)) +
  geom_col(position = position_dodge(.7), width = .6) +
  geom_errorbar(aes(ymin = (rate - 1.96 * se) * 100, ymax = (rate + 1.96 * se) * 100),
                position = position_dodge(.7), width = .15) +
  scale_fill_manual(values = col) +
  labs(title = "Retention by gate placement", subtitle = "Bars show 95% confidence intervals",
       x = NULL, y = "Players retained (%)", fill = NULL)
ggsave("figures/01_retention_by_version.png", p1, width = 7, height = 4.5, dpi = 150)

p2 <- ggplot(seg_ab %>% mutate(segment = factor(segment, levels = seg_levels)),
             aes(segment, diff_pp)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low_pp, ymax = ci_high_pp), colour = "#1f4e79") +
  labs(title = "Day-7 retention effect of moving the gate to level 40",
       subtitle = "By early-engagement segment (gate_40 minus gate_30, 95% CI; exploratory)",
       x = "Rounds played in first 14 days", y = "Difference (percentage points)")
ggsave("figures/02_segment_effect.png", p2, width = 7.5, height = 4.5, dpi = 150)

p3 <- ggplot(seg_profile, aes(segment, day7_retention_pct)) +
  geom_col(fill = "#1f4e79", width = .6) +
  geom_text(aes(label = sprintf("%.1f%%", day7_retention_pct)), vjust = -.4, size = 3.5) +
  labs(title = "Day-7 retention rises steeply with early engagement",
       x = "Rounds played in first 14 days", y = "Day-7 retention (%)") +
  ylim(0, max(seg_profile$day7_retention_pct) * 1.12)
ggsave("figures/03_retention_by_segment.png", p3, width = 7, height = 4.5, dpi = 150)

roc_df <- bind_rows(
  tibble(model = "A: day-1 return + version", fpr = 1 - A$roc$specificities, tpr = A$roc$sensitivities),
  tibble(model = "B: + 14-day rounds (leaky)", fpr = 1 - Bm$roc$specificities, tpr = Bm$roc$sensitivities))
p4 <- ggplot(roc_df, aes(fpr, tpr, colour = model)) + geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey50") + coord_equal() +
  labs(title = "Cross-validated ROC: predicting day-7 churn", x = "False positive rate",
       y = "True positive rate", colour = NULL) + theme(legend.position = "bottom")
ggsave("figures/04_roc_curves.png", p4, width = 6.5, height = 6, dpi = 150)

p5 <- df %>% filter(sum_gamerounds > 0) %>%
  ggplot(aes(sum_gamerounds)) + geom_histogram(bins = 60, fill = "#1f4e79") +
  scale_x_log10() + labs(title = "Game rounds in first 14 days (log scale)",
                         subtitle = "Strongly right-skewed: median is far below the mean",
                         x = "Rounds (log10)", y = "Players")
ggsave("figures/05_rounds_distribution.png", p5, width = 7, height = 4.5, dpi = 150)

# ---- 7. Console summary -----------------------------------------------------
options(width = 140, dplyr.width = Inf, tibble.width = Inf)
for (nm in c("01_data_validation", "02_retention_funnel", "03_ab_test", "03b_bootstrap_day7",
             "03c_power", "04_segment_profile", "04b_segment_ab_day7", "04c_lifecycle_stages",
             "05_churn_models", "05b_model_A_odds_ratios", "05c_day1_rule",
             "05d_day1_rule_performance")) {
  cat("\n##", nm, "\n"); print(read_csv(paste0("results/", nm, ".csv"), show_col_types = FALSE))
}
