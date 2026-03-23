# =============================================================================
# Fantasy Herd Analysis
# Goal: Build the best team (max points, min variance) and predict resting risk
# =============================================================================

library(tidyverse)

# --- 1. LOAD & RESHAPE -------------------------------------------------------

raw <- read_csv("fantasy_herd_joined_data.csv", show_col_types = FALSE)

# Clean the name column (contains "CowName\nEarTag" from scraping)
raw <- raw %>%
  mutate(cow_name = str_split_i(name, "\n", 1),
         ear_tag  = str_split_i(name, "\n", 2))

# Pivot the wide round-level HTML columns into long format
# Columns like: html_ROUND 4_Milk efficiency -> round + metric + value
round_cols <- raw %>%
  select(starts_with("html_")) %>%
  names()

long <- raw %>%
  select(cow_name, ear_tag, all_of(round_cols)) %>%
  pivot_longer(
    cols      = all_of(round_cols),
    names_to  = "col_name",
    values_to = "value"
  ) %>%
  mutate(
    # Parse "html_ROUND 4_Milk efficiency" into round and metric
    col_stripped = str_remove(col_name, "^html_"),
    round_name   = str_extract(col_stripped, "^[^_]+(?:\\s[^_]+)?"),
    metric       = str_remove(col_stripped, "^[^_]+(?:\\s[^_]+)?_")
  ) %>%
  select(-col_name, -col_stripped) %>%
  mutate(value = as.numeric(value))

# Create a round ordering factor
round_order <- c("DRW", "ROUND 1", "ROUND 2", "ROUND 3", "ROUND 4")
long <- long %>%
  mutate(round_name = factor(round_name, levels = round_order, ordered = TRUE))

# Wide-by-metric: one row per cow per round, columns for each scoring metric
round_scores <- long %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(
    milk_efficiency = `Milk efficiency`,
    movement        = Movement,
    milk_volume     = `Total Milk Volume`,
    kgms            = `Kilogram Milk Solids (KGMS)`,
    total_points    = `Total Points`
  )

# Flag resting rounds (0 total points = rested)
round_scores <- round_scores %>%
  mutate(is_rested = total_points == 0)

# Join back the API-level cow attributes
cow_attrs <- raw %>%
  select(cow_name, ear_tag,
         price            = api_price,
         rating           = api_rating,
         breeding_worth   = api_breedingWorth,
         production_worth = api_productionWorth,
         historic_milk    = api_historicMilkVolume,
         historic_protein = api_historicProtein,
         is_resting_now   = api_isResting) %>%
  mutate(across(c(price, breeding_worth, production_worth,
                  historic_milk, historic_protein), as.numeric),
         is_resting_now = is_resting_now %in% c("True", "true", "1"))

full <- round_scores %>% left_join(cow_attrs, by = c("cow_name", "ear_tag"))


# --- 2. COW SUMMARY STATS ----------------------------------------------------

# Per-cow aggregates across all rounds (excluding rested rounds)
cow_summary <- full %>%
  filter(!is_rested) %>%
  group_by(cow_name, ear_tag) %>%
  summarise(
    rounds_played    = n(),
    rounds_rested    = sum(is_rested),  # will be 0 here by definition
    mean_points      = mean(total_points, na.rm = TRUE),
    sd_points        = sd(total_points, na.rm = TRUE),
    min_points       = min(total_points, na.rm = TRUE),
    max_points       = max(total_points, na.rm = TRUE),
    mean_milk_eff    = mean(milk_efficiency, na.rm = TRUE),
    mean_movement    = mean(movement, na.rm = TRUE),
    mean_milk_vol    = mean(milk_volume, na.rm = TRUE),
    mean_kgms        = mean(kgms, na.rm = TRUE),
    # Trend: slope of total_points over rounds (positive = improving)
    points_trend     = if (n() >= 3)
                         coef(lm(total_points ~ as.numeric(round_name)))[2]
                       else NA_real_,
    .groups = "drop"
  )

# Count rested rounds from the FULL data (before filtering)
rest_counts <- full %>%
  group_by(cow_name, ear_tag) %>%
  summarise(rounds_rested = sum(is_rested), .groups = "drop")

cow_summary <- cow_summary %>%
  select(-rounds_rested) %>%
  left_join(rest_counts, by = c("cow_name", "ear_tag"))

# Join cow attributes
cow_summary <- cow_summary %>%
  left_join(cow_attrs, by = c("cow_name", "ear_tag"))

# Coefficient of variation (lower = more consistent)
cow_summary <- cow_summary %>%
  mutate(
    cv_points       = sd_points / mean_points,
    points_per_price = mean_points / price,
    # Composite: high mean, low variance, good value
    sharpe_like      = mean_points / pmax(sd_points, 1)  # avoid div-by-zero
  )


# --- 3. RESTING ANALYSIS -----------------------------------------------------
# A farmer rests a cow when she shows signs of fatigue: declining milk output,
# dropping efficiency, or reduced movement. The signal is in the TRAJECTORY
# of metrics leading into a rest event, not in static attributes.
#
# We build round-level features capturing recent performance decline, then
# model whether a cow gets rested NEXT round based on those signals.

cat("\n===== RESTING / FATIGUE ANALYSIS =====\n\n")

# 3a. Build lagged features: for each cow-round, compute changes from the
#     previous round. A rest event at round R is predicted by metrics at R-1.
fatigue_data <- full %>%
  arrange(cow_name, round_name) %>%
  group_by(cow_name) %>%
  mutate(
    round_num = as.numeric(round_name),
    # Lagged values (previous round's metrics)
    prev_milk_eff   = lag(milk_efficiency),
    prev_movement   = lag(movement),
    prev_milk_vol   = lag(milk_volume),
    prev_kgms       = lag(kgms),
    prev_total_pts  = lag(total_points),
    prev_rested     = lag(is_rested),
    # Round-over-round deltas (negative = declining)
    delta_milk_eff  = lag(milk_efficiency) - lag(milk_efficiency, 2),
    delta_movement  = lag(movement) - lag(movement, 2),
    delta_milk_vol  = lag(milk_volume) - lag(milk_volume, 2),
    delta_kgms      = lag(kgms) - lag(kgms, 2),
    delta_total_pts = lag(total_points) - lag(total_points, 2),
    # Was the cow rested in the NEXT round? (this is what we predict)
    rested_next     = lead(is_rested)
  ) %>%
  ungroup()

# Only keep rows where we have a previous round's data AND a known next-round
# outcome, and where the cow was NOT rested this round (can't observe fatigue
# signals from a round the cow didn't play)
model_data <- fatigue_data %>%
  filter(!is.na(rested_next),
         !is.na(prev_total_pts),
         !is_rested)

cat(sprintf("Modelling data: %d cow-round observations, %d led to rest next round (%.1f%%)\n\n",
            nrow(model_data), sum(model_data$rested_next),
            100 * mean(model_data$rested_next)))

# 3b. Fit logistic regression: what observable fatigue signals predict rest?
if (sum(model_data$rested_next) >= 5) {

  rest_glm <- glm(
    rested_next ~ prev_milk_eff + prev_movement + prev_milk_vol + prev_kgms +
                  delta_milk_eff + delta_movement + delta_milk_vol + delta_kgms,
    data   = model_data,
    family = binomial
  )
  cat("Logistic regression: fatigue signals predicting rest NEXT round\n")
  cat("(Negative coefficients = declining metric -> more likely to rest)\n\n")
  print(summary(rest_glm))

  # Odds ratios for interpretability
  cat("\nOdds ratios (exp of coefficients):\n")
  odds <- tibble(
    predictor  = names(coef(rest_glm)),
    odds_ratio = exp(coef(rest_glm)),
    p_value    = summary(rest_glm)$coefficients[, 4]
  ) %>% filter(predictor != "(Intercept)")
  print(odds, n = Inf)

} else {
  cat("Not enough rest events in this sample for logistic regression.\n")
  cat("Re-run once the 500-cow scrape completes.\n\n")
}

# 3c. Descriptive: what do cows look like the round BEFORE being rested?
cat("\nAverage metrics the round BEFORE a cow is rested vs not rested:\n")
model_data %>%
  group_by(rested_next) %>%
  summarise(
    n               = n(),
    avg_milk_eff    = mean(prev_milk_eff, na.rm = TRUE),
    avg_movement    = mean(prev_movement, na.rm = TRUE),
    avg_milk_vol    = mean(prev_milk_vol, na.rm = TRUE),
    avg_kgms        = mean(prev_kgms, na.rm = TRUE),
    avg_total_pts   = mean(prev_total_pts, na.rm = TRUE),
    avg_d_milk_eff  = mean(delta_milk_eff, na.rm = TRUE),
    avg_d_movement  = mean(delta_movement, na.rm = TRUE),
    avg_d_milk_vol  = mean(delta_milk_vol, na.rm = TRUE),
    avg_d_total_pts = mean(delta_total_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# 3d. Which rounds see the most resting? (seasonal / calendar pattern)
cat("\nResting events by round:\n")
full %>% filter(is_rested) %>% count(round_name) %>% print()

# 3e. Score every cow's current rest risk based on their most recent round
#     (how fatigued do they look RIGHT NOW?)
latest_round <- full %>%
  filter(!is_rested) %>%
  group_by(cow_name) %>%
  filter(round_name == max(round_name)) %>%
  ungroup()

# Get the second-to-last active round for deltas
second_latest <- full %>%
  filter(!is_rested) %>%
  group_by(cow_name) %>%
  filter(round_name < max(round_name)) %>%
  filter(round_name == max(round_name)) %>%
  ungroup() %>%
  select(cow_name,
         prev2_milk_eff = milk_efficiency,
         prev2_movement = movement,
         prev2_milk_vol = milk_volume,
         prev2_kgms     = kgms,
         prev2_total    = total_points)

rest_risk <- latest_round %>%
  left_join(second_latest, by = "cow_name") %>%
  mutate(
    recent_delta_milk_eff = milk_efficiency - prev2_milk_eff,
    recent_delta_movement = movement - prev2_movement,
    recent_delta_milk_vol = milk_volume - prev2_milk_vol,
    recent_delta_kgms     = kgms - prev2_kgms,
    recent_delta_total    = total_points - prev2_total,
    # Simple fatigue index: sum of z-scored declines
    # More negative = more fatigued = higher rest risk
    fatigue_index = scale(recent_delta_milk_eff)[,1] +
                    scale(recent_delta_movement)[,1] +
                    scale(recent_delta_milk_vol)[,1] +
                    scale(recent_delta_kgms)[,1]
  )

# If the logistic model was fit, use it for predicted probabilities
if (exists("rest_glm")) {
  # Build prediction frame matching model terms
  pred_frame <- rest_risk %>%
    transmute(
      cow_name,
      prev_milk_eff  = milk_efficiency,
      prev_movement  = movement,
      prev_milk_vol  = milk_volume,
      prev_kgms      = kgms,
      delta_milk_eff = recent_delta_milk_eff,
      delta_movement = recent_delta_movement,
      delta_milk_vol = recent_delta_milk_vol,
      delta_kgms     = recent_delta_kgms
    ) %>%
    filter(complete.cases(.))

  pred_frame$rest_prob <- predict(rest_glm, newdata = pred_frame, type = "response")

  cat("\nPredicted rest probability for next round (top 20 highest risk):\n")
  pred_frame %>%
    arrange(desc(rest_prob)) %>%
    select(cow_name, rest_prob, prev_milk_eff, prev_movement,
           delta_milk_eff, delta_movement) %>%
    head(20) %>%
    print(n = 20, width = Inf)
} else {
  # Fallback: use the fatigue index
  cat("\nFatigue index ranking (most fatigued = highest rest risk, top 20):\n")
  rest_risk %>%
    arrange(fatigue_index) %>%
    select(cow_name, fatigue_index, milk_efficiency, movement,
           recent_delta_milk_eff, recent_delta_movement,
           recent_delta_total) %>%
    head(20) %>%
    print(n = 20, width = Inf)
}


# --- 4. TEAM SELECTION --------------------------------------------------------
# Rank cows by a points-vs-variance metric, penalising cows that look
# fatigued (likely to be rested soon by the farmer).
#
# The rest penalty is forward-looking: based on how tired the cow looks NOW
# (declining metrics), not on whether she was rested in the past.

cat("\n\n===== TEAM SELECTION RANKINGS =====\n\n")

# Bring in rest-risk from section 3
# Use predicted probability if the model was fit, otherwise fatigue_index
if (exists("rest_glm") && exists("pred_frame")) {
  cow_rest_risk <- pred_frame %>% select(cow_name, rest_prob)
  cow_summary <- cow_summary %>%
    left_join(cow_rest_risk, by = "cow_name") %>%
    mutate(rest_prob = replace_na(rest_prob, median(rest_prob, na.rm = TRUE)))
} else if (exists("rest_risk")) {
  # Rescale fatigue_index to a 0-1 pseudo-probability
  cow_rest_risk <- rest_risk %>%
    mutate(rest_prob = plogis(-fatigue_index)) %>%  # lower index -> higher prob
    select(cow_name, rest_prob)
  cow_summary <- cow_summary %>%
    left_join(cow_rest_risk, by = "cow_name") %>%
    mutate(rest_prob = replace_na(rest_prob, 0.5))
} else {
  cow_summary <- cow_summary %>% mutate(rest_prob = 0)
}

cow_summary <- cow_summary %>%
  mutate(
    # Expected points discounted by probability of being rested next round
    expected_points    = mean_points * (1 - rest_prob),
    # Points per unit of price
    points_per_price   = mean_points / price,
    # Final composite: reward high expected output & consistency, penalise cost
    team_score         = expected_points / pmax(sd_points, 1) / price
  )

cat("Top 30 cows by team_score (fatigue-adjusted expected pts / variance / price):\n\n")
cow_summary %>%
  arrange(desc(team_score)) %>%
  select(cow_name, price, rating, mean_points, sd_points, cv_points,
         rest_prob, expected_points, team_score) %>%
  head(30) %>%
  print(n = 30, width = Inf)

# Rating-tier breakdown
cat("\n\nPerformance by rating tier:\n")
cow_summary %>%
  group_by(rating) %>%
  summarise(
    n                  = n(),
    mean_price         = mean(price, na.rm = TRUE),
    mean_pts           = mean(mean_points, na.rm = TRUE),
    mean_sd            = mean(sd_points, na.rm = TRUE),
    mean_cv            = mean(cv_points, na.rm = TRUE),
    mean_rest_prob     = mean(rest_prob, na.rm = TRUE),
    mean_pts_per_price = mean(points_per_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_pts)) %>%
  print(width = Inf)


# --- 5. VALUE ANALYSIS --------------------------------------------------------
# Are expensive cows worth it? Where are the bargains?

cat("\n\n===== VALUE ANALYSIS =====\n\n")

# Points per price by price bracket
cow_summary %>%
  mutate(price_bracket = cut(price, breaks = c(0, 6, 9, 12, 15, Inf),
                             labels = c("5-6M", "7-9M", "10-12M",
                                        "13-15M", "16M+"))) %>%
  group_by(price_bracket) %>%
  summarise(
    n          = n(),
    mean_pts   = mean(mean_points, na.rm = TRUE),
    mean_sd    = mean(sd_points, na.rm = TRUE),
    mean_value = mean(points_per_price, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  print()

# Biggest bargains: high points-per-price AND low variance
cat("\nBest value picks (high points/price, low CV):\n")
cow_summary %>%
  filter(cv_points < median(cv_points, na.rm = TRUE)) %>%
  arrange(desc(points_per_price)) %>%
  select(cow_name, price, rating, mean_points, sd_points,
         cv_points, points_per_price) %>%
  head(15) %>%
  print(n = 15, width = Inf)


# --- 6. COMPONENT ANALYSIS ---------------------------------------------------
# Which scoring components drive high totals, and which are most variable?

cat("\n\n===== SCORING COMPONENT BREAKDOWN =====\n\n")

component_stats <- full %>%
  filter(!is_rested) %>%
  group_by(cow_name) %>%
  summarise(across(c(milk_efficiency, movement, milk_volume, kgms),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x, na.rm = TRUE))),
            .groups = "drop")

# Correlation between components and total points
cat("Correlation of each component mean with mean total points:\n")
merged <- component_stats %>%
  left_join(cow_summary %>% select(cow_name, mean_points), by = "cow_name")

cor_results <- tibble(
  component   = c("milk_efficiency", "movement", "milk_volume", "kgms"),
  correlation = c(
    cor(merged$milk_efficiency_mean, merged$mean_points, use = "complete.obs"),
    cor(merged$movement_mean,        merged$mean_points, use = "complete.obs"),
    cor(merged$milk_volume_mean,     merged$mean_points, use = "complete.obs"),
    cor(merged$kgms_mean,            merged$mean_points, use = "complete.obs")
  )
) %>% arrange(desc(abs(correlation)))
print(cor_results)

# Which component contributes most variance?
cat("\nAverage within-cow SD by component (higher = less predictable):\n")
component_stats %>%
  summarise(across(ends_with("_sd"), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "component", values_to = "avg_sd") %>%
  arrange(desc(avg_sd)) %>%
  print()


# --- 7. PLOTS ----------------------------------------------------------------

# 7a. Points vs Price, coloured by rating
p1 <- ggplot(cow_summary, aes(x = price, y = mean_points)) +
  geom_point(aes(colour = rating, size = 1/cv_points), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linetype = "dashed") +
  labs(title    = "Mean Points vs Price",
       subtitle = "Larger dots = more consistent (lower CV)",
       x = "Price (M)", y = "Mean Points per Round",
       colour = "Rating", size = "Consistency") +
  theme_minimal(base_size = 13) +
  guides(size = "none")
ggsave("plot_points_vs_price.png", p1, width = 10, height = 6, dpi = 150)

# 7b. Sharpe-like score by rating
p2 <- ggplot(cow_summary, aes(x = reorder(rating, -sharpe_like, median),
                               y = sharpe_like)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(title = "Points-to-Variance Ratio by Rating",
       x = "Rating", y = "Mean Points / SD (Sharpe-like)") +
  theme_minimal(base_size = 13)
ggsave("plot_sharpe_by_rating.png", p2, width = 9, height = 6, dpi = 150)

# 7c. Fatigue signals: what do cows look like before being rested?
if (nrow(model_data) > 0 && sum(model_data$rested_next) > 0) {
  p3_data <- model_data %>%
    mutate(rested_next_label = factor(rested_next,
             labels = c("Played next round", "Rested next round")))

  p3 <- ggplot(p3_data, aes(x = prev_milk_eff, y = prev_movement,
                             colour = rested_next_label)) +
    geom_point(aes(size = prev_milk_vol), alpha = 0.5) +
    labs(title    = "Fatigue Signals Before Rest Events",
         subtitle = "Metrics from the round BEFORE a cow was rested vs played",
         x = "Milk Efficiency (prev round)",
         y = "Movement (prev round)",
         colour = "Next Round", size = "Milk Vol") +
    theme_minimal(base_size = 13)
  ggsave("plot_fatigue_signals.png", p3, width = 10, height = 6, dpi = 150)
} else {
  # Fallback: show current fatigue index distribution
  p3 <- ggplot(rest_risk %>% filter(!is.na(fatigue_index)),
               aes(x = reorder(cow_name, fatigue_index), y = fatigue_index)) +
    geom_col(aes(fill = fatigue_index < -1), show.legend = FALSE) +
    scale_fill_manual(values = c("grey60", "firebrick")) +
    coord_flip() +
    labs(title = "Current Fatigue Index (lower = more fatigued)",
         x = NULL, y = "Fatigue Index (sum of z-scored metric changes)") +
    theme_minimal(base_size = 11)
  ggsave("plot_fatigue_signals.png", p3, width = 10, height = 8, dpi = 150)
}

# 7d. Round-over-round scoring trends for top cows
top_cows <- cow_summary %>% slice_max(mean_points, n = 15) %>% pull(cow_name)

p4 <- full %>%
  filter(cow_name %in% top_cows, !is_rested) %>%
  ggplot(aes(x = round_name, y = total_points,
             group = cow_name, colour = cow_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(title = "Scoring Trends: Top 15 Cows by Mean Points",
       x = "Round", y = "Total Points", colour = "Cow") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")
ggsave("plot_top_cow_trends.png", p4, width = 12, height = 7, dpi = 150)

# 7e. Component contribution stacked bar for top cows
p5 <- full %>%
  filter(cow_name %in% top_cows, !is_rested) %>%
  group_by(cow_name) %>%
  summarise(across(c(milk_efficiency, movement, milk_volume, kgms), mean),
            .groups = "drop") %>%
  pivot_longer(-cow_name, names_to = "component", values_to = "mean_pts") %>%
  ggplot(aes(x = reorder(cow_name, -mean_pts, sum),
             y = mean_pts, fill = component)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(title = "Scoring Breakdown: Top 15 Cows",
       x = NULL, y = "Mean Points (component contribution)",
       fill = "Component") +
  theme_minimal(base_size = 13)
ggsave("plot_component_breakdown.png", p5, width = 10, height = 7, dpi = 150)

cat("\n\nPlots saved: plot_points_vs_price.png, plot_sharpe_by_rating.png,")
cat("\n             plot_fatigue_signals.png, plot_top_cow_trends.png,")
cat("\n             plot_component_breakdown.png\n")

cat("\n===== ANALYSIS COMPLETE =====\n")
