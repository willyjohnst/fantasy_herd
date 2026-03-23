# =============================================================================
# Fantasy Herd Analysis v2
# Goal: Pick 5 starters + 5 bench under $100M to maximize points per round
# =============================================================================

library(tidyverse)

# --- 1. LOAD & RESHAPE -------------------------------------------------------

raw <- read_csv("fantasy_herd_joined_data.csv", show_col_types = FALSE)

raw <- raw %>%
  mutate(cow_name = str_split_i(name, "\n", 1),
         ear_tag  = str_split_i(name, "\n", 2))

round_cols <- raw %>% select(starts_with("html_")) %>% names()

long <- raw %>%
  select(cow_name, ear_tag, all_of(round_cols)) %>%
  pivot_longer(cols = all_of(round_cols), names_to = "col_name", values_to = "value") %>%
  mutate(
    col_stripped = str_remove(col_name, "^html_"),
    round_name   = str_extract(col_stripped, "^[^_]+(?:\\s[^_]+)?"),
    metric       = str_remove(col_stripped, "^[^_]+(?:\\s[^_]+)?_")
  ) %>%
  select(-col_name, -col_stripped) %>%
  mutate(value = as.numeric(value))

round_order <- c("DRW", "ROUND 1", "ROUND 2", "ROUND 3", "ROUND 4")
long <- long %>%
  mutate(round_name = factor(round_name, levels = round_order, ordered = TRUE))

round_scores <- long %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(
    milk_efficiency = `Milk efficiency`,
    movement        = Movement,
    milk_volume     = `Total Milk Volume`,
    kgms            = `Kilogram Milk Solids (KGMS)`,
    total_points    = `Total Points`
  ) %>%
  mutate(is_rested = total_points == 0)

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


# --- 2. COW SUMMARY ----------------------------------------------------------

cow_summary <- full %>%
  group_by(cow_name, ear_tag) %>%
  mutate(active = !is_rested & !is.na(is_rested)) %>%
  summarise(
    rounds_played = sum(active),
    rounds_rested = sum(is_rested, na.rm = TRUE),
    # Mean/SD calculated only over rounds the cow actually played
    mean_points   = mean(total_points[active], na.rm = TRUE),
    sd_points     = sd(total_points[active], na.rm = TRUE),
    min_points    = min(total_points[active], na.rm = TRUE),
    max_points    = max(total_points[active], na.rm = TRUE),
    mean_milk_eff = mean(milk_efficiency[active], na.rm = TRUE),
    mean_movement = mean(movement[active], na.rm = TRUE),
    mean_milk_vol = mean(milk_volume[active], na.rm = TRUE),
    mean_kgms     = mean(kgms[active], na.rm = TRUE),
    # Recent form: last active round's points
    latest_points = last(total_points[active]),
    # Trend: slope across rounds played (positive = improving)
    points_trend  = {
      if (sum(active) >= 3)
        coef(lm(total_points[active] ~ as.numeric(round_name[active])))[2]
      else NA_real_
    },
    .groups = "drop"
  ) %>%
  left_join(cow_attrs, by = c("cow_name", "ear_tag")) %>%
  mutate(
    rest_rate        = rounds_rested / (rounds_played + rounds_rested),
    cv_points        = sd_points / mean_points,
    points_per_price = mean_points / price,
    # Standard error of the mean
    se_points        = sd_points / sqrt(pmax(rounds_played, 1)),
    # Lower bound of 80% CI: "90% confident the cow scores at least this"
    # Uses t-distribution for small samples (n=3-5 rounds)
    ci80_lower       = mean_points - qt(0.9, pmax(rounds_played - 1, 1)) * se_points,
    # Lower bound per price dollar — the key ranking metric
    ci80_lower_per_price = ci80_lower / price
  ) %>%
  filter(!is.na(price), !is.na(mean_points))


# --- 3. RESTING ANALYSIS (informational, not used in team scoring) -----------
# The model is weak (nothing significant) which tells us something useful:
# resting is driven by factors we can't observe (farmer's judgment, health
# checks, etc.), not by the performance metrics we have. The practical
# implication: don't try to dodge resting, build a bench instead.

cat("\n===== RESTING ANALYSIS (informational) =====\n\n")

cat(sprintf("Overall rest rate: %.1f%% of cow-rounds\n",
            100 * sum(cow_summary$rounds_rested) /
              sum(cow_summary$rounds_played + cow_summary$rounds_rested)))

cat(sprintf("With 5 starters: expect ~%.1f resting events per round\n",
            5 * sum(cow_summary$rounds_rested) /
              sum(cow_summary$rounds_played + cow_summary$rounds_rested)))

# Rest events by round (escalating through the season)
cat("\nResting events by round:\n")
full %>% filter(is_rested) %>% count(round_name) %>% print()

# Do higher-rated or more expensive cows rest more?
cat("\nRest rate by price tier:\n")
cow_summary %>%
  mutate(price_tier = cut(price, breaks = c(0, 7, 10, 15, Inf),
                          labels = c("$5-7M", "$8-10M", "$11-15M", "$16M+"))) %>%
  group_by(price_tier) %>%
  summarise(n = n(),
            mean_rest_rate = mean(rest_rate, na.rm = TRUE),
            mean_pts = mean(mean_points, na.rm = TRUE),
            .groups = "drop") %>%
  print()

# Key takeaway for team building
cat("\nResting increases through the season (farmer fatigue management).\n")
cat("Rest rate is similar across price tiers, so expensive cows aren't\n")
cat("rested more often. Build a strong bench rather than trying to predict it.\n")


# --- 4. TEAM SELECTION --------------------------------------------------------
# The problem: pick 5 starters + 5 bench, total ≤ $100M, maximize expected
# points per round.
#
# Instead of penalising variance symmetrically (which punishes upside),
# we rank cows by the LOWER BOUND of a confidence interval on their mean.
# This answers: "What's the worst this cow is likely to score on average?"
#
# A volatile cow averaging 70 (range 60-85) has a lower bound of ~63.
# A consistent cow averaging 55 (range 52-58) has a lower bound of ~53.
# The volatile cow wins — her FLOOR is still higher than the safe cow's
# ceiling. This is the right behaviour: we want high-scoring cows and only
# discount variance to the extent it creates genuine downside risk.

cat("\n\n===== TEAM SELECTION =====\n\n")

BUDGET <- 100

# Show how the CI approach compares to raw mean
cat("CI-based ranking vs raw mean (top 25 cows):\n")
cat("(ci80_lower = lower bound of 80% CI on mean points)\n\n")
cow_summary %>%
  filter(rounds_played >= 3) %>%
  arrange(desc(ci80_lower)) %>%
  select(cow_name, price, rating, mean_points, sd_points, rounds_played,
         se_points, ci80_lower, ci80_lower_per_price) %>%
  head(25) %>%
  print(n = 25, width = Inf)

# Value curve: points per $M by price tier
cat("\n\nPoints per $1M by price tier:\n")
cow_summary %>%
  mutate(price_tier = cut(price, breaks = c(0, 7, 10, 15, Inf),
                          labels = c("$5-7M", "$8-10M", "$11-15M", "$16M+"))) %>%
  group_by(price_tier) %>%
  summarise(
    n              = n(),
    avg_price      = mean(price),
    avg_pts        = mean(mean_points, na.rm = TRUE),
    avg_ci_lower   = mean(ci80_lower, na.rm = TRUE),
    top10_ci_lower = mean(sort(ci80_lower, decreasing = TRUE)[1:min(10, n())]),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# Elite bargains: high floor per dollar
cat("\n\nElite bargains (top 10 by CI lower bound per price, min 4 rounds):\n")
cow_summary %>%
  filter(rounds_played >= 4) %>%
  arrange(desc(ci80_lower_per_price)) %>%
  select(cow_name, price, rating, mean_points, sd_points,
         ci80_lower, ci80_lower_per_price) %>%
  head(10) %>%
  print(n = 10, width = Inf)

# Greedy team builder — rank by ci80_lower instead of raw mean
candidates <- cow_summary %>%
  filter(rounds_played >= 3) %>%
  arrange(desc(ci80_lower))

build_team <- function(candidates, budget, n_starters = 5, n_bench = 5) {
  min_bench_cost <- n_bench * min(candidates$price, na.rm = TRUE)
  
  starters <- tibble()
  remaining_budget <- budget
  
  for (i in seq_len(nrow(candidates))) {
    if (nrow(starters) >= n_starters) break
    cow <- candidates[i, ]
    slots_left <- n_starters - nrow(starters) - 1
    min_remaining <- slots_left * min(candidates$price) + min_bench_cost
    if (cow$price <= remaining_budget - min_remaining &&
        !cow$cow_name %in% starters$cow_name) {
      starters <- bind_rows(starters, cow)
      remaining_budget <- remaining_budget - cow$price
    }
  }
  
  bench_pool <- candidates %>%
    filter(!cow_name %in% starters$cow_name) %>%
    arrange(desc(ci80_lower))
  
  bench <- tibble()
  for (i in seq_len(nrow(bench_pool))) {
    if (nrow(bench) >= n_bench) break
    cow <- bench_pool[i, ]
    slots_left <- n_bench - nrow(bench) - 1
    min_remaining <- slots_left * min(bench_pool$price)
    if (cow$price <= remaining_budget - min_remaining) {
      bench <- bind_rows(bench, cow)
      remaining_budget <- remaining_budget - cow$price
    }
  }
  
  list(starters = starters, bench = bench, spent = budget - remaining_budget)
}

team <- build_team(candidates, BUDGET)

cat("\n--- OPTIMAL TEAM (ranked by CI lower bound) ---\n\n")
cat("STARTERS (5):\n")
team$starters %>%
  select(cow_name, price, rating, mean_points, sd_points,
         ci80_lower, rounds_played) %>%
  print(n = 5, width = Inf)

cat("\nBENCH (5):\n")
team$bench %>%
  select(cow_name, price, rating, mean_points, sd_points,
         ci80_lower, rounds_played) %>%
  print(n = 5, width = Inf)

starter_total    <- sum(team$starters$mean_points)
starter_ci_floor <- sum(team$starters$ci80_lower)
bench_avg        <- mean(team$bench$mean_points)
bench_ci_floor   <- mean(team$bench$ci80_lower)
total_cost       <- team$spent

cat(sprintf("\nTotal cost: $%dM / $%dM\n", total_cost, BUDGET))
cat(sprintf("Expected starter points/round: %.1f (CI floor: %.1f)\n",
            starter_total, starter_ci_floor))
cat(sprintf("Avg bench cow: %.1f pts (CI floor: %.1f)\n",
            bench_avg, bench_ci_floor))

# Resting impact
rest_rate <- sum(cow_summary$rounds_rested) /
  sum(cow_summary$rounds_played + cow_summary$rounds_rested)
avg_starter_mean <- mean(team$starters$mean_points)
rest_cost_per_event <- avg_starter_mean - bench_avg
expected_rests_per_round <- 5 * rest_rate
expected_loss <- expected_rests_per_round * rest_cost_per_event

cat(sprintf("\nResting impact: ~%.1f events/round × %.1f pt loss each = %.1f expected pts lost\n",
            expected_rests_per_round, rest_cost_per_event, expected_loss))
cat(sprintf("Net expected points/round: %.1f\n", starter_total - expected_loss))


# --- 5. VALUE ANALYSIS --------------------------------------------------------

cat("\n\n===== VALUE ANALYSIS =====\n\n")

# The key question: where does the marginal dollar buy the most points?
cat("Diminishing returns: best cow in each price point\n")
cow_summary %>%
  group_by(price) %>%
  slice_max(mean_points, n = 1) %>%
  ungroup() %>%
  select(price, cow_name, mean_points, sd_points, rating) %>%
  arrange(price) %>%
  mutate(marginal_pts = mean_points - lag(mean_points),
         marginal_cost = price - lag(price),
         marginal_pts_per_M = marginal_pts / pmax(marginal_cost, 1)) %>%
  print(n = 20, width = Inf)


# --- 6. COMPONENT ANALYSIS ---------------------------------------------------

cat("\n\n===== SCORING COMPONENT BREAKDOWN =====\n\n")

# Which components drive total points?
component_cors <- full %>%
  filter(!is_rested) %>%
  summarise(
    milk_efficiency = cor(milk_efficiency, total_points, use = "complete.obs"),
    movement        = cor(movement, total_points, use = "complete.obs"),
    milk_volume     = cor(milk_volume, total_points, use = "complete.obs"),
    kgms            = cor(kgms, total_points, use = "complete.obs")
  ) %>%
  pivot_longer(everything(), names_to = "component", values_to = "r") %>%
  arrange(desc(r))

cat("Correlation of each component with total points (round-level):\n")
print(component_cors)

# Average contribution to total points for top vs bottom half of cows
cat("\nWhat separates top cows from average cows?\n")
top_names <- cow_summary %>% slice_max(mean_points, n = 50) %>% pull(cow_name)
bot_names <- cow_summary %>% slice_min(mean_points, n = 50) %>% pull(cow_name)

bind_rows(
  full %>% filter(cow_name %in% top_names, !is_rested) %>%
    summarise(across(c(milk_efficiency, movement, milk_volume, kgms, total_points),
                     mean, na.rm = TRUE)) %>% mutate(group = "Top 50"),
  full %>% filter(cow_name %in% bot_names, !is_rested) %>%
    summarise(across(c(milk_efficiency, movement, milk_volume, kgms, total_points),
                     mean, na.rm = TRUE)) %>% mutate(group = "Bottom 50")
) %>%
  select(group, everything()) %>%
  print(width = Inf)


# --- 7. PLOTS ----------------------------------------------------------------

# 7a. Points vs Price — with CI error bars and team highlighted
p1 <- ggplot(cow_summary, aes(x = price, y = mean_points)) +
  geom_point(aes(colour = rating), alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30", linetype = "dashed") +
  # Show CI lower bounds for the team picks
  geom_errorbar(data = bind_rows(
    team$starters %>% mutate(role = "Starter"),
    team$bench %>% mutate(role = "Bench")),
    aes(ymin = ci80_lower, ymax = mean_points, colour = NULL),
    width = 0.3, alpha = 0.6) +
  geom_point(data = team$starters, aes(x = price, y = mean_points),
             colour = "red", size = 4, shape = 16) +
  geom_point(data = team$bench, aes(x = price, y = mean_points),
             colour = "dodgerblue", size = 3.5, shape = 16) +
  # Label the picks
  geom_text(data = team$starters, aes(label = cow_name),
            nudge_y = 2, size = 2.8, colour = "red") +
  geom_text(data = team$bench, aes(label = cow_name),
            nudge_y = -2, size = 2.5, colour = "dodgerblue") +
  labs(title    = "Optimal Team: CI-Based Selection",
       subtitle = "Error bars show 80% CI lower bound (the cow's scoring floor)",
       x = "Price ($M)", y = "Mean Points per Round",
       colour = "Rating") +
  theme_minimal(base_size = 13)
ggsave("plot_value_map.png", p1, width = 11, height = 7, dpi = 150)

# 7b. Points per price — finding bargains
p2 <- cow_summary %>%
  filter(rounds_played >= 4) %>%
  slice_max(points_per_price, n = 30) %>%
  ggplot(aes(x = reorder(cow_name, points_per_price), y = points_per_price,
             fill = factor(price))) +
  geom_col() +
  geom_text(aes(label = sprintf("%.0f pts, $%dM", mean_points, price)),
            hjust = -0.05, size = 3) +
  coord_flip() +
  scale_x_discrete() +
  labs(title = "Top 30 Cows by Points-per-Price",
       x = NULL, y = "Mean Points / Price ($M)",
       fill = "Price ($M)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")
ggsave("plot_bargains.png", p2, width = 11, height = 8, dpi = 150)

# 7c. Resting escalation through the season
rest_by_round <- full %>%
  group_by(round_name) %>%
  summarise(n_rested = sum(is_rested),
            n_total  = n(),
            pct      = 100 * n_rested / n_total,
            .groups = "drop")

p3 <- ggplot(rest_by_round, aes(x = round_name, y = pct)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(%d cows)", pct, n_rested)),
            vjust = -0.3, size = 3.5) +
  labs(title = "Resting Rate Escalates Through the Season",
       subtitle = "Plan bench strength for later rounds",
       x = "Round", y = "% of Cows Rested") +
  theme_minimal(base_size = 13) +
  ylim(0, max(rest_by_round$pct) * 1.3)
ggsave("plot_resting_by_round.png", p3, width = 9, height = 5, dpi = 150)

# 7d. Top cow scoring trends
top_cows <- cow_summary %>% slice_max(mean_points, n = 15) %>% pull(cow_name)

p4 <- full %>%
  filter(cow_name %in% top_cows, !is_rested) %>%
  ggplot(aes(x = round_name, y = total_points,
             group = cow_name, colour = cow_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(title = "Scoring Trends: Top 15 Cows",
       x = "Round", y = "Total Points", colour = "Cow") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")
ggsave("plot_top_cow_trends.png", p4, width = 12, height = 7, dpi = 150)

# 7e. Component breakdown for top cows
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

cat("\n\nPlots saved to working directory.\n")
cat("\n===== ANALYSIS COMPLETE =====\n")