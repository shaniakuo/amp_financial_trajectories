# =============================================================
# Financial trajectories 
# American Mosaic Project, Wave 3 (2024)
# Asian American and Latinx respondents
# =============================================================

library(tidyverse)
library(haven)
library(ggalluvial)
library(brms)

# ---- 0. Load + check -----------------------------------------
d_raw <- read_dta("AMP3_WORKING.dta")   # or read_sav("AMP3_WORKING.sav")
d_raw <- d_raw |> rename_with(tolower)

# Confirm variable names exist
grep("caseid|weight_std|raceth|v52|amdream", names(d_raw), value = TRUE)

# Inspect value labels / codes (missing codes are often 77, 98, 99 or negative)
print_labels(d_raw$racethnicity)
print_labels(d_raw$v52finances)
print_labels(d_raw$v52ratefinances)
print_labels(d_raw$v52financesfiveyears)
print_labels(d_raw$v4amdream)

table(as_factor(d_raw$v52finances),          useNA = "ifany")
table(as_factor(d_raw$v52ratefinances),      useNA = "ifany")
table(as_factor(d_raw$v52financesfiveyears), useNA = "ifany")


# ---- 1. Sample + recodes -------------------------------------
# Note: code 6 is Asian AND Pacific Islander. To restrict to Asian Americans only,
# use the 15-level race flags (Asian Indian, Chinese, Filipino, ...) instead.
d <- d_raw |>
  filter(racethnicity %in% c(4, 6)) |>
  mutate(
    group = factor(if_else(racethnicity == 6, "Asian American", "Latinx"),
                   levels = c("Asian American", "Latinx")),
    wt = as.numeric(weight_std),

    # Change items: recode BY LABEL TEXT, so the response order in the data doesn't matter.
    # 1 = Worse, 2 = About the same, 3 = Better; refused / don't know / skipped -> NA
    past_lbl   = as.character(as_factor(v52ratefinances)),
    future_lbl = as.character(as_factor(v52financesfiveyears)),
    past_chg = case_when(
      str_detect(past_lbl, regex("better", ignore_case = TRUE)) ~ 3,
      str_detect(past_lbl, regex("same",   ignore_case = TRUE)) ~ 2,
      str_detect(past_lbl, regex("worse",  ignore_case = TRUE)) ~ 1
    ),
    future_chg = case_when(
      str_detect(future_lbl, regex("better", ignore_case = TRUE)) ~ 3,
      str_detect(future_lbl, regex("same",   ignore_case = TRUE)) ~ 2,
      str_detect(future_lbl, regex("worse",  ignore_case = TRUE)) ~ 1
    ),

    # Current level: drop missing codes. ASSUMES 1 = best (e.g., "Excellent") so it is
    # REVERSED to make higher = better. If print_labels() shows 1 = worst, delete the reversal.
    cur_raw = as.numeric(v52finances),
    cur_raw = if_else(cur_raw < 1 | cur_raw >= 77, NA_real_, cur_raw)
  ) |>
  mutate(fin_current = max(cur_raw, na.rm = TRUE) + 1 - cur_raw) |>
  drop_na(past_chg, future_chg)

# Sanity checks: label -> code mapping
d |> count(past_lbl, past_chg)
d |> count(future_lbl, future_chg)
d |> count(cur_raw, fin_current)

# Readable factors
d <- d |>
  mutate(
    past_f    = factor(past_chg,   levels = 1:3, labels = c("Worse", "Same", "Better")),
    future_f  = factor(future_chg, levels = 1:3, labels = c("Worse", "Same", "Better")),
    current_f = factor(fin_current)   # relabel once you know the response options
  )

# Trajectory type: 3 x 3 combinations of past change and expected future change
d <- d |>
  mutate(fin_type = case_when(
    past_chg == 1 & future_chg == 3 ~ "Valley",        # got worse, expects better
    past_chg == 3 & future_chg == 1 ~ "Peak",          # got better, expects worse
    past_chg == 3 & future_chg == 3 ~ "Rising",
    past_chg == 1 & future_chg == 1 ~ "Declining",
    past_chg == 2 & future_chg == 2 ~ "Stable",
    past_chg == 2 & future_chg == 3 ~ "Flat, then up",
    past_chg == 2 & future_chg == 1 ~ "Flat, then down",
    past_chg == 3 & future_chg == 2 ~ "Up, then plateau",
    past_chg == 1 & future_chg == 2 ~ "Down, then plateau"
  ),
  fin_type = factor(fin_type, levels = c("Stable", "Rising", "Declining", "Valley", "Peak",
                                         "Flat, then up", "Flat, then down",
                                         "Up, then plateau", "Down, then plateau")))


# ---- 2. Cross-tabs -------------------------------------------

# 2a. Past change x future change, weighted % within group
d |>
  group_by(group, past_f, future_f) |>
  summarise(n = n(), n_wt = sum(wt), .groups = "drop") |>
  group_by(group) |>
  mutate(pct_wt = round(100 * n_wt / sum(n_wt), 1)) |>
  ungroup() |>
  print(n = 30)

# 2b. Trajectory type shares (unweighted n shown so you can spot thin cells)
type_shares <- d |>
  group_by(group, fin_type) |>
  summarise(n = n(), n_wt = sum(wt), .groups = "drop") |>
  group_by(group) |>
  mutate(pct_wt = round(100 * n_wt / sum(n_wt), 1)) |>
  ungroup()
print(type_shares, n = 30)

type_shares |>
  select(group, fin_type, pct_wt) |>
  pivot_wider(names_from = group, values_from = pct_wt)

# 2c. Trajectory type by current financial level (row % within group x current level)
d |>
  filter(!is.na(current_f)) |>
  group_by(group, current_f, fin_type) |>
  summarise(n_wt = sum(wt), .groups = "drop") |>
  group_by(group, current_f) |>
  mutate(pct_wt = round(100 * n_wt / sum(n_wt), 1)) |>
  ungroup() |>
  select(-n_wt) |>
  pivot_wider(names_from = fin_type, values_from = pct_wt, values_fill = 0) |>
  print(n = 30)


# ---- 3. Alluvial plot ----------------------------------------
# Axis 1: change over past 5 years; Axis 2: current level; Axis 3: expected change
alluv_df <- d |>
  filter(!is.na(current_f)) |>
  group_by(group, past_f, current_f, future_f, fin_type) |>
  summarise(n_wt = sum(wt), .groups = "drop") |>
  group_by(group) |>
  mutate(prop = n_wt / sum(n_wt)) |>
  ungroup()

p_alluv <- ggplot(alluv_df,
                  aes(axis1 = past_f, axis2 = current_f, axis3 = future_f, y = prop)) +
  geom_alluvium(aes(fill = fin_type), width = 1/8, alpha = 0.75) +
  geom_stratum(width = 1/8, fill = "grey95", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("vs. 5 years ago", "Today", "5 years from now"),
                   expand = c(0.08, 0.08)) +
  facet_wrap(~ group) +
  labs(y = "Weighted proportion", fill = "Trajectory type",
       title = "Perceived financial trajectories") +
  theme_minimal(base_size = 12)

p_alluv
ggsave("alluvial_fin_trajectories.png", p_alluv, width = 11, height = 6, dpi = 300)


# ---- 4. Long format: two change items per respondent ---------
# Both change items share the same Worse / Same / Better format, so they can be stacked.
# Current level is on a different scale, so it stays a person-level variable.
d_long <- d |>
  select(caseid, group, wt, fin_current, past_chg, future_chg) |>
  pivot_longer(c(past_chg, future_chg), names_to = "item", values_to = "chg") |>
  mutate(item = factor(if_else(item == "past_chg", "past", "future"),
                       levels = c("past", "future")),   # past = reference
         wt_norm = wt / mean(wt))                       # mean-1 weights for brms


# ---- 5. Priors + prior predictive check ----------------------
priors <- c(
  prior(normal(0, 1),   class = "b"),
  prior(normal(0, 1.5), class = "Intercept"),
  prior(exponential(1), class = "sd")
)

fit_prior <- brm(
  chg ~ item * group + (1 | caseid),
  family = cumulative("probit"),
  data = d_long,
  prior = priors,
  sample_prior = "only",
  chains = 4, cores = 4, iter = 1000, seed = 2026
)
pp_check(fit_prior, type = "bars", ndraws = 100)


# ---- 6. Baseline multilevel ordinal model --------------------
# Survey weights: use  chg | weights(wt_norm) ~ ...  (pseudo-likelihood; compare to unweighted)
fit_base <- brm(
  chg ~ item * group + (1 | caseid),
  family = cumulative("probit"),
  data = d_long,
  prior = priors,
  chains = 4, cores = 4, iter = 2000, seed = 2026,
  file = "fit_base"
)

summary(fit_base)
pp_check(fit_base, type = "bars_grouped", group = "item", ndraws = 100)
conditional_effects(fit_base, effects = "item:group", categorical = TRUE)

# ICC (latent probit scale, residual variance = 1)
draws <- as_draws_df(fit_base)
icc <- draws$sd_caseid__Intercept^2 / (draws$sd_caseid__Intercept^2 + 1)
quantile(icc, c(0.025, 0.5, 0.975))


# ---- 7. Expected change scores + contrasts -------------------
nd <- expand_grid(
  item  = factor(c("past", "future"), levels = levels(d_long$item)),
  group = factor(c("Asian American", "Latinx"), levels = levels(d_long$group))
)

ep <- posterior_epred(fit_base, newdata = nd, re_formula = NA)   # draws x rows x 3

# Expected score on -1 (worse) / 0 (same) / +1 (better)
exp_chg <- -1 * ep[, , 1] + 0 * ep[, , 2] + 1 * ep[, , 3]

i_past_a <- which(nd$item == "past"   & nd$group == "Asian American")
i_fut_a  <- which(nd$item == "future" & nd$group == "Asian American")
i_past_l <- which(nd$item == "past"   & nd$group == "Latinx")
i_fut_l  <- which(nd$item == "future" & nd$group == "Latinx")

# Optimism relative to recent experience (future minus past), by group
fut_minus_past_asian  <- exp_chg[, i_fut_a] - exp_chg[, i_past_a]
fut_minus_past_latinx <- exp_chg[, i_fut_l] - exp_chg[, i_past_l]
group_diff            <- fut_minus_past_latinx - fut_minus_past_asian

contrasts <- tibble(
  contrast = c("Asian Am: past change", "Asian Am: expected future change",
               "Latinx: past change",   "Latinx: expected future change",
               "Asian Am: future - past", "Latinx: future - past",
               "Latinx minus Asian Am (future - past)"),
  median  = c(median(exp_chg[, i_past_a]), median(exp_chg[, i_fut_a]),
              median(exp_chg[, i_past_l]), median(exp_chg[, i_fut_l]),
              median(fut_minus_past_asian), median(fut_minus_past_latinx),
              median(group_diff)),
  lower95 = c(quantile(exp_chg[, i_past_a], 0.025), quantile(exp_chg[, i_fut_a], 0.025),
              quantile(exp_chg[, i_past_l], 0.025), quantile(exp_chg[, i_fut_l], 0.025),
              quantile(fut_minus_past_asian, 0.025), quantile(fut_minus_past_latinx, 0.025),
              quantile(group_diff, 0.025)),
  upper95 = c(quantile(exp_chg[, i_past_a], 0.975), quantile(exp_chg[, i_fut_a], 0.975),
              quantile(exp_chg[, i_past_l], 0.975), quantile(exp_chg[, i_fut_l], 0.975),
              quantile(fut_minus_past_asian, 0.975), quantile(fut_minus_past_latinx, 0.975),
              quantile(group_diff, 0.975)),
  pr_positive = c(mean(exp_chg[, i_past_a] > 0), mean(exp_chg[, i_fut_a] > 0),
                  mean(exp_chg[, i_past_l] > 0), mean(exp_chg[, i_fut_l] > 0),
                  mean(fut_minus_past_asian > 0), mean(fut_minus_past_latinx > 0),
                  mean(group_diff > 0))
)
contrasts


# ---- 8. Trajectory types by group, with uncertainty ----------
# Posterior share of each type (Valley, Peak, ...) in each group
fit_type <- brm(
  fin_type ~ group,
  family = categorical(link = "logit"),
  data = d,
  prior = prior(normal(0, 1.5), class = "b"),
  chains = 4, cores = 4, iter = 2000, seed = 2026,
  file = "fit_type"
)

nd_g <- tibble(group = factor(c("Asian American", "Latinx"), levels = levels(d$group)))
ep_t <- posterior_epred(fit_type, newdata = nd_g)   # draws x 2 x 9 types
dimnames(ep_t)[[3]]                                 # confirm category order

i_valley <- which(dimnames(ep_t)[[3]] == "Valley")
i_peak   <- which(dimnames(ep_t)[[3]] == "Peak")

valley_asian  <- ep_t[, 1, i_valley]
valley_latinx <- ep_t[, 2, i_valley]
peak_asian    <- ep_t[, 1, i_peak]
peak_latinx   <- ep_t[, 2, i_peak]

tibble(
  quantity = c("Valley share: Asian Am", "Valley share: Latinx", "Valley: Latinx - Asian Am",
               "Peak share: Asian Am",   "Peak share: Latinx",   "Peak: Latinx - Asian Am"),
  median  = c(median(valley_asian), median(valley_latinx), median(valley_latinx - valley_asian),
              median(peak_asian),   median(peak_latinx),   median(peak_latinx - peak_asian)),
  lower95 = c(quantile(valley_asian, 0.025), quantile(valley_latinx, 0.025),
              quantile(valley_latinx - valley_asian, 0.025),
              quantile(peak_asian, 0.025), quantile(peak_latinx, 0.025),
              quantile(peak_latinx - peak_asian, 0.025)),
  upper95 = c(quantile(valley_asian, 0.975), quantile(valley_latinx, 0.975),
              quantile(valley_latinx - valley_asian, 0.975),
              quantile(peak_asian, 0.975), quantile(peak_latinx, 0.975),
              quantile(peak_latinx - peak_asian, 0.975))
)
