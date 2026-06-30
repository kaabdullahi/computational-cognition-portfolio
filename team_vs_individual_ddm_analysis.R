library(tidyverse)
library(janitor)

# Load and clean data
df <- read_csv("fullData_C 2026.csv", skip = 1) %>%
  clean_names() %>%
  select(-x1, -v1)

# Sanity checks
df %>%
  group_by(round(choice_difficulty)) %>%
  summarise(mean_acc = mean(accuracy, na.rm = TRUE),
            mean_rt = mean(reaction_time, na.rm = TRUE))

df %>%
  group_by(block) %>%
  summarise(
    mean_acc = mean(accuracy, na.rm = TRUE),
    mean_rt = mean(reaction_time, na.rm = TRUE),
    mean_opt_dist = mean(distance_from_optimality, na.rm = TRUE),
    n = n()
  )

# Paired t-tests (Team vs Separate)
wide_opt <- df %>%
  group_by(dyad, block) %>%
  summarise(mean_opt = mean(distance_from_optimality, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = block, values_from = mean_opt)

t.test(wide_opt$Team, wide_opt$Separate, paired = TRUE)

# Effect size
diff_scores <- wide_opt$Team - wide_opt$Separate
cohens_d <- mean(diff_scores) / sd(diff_scores)
cohens_d

# EZ-diffusion model
ez_diffusion <- function(pc, rt, vrt, s = 0.1) {
  if (pc <= 0.5) pc <- 0.51
  if (pc >= 1) pc <- 0.99
  logit_pc <- log(pc / (1 - pc))
  x <- logit_pc * (pc^2 * logit_pc - pc * logit_pc + pc - 0.5) / vrt
  v <- sign(pc - 0.5) * s * (x)^(1/4)
  a <- (s^2 * logit_pc) / v
  y <- -v * a / (s^2)
  mdt <- (a / (2*v)) * ((1 - exp(-y)) / (1 + exp(-y)))
  t0 <- rt - mdt
  list(v = v, a = a, t0 = t0)
}

ddm_by_dyad_block <- df %>%
  group_by(dyad, block) %>%
  summarise(
    acc = mean(accuracy, na.rm = TRUE),
    mean_rt = mean(reaction_time, na.rm = TRUE) / 1000,
    var_rt = var(reaction_time, na.rm = TRUE) / 1e6,
    n = n(),
    .groups = "drop"
  ) %>%
  filter(acc > 0.5, acc < 1, !is.na(var_rt))

ddm_results <- ddm_by_dyad_block %>%
  rowwise() %>%
  mutate(
    fit = list(ez_diffusion(acc, mean_rt, var_rt)),
    drift_rate = fit$v,
    boundary = fit$a,
    nondecision_time = fit$t0
  ) %>%
  ungroup() %>%
  select(-fit)

print(ddm_results, n = Inf)


#"Added EZ-diffusion analysis: team vs individual decision-making"
