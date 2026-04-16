# Marco Baldo 25-11-2024 
# ANOVA Discrimination - variable decomposition analysis
# Multifunctionality Article: 

library(tidyverse)
library(readxl)
library(patchwork)
library(ggplot2)

# Load data
df <- read_excel("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260407_MF_ES_score_individual_values.xlsx",
                 sheet = "Analysis")

# Rename for clarity
var_labels <- c(
  harvest           = "Annual harvest",
  increment         = "Annual increment",
  `Total carbon`    = "Carbon stock",
  `Deadwood carbon` = "Carbon in deadwood",
  LAI               = "LAI",
  NEP               = "NEP",
  Shannon           = "Shannon index",
  `Standing volume` = "Standing volume"
)

svc_labels <- c(
  Timber      = "Wood production",
  Climate     = "Climate regulation",
  Bidiversity = "Biodiversity",
  Water       = "Water regulation"
)

df <- df |>
  mutate(Variable           = recode(Variable,          !!!var_labels),
         `Ecosystem service` = recode(`Ecosystem service`, !!!svc_labels),
         Management         = as.factor(Management),
         Climate            = as.factor(Climate))


# Discriminatory power: SD of cell means
disc_power <- df |>
  group_by(`Ecosystem service`, Variable, Climate, Management) |>
  summarise(cell_mean = mean(Value), .groups = 'drop') |>
  group_by(`Ecosystem service`, Variable) |>
  summarise(disc = sd(cell_mean), .groups = 'drop')

# Two-way ANOVA with replication
anova_res <- df |>
  group_by(`Ecosystem service`, Variable) |>
  group_modify(~ {
    fit <- aov(Value ~ Management * Climate, data = .x)
    s   <- summary(fit)[[1]]
    rownames(s) <- trimws(rownames(s))
    ss_total <- sum(s$`Sum Sq`)
    tibble(
      pct_mgmt    = s['Management',         'Sum Sq'] / ss_total * 100,
      pct_climate = s['Climate',            'Sum Sq'] / ss_total * 100,
      pct_inter   = s['Management:Climate', 'Sum Sq'] / ss_total * 100,
      pct_resid   = s['Residuals',          'Sum Sq'] / ss_total * 100,
      F_mgmt      = s['Management',         'F value'],
      F_climate   = s['Climate',            'F value'],
      F_inter     = s['Management:Climate', 'F value'],
      p_mgmt      = s['Management',         'Pr(>F)'],
      p_climate   = s['Climate',            'Pr(>F)'],
      p_inter     = s['Management:Climate', 'Pr(>F)'],
    )
  }) |> ungroup()

results <- disc_power |>
  left_join(anova_res, by = c('Ecosystem service', 'Variable')) |>
  arrange(desc(disc)) |>
  mutate(Variable = fct_reorder(Variable, disc))

max_sd <- max(results$disc)

results <- results |>
  mutate(sig = case_when(
    p_mgmt < 0.001 ~ "***",
    p_mgmt < 0.01  ~ "**",
    p_mgmt < 0.05  ~ "*",
    TRUE           ~ "ns"
  ))


#-------------------------------------------------------------------------------
# Let's plot them

# Colours
svc_colors <- c(
  'Wood production'    = '#185FA5',
  'Biodiversity'       = '#1D9E75',
  'Climate regulation' = '#D85A30',
  'Water regulation'   = '#BA7517'
)
src_colors <- c(
  Management              = '#185FA5',
  Climate                 = '#97C459',
  Interaction              = '#D0CECA',
  `Residual (within-cell)` = '#787670'
)


# Panel A: discriminatory power
pA <- ggplot(results,
             aes(x = disc, y = Variable,
                 fill = `Ecosystem service`)) +
  geom_text(aes(x = disc, label = sig), hjust = -0.3, size = 3.5) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = svc_colors,
                    name   = 'Ecosystem service') +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, max_sd * 1.12)) +
  labs(x = 'Discriminatory power (SD of cell means)',
       y = NULL, title = 'a') +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = 'bottom')+
  # pA legend
  theme(legend.position = 'bottom',
        legend.title = element_text(face = 'bold'),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(4,4,4,4))+
  guides(fill = guide_legend(nrow = 2)) # to edit the legend panel order

# Panel B: variance decomposition, bars scaled to 100%,
#          bar height proportional to discriminatory power
plot_decomp <- results |>
  select(Variable, disc, pct_mgmt, pct_climate, pct_inter, pct_resid) |>
  pivot_longer(cols = starts_with('pct_'),
               names_to  = 'source',
               values_to = 'pct') |>
  mutate(
    source = recode(source,
                    pct_mgmt    = 'Management',
                    pct_climate = 'Climate',
                    pct_inter   = 'Interaction',
                    pct_resid   = 'Residual (within-cell)'),
    source = factor(source, levels = rev(c("Management", "Climate", "Interaction", "Residual (within-cell)")))
  )

pB <- ggplot(plot_decomp,
             aes(x = pct, y = Variable,
                 fill = source)) +
  geom_col(position = 'stack') +
  scale_fill_manual(values = src_colors,
                    name   = 'Variance source') +
  scale_x_continuous(expand = c(0,0), limits = c(0,101),
                     labels = function(x) paste0(x,'%')) +
  labs(x = 'Variance decomposition (%)',
       y = NULL, title = 'b') +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_blank(),
        legend.position    = 'bottom')+
  # pB  
  theme(legend.position = 'bottom',
        legend.title = element_text(face = 'bold'),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(4,4,4,4))+
  guides(fill = guide_legend(nrow = 2)) # to edit the legend panel order

pA + pB  # no plot_layout guides collect

ggsave("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/Fig_indicator_analysis_final.pdf",
       width = 32, height = 15, units = "cm")


dev.off()

# See the plot in the console

final_panel <- pA + pB

final_panel
