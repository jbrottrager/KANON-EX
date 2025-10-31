################################# DNBxAbitur ################################### 

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(tidyverse)
library(ggridges)
library(ggthemes)
library(rpart)
library(rpart.plot)

#------------------------------------------------------------------------------#
# Read in data ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Projects\\CANON-EX")

# German National Library data
dnb <- read.csv(
  "251014_dnb_results_filtered.csv",
  sep = ";",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# School reading list data
df <- read.csv(
  "metadata_abitur.csv",
  sep = ";",
  encoding = "UTF-8"
)

#------------------------------------------------------------------------------#
# Define helper objects -------------------------------------------------------#
#------------------------------------------------------------------------------#

state_cols <- c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV",
                "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")

#------------------------------------------------------------------------------#
# Clean and summarise DNB data ------------------------------------------------#
#------------------------------------------------------------------------------#

dnb_detail <- dnb %>%
  mutate(
    # Extract numeric publication year (first four digits)
    #pub_year = str_extract(pub_year, "\\d{4}"),
    #pub_year = as.numeric(pub_year),
    
    # Clean language codes
    language_codes = str_remove_all(language_codes, "1\\\\p"),
    language_codes = str_squish(str_remove_all(language_codes, "^\\|\\||\\|\\|$")),
    language_codes = str_replace_all(language_codes, "\\|\\|\\s*\\|\\|", "||"),
    language_codes = na_if(language_codes, ""),
    
    # Extract first series entry (before ||)
    series_first = str_split(series, "\\|\\|", simplify = TRUE)[, 1],
    title_original = str_squish(title_original)
  ) %>%
  select(title_original, author_lastname, author_firstname,
         pub_year, language_codes, series_first)

# Summarise at the title level
dnb_title_summary <- dnb_detail %>%
  group_by(title_original, author_lastname, author_firstname) %>%
  summarise(
    earliest_pub = min(pub_year, na.rm = TRUE),
    latest_pub   = max(pub_year, na.rm = TRUE),
    # Editions: entries with no multiple language codes
    n_editions = sum(!is.na(language_codes) & !str_detect(language_codes, "\\|\\|"), 
                     na.rm = TRUE),
    # Translations: count multiple languages in entries
    n_translations = sum(sapply(language_codes, function(x) {
      if (is.na(x)) return(0)
      parts <- str_split(x, "\\|\\|")[[1]]
      max(length(parts) - 1, 0)
    })),
    .groups = "drop"
  ) %>%
  arrange(desc(n_editions), desc(n_translations))

# write.csv(dnb_detail, "dnb_detail.csv")
# Manual check!
dnb_detail <- read.csv("dnb_detail.csv")

#------------------------------------------------------------------------------#
# Summarise school reading data -----------------------------------------------#
#------------------------------------------------------------------------------#

abitur_summary <- df %>%
  mutate(
    states = rowSums(select(., all_of(state_cols)) == 1, na.rm = TRUE),
    title = str_squish(title)
  ) %>%
  group_by(title, author_lastname, author_firstname, pub_year_clean, gender) %>%
  summarise(
    total_states = sum(states, na.rm = TRUE),
    first_abitur_year = min(year, na.rm = TRUE),
    n_years = n_distinct(year),
    .groups = "drop"
  )

#------------------------------------------------------------------------------#
# Join datasets ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

dnb_with_abitur <- merge(dnb_detail, abitur_summary, 
                         by.x = c("title_original", "author_lastname", "author_firstname"), 
                         by.y = c("title", "author_lastname","author_firstname"),
                         all = TRUE)


dnb_with_abitur <- dnb_with_abitur %>%
  mutate(
    literature_period = ifelse(pub_year_clean < 1950, "Classic", "Contemporary")
  )

#------------------------------------------------------------------------------#
# Compute pre-Abitur signals --------------------------------------------------#
#------------------------------------------------------------------------------#

dnb_signals <- dnb_with_abitur %>%
  filter(!is.na(first_abitur_year)) %>%
  mutate(
    pre_abitur = pub_year_clean < first_abitur_year
  ) %>%
  group_by(title_original, author_lastname, author_firstname, literature_period) %>%
  summarise(
    n_pre_abitur_editions = sum(pre_abitur & (is.na(language_codes) | 
                                                !str_detect(language_codes, "\\|\\|")), na.rm = TRUE),
    n_pre_abitur_translations = sum(
      pre_abitur & map_int(language_codes, function(x) {
        if (is.na(x)) return(0)
        parts <- str_split(x, "\\|\\|")[[1]]
        max(length(parts) - 1, 0)
      }) > 0,
      na.rm = TRUE
    ),
    series_pre_abitur = paste(unique(series_first[pre_abitur]), collapse = ", "),
    .groups = "drop"
  )

#------------------------------------------------------------------------------#
# Edition pressure: Are Abitur titles more often reprinted? -------------------#
#------------------------------------------------------------------------------#

df_compare <- dnb_title_summary %>%
  mutate(in_abitur = title_original %in% abitur_summary$title)

pubyears <- unique(subset(dnb_with_abitur, 
                          select = c("title_original", "pub_year_clean", 
                                     "literature_period")))

df_compare <- merge(df_compare, pubyears, 
                    by = "title_original", all.x = TRUE)

df_compare <- df_compare %>%
  mutate(literature_period = recode(literature_period,
                                    "Classic" = "Klassiker",
                                    "Contemporary" = "Zeitgenössische Literatur"))

ggplot(df_compare, aes(x = n_editions)) +
  geom_histogram(
    aes(y = ..count../sum(..count..), fill = literature_period),
    position = "identity",
    colour = "black",       # black outlines
    alpha = 0.6,            # transparency for overlap
    bins = 25
  ) +
  scale_fill_manual(values = c("black", "white")) +
  labs(
    x = "Anzahl der Ausgaben",
    y = "Anteil der Titel",
    fill = "Kategorie"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.key.size = unit(0.8, "lines")
  )

ggsave("img//edition_pressure.png", dpi = 360, width = 16, height = 6, 
       units = "cm")

#------------------------------------------------------------------------------#
# Series gateways: Frequent series before Abitur inclusion --------------------#
#------------------------------------------------------------------------------#

series_signal <- dnb_with_abitur %>%
  filter(!is.na(first_abitur_year) & pub_year < first_abitur_year) %>%
  count(series_first, literature_period, sort = TRUE) %>%
  filter(!is.na(series_first) & series_first != "" & series_first != "nan")

series_signal <- series_signal %>%
  mutate(literature_period = recode(literature_period,
                                    "Classic" = "Klassiker",
                                    "Contemporary" = "Zeitgenössische Literatur"))
ggplot(series_signal[1:15,], 
       aes(x = reorder(series_first, n), y = n, fill = literature_period)) +
  geom_col(position = "dodge", colour = "black", alpha = .6,) +
  coord_flip() +
  labs(
    x = "Reihe (Top 15)",
    y = "Anzahl (vor Aufnahme ins Abitur)",
    fill = "Kategorie"
  ) +
  scale_fill_manual(values = c("black", "white")) +
  theme_minimal() +
  theme(
    legend.position = c(0.7, 0.25),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.key.size = unit(0.8, "lines")
  )

ggsave("img//series_gateways.png", dpi = 360, width = 16, height = 6, 
       units = "cm")

#------------------------------------------------------------------------------#
# Language reach: Translation diversity before Abitur inclusion ---------------#
#------------------------------------------------------------------------------#

lang_diversity <- dnb_with_abitur %>%
  filter(!is.na(first_abitur_year) & pub_year < first_abitur_year) %>%
  mutate(
    langs = map(language_codes, ~ {
      if (is.na(.x)) return(character(0))
      str_split(.x, "\\|\\|")[[1]] %>%
        trimws() %>%
        toupper() %>%
        unique()
    }),
    langs_no_ger = map(langs, ~ .x[!.x %in% c("GER", "DEU", "DE")])
  ) %>%
  group_by(title_original, author_lastname, author_firstname, literature_period) %>%
  summarise(
    all_langs_pre = paste(sort(unique(unlist(langs_no_ger))), collapse = ", "),
    total_langs_pre = n_distinct(unlist(langs_no_ger)),
    .groups = "drop"
  ) %>%
  mutate(
    literature_period = recode(literature_period,
                               "Classic" = "Klassiker",
                               "Contemporary" = "Zeitgenössische Literatur")
  )

ggplot(lang_diversity, aes(x = total_langs_pre)) +
  geom_histogram(
    aes(y = ..count../sum(..count..), fill = literature_period),
    position = "identity",
    colour = "black",       # black borders
    alpha = .6,            # transparency
    bins = 25
  ) +
  scale_fill_manual(values = c("black", "white")) + 
  labs(
    x = "Sprachliche Diversität (bevor ein Text zur Abiturlektüre wird)",
    y = "Anteil der Titel",
    fill = "Kategorie"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.key.size = unit(0.8, "lines")
  )

ggsave("img//lang_diversity.png", dpi = 360, width = 16, height = 6, 
       units = "cm")

#------------------------------------------------------------------------------#
# Decision Trees --------------------------------------------------------------#
#------------------------------------------------------------------------------#

model_data <- abitur_summary %>%
  left_join(dnb_signals,
            by = c("title" = "title_original",
                   "author_lastname", "author_firstname")) %>%
  mutate(literature_period = factor(literature_period, levels = c("Classic", "Contemporary")))

model_data_classic <- model_data %>%
  filter(literature_period == "Classic")

model_data_contemp <- model_data %>%
  filter(literature_period == "Contemporary")

tree_years_classic <- rpart(n_years ~ n_pre_abitur_editions + n_pre_abitur_translations,
                            data = model_data_classic)

tree_years_contemp <- rpart(n_years ~ n_pre_abitur_editions + n_pre_abitur_translations,
                            data = model_data_contemp)

tree_states_classic <- rpart(total_states ~ n_pre_abitur_editions + n_pre_abitur_translations,
                             data = model_data_classic)

tree_states_contemp <- rpart(total_states ~ n_pre_abitur_editions + n_pre_abitur_translations,
                             data = model_data_contemp)

png("img/decisionTree_classics_years.png", res = 360, width = 16, height = 12, 
    units = "cm")

rpart.plot(
  tree_years_classic,
  type = 3,
  extra = 101,
  box.palette = "Greys",
  branch.lty = 1,
  shadow.col = 0,
  split.fun = function(x, labs, digits, varlen, faclen) {
    labs <- gsub("n_pre_abitur_editions", "# von Ausgaben\nvor Aufnahme in Leselisten", labs)
    labs <- gsub("n_pre_abitur_translations", "# von Übersetzungen\nvor Aufnahme in Leselisten", labs)
    return(labs)
  }
)

dev.off()

png("img/decisionTree_contemporary_years.png", res = 360, width = 16, height = 12, 
    units = "cm")

rpart.plot(
  tree_years_contemp,
  type = 3,
  extra = 101,
  box.palette = "Greys",
  branch.lty = 1,
  shadow.col = 0,
  split.fun = function(x, labs, digits, varlen, faclen) {
    labs <- gsub("n_pre_abitur_editions", "# von Ausgaben\nvor Aufnahme in Leselisten", labs)
    labs <- gsub("n_pre_abitur_translations", "# von Übersetzungen\nvor Aufnahme in Leselisten", labs)
    return(labs)
  }
)

dev.off()

png("img/decisionTree_classics_states.png", res = 360, width = 16, height = 12, 
    units = "cm")

rpart.plot(
  tree_states_classic,
  type = 3,
  extra = 101,
  box.palette = "Greys",
  branch.lty = 1,
  shadow.col = 0,
  split.fun = function(x, labs, digits, varlen, faclen) {
    labs <- gsub("n_pre_abitur_editions", "# von Ausgaben\nvor Aufnahme in Leselisten", labs)
    labs <- gsub("n_pre_abitur_translations", "# von Übersetzungen\nvor Aufnahme in Leselisten", labs)
    return(labs)
  }
)

dev.off()

png("img/decisionTree_contemporary_states.png", res = 360, width = 16, height = 12, 
    units = "cm")

rpart.plot(
  tree_states_contemp,
  type = 3,
  extra = 101,
  box.palette = "Greys",
  branch.lty = 1,
  shadow.col = 0,
  split.fun = function(x, labs, digits, varlen, faclen) {
    labs <- gsub("n_pre_abitur_editions", "# von Ausgaben\nvor Aufnahme in Leselisten", labs)
    labs <- gsub("n_pre_abitur_translations", "# von Übersetzungen\nvor Aufnahme in Leselisten", labs)
    return(labs)
  }
)

dev.off()

#------------------------------------------------------------------------------#
# Regression signals: Pre-Abitur publishing vs curricular longevity -----------#
#------------------------------------------------------------------------------#

# Regression without literature period (baseline)
model_fit <- lm(n_years ~ n_pre_abitur_editions + n_pre_abitur_translations,
                data = model_data)
summary(model_fit)

# Regression including literature_period as a categorical predictor
model_fit2 <- lm(n_years ~ n_pre_abitur_editions + n_pre_abitur_translations + literature_period,
                 data = model_data)
summary(model_fit2)

# Optional: include interactions to see if the effect of editions and translations differs by literature period
model_fit3 <- lm(n_years ~ (n_pre_abitur_editions + n_pre_abitur_translations) * literature_period,
                 data = model_data)
summary(model_fit3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#