#############################
## CODE UNI DUISBURG-ESSEN ##
#############################

# remotes::install_github("hrbrmstr/waffle")

librarian::shelf(tidyverse, readxl, countrycode, ggtext, MetBrewer, scales, waffle)

## GLOBAL INEQUALITY (WORLD INEQUALITY REPORT 2022)

df <- tibble(group = fct_inorder(c("Untere 50%", "Mittlere 40%", "Oberste 10%")),
       'Bevölkerungsanteil' = c(50,40,10),
       'Einkommensanteil' = c(9,39,52),
       'Vermögensanteil' = c(2,22,76)) |> 
  pivot_longer(-group, names_to = "cat", values_to = "value")

df |> ggplot() +
  geom_waffle(aes(fill = group, values = value),
              color = "white", size=0.9, n_rows = 5) +
  facet_wrap(~cat, ncol = 1) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = met.brewer("Juarez")) +
  coord_equal() +
  labs(
    title = "Globale Verteilung von Einkommen und Vermögen",
    caption="Quelle: World Inequality Report 2022. Grafik: @matschnetzer"
  ) +
  theme_minimal(base_family = "Cabinet Grotesk") +
  theme(strip.text.x = element_text(size = 12, hjust = 0, 
                                    margin = margin(b = .3, t = .5, unit = "lines")),
        plot.caption = element_text(size = 8, margin = margin(t = 1, unit = "lines")),
        plot.title = element_text(size = 16, hjust = 0.5,
                                  margin = margin(b = 1, unit = "lines")),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("wealthshares_global.png", width = 6, height = 6, dpi=320, bg = "white")



# WEALTH AT SELECTED PERCENTILES

# Load original data from ECB: https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_Tables_Wave_2021_July2023.zip 
hfcs <- readxl::read_xlsx("HFCS _Statistical_Tables _Wave_2021_July_2023.xlsx", 
                          sheet = "J3 Net wealth per household ", range = "A4:Z24") |> 
  janitor::clean_names() |> 
  rename(measure = x1) |> select(-x2, -x3) |> 
  filter(measure %in% c("Mean", "p20", "p50", "p80")) |> 
  mutate(across(-measure, as.numeric))

# Alternatively load RData file
load("hfcs_2021.RData")

# Data into long format with "pivot_longer", capitalize country codes with "toupper" and merge country names from countrycode package with "left_join"
findat <- hfcs |> 
  pivot_longer(-measure, names_to = "country", values_to = "value") |> 
  mutate(across(c(country, measure), toupper)) |> 
  left_join(countrycode::codelist |> select(iso2c, country.name.en), 
            by = c("country" = "iso2c")) |> 
  mutate(country.name.en = ifelse(country == "EURO_AREA", "Euro Area", country.name.en),
         country.name.en = fct_reorder(country.name.en, value))

findat |> 
  ggplot(aes(x = country.name.en, y = value, group = country, color = measure)) +
  geom_line(color = "gray90", alpha = 0.15, linewidth = 2) +
  geom_point(size = 2) +
  scale_color_manual(values = met.brewer("Isfahan2"), 
                     guide = guide_legend(override.aes = list(size = 4))) +
  scale_y_continuous(labels = scales::number_format(prefix = "€", suffix = "K", 
                                                    big.mark = ",")) +
  coord_flip() +
  labs(x = NULL, y = NULL, color = NULL, 
       title = "Who are the <span style='color:gold;'>**richest**</span> Europeans?",
       subtitle = "Percentiles of net wealth distributions in thousand Euros",
       caption = "Data: HFCS 2017, ECB. Figure: @matschnetzer") + 
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.background = element_rect(fill = "black"),
        plot.margin = margin(t = 1, b = 1, l = 1, r = 1, unit = "lines"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 20),
        plot.subtitle = element_text(margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(margin = margin(t = 2, unit = "lines"), size = 7),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.1, color = "gray80"),
        legend.position = c(0.7, 0.3),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 8))

ggsave("wealth.png", width = 6, height = 5, dpi = 320)
