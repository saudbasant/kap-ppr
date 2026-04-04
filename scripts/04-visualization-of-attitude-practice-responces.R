#load packages----
source("scripts/00-setup.R")
library(ggstats)


#import data----
data <- read_csv(here("data/processed-339/processed339.csv"))


#Visualize attitude response----
attitude_df <- data %>% 
  select(33:43) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(across(everything(), ~ fct_relevel(.x, "Agree", "Neutral", "Disagree"))) |> 
  as.data.frame() 

#build Likert object
gglikert(attitude_df,
         labels_size = 5,         #size of percentage label
         labels_accuracy = .1,     #Decimal level of the labels
         labels_hide_below = F,    #Hiding proportions over bars is disabled
         add_totals = F,           #Remove the totals on left and right side
         y_label_wrap = 40        #Adjust the number of characters per line of the y axis label
         ) +
  # ── Custom bar colors 
  scale_fill_manual(
    values = c(
      "Agree"    = "#1D9E75",   # teal
      "Neutral"  = "#B4B2A9",   # gray
      "Disagree" = "#D85A30"    # coral
    )
  ) +
  # ── Bold percentage labels inside bars
  theme(
    # y-axis item labels
    axis.text.y  = element_text(size = 14, colour = "black"),
    # x-axis percentage labels
    axis.text.x  = element_text(size = 14, colour = "black"),
    # legend text
    legend.text  = element_text(size = 16, face = "bold"),      #Bottom legend
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#export the plot
ggsave(here("outputs/figures/attitudes_likert.tiff"), width = 12, height = 8, 
            dpi = 300, compression = "lzw")

ggsave(here("outputs/figures/attitudes_likert.png"), width = 12, height = 8, 
            dpi = 300)







# Visualize practice response----- 
practice_df <- data |> 
  select(44:50)

#convert to long format
practice_long <- practice_df |> 
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  )
  
  
# Calculate summary stats
library(binom)

summary_data <- practice_long %>%
  group_by(Question, Response) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Question) %>%
  mutate(
    total = sum(n),
    prop = n / total
  ) %>%
  ungroup()


#Create label
plot_data <- summary_data %>%
  mutate(
    label = paste0(
      round(prop * 100, 1), "% (", n, ")"
    )
  )

#ensure consistent stacking
plot_data$Response <- factor(plot_data$Response, levels = c("No", "Yes"))

# Ensure logical order of questions
plot_data$Question <- factor(
  plot_data$Question,
  levels = rev(unique(plot_data$Question))
)
#Adjust the number of characters per line of the y axis label
plot_data$Question <- str_wrap(plot_data$Question, width = 40)


#plot
ggplot(plot_data, aes(x = Question, y = prop, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  
  geom_text(      #text of the summary data
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 5
  ) +
  
  coord_flip() +
  scale_fill_manual(values = c("No" = "#E64B35", "Yes" = "#4DBBD5")) + 
  # ("No" = "#D55E00", "Yes" = "#009E73"): Colorblind-safe
  # ("No" = "#B0BEC5", "Yes" = "#1E88E5"): Blue based
  # ("No" = "#D9D9D9", "Yes" = "#4D4D4D"): Minimalist grayscale (top-tier journals love this)
  # ("No" = "#E64B35", "Yes" = "#4DBBD5") : Nature science
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    x = "Practice Questions",
    y = "Percentage",
    fill = "Response"
  ) +
  
  theme(
    # y-axis item labels
    axis.text.y  = element_text(size = 14, colour = "black"),
    axis.title.y = element_text(size = 14, colour = "black"),
    # x-axis percentage labels
    axis.text.x  = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 14, colour = "black"),
    # legend text
    legend.text  = element_text(size = 16, face = "bold"),      #Bottom legend
    legend.title = element_blank(),
    legend.position = "bottom", 
  
  )


#export the plot
ggsave(here("outputs/figures/practicess.tiff"), width = 12, height = 8, 
       dpi = 300, compression = "lzw")

ggsave(here("outputs/figures/practices2.png"), width = 12, height = 8, 
       dpi = 300)




