#Load requied packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, gt, gtsummary, cardx, easystats, broom.helpers,
               metan, ggplot2)

#Correlation between KAP----
# import data
data <- read_csv(here("clean_data/KAP_reg_final.csv")) #check.names = FALSE, to avoid dots in between variable names

names(data)

#create data for correlation
corr_data <-data |> 
  select(28,41,51)

view(corr_data)

#calculate corr coefficient and store the output in a variable
M <- corr_coef(corr_data)

#plot the output (Option A)
plot(M)
M



# Save a plot as TIFF
ggsave("R/SAURES_2023//FMD/Figures/correlation_plotFMD.tiff", units="in", 
       width=5, height=2, dpi=900, compression = 'lzw')

ggsave("R/SAURES_2023/FMD/Figures/correlation_plotFMD.tiff", 
       width = 10, height = 6, dpi = 300, units = "in")
dev.off()

#export the correlation chart as png
png(filename='figures/correlation_plot.png', res=1200, units = 'cm',
    width = 15, height = 10)
plot(M)
dev.off() #to close the export process








