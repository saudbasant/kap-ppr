#Load requied packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, gt, gtsummary, cardx, easystats, broom.helpers,
              corrplot)

#Correlation between KAP----
# import data
data <- read_csv(here("clean_data/KAP_reg_final.csv")) #check.names = FALSE, to avoid dots in between variable names

names(data)

#create data for correlation
corr_data <-data |> 
  select(28,41,51)

#calculate the correlation matrix
corr_matrix <- cor(corr_data, use = "pairwise.complete.obs", 
                   method = "pearson")

#Add Significance Levels
p_matrix <- cor_pmat(corr_data)


#visualize 
#change the corrplot function as described above 
trace(corrplot, edit=TRUE)
  

M<-cor(corr_data)
res1 <- cor.mtest(corr_data, conf.level = .95)

png(filename='figures/correlation_plot.png', res=900, units = 'cm',
    width = 10, height = 10)

corrplot(M,
         method="square",       # square tiles
         type="lower",          # lower triangle only
         p.mat = res1$p,        # p-values matrix
         insig = "label_sig",    # label significant cells with stars
         sig.level = c(.001, .01, .05),  # levels for significance
         pch.cex = 1.8,  # size of stars
         number.cex = 0.9,       # correlation value size
         pch.col = "green",  # color of significance stars
         tl.col="black",     # text label color
         tl.cex=0.7,
         tl.srt = 90,
         addCoef.col = "white",  # add correlation values
         diag = T,
         outline = F,
         )


dev.off() #to close the export process
