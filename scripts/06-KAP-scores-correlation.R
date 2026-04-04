#load packages----
source("scripts/00-setup.R")
library(ggcorrplot)


#import data----
data <- read_csv(here("data/processed-339/coded339.csv"))

#create data for correlation
corr_data <-data |> 
  select(33,46,57)

#calculate the correlation matrix
corr_matrix <- cor(corr_data, 
                   use = "pairwise.complete.obs", 
                   method = "spearman")  #KAP scores are not normally distributed, checked in quality check step

#Add Significance Levels
p_matrix <- cor_pmat(corr_data)


#visualize 
#change the corrplot function as described below
trace(corrplot, edit=TRUE)

# # Line-263
# # ORIGINAL - draws white background
# symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1,
# len.DAT, 2), bg = bg, fg = bg)

# # CHANGE bg and fg from `bg` to match your cell colors
# symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1, 
# len.DAT, 2), bg = col.fill, fg = col.fill)  # ← use col.fill

#Line-449
# # adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#        labels = point, col = pch.col, cex = pch.cex, 
#        lwd = 2)
  

M<-cor(corr_data)
res1 <- cor.mtest(corr_data, 
                  conf.level = .95,
                  method = "spearman")

png(filename=here('outputs/figures/KAP-correlation-plot.png'), 
    res=900, units = 'cm',
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
         addCoef.col = "yellow",  # add correlation values
         diag = T,
         outline = F
         )


dev.off() #to close the export process



# =============================================================================
# CORRELATION PLOT - Knowledge, Attitude, and Practice (KAP) Scores
# =============================================================================
# PURPOSE: Creates a lower-triangle correlation matrix plot with significance
#          stars and correlation coefficients, exported as high-res PNG
# REQUIRES: corrplot package with manual trace edits (see instructions below)
# =============================================================================

# STEP 1: Calculate Pearson correlation matrix ----------------------------
# Uses pairwise complete observations to handle missing data
corr_matrix <- cor(corr_data, use = "pairwise.complete.obs", 
                   method = "pearson")

# STEP 2: Calculate significance (p-value) matrix -------------------------
p_matrix <- cor_pmat(corr_data)

# STEP 3: Edit corrplot internals (run ONCE per session) ------------------
# Opens an interactive editor to patch two lines in the corrplot source:
#
# EDIT 1 — Line ~263: Remove white background margins around squares
#   ORIGINAL:
#     symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1,
#       len.DAT, 2), bg = bg, fg = bg)
#   REPLACE WITH:
#     symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1,
#       len.DAT, 2), bg = col.fill, fg = col.fill)
#   WHY: The original draws a white rectangle behind each cell; changing
#        bg/fg to col.fill makes the background match the cell color,
#        eliminating white margins between squares.
#
# EDIT 2 — Line ~449: Adjust vertical position of significance stars
#   ORIGINAL:
#     text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs],
#          labels = point, col = pch.col, cex = pch.cex, lwd = 2)
#   REPLACE WITH:
#     text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs]) + 0.25,
#          labels = point, col = pch.col, cex = pch.cex, lwd = 2)
#   WHY: +0.25 shifts stars upward so they don't overlap the correlation
#        coefficient values displayed in the lower half of each cell.
#
trace(corrplot, edit = TRUE)

# STEP 4: Prepare correlation and p-value matrices ------------------------
M     <- cor(corr_data)                        # Correlation matrix
res1  <- cor.mtest(corr_data, conf.level = .95) # Significance test results

# STEP 5: Open PNG device for export --------------------------------------
# Saves to outputs/figures/ folder relative to project root (via here())
# Resolution: 900 dpi | Size: 10cm x 10cm
png(filename = here("outputs/figures/KAP-correlation-plot.png"),
    res   = 900,
    units = "cm",
    width = 10,
    height = 10)

# STEP 6: Draw the correlation plot ---------------------------------------
corrplot(M,
         method   = "square",      # Filled square tiles for each cell
         type     = "lower",       # Show lower triangle only
         p.mat    = res1$p,        # Supply p-value matrix for significance
         insig    = "label_sig",   # Label significant cells with stars (* ** ***)
         sig.level = c(.001, .01, .05), # Thresholds: *** p<.001, ** p<.01, * p<.05
         pch.cex  = 1.8,           # Size of significance stars
         number.cex = 0.9,         # Size of correlation coefficient text
         pch.col  = "green",       # Color of significance stars
         tl.col   = "black",       # Color of variable name labels
         tl.cex   = 0.7,           # Size of variable name labels
         tl.srt   = 90,            # Rotate top labels 90 degrees
         addCoef.col = "yellow",   # Color of correlation coefficients in cells
         diag     = TRUE,          # Include diagonal (self-correlations = 1.00)
         outline  = FALSE          # No border outline around squares
)

# STEP 7: Close the PNG device and save the file --------------------------
dev.off()



##METAN
library(metan)
#create data for correlation
corr_data <-data |> 
  select(37,50,61)

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


