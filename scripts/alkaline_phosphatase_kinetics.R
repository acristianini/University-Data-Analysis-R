library(ggplot2)
library(drc) 
# load packages

View(alp_data_control)
table_1 <- alp_data_control
View(alp_data_low_inhibitor)
table_2 <- alp_data_low_inhibitor
View(alp_data_high_inhibitor)
table_3 <- alp_data_high_inhibitor
#assigning variables to imported data sets from excel /\

#table 1 (CONTROL) is based on Table 1: Effect of substrate concentration on alkaline phosphatase activity.
#table 2 (LOW) is based on Table 2: Inhibition of alkaline phosphatase from calf intestine by low [inhibitor].
#table 3 (HIGH) is based on Table 3: Inhibition of alkaline phosphatase from calf intestine by high [inhibitor].

model.drm <- drm(v ~ PNPP, data = table_1, fct = MM.2())
summary(model.drm) # 'parameters that are estimated: 'd' (= V) and 'e' (= K) parameters
mm <- data.frame(PNPP = seq(0, max(table_1$PNPP), length.out = 100))
#creating a sequence of data for PNPP
mm$v <- predict(model.drm, newdata = mm)
#use created sequence of data to predict v
View(mm)
#control data /\

model.drm.low <- drm(v ~ PNPP, data = table_2, fct = MM.2())
summary(model.drm.low)
mml <- data.frame(PNPP = seq(0, max(table_2$PNPP), length.out = 100))
mml$v <- predict(model.drm.low, newdata = mml)
View(mml)
#same repeated for low data /\

model.drm.high <- drm(v ~ PNPP, data = table_3, fct = MM.2())
summary(model.drm.high)
mmh <- data.frame(PNPP = seq(0, max(table_3$PNPP), length.out = 100))
mmh$v <- predict(model.drm.high, newdata = mmh)
View(mmh)
#same repeated for high data /\

my_legend_text <- "Figure.1: Michaelis-Menten analysis of alkaline phosphatase activity at varying substrate concentrations, inhibited by two distinct concentrations of inhibitor. Each reaction mixture (2.5ml) contained 0.1M ethanolamine buffer (pH 10), a constant concentration (0.08U/ml final) of
Alkaline Phosphatase (ALP) enzyme and varying concentrations of p-nitrophenyl phosphate (PNPP) substrate (between 0 and 2000μM). For the inhibition assays, 0.5ml of 1.5mM (low) or 3.0 (high) inhibitor were added. The rate of change of absorbance was measured using a spectrophotometer
over the initial 30 seconds of the reaction.
Key: markers represent observed data points; solid lines represent Michaelis-Menten non-linear regression fits. Black: Control (no inhibitor); Blue: Low inhibitor; Red: High inhibitor.
Results: as the concentration of inhibitor increased, the Km of the reaction increased as well (indicated by decreased gradient of non-linear regression fits) while the Vmax remained the same. This is characteristic of competitive inhibition."


ggplot(table_1, aes(x = PNPP, y = v)) + #adding control data points on plot (table 1)
  geom_point(colour = 'black', alpha = 0.5) + #Colour of points is set to black, transparency (alpha) is set to 0.5
  geom_line(data = mm, aes(x = PNPP, y = v), #prediction line/ standard curve added here. 
            colour = 'black') + #colour of line is set to black
  geom_point(data = table_2, aes(x = PNPP, y = v), #adding low [inhibitor] data points on plot (table 2)
             colour = 'blue', #colour of low [inhibitor] points set to blue
             alpha = 0.5) +  #transparency of points set to 0.5
  geom_line(data = mml, aes(x = PNPP, y = v), #prediction line/ standard curve added here.
            colour = "blue") + #Colour of line set to blue
  geom_point(data = table_3, aes(x = PNPP, y = v), #adding high [inhibitor] data points on plot (table 3)
             colour = 'red', #colour of high [inhibitor] points set to red
             alpha = 0.5) + #transparency of points set to 0.5
  geom_line(data = mmh, aes(x = PNPP, y = v), #prediction line/ standard curve added here.
            colour = 'red') + #Colour of line set to red
  theme_classic() + #theme of graph set to classic
  xlab("[PNPP] final (μM)") + # x axis title
  ylab("v (Change in A/min)") + # y axis title
  ggtitle("Michaelis-Menten analysis of alkaline phosphatase activity at varying substrate concentrations, inhibited by two distinct concentrations of inhibitor.") +
  labs(caption = my_legend_text) +
  theme(plot.caption = element_text(size = 8, hjust = 0)) # adds figure legend (defined on code line 40), adjusts font size to 8 and makes the figure legend left alligned (start of the figure legend text is anchored to the left side of the plot)

  



