#Libraries required
library(cowplot) 
library(ggplot2)
library(lme4)
library(emmeans)

data_1 <- read.csv("data/data_1.csv")

t_wingLength <- t.test(data_1$WingLength ~ data_1$Density)

print(t_wingLength)

print("--------------------------------------------")

t_weight <- t.test(data_1$Weight ~ data_1$Density)

print(t_weight)

#---------------------------------------------------------------------------
data_2 <- read.csv("data/data_2.csv")

plot_scatter_density <- function(dt, x1, y1, xcolor, xlim, ylim, x_label, s_colors){
  
  # Main plot
  pmain <- ggplot(dt, aes_string(x = x1, y = y1, color = xcolor))+
    geom_point(aes_string(color=xcolor), size=5)+
    expand_limits(x=xlim, y=ylim)+
    scale_color_manual(values=s_colors, labels = c("High larval density", "Low larval density"))+
    scale_fill_manual(values=s_colors,
                      breaks=c("Low", "High"),
                      labels = c("High larval density", "Low larval density"))+
    geom_smooth(method=lm, color="black", alpha=0.2)+
    labs(x=x_label, y = "Wingbeat frequency (Hz)")+
    theme(legend.text=element_text(size=28))+
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_line(size = 0.5, linetype = 'solid')
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,text = element_text(size=35),
      strip.background=element_rect(fill="black"),
      legend.position="bottom",
      legend.title = element_blank(),
      legend.background = element_blank(),
      axis.line.x = element_line(size=0.5),
      axis.line.y = element_line(size=0.5),
      plot.margin = margin(3.2,2,2,2, "cm")
    )
  # Marginal densities along x axis
  xdens <- axis_canvas(pmain, axis = "x")+
    geom_density(data = dt, aes_string(x = x1, fill = xcolor),
                 alpha = 0.7, size = 0.5)+
    scale_fill_manual(values=rev(s_colors),
                      breaks=c("Low", "High"),
                      labels = c("Low larval density", "High larval density"))
  
  # Marginal densities along y axis
  # Need to set coord_flip = TRUE, if you plan to use coord_flip()
  ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
    geom_density(data = dt, aes_string(x = y1, fill = xcolor),
                 alpha = 0.7, size = 0.5)+
    scale_fill_manual(values=rev(s_colors),
                      breaks=c("Low", "High"),
                      labels = c("Low larval density", "High larval density"))+
    coord_flip()
  
  p1n <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
  p2n<- insert_yaxis_grob(p1n, ydens, grid::unit(.2, "null"), position = "right")
  
  print(ggdraw(p2n))
}
s_colors_female <- c("pink3", "lightsteelblue")

# Getting the mean weight and wing lenght for each mosquito
dtx <- as.data.frame(aggregate(list(data_2[,c("Temperature","Wbf")]), by=list(data_2$Mosquito_ID, 
                                                                              data_2$Density, 
                                                                              data_2$Weight, 
                                                                              data_2$WingLength), FUN=mean))
names(dtx) <- c("Mosquito","Density", "Weight", "WingLength", "Temperature","Wbf")


plot_scatter_density(dtx, "Weight", "Wbf", "Density", c(0.18, 1.9), c(500, 605), "Dry weight (mg)",s_colors =  s_colors_female)
plot_scatter_density(dtx, "WingLength", "Wbf", "Density", c(2.3, 3.15), c(500, 605), "Wing length (mm)",s_colors =  s_colors_female)
#---------------------------------------------------------------------------


lm_WL <- lm(Wbf ~ WingLength, data=dtx)
print(lm_WL)
print(summary(lm_WL))

print("----------------------------------------------------------")

lm_W <- lm(Wbf ~ Weight, data=dtx)
print(lm_W)
print(summary(lm_W))


#---------------------------------------------------------------------------
mod_ancova <- aov(Wbf ~ WingLength*Weight + Density, data=dtx)
print(summary(mod_ancova))


#---------------------------------------------------------------------------

print(t.test(data_2$Wbf ~ data_2$Density)) 



#---------------------------------------------------------------------------
re <- read.csv("data/data_3.csv")
vcol <- c("dodgerblue2",
          "lightsalmon2",
          "lightsteelblue",
          "plum3",
          "olivedrab3",
          "black",
          "darkorchid",
          "lightyellow4",
          "orangered",
          "green3","#597DBE", "#76BF72", "#AF85BE", "#C76E6E",
          "#f2c25a", "#597a17", "#0be66a","navajowhite2", "bisque1",
          "chocolate", "pink3", "magenta", "thistle1",
          "yellowgreen","gray84", "lavender", "yellow", "lightpink2",
          "mediumpurple", "lightsteelblue3", "#e8a23a", "#3ae894", "#2fc47c",
          "#a9f5d0", "#548DCB", "#F0CB52"
)

cols <- c("Low larval density" = vcol[35], "High larval density"=vcol[36])

re$Density <- factor(re$Density, levels = c("Low larval density", "High larval density"))

p1 <- ggplot(re, aes(x = Range, y = Wbf,fill=Density))+
  geom_boxplot(alpha=0.7, lwd=1.5)+
  xlab("Temperature") +
  ylab("Wingbeat frequency (Hz)") +
  #scale_color_manual(name="", labels=c("Large", "Small"))+
  #scale_color_grey()+
  #scale_fill_grey()+
  scale_fill_manual(values=cols,
                    breaks=c("Low larval density", "High larval density"),
                    labels = c("Low larval density", "High larval density"))+
  theme_bw()+
  guides(fill=guide_legend(reverse = TRUE))+
  theme(
    plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank(),
    text = element_text(size=30),
    legend.position = "bottom",#c(0.8, 0.176), 
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.line.x = element_line(size=.8),
    axis.line.y = element_line(size=.8),
    legend.key.width = unit(1.5,"cm"),
    legend.key.height= unit(2,"cm"),
    axis.ticks = element_line(size = 1)
  ) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

print(p1)

#---------------------------------------------------------------------------

re <- read.csv("data/data_4.csv")

re$Tray <- as.factor(re$Tray)
re$Density <- as.factor(re$Density)
re$Range <- as.factor(re$Range)

print("==============================================================")

model <- lmer(Wbf ~ Range + Density + Range:Density + (1|Tray), data=re)  

print(summary(model))
print("----------------------------------------------------")
print(anova(model))
print("----------------------------------------------------")
print(emmeans(model, pairwise ~ Range + Density + Range:Density, adjust = "tukey"))





