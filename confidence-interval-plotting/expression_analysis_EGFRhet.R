library(reshape2)
library(ggplot2)
library(RColorBrewer)


setwd("D:/universidad/Buffa/egfr_het/outputs")
setwd("C:/Users/pvictori/Buffa/egfr_het/outputs")
exp = "d12ex"
filename = paste(exp,".csv", sep = "")
number_of_groups = 2
lines_to_skip = 34
reverse = FALSE

df = read.csv(filename, skip = lines_to_skip, header = TRUE)
dfr = df[2:6]
dfr$num = 1:nrow(dfr)
colnames(dfr) = c("EGFRhi", "EGFRlo", "total", "EGFR", "gefitinib", "timepoint")
dflines = dfr[c(1,2,6)]
dfareas = dfr[3:6]
mlines = melt(dflines,id.vars = "timepoint")
mareas = melt(dfareas,id.vars = "timepoint")
mareas$variable = factor(mareas$variable, levels = c("total", "gefitinib", "EGFR")) #change order of factors
l_col = c("#f39237", "#1c77c3")
l_leg_labels = c("EGFRhi", "EGFRlo")

if(reverse){
  l_col = c("#1c77c3", "#f39237")
  l_labels = c("EGFRlo", "EGFRhi")
}
a_col = c("#c7d59f", "#f5dd90", "#daddd8")
a_leg_labels = c("total cells", "gefitinib active", "EGFR active")

p = ggplot(mareas, aes(x=timepoint))
p = p + scale_color_manual(labels = l_leg_labels, values = l_col)
p = p + scale_fill_manual(labels = a_leg_labels, values = a_col)
p = p + geom_area(position = "identity", aes(y = value, group = variable, fill = variable))
p = p + geom_line(data = mlines, aes(y = value, group = variable, color = variable))
p = p + labs(title = "Cell growth and expression", x = "time point", y = "number of cells", color = "Subpopulation", fill = "Expression")
p = p + coord_cartesian(ylim=c(0,400)) #for comparison with other plots
p = p + theme_minimal()
p = p + theme(axis.text.x = element_text(size=12))
p = p + theme(axis.text.y = element_text(size=12))
p = p + theme(axis.title.x = element_text(size=16, face = "bold"))
p = p + theme(axis.title.y = element_text(size=16, face = "bold"))

png(paste(exp, ".png", sep = ""), width = 6000, height = 3000, type = "cairo-png", res = 400)
plot(p)
dev.off()
