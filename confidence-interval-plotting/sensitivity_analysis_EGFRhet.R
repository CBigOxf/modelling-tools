library(reshape2)
library(foreach)
library(doParallel)
library(tidyverse)
library(RColorBrewer)

setwd("C:/Users/pvictori/Buffa/egfr_het/outputs")
exp = "d5.3"
filename = paste(exp,".csv", sep = "")
number_of_groups = 2
lines_to_skip = 31
wexact = FALSE
reverse = FALSE
it = 100

pdiffs = read_csv(filename, skip = 10, n_max = 25, col_names = FALSE)
pdiffs = as.data.frame(t(pdiffs))
pdiffs = pdiffs[,c(4,8)]
pdiffs = pdiffs[complete.cases(pdiffs), ]


df = read_csv(filename, skip = lines_to_skip, n_max = 5, col_names = FALSE)
df = read.csv(filename, skip = lines_to_skip, header = TRUE)
df = df[-1]
dfr = df[, grep("^c", colnames(df))]
dfr$num = 1:nrow(dfr)
melted = melt(dfr,id.vars = "num")

#generate list of regex for each group
regexv = c("EGFRhi", "EGFRlo")

#create a fourth column to group the 100 repetitions of each mutation together 
for(i in regexv){
  melted[grep(i, x = melted$variable),4] = i
}

melted$variable = gsub("count.objects.with..my.mutation.group...", "", melted$variable)
colnames(melted) = c("timepoint", "exp", "value", "group")

#all groups with confidence intervals
#a list with a dataframe for each mutation group
glist = split(melted, f = melted$group)

#Parallel loop
#setup parallel backend
cl = makeCluster(number_of_groups)
registerDoParallel(cl)

glistConf = foreach(i=1:number_of_groups) %dopar% {
  dfi = glist[[i]]
  dfi = dfi[, -4]
  dfi$exp = substr(dfi$exp, 1, 1)
  
  tpList = list() #a list to be converted in a df of timepoints
  for(j in 1:it){
    t = dfi$value[dfi$timepoint == j]
    
    if(length(unique(t)) == 1){ # special case in which the test cannot be computed because all observations are equal - confidence interval will have length 0
      confi = rep.int(t[[1]],2)
      estimate = t[[1]]
    }
    else{
      w = wilcox.test(t, conf.int = TRUE, conf.level = 0.95, exact = wexact)
      confi = w$conf.int
      estimate = w$estimate
    }
    
    #only one entry per timepoint - consolidate all 100 repetitions in one pseudomedian and one confidence interval
    vec = numeric(5)
    vec[1] = j
    vec[2] = dfi$exp[1]
    vec[3] = estimate
    vec[4] = confi[1]
    vec[5] = confi[2]
    tpList[[j]] = vec
  }
  as.data.frame(do.call("rbind", tpList), stringsAsFactors = FALSE)
}

unipm = as.data.frame(do.call("rbind", glistConf), stringsAsFactors = FALSE)
unipm = as.data.frame(lapply(unipm, as.numeric), stringsAsFactors = FALSE)
unipm$V2 = as.factor(unipm$V2)

col = c("#f39237", "#1c77c3")
leg_labels = c("EGFRhi", "EGFRlo")

if(reverse){
  col = c("#1c77c3", "#f39237")
  labels = c("EGFRlo", "EGFRhi")
}

p = ggplot(unipm, aes(x=V1, y = V3, group = V2))
p = p + scale_color_manual(labels = leg_labels, values = col)
p = p + scale_fill_manual(labels = leg_labels, values = col)
p = p + geom_ribbon(aes(ymin = V4, ymax = V5, fill = V2), alpha = .3)
p = p + geom_line(aes(color = V2))
p = p + labs(title = "Growth confidence interval", x = "time point", y = "number of cells", color = "Subpopulation", fill = "Subpopulation")
p = p + theme_minimal()
p = p + theme(axis.text.x = element_text(size=12))
p = p + theme(axis.text.y = element_text(size=12))
p = p + theme(axis.title.x = element_text(size=16, face = "bold"))
p = p + theme(axis.title.y = element_text(size=16, face = "bold"))

png(paste(exp, ".png", sep = ""), width = 6000, height = 3000, type = "cairo-png", res = 400)
plot(p)
dev.off()

#print time and total execution time 
endTime = Sys.time() - startTime
print(paste("Script analysisComp finished executing at: ", Sys.time(), "and took ", endTime, " minutes"))
