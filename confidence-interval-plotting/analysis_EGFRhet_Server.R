library(reshape2)
library(foreach)
library(doParallel)

startTime = Sys.time()
print(paste("Script analysisComp started executing at: ", startTime))
args = commandArgs(trailingOnly=TRUE)

exp = args[1]
filename = paste(exp,".csv", sep = "")
number_of_groups = as.numeric(args[2])
lines_to_skip = 33
wexact = FALSE
if(length(args) == 3){
  if(args[3] == "exact"){
    wexact = TRUE
  }
  else{
    lines_to_skip = as.numeric(args[3])
  }
}

if(length(args) == 4){
  lines_to_skip = as.numeric(args[4])
}

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
  for(j in 1:2001){
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
save(unipm, file = "unipm.RData")

#print time and total execution time 
endTime = Sys.time() - startTime
print(paste("Script analysisComp finished executing at: ", Sys.time(), "and took ", endTime, " minutes"))