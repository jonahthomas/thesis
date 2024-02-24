#Frag requires requires an adequately current version of GGIR

library(GGIR)
library(GENEAread)
 
## Not run:
mode= c(1:5)
datadir= "C:/Users/jjct8/acutesnack_axivity" #Where is the data
outputdir= "C:/Users/jjct8/OneDrive - Loughborough University/Documents - Jonah PhD Documents/acute_study/data/axivity" #Where do you want to store the data?
studyname="Snacktivity WP2"

GGIR(#-------------------------------
# General parameters
#-------------------------------
mode=mode,
datadir=datadir,
outputdir=outputdir,
studyname=studyname,
overwrite = FALSE,
do.imp=TRUE,
idloc=1,
print.filename=TRUE,
storefolderstructure = FALSE,
#-------------------------------
# Part 1 parameters:
#-------------------------------
windowsizes = c(5,900,3600), #5 s epoch, 15 min non-wear detection resolution, 60 min non-wear evaluation window
do.cal=TRUE,
do.enmo = TRUE, #ENMO physical activity metric
do.anglez=TRUE, # for sleep analysis
do.anglex=TRUE,
do.angley=TRUE,
chunksize=1,
printsummary=TRUE,
#-------------------------------
# Part 2 parameters:
#-------------------------------
strategy = 1, #see tutorial for explanation of strategies
ndayswindow=7,
hrs.del.start = 0,
hrs.del.end = 0,
maxdur = 14,
includedaycrit = 0, # min valid hrs per day
M5L5res = 10,
winhr = c(c(960/60),c(840/60),c(720/60),c(600/60),c(540/600),c(480/60),c(360/60),c(300/60),c(240/60),c(120/60),c(60/60),c(30/60),c(15/60),c(10/60),c(5/60)),# - this is the most active  16h, 14h, 12h, 10h, 9h, 8 h, 6h, 5h, 4h, 2h, 1h, 30, 15, 10, 5 min for continuous windows
qM5L5 = c(0.25,0.5,0.75),#to give percentiles for continuous MXLX variables. This is the 25th, 50th, 75th %iles
iglevels = TRUE,
qlevels = c(c(480/1440),c(600/1440),c(720/1440),c(840/1440),c(960/1440),c(1080/1440),c(1200/1440),c(1320/1440),c(1380/1440),c(1395/1440),c(1410/1440),c(1420/1440),c(1425/1440),c(1430/1440),c(1435/1440),c(1438/1440),c(1439/1440)), #quantiles to calculate -  this is the most active 16h, 14h, 12h, 10h, 8h, 6h, 4h, 2h, 60, 45, 30, 20, 15, 10, 5, 2, 1 min accumulated across day
qwindow=c(0,24), #window for calculation of quantiles  
ilevels = c(seq(0,9,by=1),seq(10,95,by=5),seq(100,975,by=25),seq(1000,2000,by=100),8000), #gives acceleration distribution by increments defined 
mvpathreshold =c(100,250,400), #thresholds for MVPA, total and bouts 1, 5 & 10 min acceleration>threshold for >80% of time d=or Hildebrand moderate (100) and vigorous thresholds (400) and for brisk walking (250)
boutcriter = c(0.8),
mvpadur = c(1,5,10),
bout.metric = 6,
#-------------------------------
# Part 3 parameters:
#-------------------------------
timethreshold= c(5),
anglethreshold=5,
ignorenonwear = TRUE,
#-------------------------------
# Part 4 parameters:
#-------------------------------
excludefirstlast = FALSE,
includenightcrit = 16,
def.noc.sleep = c(1),
#loglocation -"E:/sleep_log.csv",
outliers.only = FALSE,
criterror = 4,
relyonsleeplog = FALSE,
sleeplogidnum = FALSE,
colid=1,
coln1=2,
do.visual = TRUE,
nnights = 8,
#------------------------------- 
# Part 5 parameters: 
#------------------------------- 
# Key functions: Merging physical activity with sleep analyses 
threshold.lig = c(40), 
threshold.mod = c(100), 
threshold.vig = c(400), 
boutcriter = 0.8, 
boutcriter.in = 0.9, 
boutcriter.lig = 0.8, 
boutcriter.mvpa = 0.8, 
boutdur.in = c(10,20,30), 
boutdur.lig = c(1,2,5,10), 
boutdur.mvpa = c(1,2,5,10), 
timewindow = c("MM","WW"), 
frag.metrics="all", # New frag metrics to inspect
part5_agg2_60seconds=TRUE, # Dependency for frag.metrics
#-----------------------------------
# Report generation
#-------------------------------
do.report=c(2),
visualreport=TRUE,
dofirstpage = TRUE,
viewingwindow=1)
## End(Not run)
