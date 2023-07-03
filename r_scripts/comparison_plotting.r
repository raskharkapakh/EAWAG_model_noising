

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Init ----
Sys.setenv(LANG="EN")
set.seed(13)   # Always set seed to a lucky number
getwd()        # Show working directory. It needs to be the location of 'main.r'
rm(list=ls())  # Free work space
graphics.off() # Clean graphics display


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Libraries ----
if (!require("dplyr")){install.packages("dplyr"); library("dplyr")}
if (!require("readr")){install.packages("readr"); library("readr")}

# to compute AUC
if (!require("pROC")){install.packages("pROC"); library("pROC")}                       

# For Neural Network
if (!require("reticulate")){install.packages("reticulate"); library("reticulate")}
#install_miniconda()              # run this the very first time reticulate is installed
#install.packages("tensorflow")
library("tensorflow")
#install_tensorflow()             # run this line only when opening new R session
#install.packages("keras")
library("keras")
#install_keras()                  # run this line only when opening new R session
#use_condaenv()

# caret has to be loaded at the end to not cache function 'train'
if (!require("caret")){install.packages("caret"); library("caret")}

if (!require("jsonlite")){install.packages("jsonlite"); library("jsonlite")}



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Custom R scripts ----
source("ml_models.r")
source("plotting.r")
source("performances_assessment.r")
source("global_variables.r")


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Global variables ----
list.experiences <- list("baseline"          = "experiment_no_noise_29_06_2023_18h13",
                         "missdetection"     = "experiment_miss_gammaridae_50_01_07_2023_03h37",
                         "gaussian noise"    = "experiment_temp_5_30_06_2023_05h53",
                         "less datapoints"   = "experiment_subset_500_01_07_2023_11h23")

list.exp.gauss   <- list("baseline"          = "experiment_no_noise_29_06_2023_18h13",
                         "low noise"         = "experiment_temp_1_29_06_2023_20h44",
                         "mid noise"         = "experiment_temp_3_29_06_2023_23h07",
                         "high noise"        = "experiment_temp_5_30_06_2023_05h53")

list.exp.miss    <- list("baseline"          = "experiment_no_noise_29_06_2023_18h13",
                         "low noise"         = "experiment_miss_gammaridae_10_30_06_2023_13h02",
                         "mid noise"         = "experiment_miss_gammaridae_25_30_06_2023_19h54",
                         "high noise"        = "experiment_miss_gammaridae_50_01_07_2023_03h37")

list.exp.subset  <- list("baseline"          = "experiment_no_noise_29_06_2023_18h13",
                         "low noise"         = "experiment_subset_2000_01_07_2023_16h52",
                         "mid noise"         = "experiment_subset_1000_01_07_2023_13h28",
                         "high noise"        = "experiment_subset_500_01_07_2023_11h23")

dir.input       <- "../input_data/"
dir.output      <- "../output_data/"
file.prev.taxa  <- "All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv"
prev.taxa       <- read.csv(paste0(dir.input, file.prev.taxa),
                            header = T, 
                            sep = ",", 
                            stringsAsFactors = F)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Fig 1: multi ICE ----

env.fact.under.obs <- "temperature"
taxon.under.obs <- "Occurrence.Gammaridae"

multi.ice <- lapply(list.exp.miss, FUN=function(name){
                     
  dir.experiment          <- paste0(dir.output, name, "/")
  
  models.fit      <- load.models(path=dir.experiment, split.type="FIT")
  std.const.fit   <- readRDS(file=paste0(dir.experiment, "standardization_constant_FIT.rds"))
  
  ice.dfs <- plot.ice(models.performance=models.fit,
                      env.factor=env.fact.under.obs,
                      taxa=taxon.under.obs,
                      standardization.constant=std.const.fit[[1]],
                      nb.sample=100,
                      resolution=200)
  
  observations              <- ice.dfs[["observations"]]
  #env.factor.sampled        <- ice.dfs[["env.factor.sampled"]]
  observations.mean         <- observations %>%
    group_by(across(all_of(env.fact.under.obs)), model) %>%
    summarise(avg = mean(pred))
  
  observations.mean["noise"] <- name
  
  return(observations.mean)
})

final.multi.ice <- bind_rows(multi.ice, .id = "column_label")

fig1 <- ggplot(data=final.multi.ice) +
  geom_line(aes(x=.data[[env.fact.under.obs]],
                y=avg,
                group=column_label,
                colour=column_label),
            size=1) +
  facet_wrap(~model)


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Fig 2: multi box plot ----

# somehow this function takes ~10 min to run
multi.all.results <- lapply(list.exp.miss, FUN=function(name){
  
  dir.experiment <- paste0(dir.output, name, "/")
  models.cv      <- load.models(path=dir.experiment, split.type="CV")
  
  all.results    <- summarize.all.results(models.cv, prev.taxa)
  
  rm(models.cv) 
  
  all.results <- restructure.all.results(all.results)
  
  all.results["noise"] <- name
  
  return(all.results)
})

final.multi.all.results <- bind_rows(multi.all.results, .id = "column_label")

fig2 <- ggplot(data=final.multi.all.results,
       aes(x=column_label, y=dev)) +
geom_boxplot(aes(fill=fit_pred)) +
facet_wrap(~model) + 
theme_minimal() +
theme(legend.title=element_blank())




dir <- "../output_data/comparison_plots/"

pdf(paste0(dir, "ice_gammaridae.pdf"))
print(fig1)
dev.off()

pdf(paste0(dir, "boxplot_gammaridae.pdf"))
print(fig2)
dev.off() 

