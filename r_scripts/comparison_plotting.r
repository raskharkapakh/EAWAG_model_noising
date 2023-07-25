

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
list.experiences    <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "missdetection"     = "experiment_miss_gammaridae_50_01_07_2023_03h37",
                            "gaussian noise"    = "experiment_temp_5_30_06_2023_05h53",
                            "less datapoints"   = "experiment_subset_500_01_07_2023_11h23")

# temp DONE
list.exp.gauss      <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "low noise"         = "gaussian_temp_1_21_07_2023_14h20",
                            "mid noise"         = "gaussian_temp_3_21_07_2023_16h42",
                            "high noise"        = "gaussian_temp_5_21_07_2023_22h09")

# missdetection gammaridae DONE
list.exp.miss.gamma <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "low noise"         = "gammaridae_missdetection_10_22_07_2023_04h08",
                            "mid noise"         = "gammaridae_missdetection_25_22_07_2023_10h15",
                            "high noise"        = "gammaridae_missdetection_50_22_07_2023_16h37")

# missdetection gammaridae DONE
list.exp.miss.all   <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "low noise"         = "all_taxa_missdetection_10_22_07_2023_23h01",
                            "mid noise"         = "all_taxa_missdetection_25_23_07_2023_05h43",
                            "high noise"        = "all_taxa_missdetection_50_23_07_2023_12h09")

# subset DONE
list.exp.subset     <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "low noise"         = "subset_2000_24_07_2023_00h40",
                            "mid noise"         = "subset_1000_23_07_2023_21h24",
                            "high noise"        = "subset_500_23_07_2023_19h25")

# remove factor
list.remove.fact    <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "low noise"         = "env_fact_removed_1_24_07_2023_03h30",
                            "mid noise"         = "env_fact_removed_4_24_07_2023_06h16",
                            "high noise"        = "env_fact_removed_8_24_07_2023_08h30")

# adding factor
#list.dummy          <- list("baseline"          = "baseline_21_07_2023_12h00",
#                            "low noise"         = "add_env_fact_1_24_07_2023_10h41",
#                            "mid noise"         = "TODO",
#                            "high noise"        = "TODO")

# gaussian temperature and velocity DONE
list.gauss.temp.vel <- list("baseline"          = "baseline_21_07_2023_12h00",
                            "low noise"         = "gaussian_velocity_3_24_07_2023_23h09",
                            "mid noise"         = "gaussian_temp_3_21_07_2023_16h42",
                            "high noise"        = "gaussian_temp_velocity_3_25_07_2023_06h38")





create.comparison.plots("dummy", list.dummy)
  
