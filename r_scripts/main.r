###############################################################################|
#                                                                              #
#  --- Study of the influence of noise on overfitting of machine learning ---  #
#            --- and statistical species distribution models. ---              #
#                                                                              #
#                          --- February 2023 ---                               #
#                                                                              #
#                   --- Emma Chollet, Gaspard Fragnière ---                    #
#                --- Andreas Scheidegger and Nele Schuwirth ---                #
#                                                                              #
#                      --- emma.chollet@eawag.ch ---                           #
#                                  l                                            #
###############################################################################|


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
source("performances_assessment.r")
source("training_pipeline.r")
source("plotting.r")

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# experiment variables ----
for(prob in c(0.1, 0.25, 0.5)){
  file.input.data         <- "All_2729samples_9envfact_lme.area.elev_ModelInputs.csv"
  file.prev.taxa          <- "All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv"
  
  prob.percent=as.integer(prob*100)
  
  experiment.name         <- paste0("experiment_miss_gamma_",#TODO: change name
                                    prob.percent,
                                    "_",
                                    format(Sys.time(), "%d_%m_%Y_%Hh%M"))
  number.split            <- 3
  split.criterion         <- "SiteId"
  number.sample           <- 10000
  models                  <- list("null", "glm", "gamloess", "rf", "ann")
  env.factor              <- c("Temperature"                      = "temperature",       # Temp
                               "Flow velocity"                    = "velocity",          # FV
                               "Riparian agriculture"             = "A10m",              # A10m
                               "Livestock unit density"           = "cow.density",       # LUD
                               "Insecticide application rate"     = "IAR",               # IAR
                               "Urban area"                       = "urban.area",        # Urban
                               "Forest-river intersection"        = "FRI",               # FRI
                               "Forest-river intersection buffer" = "bFRI",              # bFRI
                               "Width variability"                = "width.variability") # WV
  
  env.factor.full         <- c(env.factor,
                               "Temperature2"                     = "temperature2",
                               "Velocity2"                        = "velocity2")
  
  noise1 <- list("type"       = "gaussian",
                 "target"     = "temperature",
                 "amount"     = 5,
                 "parameters" = list("min"=-5, "max"=35))
  
  noise2 <- list("type"       = "missdetection",
                 "target"     = "Occurrence.Gammaridae",
                 "amount"     = prob, 
                 "parameters" = NULL)
  
  noise  <- list("noise1"     = noise2)
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # training model ----
  training.pipeline(file.input.data=file.input.data,
                    file.prev.taxa=file.prev.taxa,
                    experiment.name=experiment.name,
                    number.split=number.split,
                    split.criterion=split.criterion,
                    number.sample=number.sample,                   
                    models=models,
                    noise=noise,
                    env.factor=env.factor,
                    env.factor.full=env.factor.full)
  
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # plotting models and saving model ----
  create.plots(experiment.name=experiment.name,
               file.prev.taxa=file.prev.taxa)
}
for (nb.sample in c(500, 1000, 2000)){  
  
  file.input.data         <- "All_2729samples_9envfact_lme.area.elev_ModelInputs.csv"
  file.prev.taxa          <- "All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv"
  experiment.name         <- paste0("experiment_subsetsize", #TODO: change name
                                    nb.sample, 
                                    "_",
                                    format(Sys.time(), "%d_%m_%Y_%Hh%M"))
  number.split            <- 3
  split.criterion         <- "SiteId"
  number.sample           <- nb.sample
  models                  <- list("null", "glm", "gamloess", "rf", "ann")
  env.factor              <- c("Temperature"                      = "temperature",       # Temp
                               "Flow velocity"                    = "velocity",          # FV
                               "Riparian agriculture"             = "A10m",              # A10m
                               "Livestock unit density"           = "cow.density",       # LUD
                               "Insecticide application rate"     = "IAR",               # IAR
                               "Urban area"                       = "urban.area",        # Urban
                               "Forest-river intersection"        = "FRI",               # FRI
                               "Forest-river intersection buffer" = "bFRI",              # bFRI
                               "Width variability"                = "width.variability") # WV
  
  env.factor.full         <- c(env.factor,
                               "Temperature2"                     = "temperature2",
                               "Velocity2"                        = "velocity2")
  
  noise1 <- list("type"       = "gaussian",
                 "target"     = "temperature",
                 "amount"     = 5,
                 "parameters" = list("min"=-5, "max"=35))
  
  noise2 <- list("type"       = "missdetection",
                 "target"     = "Occurrence.Simuliidae",
                 "amount"     = 0, 
                 "parameters" = NULL)
  
  noise  <- list()
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # training model ----
  training.pipeline(file.input.data=file.input.data,
                    file.prev.taxa=file.prev.taxa,
                    experiment.name=experiment.name,
                    number.split=number.split,
                    split.criterion=split.criterion,
                    number.sample=number.sample,                   
                    models=models,
                    noise=noise,
                    env.factor=env.factor,
                    env.factor.full=env.factor.full)
  
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # plotting models and saving model ----
  create.plots(experiment.name=experiment.name,
               file.prev.taxa=file.prev.taxa)
}

