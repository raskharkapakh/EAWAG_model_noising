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
#                                  l                                           #
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

# NO NOISE
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# experiment variables ----
file.input.data         <- "All_2729samples_9envfact_lme.area.elev_ModelInputs.csv"
file.prev.taxa          <- "All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv"

experiment.name         <- paste0("baseline_",
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
               "parameters" = list("min"=0, "max"=35))

noise2 <- list("type"       = "missdetection",
               "target"     = "Occurrence.Gammaridae",
               "amount"     = 0.1, 
               "parameters" = NULL)

noise3 <- list("type"       = "add_factor",
               "target"     = "random1", # new factor name
               "amount"     = NULL, 
               "parameters" = NULL)

noise4 <- list("type"       = "remove_factor",
               "target"     = "temperature",
               "amount"     = NULL, 
               "parameters" = NULL)

noise  <- list("noise1"     = noise1,
               "noise2"     = noise2,
               "noise3"     = noise3,
               "noise4"     = noise4)

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



# TEMP variation
# ==============================================================================
for (sd in c(1,3,5)){
  
  
  experiment.name         <- paste0("gaussian_temp_", sd, "_",
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
                 "amount"     = sd,
                 "parameters" = list("min"=0, "max"=35))
  
  noise  <- list("noise1"     = noise1)

  
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

# Gammaridae variation
# ==============================================================================
for (p in c(10,25,50)){
  
  
  experiment.name         <- paste0("gammaridae_missdetection_", p, "_",
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
                 "amount"     = p/100.0, 
                 "parameters" = NULL)
  
  noise3 <- list("type"       = "add_factor",
                 "target"     = "random1", # new factor name
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise4 <- list("type"       = "remove_factor",
                 "target"     = "temperature",
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise  <- list("noise2"     = noise2)
  
  
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

# All taxa variation
# ==============================================================================
for (p in c(10,25,50)){
  
  
  experiment.name         <- paste0("all_taxa_missdetection_", p, "_",
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
                 "amount"     = p/100.0, 
                 "parameters" = NULL)
  
  noise3 <- list("type"       = "add_factor",
                 "target"     = "random1", # new factor name
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise4 <- list("type"       = "remove_factor",
                 "target"     = "temperature",
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise  <- list("noise2"     = noise2)
  
  noise <- lapply(TAXA.COLNAMES, FUN=function(taxon){
    noise_taxon <- list("type"       = "missdetection",
                        "target"     = taxon,
                        "amount"     = p/100.0, 
                        "parameters" = NULL)
    
    return(noise_taxon)
  })
  
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

# subset variation
# ==============================================================================
for (s in c(500,1000,2000)){
  
  
  experiment.name         <- paste0("subset_", s, "_",
                                    format(Sys.time(), "%d_%m_%Y_%Hh%M"))
  number.split            <- 3
  split.criterion         <- "SiteId"
  number.sample           <- s
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
                 "amount"     = p/100.0, 
                 "parameters" = NULL)
  
  noise3 <- list("type"       = "add_factor",
                 "target"     = "random1", # new factor name
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise4 <- list("type"       = "remove_factor",
                 "target"     = "temperature",
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise  <- list("noise2"     = noise2)
  
  noise <-list()
  
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


# removing taxa variation
# ==============================================================================
for (index in c(1,2,3)){
  
  l <- list(list("velocity"),
            list("velocity",     
                 "A10m",
                 "cow.density", 
                 "IAR"),
            list("velocity",     
                 "A10m",
                 "cow.density", 
                 "IAR",      
                 "urban.area",
                 "FRI",
                 "bFRI",            
                 "width.variability"))
  
  to_remove <- l[[index]]
  
  
  experiment.name         <- paste0("env_fact_removed_", length(to_remove), "_",
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
                 "amount"     = p/100.0, 
                 "parameters" = NULL)
  
  noise3 <- list("type"       = "add_factor",
                 "target"     = "random1", # new factor name
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise4 <- list("type"       = "remove_factor",
                 "target"     = "temperature",
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise  <- list("noise2"     = noise2)
  
  noise <- lapply(to_remove, FUN=function(target){
    noise.env.fact <- list("type"       = "remove_factor",
                           "target"     = target,
                           "amount"     = NULL, 
                           "parameters" = NULL)
    
    return(noise.env.fact)
  })
  
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






# adding random factor
# ==============================================================================
for (nb in c(3,9)){
  
  
  experiment.name         <- paste0("add_env_fact_", nb, "_",
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
                 "amount"     = p/100.0, 
                 "parameters" = NULL)
  
  noise3 <- list("type"       = "add_factor",
                 "target"     = "random1", # new factor name
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise4 <- list("type"       = "remove_factor",
                 "target"     = "temperature",
                 "amount"     = NULL, 
                 "parameters" = NULL)
  
  noise  <- list("noise2"     = noise2)
  
  noise <- lapply(as.list(seq(1, nb)), FUN=function(n){
    noise.fact <- list("type"       = "add_factor",
                       "target"     = paste0("random",n), # new factor name
                       "amount"     = NULL, 
                       "parameters" = NULL)
    
    return(noise.fact)
  })
  
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
















# =========================
# =========================
# =========================
# =========================
# =========================






# noise on flow velocity
experiment.name         <- paste0("gaussian_velocity_3",
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
               "target"     = "velocity",
               "amount"     = 3,
               "parameters" = list("min"=-Inf, "max"=Inf))



noise  <- list("noise"     = noise1)


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








# noise on flow velocity
experiment.name         <- paste0("gaussian_temp_velocity_3",
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
               "amount"     = 3,
               "parameters" = list("min"=-5, "max"=35))

noise1 <- list("type"       = "gaussian",
               "target"     = "velocity",
               "amount"     = 3,
               "parameters" = list("min"=-Inf, "max"=Inf))



noise  <- list("noise1"     = noise1,
               "noise2"     = noise2)


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
  


