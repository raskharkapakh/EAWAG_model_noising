
training.pipeline <- function(
    file.input.data="All_2729samples_9envfact_lme.area.elev_ModelInputs.csv",
    file.prev.taxa="All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv",
    experiment.name="experiment_test",
    number.split=3,
    split.criterion="SiteId",
    number.sample=10000,
    models=list("null", "glm", "gamloess", "rf", "ann"),
    noise=NULL,
    env.factor,
    env.factor.full
  ){
  
  source("util.r")
  source("global_variables.r")
  source("data_noising.r")
  source("dataset_preprocessing.r")
  source("ml_models.r")
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Global variables ----
  ENV.FACT.COLNAMES       <- env.factor
  ENV.FACT.COLNAMES.FULL  <- env.factor.full
  dir.input.data          <- "../input_data/"
  dir.output              <- "../output_data/"
  dir.experiment          <- paste0(dir.output, experiment.name, "/")
  dir.metadata            <- paste0(dir.experiment, "metadata.json")
  dir.create(dir.experiment)
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
  # saving metadata to JSON file ----
  metadata.list           <- list("input_data"       = file.input.data,
                                  "prev_taxa"        = file.prev.taxa,
                                  "experiment.name"  = experiment.name,
                                  "number_split"     = number.split,
                                  "split.criterion"  = split.criterion,
                                  "number.sample"    = number.sample,
                                  "models"           = models,
                                  "env_factor"       = env.factor,
                                  "env_factor_full"  = env.factor.full,
                                  "noise"            = noise) 
  
  metadata.json           <- toJSON(metadata.list)
  write(metadata.json, dir.metadata)
  
  
    
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Open data set ----
  data <- read.csv(paste0(dir.input.data, file.input.data),
                   header = T, 
                   sep = ",", 
                   stringsAsFactors = F)
  
  prev.taxa <- read.csv(paste0(dir.input.data, file.prev.taxa),
                        header = T, 
                        sep = ",", 
                        stringsAsFactors = F)
  
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Add noise to data set ----
  noised.data <- add.noise(data, number.sample, noise)
  
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Preprocess dataset ----
  
  # Replace 0s and 1s in taxas' columns respectively by labels "absent"/"present"
  noised.data <- categorize.taxa.occurence(noised.data)
  
  # Split and standardize data
  preprocessed.data.cv  <- preprocess.data(data=noised.data,
                                           dir=dir.experiment,
                                           split.type="CV",
                                           nb.split=number.split,
                                           splitting.criterion=split.criterion)
  
  preprocessed.data.fit <- preprocess.data(data=noised.data,
                                           dir=dir.experiment,
                                           split.type="FIT")
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Train models (GLM, gamLoess, RF, ANN) and save them ----
  models.cv  <- apply.ml.models(data=preprocessed.data.cv,
                                models=models,
                                split.type="CV")
  
  save.models(models=models.cv,
              path=dir.experiment,
              split.type="CV")
  
  models.fit <- apply.ml.models(data=preprocessed.data.fit,
                                models=models,
                                split.type="FIT")
  
  save.models(models=models.fit,
              path=dir.experiment,
              split.type="FIT")

}

