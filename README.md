# How robust are species distribution models against noise in the data?

### Background

TODO

### Overview

TODO

## Data

The input data is already pre-processed (pre-processing information available upon request).

* The macroinvertebrate dataset comes from the MIDAT database, run by info fauna CSCF (Centre Suisse de Cartographie de la Faune) & karch. It consists of 2729 observations of the presence [1] and absence [0] of 60 taxa taken at 1802 different sites covering the whole of Switzerland.
* The environmental dataset consists of nine environmental factors selected and calculated based on expert knowledge and previous studies (see Manuscript for more details). They are derived from the Swiss Federal Office for the Environment (water quality monitoring data, hydrological data, hydromorphological data, land use data), the Swiss Federal Office of Topography (topographical data), and the Swiss Federal Statistical Office (population statistics).

TODO: to modify once the streambugs data has been successfully generated.


## Overal repo structure

The goal of this package is to assess the effect of noise on different models (glm, gamloess, rf and a nn). The package is divided into four folders:

* **input_data**: All the files necessary for training the models (e.g. the dataset, the file concerning the prevalence of taxa):
    * __All_2729samples_9envfact_lme.area.elev_ModelInputs.csv__ : Models input data (2729 observations of presence [1] and absence [0] of 60 taxa and calculation of nine environmental factors).
    * __All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv__ : Information on prevalence and taxonomic level of the 60 taxa.

*	**output_data**: Where all the models are stored after training. The different plots assessing the performance are also stored there. More specifically, when model have been trained in a specific noising scenarios, they are stored in a folder, containing:
    * the different models stored in a RDS file. There are two files: one for the models when trained in FIT mode and one for when the models are trained in Cross Validation (CV) mode
    * The datasets after being preprocessed. The preprocessing consists of standardizing the different environmental factors used for prediction. Moreover, preprocessing the dataset for CV requires to already create the different splits.
    * The standardization constant (mean and variance) for every environmental factor, both in FIT and CV mode.
    * The different plots to assess the performance of a given models.
    * The metadata describing the parameters used to train the model. E.g. number of split, what noises have been added to the data, ...
*	**plots**: In this folder are stored the plots to compare the models' performances under different noising scenario
*	**r_script**: Where all the different scripts are stored. The most important script to know for the user are:
    * **main.r**: with this script, the user can specify some parameters (e.g. what models to train, how many splits are desired for cross-validation, ...) and then train different models according to those parameters
    * **comparison_plots.r**: With this script, the user can give a list of models under different settings and plot comparison of how the models are performing under those different settings. The generated plots are then saved in the folder "plots".

## User guide

### main.r

the parameters to train the model:

* **experiment.name**: The name to give to the folder where the models, preprocessed datasets, standardization constants and metadata will be saved.
* **number.split**: how many splits are desired for the cross validation.
* **split.criterion**: grouping factor when splitting the data.
* **number.sample**: how many datapoints to take to train the models.
* **models**: the models that will be trained. The model that could be trained are Generalized Linear Model (glm), Generative Additive Model (gamloess), Random Forest (rf), Neural Network (ann) and a baseline model (null).
* **env.factor**: The environmental factor to use to make the prediction in all the model except gamloess.
* **env.factor.full**: The environmental factor to use to make the prediction only for the gamloess model.
* **noise**: List of noises to add the dataset: example of noises are given:
    * gaussian.noise <- list("type" = "gaussian", "target" = "temperature", "amount" = 5, "parameters" = list("min"=0, "max"=35))

    * missdetection.noise <- list("type" = "missdetection", "target" = "Occurrence.Gammaridae", "amount" = 0.1, "parameters" = NULL)

    * new.factor.noise <- list("type" = "add_factor", "target" = "random1", "amount" = NULL, "parameters" = NULL)

    *	Remove.factor.noise <- list("type" = "remove_factor", "target" = "temperature", "amount" = NULL, "parameters" = NULL)

### comparison_plotting.r

TODO
