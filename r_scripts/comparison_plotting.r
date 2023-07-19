

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

list.exp <- list.exp.gauss

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Fig 1: multi ICE (PDP only) ----

env.fact.under.obs <- "temperature"
taxon.under.obs <- "Occurrence.Gammaridae"

multi.ice <- lapply(list.exp, FUN=function(name){
                     
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
multi.all.results <- lapply(list.exp, FUN=function(name){
  
  dir.experiment <- paste0(dir.output, name, "/")
  models.cv      <- load.models(path=dir.experiment, split.type="CV")
  
  all.results    <- summarize.all.results(models.cv, prev.taxa)
  
  rm(models.cv) 
  
  all.results <- restructure.all.results(all.results)
  
  all.results["noise"] <- name
  
  return(all.results)
})

final.multi.all.results <- bind_rows(multi.all.results, .id = "column_label")

ggplot(data=final.multi.all.results) +
geom_boxplot(aes(x=column_label,
                 y=dev,
                 fill=fit_pred)) +
scale_x_discrete(limits=names(list.exp)) +
facet_wrap(~model) + 
theme_minimal() +
theme(legend.title=element_blank())



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Fig 3: "box" plot with a single taxon ----

multi.all.results <- lapply(list.exp, FUN=function(name){
  
  dir.experiment <- paste0(dir.output, name, "/")
  models.cv      <- load.models(path=dir.experiment, split.type="CV")
  
  all.results    <- summarize.all.results(models.cv, prev.taxa)
  
  rm(models.cv) 
  
  all.results <- restructure.all.results(all.results)
  
  all.results["noise"] <- name
  
  return(all.results)
})

final.multi.all.results <- bind_rows(multi.all.results, .id = "column_label")

taxa.to.keep <- "Gammaridae"

filtered.multi.all.results <- final.multi.all.results %>%
                              filter(taxa == taxa.to.keep)

fig3 <- ggplot(data=filtered.multi.all.results) +
  geom_point(aes(x=column_label,
                   y=dev,
                   shape=model, 
                   color=fit_pred)) +
  scale_x_discrete(limits=names(list.exp)) +
  #facet_wrap(~fit_pred) + 
  theme_minimal() +
  theme(legend.title=element_blank())
  

# TODO: “box plot” over a single taxa. The x-axis is for different scenario. The
#       y-axis is for the deviance. The different should be plotted on the same
#       column but with a different shape (e.g. square, triangle, star, …) and
#       two colors should be used to distinguish between fitting and predicting.



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Fig 4: dual ICE ----

experiment1 <- "dummy_exp1" # baseline
experiment2 <- "dummy_exp2"   # extreme noise

env.fact.under.obs <- "temperature"
taxon.under.obs <- "Occurrence.Gammaridae"

list.dual.exp <- list("exp1" = experiment1,
                      "exp2" = experiment2)

dual.ice <- lapply(list.dual.exp, FUN=function(name){
  
  dir.experiment          <- paste0(dir.output, name, "/")
  
  models.fit      <- load.models(path=dir.experiment, split.type="FIT")
  std.const.fit   <- readRDS(file=paste0(dir.experiment, "standardization_constant_FIT.rds"))
  
  ice.dfs <- plot.ice(models.performance=models.fit,
                      env.factor=env.fact.under.obs,
                      taxa=taxon.under.obs,
                      standardization.constant=std.const.fit[[1]],
                      nb.sample=100,
                      resolution=200)
  
  return(ice.dfs)
})

obs1      <- dual.ice[["exp1"]][["observations"]]
env.fact1 <- dual.ice[["exp1"]][["env.factor.sampled"]]
obs2      <- dual.ice[["exp2"]][["observations"]]
env.fact2 <- dual.ice[["exp2"]][["env.factor.sampled"]]

obs       <- bind_rows(list(obs1, obs2),
                       .id = "column_label")
env.fact  <- bind_rows(list(env.fact1, env.fact2),
                       .id = "column_label")

observations.mean         <- obs %>%
  group_by(across(all_of(env.fact.under.obs)), model, column_label) %>%
  summarise(avg = mean(pred))
observations.mean.bounds  <- observations.mean %>% group_by(model, column_label) %>%
  summarise(x.mean=max(across(all_of(env.fact.under.obs))),
            y.mean.min=min(avg),
            y.mean.max=max(avg))

ggplot(data=obs) +
  geom_line(aes(x=.data[[env.fact.under.obs]],
                y=pred,
                group=interaction(observation_number,column_label),
                color=column_label),
            alpha=0.4,
            show.legend = FALSE) +
  geom_line(data=observations.mean,
            aes(x=.data[[env.fact.under.obs]], y=avg, color=column_label),
            size=1.5) +
  geom_rug(data = env.fact,
           aes(x=variable,
               color=column_label),
           alpha=0.7,
           inherit.aes=F) + 
  geom_segment(data=observations.mean.bounds,
               inherit.aes = FALSE,
               lineend="round",
               linejoin="round",
               aes(x=x.mean,
                   y=y.mean.min,
                   xend=x.mean,
                   yend=y.mean.max,
                   color=column_label),
               arrow=arrow(length = unit(0.3, "cm"),
                           ends = "both")) +
  facet_wrap(~model)




# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Fig 5: multi-scenario bellplots ----
multi.all.results <- lapply(list.exp, FUN=function(name){
  
  dir.experiment <- paste0(dir.output, name, "/")
  models.cv      <- load.models(path=dir.experiment, split.type="CV")
  
  all.results    <- summarize.all.results(models.cv, prev.taxa)
  
  rm(models.cv) 
  
  all.results <- restructure.all.results(all.results)
  
  all.results["noise"] <- name
  
  return(all.results)
})

final.multi.all.results <- bind_rows(multi.all.results, .id = "column_label")

color.map <- c('1'            = 'deepskyblue',   # Generalized Linear Model
               '2'            = 'green',         # Generalized Additive Model
               '3'            = 'orange',        # Artificial Neural Network
               '4'            = 'red')           # Random Forest
  
names(color.map) <- names(list.exp)

fig5 <- ggplot(data=final.multi.all.results,
               aes(x=prevalence, y=dev, color=column_label)) + 
  geom_point(data=final.multi.all.results) +
  facet_wrap(~model + fit_pred)  +
  scale_color_manual(values=color.map) + 
  theme_minimal() + 
  theme(legend.title=element_blank())







# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Saving plots ----
dir <- "../output_data/comparison_plots/"

pdf(paste0(dir, "pdp_subset.pdf"))
print(fig1)
dev.off()

pdf(paste0(dir, "boxplot_extreme.pdf"))
print(fig2)
dev.off() 

pdf(paste0(dir, "single_boxplot_extreme.pdf"))
print(fig3)
dev.off()

pdf(paste0(dir, "dummy_dual_ice_comparison.pdf"))
print(fig4)
dev.off()

pdf(paste0(dir, "bell_extreme.pdf"))
print(fig5)
dev.off() 

