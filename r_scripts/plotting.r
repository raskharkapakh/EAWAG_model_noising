create.plots <- function(experiment.name="experiment_name",
                         file.prev.taxa="All_2729samples_9envfact_lme.area.elev_PrevalenceTaxa.csv"){
  
  
  
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
  
  
  # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Custom R scripts ---- 
  source("global_variables.r")
  source("ml_models.r")
  source("performances_assessment.r")
  source("dataset_preprocessing.r")
  
  # load the the files required for plotting ----
  dir.input.data  <- "../input_data/"
  prev.taxa       <- read.csv(paste0(dir.input.data, file.prev.taxa),
                              header = T, 
                              sep = ",", 
                              stringsAsFactors = F)
  dir.output              <- "../output_data/"
  dir.experiment          <- paste0(dir.output, experiment.name, "/")
  
  
  models.fit      <- load.models(path=dir.experiment,
                                 split.type="FIT")
  models.cv       <- load.models(path=dir.experiment, 
                                 split.type="CV")
  std.const.cv    <- readRDS(file=paste0(dir.experiment, "standardization_constant_CV.rds"))
  std.const.fit   <- readRDS(file=paste0(dir.experiment, "standardization_constant_FIT.rds"))
  
  # Variable for plots ----
  all.results <- summarize.all.results(models.cv, prev.taxa)
  
  ggplot.all.results <- restructure.all.results(all.results)
  
  color.map <- c('glm'           = 'deepskyblue',   # Generalized Linear Model
                 'gamloess'      = 'green',         # Generalized Additive Model
                 'ann'           = 'orange',        # Artificial Neural Network
                 'rf'            = 'red',           # Random Forest
                 'null'          = 'black')         # Null Model
  
  
  # Fig. 1 ----
  gg.plot.dev.infos <-  ggplot.all.results %>%
    group_by(fit_pred, model) %>%
    summarise(mean=mean(dev),
              max=max(dev),
              min=min(dev),
              median=median(dev))
  
  median.null.pred <- min((gg.plot.dev.infos %>%
                             filter(fit_pred=="pred" & model=="null"))["median"])
  median.best.pred <- min((gg.plot.dev.infos %>%
                             filter(fit_pred=="pred"))["median"])
  
  fig1 <- ggplot(data=ggplot.all.results,
                aes(x=model, y=dev)) +
          geom_boxplot(aes(fill=fit_pred)) +
          geom_hline(aes(yintercept=median.null.pred,
                         color="median null model (pred)"),
                     linetype="dashed") + 
          geom_hline(aes(yintercept=median.best.pred,
                         color="median best model (pred)"),
                     linetype="dashed") +
          scale_colour_manual(values = c('green','black')) +
          theme_minimal() +
          theme(legend.title=element_blank())
  
  
  # Fig. 2 ----
  fig2 <- ggplot(data=subset(ggplot.all.results, model %in% "null"),
                 aes(x=prevalence, y=dev, color=model)) +
          geom_smooth(se = FALSE) + 
          geom_point(data=ggplot.all.results) +
          facet_wrap(~fit_pred) +
          scale_color_manual(values=color.map) +
          theme_minimal() + 
          theme(legend.title=element_blank())
  
  
  # Fig. 3 ----
  env.fact.under.obs <- "temperature"
  taxon.under.obs <- "Occurrence.Gammaridae"
  
  ice.dfs <- plot.ice(models.performance=models.fit,
                      env.factor=env.fact.under.obs,
                      taxa=taxon.under.obs,
                      standardization.constant=std.const.fit[[1]],
                      nb.sample=100,
                      resolution=200)
  
  observations              <- ice.dfs[["observations"]]
  env.factor.sampled        <- ice.dfs[["env.factor.sampled"]]
  observations.mean         <- observations %>%
                                  group_by(across(all_of(env.fact.under.obs)), model) %>%
                                  summarise(avg = mean(pred))
  observations.mean.bounds  <- observations.mean %>% group_by(model) %>%
                                          summarise(x.mean=max(across(all_of(env.fact.under.obs))),
                                                    y.mean.min=min(avg),
                                                    y.mean.max=max(avg))
  
  fig3 <- ggplot(data=observations) +
          geom_line(aes(x=.data[[env.fact.under.obs]],
                        y=pred,
                    group=observation_number, 
                    color=as.character(observation_number)),
                    show.legend = FALSE) +
          geom_line(data=observations.mean,
                    aes(x=.data[[env.fact.under.obs]], y=avg),
                    size=1.5) +
          geom_rug(data = env.factor.sampled,
                   aes(x=variable), 
                   color="grey20",
                   alpha=0.7,
                   inherit.aes=F) + 
          geom_segment(data=observations.mean.bounds,
                       inherit.aes = FALSE,
                       lineend="round",
                       linejoin="round",
                       aes(x=x.mean,
                           y=y.mean.min,
                           xend=x.mean,
                           yend=y.mean.max),
                       arrow=arrow(length = unit(0.3, "cm"),
                                   ends = "both")) +
          facet_wrap(~model)
  
  
  # Fig 4
  noised.dataset <- models.fit[[1]][[1]][[1]][[1]][["observation"]]
  taxa.col <- colnames(noised.dataset)[which(grepl("Occurrence.", colnames(noised.dataset)))]
  
  noised.prevalence <- lapply(taxa.col, FUN=function(taxon){
    occurence  <- noised.dataset[[taxon]]
    prevalence <- length(occurence[occurence=="present"])/length(occurence) 
    return(prevalence)
  })  
  
  noised.prev.taxa <- prev.taxa  
  noised.prev.taxa[["Prevalence"]] <- noised.prevalence
  
  prev.taxa[["Status"]] <- "unnoised"
  noised.prev.taxa[["Status"]] <- "noised"
  
  full.prev.taxa <- rbind(prev.taxa, noised.prev.taxa)
  full.prev.taxa[["Occurrence.taxa"]] <- as.factor(full.prev.taxa[["Occurrence.taxa"]])
  full.prev.taxa$Prevalence <- unlist(full.prev.taxa$Prevalence)
  full.prev.taxa$Occurrence.taxa <- gsub("Occurrence.", "", full.prev.taxa$Occurrence.taxa)
  
  fig4 <- ggplot(data=full.prev.taxa,
                 aes(x=Prevalence,
                     y=Occurrence.taxa,
                     color=Status)) + 
          geom_point() +
          scale_y_discrete(limits = rev(gsub("Occurrence.", "", prev.taxa$Occurrence.taxa))) +
          labs(x="Prevalence", y="Taxa")
          theme_minimal() + 
          theme(legend.title=element_blank())
  
  
  # Saving figures to experiment folder ----
  pdf(paste0(dir.experiment, "fig1.pdf"))
  print(fig1)
  dev.off()
  
  pdf(paste0(dir.experiment, "fig2.pdf"))
  print(fig2)
  dev.off() 
  
  pdf(paste0(dir.experiment, "fig3.pdf"))
  print(fig3)
  dev.off()
  
  pdf(paste0(dir.experiment, "fig4.pdf"))
  print(fig4)
  dev.off()
  
}
