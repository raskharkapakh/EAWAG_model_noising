ENV.FACT.COLNAMES       <- c("Temperature"                      = "temperature",       # Temp
                             "Flow velocity"                    = "velocity",          # FV
                             "Riparian agriculture"             = "A10m",              # A10m
                             "Livestock unit density"           = "cow.density",       # LUD
                             "Insecticide application rate"     = "IAR",               # IAR
                             "Urban area"                       = "urban.area",        # Urban
                             "Forest-river intersection"        = "FRI",               # FRI
                             "Forest-river intersection buffer" = "bFRI",              # bFRI
                             "Width variability"                = "width.variability") # WV

ENV.FACT.FULL.COLNAMES  <- c(ENV.FACT.COLNAMES,
                             "Temperature2"                     = "temperature2",
                             "Velocity2"                        = "velocity2")

TAXA.COLNAMES      <-  c("Simuliidae"                      = "Occurrence.Simuliidae",
                              "Oligochaeta"                     = "Occurrence.Oligochaeta",
                              "Limoniidae"                      = "Occurrence.Limoniidae",
                              "Limnephilidae"                   = "Occurrence.Limnephilidae",
                              "Rhyacophilidae"                  = "Occurrence.Rhyacophilidae",
                              "Elmidae"                         = "Occurrence.Elmidae",
                              "Gammaridae"                      = "Occurrence.Gammaridae",
                              "Heptageniidae"                   = "Occurrence.Heptageniidae",
                              "Nemouridae"                      = "Occurrence.Nemouridae",
                              "Empididae"                       = "Occurrence.Empididae",
                              "Hydropsychidae"                  = "Occurrence.Hydropsychidae",
                              "Leuctridae"                      = "Occurrence.Leuctridae",
                              "Ceratopogonidae"                 = "Occurrence.Ceratopogonidae",
                              "Prostigmata"                     = "Occurrence.Prostigmata",
                              "Psychodidae"                     = "Occurrence.Psychodidae",
                              "Leptophlebiidae"                 = "Occurrence.Leptophlebiidae",
                              "Sphaeriidae"                     = "Occurrence.Sphaeriidae",
                              "Taeniopterygidae"                = "Occurrence.Taeniopterygidae",
                              "Perlodidae"                      = "Occurrence.Perlodidae",
                              "Athericidae"                     = "Occurrence.Athericidae",
                              "Psychomyiidae"                   = "Occurrence.Psychomyiidae",
                              "Nematoda"                        = "Occurrence.Nematoda",
                              "Hydraenidae"                     = "Occurrence.Hydraenidae",
                              "Planariidae"                     = "Occurrence.Planariidae",
                              "Sericostomatidae"                = "Occurrence.Sericostomatidae",
                              "Tipulidae"                       = "Occurrence.Tipulidae",
                              "Polycentropodidae"               = "Occurrence.Polycentropodidae",
                              "Odontoceridae"                   = "Occurrence.Odontoceridae",
                              "Dugesiidae"                      = "Occurrence.Dugesiidae",
                              "Lymnaeidae"                      = "Occurrence.Lymnaeidae",
                              "Erpobdellidae"                   = "Occurrence.Erpobdellidae",
                              "Scirtidae"                       = "Occurrence.Scirtidae",
                              "Ephemerellidae"                  = "Occurrence.Ephemerellidae",
                              "Asellidae"                       = "Occurrence.Asellidae",
                              "Ephemeridae"                     = "Occurrence.Ephemeridae",
                              "Chloroperlidae"                  = "Occurrence.Chloroperlidae",
                              "Dytiscidae"                      = "Occurrence.Dytiscidae",
                              "Stratiomyidae"                   = "Occurrence.Stratiomyidae",
                              "Hydroptilidae"                   = "Occurrence.Hydroptilidae",
                              "Hydrobiidae"                     = "Occurrence.Hydrobiidae",
                              "Perlidae"                        = "Occurrence.Perlidae",
                              "Planorbidae"                     = "Occurrence.Planorbidae",
                              "Glossosomatidae"                 = "Occurrence.Glossosomatidae",
                              "Blephariceridae"                 = "Occurrence.Blephariceridae",
                              "Glossiphoniidae"                 = "Occurrence.Glossiphoniidae",
                              "Capniidae"                       = "Occurrence.Capniidae",
                              "Goeridae"                        = "Occurrence.Goeridae",
                              "Philopotamidae"                  = "Occurrence.Philopotamidae",
                              "Dixidae"                         = "Occurrence.Dixidae",
                              "Gyrinidae"                       = "Occurrence.Gyrinidae",
                              "Caenidae"                        = "Occurrence.Caenidae",
                              "Leptoceridae"                    = "Occurrence.Leptoceridae",
                              "Calopterygidae"                  = "Occurrence.Calopterygidae",
                              "Tabanidae"                       = "Occurrence.Tabanidae",
                              "Pediciidae"                      = "Occurrence.Pediciidae",
                              "Cordulegastridae"                = "Occurrence.Cordulegastridae",
                              "Anthomyiidae"                    = "Occurrence.Anthomyiidae",
                              "Lepidostomatidae"                = "Occurrence.Lepidostomatidae",
                              "Physidae"                        = "Occurrence.Physidae",
                              "Sialidae"                        = "Occurrence.Sialidae")

# This subset is meant for increasing speed during testing. When every is ready,
# the content of TAXA.COLNAMES should be replaced by TAXA.COLNAMES.FULL
TAXA.COLNAMES.PART          <- c("Simuliidae"                        = "Occurrence.Simuliidae",
                            "Gammaridae"                        = "Occurrence.Gammaridae",
                            "Stratiomyidae"                     = "Occurrence.Stratiomyidae",
                            "Sialidae"                          = "Occurrence.Sialidae")

TUNE.GRID.GLM          <- NULL # add tuning parameters for GLM here 

TUNE.GRID.GAMLOESS     <- NULL # add tuning parameters for GamLoess here

TUNE.GRID.RF           <- expand.grid(mtry                      = c(1,2,4,8))

#TUNE.GRID.ANN          <- expand.grid(num.units                 = c(4, 8, 32),
#                                      num.layers                = c(3, 5, 10),
#                                      learning.rate             = c(0.1, 0.01),
#                                      num.epochs                = c(50, 100),
#                                      batch.size                = c(16, 64))

# simplified tune grid for precessing speed increase
TUNE.GRID.ANN          <- expand.grid(num.units                 = c(4),
                                      num.layers                = c(5),
                                      learning.rate             = c(0.01),
                                      num.epochs                = c(50),
                                      batch.size                = c(64))

TUNE.GRID              <- list("glm"                            = TUNE.GRID.GLM,
                               "gamloess"                       = TUNE.GRID.GAMLOESS,
                               "rf"                             = TUNE.GRID.RF,
                               "ann"                            = TUNE.GRID.ANN)
