# machine learning model ----
apply.ml.models <- function(data,
                            models=c("null", "glm", "gamloess", "rf", "ann"),
                            split.type="FIT"){
  
  
  # This function takes as input a dataset, and what split.type was used to 
  # split the dataset. It then returns the trained models. The models trained 
  # have the following structure:
  #     -> model name
  #       -> split
  #         -> training, testing
  #           -> taxa 
  #             -> model, observation, prediction factors, prediction 
  #                probabilities, likelihood, performance
  #
  # arguments:
  #   - data: the dataset used to train the models
  #   - models: what models need to be applied  
  #   - split.type: how the dataset has been split
  #
  # returns:
  #   - models: the trained models (glm, gamLoess, rf, ann) 

  trained.null      <- if ("null" %in% models) apply.null.model(data, split.type) else NULL
  trained.glm       <- if ("glm" %in% models) apply.caret.model(data, split.type, ENV.FACT.FULL.COLNAMES, 'glm') else NULL
  trained.gamloess  <- if ("gamloess" %in% models) apply.caret.model(data, split.type, ENV.FACT.COLNAMES, 'gamLoess') else NULL
  trained.rf        <- if ("rf" %in% models) apply.caret.model(data, split.type, ENV.FACT.COLNAMES, 'rf') else NULL
  trained.ann       <- if ("ann" %in% models) apply.ann.model(data, split.type) else NULL
  
  trained.ml.models  <- list("null"     = trained.null,
                             "glm"      = trained.glm,
                             "gamloess" = trained.gamloess,
                             "rf"       = trained.rf,
                             "ann"      = trained.ann)
  
  # remove models == NULL
  trained.ml.models <- trained.ml.models[!sapply(trained.ml.models, is.null)]
  
  return(trained.ml.models)
}


# null model ----
apply.null.model <- function(data, split.type){
  

  # This function create the null model and returns its performance. The null 
  # model is used as baseline and simply define the probability of occurrence 
  # of a taxon as the number of time this taxon has been observed divided by the 
  # total number of observation.
  #
  # arguments
  #   - data: the observation used to create the null model
  #   - split.type: how the data has been split (FIT, CV, ODG)
  #
  # returns:
  #   - null.model: the null model, i.e. for each splits and each taxa, it is a 
  #                 list of: 
  #                   1. model
  #                   2. Observation (the data needed for prediction)
  #                   3. Prediction of each observation, 
  #                   4. Prediction probability of each observation
  #                   5. Likelihood of each observation 
  #                   6. Standard Deviance of the null model
  #                   7. Area Under the Curve (AUC) of the null model

    
  if (split.type == 'FIT'){
    
    training.data <- na.omit(data[["Entire dataset"]])
    
    prob <- lapply(TAXA.COLNAMES,
                   FUN=get.null.model,
                   training.data)
    
    model.performances <- lapply(TAXA.COLNAMES,
                                 FUN=null.model.perf,
                                 prob,
                                 training.data)
   
    null.model <- list("entire_dataset" = list("training" = model.performances))
    
  } else if (split.type == 'CV'){
    
    nb.split <- length(data)
    null.model <- vector("list", nb.split)
    names(null.model) <- names(data) # split1, split2, ...
    
    # loop over the different splits
    for (i.split in seq(nb.split)){
      
      split <- data[[i.split]]
      
      training.data <- na.omit(split[[1]])
      testing.data <- na.omit(split[[2]])  
      
      prob <- lapply(TAXA.COLNAMES,
                     FUN=get.null.model,
                     training.data)
      
      train.models <- lapply(TAXA.COLNAMES, 
                             FUN=null.model.perf,
                             prob,
                             training.data)
      test.models <- lapply(TAXA.COLNAMES,
                            FUN=null.model.perf,
                            prob,
                            testing.data)
      
      null.model[[i.split]] <- list("training" = train.models,
                                    "testing"  = test.models)
    }
    
  } else if (split.type == 'ODG'){
    
    # TODO: to write
    
    null.model <-  0
  }
  
  return(null.model)
}

get.null.model  <- function(taxa, data){
  

  # Compute the null model for a taxon given as argument. I.e. for the taxon  
  # given as argument, the probability of occurrence is given by the number of 
  # time this taxon was observed in the data over the total number of 
  # observations in the data
  #
  # arguments
  #   - taxa: the taxon whose probability of occurrence need to be computed
  #   - data: the observations used to compute the probability of occurrence
  #
  # returns:
  #   - prob.presence: the probability of occurrence of given taxon

    
  taxa.column   <- data[[taxa]]
  nb.taxa       <- nrow(data)
  nb.presence   <- length(which(taxa.column=="present"))
  prob.presence <- nb.presence/nb.taxa
  
  return(prob.presence)
}

null.model.perf <- function(taxa, prob, data){
  

  # This function returns the performance of the null model for a taxon given as
  # argument. The performance consists of : 
  #     1. Model
  #     2. Observation (the data that needed a prediction)
  #     3. Prediction of each observation, 
  #     4. Prediction probability of each observation
  #     5. Likelihood of each observation 
  #     6. Standard Deviance of the null model
  #     7. Area Under the Curve (AUC) of the null model
  #
  # arguments
  #   - taxa: The taxon for which to compute the performance
  #   - prob: The probability (~model) used to make the prediction for each
  #           observation
  #   - data: The observations used to predict 
  #
  # returns:
  #   - model.performance: the performance of the null model for the given taxa
  
  
  nb.sample <- nrow(data)
  taxa.prob <- prob[[gsub("Occurrence.", "", taxa)]]

  binary.prediction <- rbinom(n=nb.sample, size=1, prob=taxa.prob)
  prediction <- as.factor(ifelse(binary.prediction>0.5, "present", "absent"))
  prediction.probability <- data.frame("absent" = rep(1 - taxa.prob, nb.sample),
                                       "present" = rep(taxa.prob, nb.sample))
  
  model.performances <- performance.wrapper(model=prob,
                                            observation=data,
                                            prediction=prediction,
                                            prediction.probability=prediction.probability,
                                            taxa=taxa)
  
  return(model.performances)
}


# ann model ----
apply.ann.model <- function(data, split.type){
  
  
  # This function create the Artificial Neural Network (ANN) model and returns
  # its performance.The ANN is implemented using the Keras Library
  #
  # arguments
  #   - data: the observation used to train (and test) the ANN model
  #   - split.type: how the data has been split (FIT, CV, ODG)
  #
  # returns:
  #   - ann.model: the ANN, i.e. for each splits and each taxa, it is a list of: 
  #                   1. model
  #                   2. Observation (the data needed for prediction)
  #                   3. Prediction of each observation, 
  #                   4. Prediction probability of each observation
  #                   5. Likelihood of each observation 
  #                   6. Standard Deviance of the ann model
  #                   7. Area Under the Curve (AUC) of the ann model
  
  
  if (split.type == 'FIT'){
    
    # Get training set
    training.data <- na.omit(data[["Entire dataset"]])
    X.train         <- as.matrix(training.data[ ,ENV.FACT.COLNAMES])
    Y.train         <- as.matrix(training.data[ ,TAXA.COLNAMES])
    encoded.Y.train <- as.matrix(ifelse(Y.train == 'absent', 0, 1))
    
    # Get hyperparameters
    hyperparameters <- get.best.hyperparameters(data=training.data)
    
    # train model
    ann <- train.ann.model(hyperparameters,
                           X.train,
                           encoded.Y.train)
    
    # makes prediction
    prediction.probability <- ann %>% predict(X.train)
    colnames(prediction.probability) <- TAXA.COLNAMES

    # Consolidate results
    model.performances <- lapply(TAXA.COLNAMES,
                                 FUN=ann.model.perf,
                                 ann,
                                 prediction.probability,
                                 training.data)

    ann.model <- list("entire_dataset" = list("training" = model.performances))
    
  } 
  else if (split.type == 'CV'){
    
    nb.split <- length(data)
    ann.model <- vector("list", nb.split)
    names(ann.model) <- names(data) # split1, split2, ...
    
    # loop over the different splits
    for (i.split in seq(nb.split)){
      
      # Open datasets (Training and testing)
      split <- data[[i.split]]
      
      # Get training set
      training.data <- na.omit(split[[1]])
      X.train         <- as.matrix(training.data[ ,ENV.FACT.COLNAMES])
      Y.train         <- as.matrix(training.data[ ,TAXA.COLNAMES])
      encoded.Y.train <- as.matrix(ifelse(Y.train == 'absent', 0, 1))
      
      # Get testing set
      testing.data <- na.omit(split[[2]])
      X.test <- as.matrix(testing.data[ ,ENV.FACT.COLNAMES])
      Y.test <- as.matrix(testing.data[ ,TAXA.COLNAMES])
      encoded.Y.test <- ifelse(Y.test == 'absent', 0, 1)
      
      
      # Get hyperparameters
      hyperparameters <- get.best.hyperparameters(data=training.data)
      
      # Train model
      ann <- train.ann.model(hyperparameters,
                             X.train,
                             encoded.Y.train)
      
      # Makes prediction for training
      train.prediction.probability <- ann %>% predict(X.train)
      colnames(train.prediction.probability) <- TAXA.COLNAMES
      
      # Makes prediction for testing
      test.prediction.probability <- ann %>% predict(X.test)
      colnames(test.prediction.probability) <- TAXA.COLNAMES
      
      
      # Consolidate results
      train.model.performances <- lapply(TAXA.COLNAMES,
                                         FUN=ann.model.perf,
                                         ann,
                                         train.prediction.probability,
                                         training.data)
      
      test.model.performances  <- lapply(TAXA.COLNAMES, 
                                         FUN=ann.model.perf,
                                         ann,
                                         test.prediction.probability,
                                         testing.data)
      
      ann.model[[i.split]] <- list("training" = train.model.performances,
                                   "testing"  = test.model.performances)
      }
  }
  else if (split.type == 'ODG'){
    
    # TODO: to write
    
    ann.model <- 0
    
  }
  
  return(ann.model)
}

get.best.hyperparameters <- function(data){
  
  
  # The function is used to find the best hyperparameters for the ANN. It 
  # performs a 3-split Cross Validation for all hyperparameters found in the 
  # global variable TUNE.GRID[["ann"]] and returns the hyperparameters that lead
  # to the lowest loss (i.e. standard deviance). The hyperparameters to tune
  # are:
  #     1. Number of unit (i.e. width of hidden layers)
  #     2. Number (of hidden) layers
  #     3. Learning rate
  #     4. Number of epochs
  #     5. Batch size
  #
  # arguments
  #   - data: the data used to train and validate the ANN with different
  #
  # returns:
  #   - hyperparameters: the hyperparameters leading to the lowest standard
  #                      deviance
  
  
  # Get a list of all permutation of hyperparameters
  tune.grid <- TUNE.GRID[['ann']]
  nb.folds  <- 3.0
  
  folds <- groupKFold(data$SiteId, nb.folds)
  
  sum.losses <- list(double(nrow(tune.grid)))
  
  for (fold.index in seq(nb.folds)){
    
    train <- data[folds[[fold.index]],]
    test  <- data[-folds[[fold.index]],]
    
    
    X.train         <- as.matrix(train[ ,ENV.FACT.COLNAMES])
    Y.train         <- as.matrix(train[ ,TAXA.COLNAMES])
    encoded.Y.train <- as.matrix(ifelse(Y.train == 'absent', 0, 1))
    
    X.test          <- as.matrix(test[ ,ENV.FACT.COLNAMES])
    Y.test          <- as.matrix(test[ ,TAXA.COLNAMES])
  
    # Get the corresponding ann models
    models <- apply(tune.grid, MARGIN=1, FUN=train.ann.model, X.train, encoded.Y.train)
    # Compute corresponding standard deviances
    losses <- lapply(models, FUN=get.loss.score, X.test, Y.test)
    sum.losses <- mapply(sum, sum.losses, losses, SIMPLIFY=FALSE)
  }
  
  # loss is average over all split
  losses = lapply(sum.losses, FUN=function(x){x/nb.folds})
  # Get hyperparameter leading to smallest score
  index.lowest.loss <- which.min(losses)
  hyperparameters <- tune.grid[index.lowest.loss,]
  
  return(hyperparameters)
}

get.loss.score <- function(model, X, Y){

  
  # For a neural network model given as argument, this function compute the loss
  # resulting from predicting the response variables from the environmental 
  # factors. It computes the loss (standard deviance) for each taxon and then 
  # average it to produce a single loss for all the network.
  #
  # arguments
  #   - model: the Neural network used to predict the response variables   
  #   - X: the environmental factors (features)
  #   - Y: the true response variables (labels)
  #
  # returns:
  #   - loss: the resulting loss function

    
  pred.prob <- model %>% predict(X)
  colnames(pred.prob) <- TAXA.COLNAMES
  
  all.losses <- lapply(TAXA.COLNAMES, FUN=function(taxa, pred.vector, labels){
    
    prediction.probability <- data.frame("absent"  = 1-pred.vector[,taxa],
                                         "present" = pred.vector[,taxa])
    
    likelihood   <- compute.likelihood(labels, taxa, prediction.probability)
    performances <- -2*sum(log(likelihood)) / nrow(labels)
    
    return(performances)
  }, pred.prob, Y)
  
  loss <- mean(unlist(all.losses))

  return(loss)
}

train.ann.model <- function(hyperparameters, X, Y){
  
  
  # This function creates a Neural Network with the hyperparameters given as 
  # arguments and trains it with the data (X, Y) provided as arguments.
  #
  # arguments
  #   - hyperparameters: the hyperparameters used to create the network (number 
  #                      of units, number of layers and learning rate) and to  
  #                      train it (number of epoch, batch size) 
  #   - X: the environmental factors (features) used to train the network 
  #   - Y: the response variables (labels) used when training the network
  #
  # returns:
  #   - ann.model: the trained ANN 
  
  # get I/O size
  input.size      <- ncol(X)
  output.size     <- ncol(Y)
  
  # get hyper.parameters
  num.units       <- hyperparameters[[1]]
  num.layers      <- hyperparameters[[2]]
  learning.rate   <- hyperparameters[[3]]
  num.epochs      <- hyperparameters[[4]]
  batch.size      <- hyperparameters[[5]]
  
  
  # Get neural network model 
  ann <- get.ann.model(input.size=input.size,
                       output.size=output.size,
                       num.units=num.units,
                       num.layers=num.layers,
                       learning.rate=learning.rate)
  
  # Fit the model
  history <- ann %>% fit(x=X,
                         y=Y,
                         epochs=num.epochs,
                         batch_size=batch.size)
  
  return(ann)
}

get.ann.model <- function(input.size, output.size, num.units, num.layers, learning.rate){
  
  
  # This function instantiates and compiles the neural network according to the  
  # parameters given as arguments.
  #
  # arguments
  #   - input.size: size of the input layer  
  #   - output.size: size of the output layer
  #   - num.units: number of units (i.e width of hidden layer)
  #   - num.layers: number of layers opf the neural network
  #   - learning.rate: the learning rate
  #
  # returns:
  #   - ann.model: the untrained neural network
  
  
  # input layer
  model <- keras_model_sequential() %>% 
    layer_dense(units=num.units, input_shape=input.size) %>% 
    layer_activation_leaky_relu()
  
  # hidden layers
  if (num.layers>1){
    for (i in 1:(num.layers-1)){
      model <- model %>% layer_dense(units=num.units) %>% 
        layer_activation_leaky_relu()
    }
  }
  
  #output layer
  model <- model %>% layer_dense(units=output.size, activation="sigmoid")
  
  # display model
  summary(model) 
  
  # Compile model
  opt <- optimizer_adam(learning_rate=learning.rate)
  model %>% compile(optimizer = opt,
                    loss='binary_crossentropy',
                    metrics=list('accuracy'))
  
  return(model)
}

ann.model.perf <- function(taxa, model, full.prediction.probability, data){
  
  # This function returns the performance of the ANN model. The performance
  # consists of : 
  #     1. Model
  #     2. Observation (the data that needed a prediction)
  #     3. Prediction of each observation, 
  #     4. Prediction probability of each observation
  #     5. Likelihood of each observation 
  #     6. Standard Deviance of the ANN
  #     7. Area Under the Curve (AUC) of the ANN
  #
  # arguments
  #   - model: the ANN model
  #   - full.prediction.probability: the predicted probability for each taxon
  #                                  and each observation
  #   - data: The observations used to predict
  #
  # returns:
  #   - model.performance: the performance of the ANN model for all taxa
  
  taxa.pred.prob <- full.prediction.probability[,taxa]

  prediction <- as.factor(ifelse(taxa.pred.prob>0.5, "present", "absent"))
  prediction.probability <- data.frame("absent"  = 1 - taxa.pred.prob,
                                       "present" = taxa.pred.prob)
  
  model.performances <- performance.wrapper(model=model, #0 change back to model=0, for saving dataset
                                            observation=data,
                                            prediction=prediction,
                                            prediction.probability=prediction.probability,
                                            taxa=taxa)
  
  
  return(model.performances)
}


# caret model ----
apply.caret.model <- function(data, split.type, env.fact, model.name){
  
  
  # This function is used to create the different machine learning based models
  # and returns their performances.The models are created using the Caret
  # library
  #
  # arguments
  #   - data: the observation used to train (and test) the ANN model
  #   - split.type: how the data has been split (FIT, CV, ODG)
  #   - env.fact: a list of the environmental factor (i.e. features) to use to  
  #               train the model
  #   - model.name: the name of the model to create with caret
  #
  # returns:
  #   - caret.model: the ANN, i.e. for each splits and each taxon, it is a list
  #                  of: 
  #                   1. model
  #                   2. Observation (the data needed for prediction)
  #                   3. Prediction of each observation, 
  #                   4. Prediction probability of each observation
  #                   5. Likelihood of each observation 
  #                   6. Standard Deviance of the used model
  #                   7. Area Under the Curve (AUC) of the used model
  
  
  if (split.type == 'FIT'){
    
    training.data <- na.omit(data[["Entire dataset"]])
    models <- lapply(TAXA.COLNAMES,
                    FUN=train.caret.model,
                    training.data,
                    env.fact,
                    model.name)
    
    caret.models <- list("entire_dataset" = list("training" = models))
    
    
  } else if (split.type == 'CV'){
    
    nb.split <- length(data)
    caret.models <- vector("list", nb.split)
    names(caret.models) <- names(data) # split1, split2, ...
    
    # loop over the different splits
    for (i.split in seq(nb.split)){
      
      split <- data[[i.split]]
      
      training.data <- na.omit(split[[1]])
      testing.data <- na.omit(split[[2]])  
      
      # train
      train.models <- lapply(TAXA.COLNAMES,
                             FUN=train.caret.model,
                             training.data,
                             env.fact,
                             model.name)
      
      # get only the train models without the related performances
      models <- lapply(train.models,"[[", "model")
      
      test.models <- lapply(TAXA.COLNAMES,
                            FUN=test.model.perf,
                            models,
                            testing.data)
      
      caret.models[[i.split]] <- list("training" = train.models,
                                      "testing"  = test.models)
    }

  } else if (split.type == 'ODG'){

    # TODO: to write

    caret.models = 0
  }
  
  
  return(caret.models)
}

train.caret.model <- function(taxa, train.data, env.fact, method){
  
  
  # This function trains a caret machine learning model to predict a the 
  # presence/absence of a taxon based on some environmental factors. The method
  # to be used is given as argument as method. The data is provided in
  # train.data. The environmental factors (features) and taxon (label) are also
  # provided as arguments.
  #
  # arguments:
  #   - taxa: the presence/absence of which taxon should the model predicts
  #   - train.data: the data used to train the model 
  #   - env.fact: the environmental factors that should be used for the 
  #               prediction 
  #   - method: What ML method should the model implements 
  #
  # returns:
  #   - model.performance: the performances of the trained model, i.e
  #     1. Model
  #     2. Observation (the data that needed a prediction)
  #     3. Prediction of each observation, 
  #     4. Prediction probability of each observation
  #     5. Likelihood of each observation 
  #     6. Standard Deviance of the model
  #     7. Area Under the Curve (AUC) of the model
  
  
  # obtain tune.grid for parameter tuning
  tune.grid <- TUNE.GRID[[method]]
  
  if (is.null(tune.grid)){
    trainctrl <- trainControl(verboseIter = TRUE)
  } 
  else {
    nb.folds <- 3
    folds <- groupKFold(train.data$SiteId, nb.folds)
    trainctrl <- trainControl(method='cv',                  
                              number=nb.folds,                     
                              index=folds,
                              classProbs=T,                 
                              summaryFunction=standard.deviance,
                              selectionFunction=lowest,
                              verboseIter=TRUE)
  }
  
  # feature2 needs to be written as I(feature^2) in the formula such that the
  # model squares it.
  feature.string <- lapply(env.fact, 
                           FUN=function(x){
                             if (endsWith(x, "2")){
                                return(paste0("I(", gsub("2", "", x), "^2)"))                            
                             } else{
                               return(x)  
                             } 
                           })
  
  formula.string <- paste0(taxa,
                           " ~ ",
                           paste(feature.string, collapse = ' + '))
  
  caret.model <- train(form=formula(formula.string),
                      data=train.data,
                      method=method,
                      trControl=trainctrl,
                      tuneGrid=tune.grid)
  
  
  model.performance <- caret.model.perf(caret.model, train.data, taxa)
  return(model.performance)
}

caret.model.perf <- function(model, data, taxa){
  

  # This function returns the performance of a caret model. The performances
  # consists of : 
  #     1. Model
  #     2. Observation (the data that needed a prediction)
  #     3. Prediction of each observation, 
  #     4. Prediction probability of each observation
  #     5. Likelihood of each observation 
  #     6. Standard Deviance of the caret model
  #     7. Area Under the Curve (AUC) of the caret model
  #
  # arguments
  #   - model: the caret model
  #   - data: The observations used to predict
  #   - taxon: the taxon for which the model under consideration is trained to 
  #            make the prediction
  #
  # returns:
  #   - model.performance: the performance of the caret model for the taxon 
  #                        given as argument.
  
  
  # Compute model's performances
  prediction <- predict(model, data)
  prediction.probability <- predict(model, data, type = 'prob')
  
  
  # Save model's performances
  model.performances <- performance.wrapper(model=model,
                                            observation=data,
                                            prediction=prediction,
                                            prediction.probability=prediction.probability,
                                            taxa=taxa)
  
  return(model.performances)
}

test.model.perf <- function(taxa, models, data){
  
  
  # This function is getting a specific model from a list of models given as  
  # argument and then calling the caret.model.performance with the model and 
  # data provided as argument. This is done to get performance of models trained
  # with training data, but performance computed with testing data.
  
  # description
  #
  # arguments:
  #   - taxa: name of the taxon to predict 
  #   - models: list of the models to predict every taxon (each model can  
  #             predict the presence/absence of a single taxon)
  #   - data: the data to use to test the model
  #
  # returns:
  #   - perf: the performance of the model to predict the taxon provided as
  #           argument when tested with the data given as argument 
  
  taxa.short <- gsub("Occurrence.", "", taxa)
  model <- models[[taxa.short]]
  perf <- caret.model.perf(model, data, taxa)
  
  return(perf)
}


# misc functions ----
compute.likelihood <- function(data, taxa, prediction.probability){
  
  
  # The function uses true response variable (labels) and prediction probability
  # to compute the likelihood for the different observation
  #
  # arguments:
  #   - data: dataframe containing the responses variables for all taxa
  #   - taxa: the taxon for which likelihood should be computed 
  #   - prediction.probability: the prediction probability of of the given taxon, 
  #                             for the different observation of data
  #
  # returns:
  #   - likelihood: the likelihood for the given taxon for the different 
  #                 observations 
  
  
  lev <- c("present", "absent")
  taxa.column <- data[,taxa]
  likelihood <- 1:dim(data)[1]
  
  likelihood[which(taxa.column==lev[1])] <- prediction.probability[which(taxa.column==lev[1]), lev[1]]
  likelihood[which(taxa.column==lev[2])] <- prediction.probability[which(taxa.column==lev[2]), lev[2]]
  
  # clipping value to be above threshold value
  threshold <- 1e-4
  likelihood[which(likelihood < threshold)] <- threshold
  
  
  return(likelihood)
  
}

standard.deviance <- function(data, lev=c("present","absent"), model=NULL){
  
  
  # This function computes the standard deviance metric for given data, factors
  # and model. This function is used as loss when training the caret models 
  #
  # arguments:
  #   - data: the data to use to compute the standard deviance.
  #   - lev: the factors of the classification of the taxa
  #   - model: the model to use to compute the standard deviance
  #
  # returns:
  #   - standard.dev: the loss to return
  
  
  no.obs <- dim(data)[1]
  likelihood <- 1:no.obs
  
  likelihood[which(data$obs==lev[1])] <- data[which(data$obs==lev[1]), lev[1]]
  likelihood[which(data$obs==lev[2])] <- data[which(data$obs==lev[2]), lev[2]]
  
  threshold <- 1e-4
  likelihood[which(likelihood < threshold)] <- threshold
  
  standard.dev <- -2*sum(log(likelihood)) / no.obs
  names(standard.dev) <- "StandardizedDeviance"
  
  return(standard.dev)
}

lowest <- function (x, metric, maximize = F){
  
  
  # returns the lowest value of a specific metric 
  #
  # arguments:
  #   - x: dataframe containing different metric for each hyperparameters
  #   - metric: metric in datadframe x to get the lowest
  #   - maximize: flag not being used
  #
  # returns:
  #   - best: the lowest value of the metric  
  
  
  best <- which.min(x[, metric])
  return(best)
}

performance.wrapper <- function(model, observation, prediction, prediction.probability, taxa){

  
  # Function to summarize the performance of a model in a list of performances. 
  # The full performances are:
  #     1. Model
  #     2. Observation (the data that needed a prediction)
  #     3. Prediction of each observation, 
  #     4. Prediction probability of each observation
  #     5. Likelihood of each observation 
  #     6. Standard Deviance of the model
  #     7. Area Under the Curve (AUC) of the model
  #
  # arguments:
  #   - model: the model to assess the performances of
  #   - observation: the observations used to make the prediction
  #   - prediction: the categorical predictions (absent/present)
  #   - prediction.probability: the probability of presence of the given taxon 
  #                             for the different observations
  #   - taxa: the taxon under consideration
  #
  # returns:
  #   - performances: the performances as described above  
  
  likelihood <- compute.likelihood(observation, taxa, prediction.probability)
  performance <- -2*sum(log(likelihood)) / nrow(observation)
  
  auc <- auc(response =ifelse(observation[,taxa]=="present", 1, 0),
             predictor=prediction.probability[["present"]])
  
  model.performances <- list()
  
  model.performances[["model"]]                       <- model
  model.performances[["observation"]]                 <- observation
  model.performances[["prediction_factors"]]          <- prediction
  model.performances[["prediction_probabilities"]]    <- prediction.probability
  model.performances[["likelihood"]]                  <- likelihood
  model.performances[["standard_deviance"]]           <- performance
  model.performances[["auc"]]                         <- auc
  
  return(model.performances)
}


save.models <- function(models, path, split.type){
  
  models.path <- paste0(path, "/models_", split.type, ".rds") 
  
  if (file.exists(models.path)){
    cat("Models already in directory:", path, "\n")
    cat("Cannot overwrite existing models, please choose other directory.\n")
    return(0)
  }
  
  models.copy <- models
  
  if (split.type == "FIT"){
    
    
    if (!is.null(models[["ann"]])){
    
      # extract ANN
      ann <- models[["ann"]][["entire_dataset"]][["training"]][[1]][["model"]]
      
      # replace ANN by 0 in models' list
      taxa.list = names(models.copy[["ann"]][["entire_dataset"]][["training"]])
      
      for (taxon in taxa.list){
        models.copy[["ann"]][["entire_dataset"]][["training"]][[taxon]][["model"]] <- 0
      }
      
      # save ANN to file
      ann.path = paste0(path, "/ann_", split.type)
      save_model_tf(ann, ann.path)
    
    }
    # save models list
    saveRDS(models.copy, file=models.path)
    
    
  }else if (split.type == "ODG"){
    # TODO
  }else if (split.type == "CV"){
    
    
    if (!is.null(models[["ann"]])){
      split.names <- names(models.copy[[1]])
      
      for (split in split.names){
      
        # extract ann of current split
        current.ann <- models[["ann"]][[split]][["training"]][[1]][["model"]]
        
        # for every split both in training and testing replace ann by 0
        taxa.list = names(models.copy[["ann"]][[split]][["training"]])
        
        for (taxon in taxa.list){
          models.copy[["ann"]][[split]][["training"]][[taxon]][["model"]] <- 0
          models.copy[["ann"]][[split]][["testing"]][[taxon]][["model"]] <- 0
        }
        
        # save ann of current split
        current.ann.path = paste0(path, "/ann_", split.type, "_", split)
        
        
        save_model_tf(current.ann, current.ann.path)
        
        
      }
    }
    
    # save list of models
    saveRDS(models.copy, file=models.path)
  }
  
  return(0)
}

load.models <- function(path, split.type){
  
  if (split.type=='FIT'){
    
    models.path <- paste0(path, "/models_", split.type, ".rds")
    models.list <- readRDS(file=models.path)  # load models' list

    
    
    
    ann.path = paste0(path, "/ann_", split.type)
    
    # load ann and copy it back in list if it exists
    if(dir.exists(ann.path)){
    
      ann <- load_model_tf(ann.path)
      taxa.list = names(models.list[["ann"]][["entire_dataset"]][["training"]])
      
      for (taxon in taxa.list){
        models.list[["ann"]][["entire_dataset"]][["training"]][[taxon]][["model"]] <- ann
      }
    }
    
    return(models.list)
    
  }else if (split.type=='ODG'){
    # TODO
  }else if (split.type=='CV'){
    
    models.path <- paste0(path, "/models_", split.type, ".rds")
    models.list <- readRDS(file=models.path)
      
    split.names <- names(models.list[[1]])
    
    for (split in split.names){
      
      current.ann.path <- paste0(path, "/ann_", split.type, "_", split)
      
      # load current split's ann model and copy it back in list if it exists
      if(dir.exists(current.ann.path)){
        
        current.ann <- load_model_tf(current.ann.path)
        taxa.list = names(models.list[["ann"]][[split]][["training"]])
        
        for (taxon in taxa.list){
          models.list[["ann"]][[split]][["training"]][[taxon]][["model"]] <- current.ann
          models.list[["ann"]][[split]][["testing"]][[taxon]][["model"]]  <- current.ann
        }
      }
    }
    
    return(models.list)
  }
  
  return(0)
}
