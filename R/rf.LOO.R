rf.LOO <- function(data, sub_id, xvars, yvar, method, ntree, progress){

    # Set defaults + seed for consistency
    require(randomForest)
    require(caret)
    set.seed(123)
    if(missing(progress)){
        progress<-TRUE}
    if(missing(ntree)){
        ntree <- 1000 }
    if (missing(method)){
        method<-"LOBO" }
    # Make sure Y-var is factor
    if ( !is.factor(data[[yvar]])){
        warning(paste("Variable", yvar, "is not factor! Converted to factor!"))
        data[[yvar]] <- factor(data[[yvar]]) }

    # Initalize object
    rf_loo <- NULL
    # Save Model Information
    rf_loo$model$xvar <- xvars
    rf_loo$model$predicting <- yvar
    rf_loo$model$trees <- ntree
    rf_loo$model$metohd <- method

    # Generate rest of objects we will need
    rf_loo$subjects <- NULL
    rf_loo$subjects$conf_mat <- NULL
    rf_loo$subjects$predictions <- NULL
    rf_loo$subjects$error_rate <- NULL
    rf_loo$mean_error <- NULL

    # If the model is a LOSO:
    if(method=="LOSO"){

        # Progress Trackers
        if (progress == TRUE){pb = txtProgressBar(min=0, max=length(unique(data[[sub_id]])), initial=0, style=3); stepi=0}

        # Combine all variables for subsetting
        Vars=c(yvar, xvars)

        # Initiate subject level loop
        for (i in unique(data[[sub_id]])){

            # Update progress bar
            if (progress == TRUE){stepi=stepi+1; setTxtProgressBar(pb,stepi)}

            # Run models
            try({
                # Make the DF without the subject
                df.train <- dplyr::filter(data, sub_id!=i)
                df.train <- df.train[, Vars]
                df.train <- na.omit(df.train)

                # Make a random forest with the training data
                forest.train <- randomForest( as.formula(paste(yvar, "~", paste(xvars, collapse="+"))),
                                              data=df.train,
                                              ntree=ntree,
                                              importance = TRUE)

                # Make the test data frame with the subject left out
                df.test <- dplyr::filter(data, sub_id==i)
                df.test <- df.test[, Vars]
                df.test <- na.omit(df.test)

                # Make the predictions, and save as df
                predictions <- predict(forest.train, newdata=df.test)
                actual <- df.test[[yvar]]
                prediction_df <- data.frame(actual=actual, predicted=predictions)
                rf_loo$subject[[i]]$predictions <- prediction_df

                temp_df <- rf_loo$subject[[i]]$predictions
                # Get confusion Matrix for Subject
                rf_loo$subject[[i]]$conf_mat <- confusionMatrix(reference=temp_df[[1]], data=temp_df[[2]])
                rf_loo$subject[[i]]$error_rate <- 1- rf_loo$subject[[i]]$conf_mat$overall["Accuracy"][[1]]
                rf_loo$errors[i] <- 1-rf_loo$subject[[i]]$conf_mat$overall["Accuracy"][[1]]
            })
        }
        # Close progress bar
        if (progress == TRUE){close(pb)
        }
        # If we specify LOBO
    }else if(method=="LOBO"){

        # Setup progress bar
        if (progress == TRUE){pb = txtProgressBar(min = 0, max=length(unique(data[[sub_id]])), initial = 0, style=3); stepi=0}

        # Combine all variables for subsetting
        Vars=c(yvar, xvars)

        # Initiate subject level loop
        for (i in unique(data[[sub_id]])){

            # Update progress bar
            if (progress == TRUE){stepi=stepi+1; setTxtProgressBar(pb,stepi)}

            # Run Models
            try({ # i='sub_001'

                # Make a df for a single subject
                df.rf <- dplyr::filter(data, sub_id==i)
                df.rf <- df.rf[, Vars]
                df.rf <- na.omit(df.rf)

                # Make the predictions object
                rf_loo$subject[[i]]$predictions

                # Initialize dataframe for actual and predicted values
                prediction_df <- data.frame(matrix(ncol=2, nrow=0))
                colnames(prediction_df) <- c("actual", "predicted")
                for (j in 1:nrow(df.rf)) { # j=3
                    # Subset data frames
                    df.test <- df.rf[j,]
                    df.train <- df.rf[-c(j),]
                    # Train forest
                    forest.train <- randomForest (as.formula(paste(yvar,"~", paste(xvars,collapse="+"))),
                                                  data=df.train,
                                                  ntree=ntree,
                                                  importance = TRUE)

                    # Get the actual and predicted value
                    predicted <- as.character(predict(forest.train, newdata=df.test))
                    actual <- as.character(df.test[[yvar]])
                    temp_prediction <- data.frame(actual=actual, predicted=predicted)
                    prediction_df <- rbind(prediction_df, temp_prediction)

                }

                # Save predictions and confusion matrix
                rf_loo$subject[[i]]$predictions <- prediction_df

                # Make a temp dataframe for confusion matrix
                temp_df <- rf_loo$subject[[i]]$predictions
                temp_df[[1]] <- as.factor(temp_df[[1]])
                temp_df[[2]] <- as.factor(temp_df[[2]])
                # Generate confusion Matrix
                rf_loo$subject[[i]]$conf_mat <- confusionMatrix(reference=temp_df[[1]], data=temp_df[[2]])
                rf_loo$subject[[i]]$error_rate <- 1- rf_loo$subject[[i]]$conf_mat$overall["Accuracy"][[1]]
                rf_loo$errors[i] <- 1-rf_loo$subject[[i]]$conf_mat$overall["Accuracy"]

            }, silent=T)
        }
        # Close progress bar
        if (progress == TRUE){close(pb)}
    }

    # Return Model
    if(method=="LOSO"){
        class(rf_loo) <- "RF-LOSO"
    }
    if(method=="LOBO"){
        class(rf_loo) <- "RF-LOBO"
    }
    rf_loo$mean_error <- mean(rf_loo$errors, na.rm=TRUE)
    return(rf_loo)
}
