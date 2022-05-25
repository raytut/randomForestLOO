rf.boot <- function(data, sub_id, xvars, yvar, method, ntree, iterations, ncore){
    # Load libs
    require(dplyr)
    require(randomForest)
    require(future)
    require(doSNOW)

    # Assign default values
    if(missing(iterations)){
        iterations<-5000}
    if(missing(ntree)){
        ntree <- 1000 }
    if (missing(method)){
        method<-"LOBO" }
    if (missing(ncore)){
        ncore<-2 }

    # Set up empty lists to use later
    rf.output <- NULL
    rf.output$results <- NULL

    # Inform the user and set up progress bar
    print (paste("Running", iterations, "bootstrap iterations on", ncore, "cores. Grab a coffee, I'ĺl let you know when I'm done..."), quote=F)
    pb <- txtProgressBar(max=iterations, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    # Assign cluster nodes
    cl <- makeCluster(ncore-1)
    registerDoSNOW(cl, type="FORK")
    # Start parallel loop using dosnow
    forest.boot <- foreach(i=1:iterations, .options.snow=opts, .errorhandling="remove",
                                 .export = c( "sub_id", "xvars", "yvar", "ntree","method", "progress", "rf.output", "rf.LOO"),
                                 .packages = c("dplyr", "randomForest"), .verbose=FALSE)  %dopar% {

        # Specify data frame and shuffle coloumns
        data_shuffle <- data
        data_shuffle[[yvar]] <- sample(data_shuffle[[yvar]], replace=T)

        # Run LOBO function on shuffled Data
        forest.lobo.shuffle <- rf.LOO(data=data_shuffle,
                                       sub_id=sub_id,
                                       xvars=xvars,
                                       yvar=yvar,
                                       ntree=ntree,
                                       method=method,
                                       progress=F)
        # Record the prediction rates
        forest.shuffle.probability <- forest.lobo.shuffle$errors
        forest.shuffle.probability <- as.data.frame(forest.shuffle.probability)
    }
    # Stop the cluster
    stopCluster(cl)
    close(pb)
    print("Bootstraps done! Cleaning up the results...")

    # Consolidate the results into a clean data frame (This cant be done in parallel function due to some iterations failing)
    try({
        df.boot <-  forest.boot[[1]]
        df.boot$id <-  rownames(forest.boot[[1]])
        colnames(df.boot)[1] <- "iter_1"
        # Merge remaining dataframes into one
        for (dataframes in 2:length(forest.boot)){
            df.temp <- forest.boot[[dataframes]]
            df.temp$id <-  rownames(df.temp)
            colnames(df.temp)[1] <- paste("iter", dataframes, sep="_")
            df.boot  <- merge(df.boot, df.temp, by="id", all = TRUE)
        }
    },silent=TRUE)

    # Combine output into a single item
    rf.output$results <- df.boot
    rf.output$results$mean_error <- rowMeans(rf.output$results [, -which(names(rf.output$results) %in% c("id"))], na.rm=TRUE)
    rf.output$errors <- rf.output$results %>% dplyr::select(id, mean_error)
    rf.output$mean_error <- mean(rf.output$errors$mean_error, na.rm=TRUE)

    # Save a summary
   try({
        rf.output$summary <- psych::describe(rf.output$results$mean_error)
        rf.output$summary$sd <-  mean((psych::describe(df.boot))$sd, na.rm=TRUE)
        rf.output$summary$se <- mean((psych::describe(df.boot))$se, na.rm=T)}, silent=TRUE)
    print("Done!")
    class(rf.output) <- "RF-Boot"
    return(rf.output)
}
