rf.null_test <- function(loo_input, bootstrap_error, model_type, method){

    # If type if missing, assume LOBO
    if(missing(model_type)){
        model_type="LOBO"}
    # If method not specified, test against null distribution
    if(missing(method)){
        method="norm" }

    # Load Required library + Initiate output variables
    require(poolr)
    require(purrr)
    null_test=list()
    null_test$p_val=array()

    # Load in the bootstrapped data here
    df.boot <- bootstrap_error$results

    # Begin loop
    for (i in 1:length(df.boot[,1])){
        # Select subject + bootstrap errors
        sub=(df.boot[i,1])
        sub_bootstrap <-(df.boot[i,2:(ncol(df.boot)-1)])
        # Turn to a coloumn
        sub_bootstrap <- tidyr::pivot_longer(sub_bootstrap, cols=(1:ncol(sub_bootstrap)))
        sub_bootstrap <- sub_bootstrap[2]
        # Get mean and sd
        sub_bootstrap_mean <- mean(sub_bootstrap[[1]], na.rm=TRUE)
        sub_bootstrap_sd <- sd(sub_bootstrap[[1]], na.rm=TRUE)
        # Do a Z transformation of toal scores
        sub_bootstrap_z <-  scale(sub_bootstrap[[1]], center = TRUE, scale = TRUE)

        # If LOBO and P value calcualted from normal distribution
        if (model_type=="LOBO"){
            if (method=="norm"){
                # Get mean error
                sub_mean_error<- (loo_input$errors[i])
                # Z-Transfomr and divide by sd for Z-Score
                sub_mean_error_z <- (loo_input$errors[i]) - sub_bootstrap_mean
                sub_z_score <- sub_mean_error_z/sub_bootstrap_sd
                # Append to list
                null_test$p_val[i]<-pnorm(sub_z_score)
            }}

        # If LOBO + P value from actual distribution of error
        if (model_type=="LOBO"){
            if (method=="actual"){
                # Get mean error + Calculate proprotion less than
                sub_mean_error<- (loo_input$errors[i])
                null_test$p_val[i]<-(sum((sub_bootstrap[[1]] < sub_mean_error),na.rm=T))/(length(sub_bootstrap[[1]]))
            }}

        # If LOSO + Normal distribution
        if (model_type=="LOSO"){{
            if (method=="norm")
                # Get mean error
                sub_mean_error<- (loo_input$errors[i])
                # Z-Transfomr and divide by sd for Z-Score
                sub_mean_error_z<- (loo_input$errors[i]) - sub_bootstrap_mean
                sub_z_score <- sub_mean_error_z/sub_bootstrap_sd
                # Append to list
                null_test$p_val[i]<-pnorm(sub_z_score)   }}

        # If LOSO + Actual
        if (model_type=="LOSO"){{
            if (method=="actual")
                # Get mean error of model
                sub_mean_error <- loo_input$errors[i]
                # compare to null
                null_test$p_val[i]<-(sum((sub_bootstrap[[1]] < sub_mean_error),na.rm=TRUE))/(length(sub_bootstrap[[1]]))
            }}
    }

    # Remove the nans to combile the p-values
    p_val <- null_test$p_val[!is.na(null_test$p_val)]
    null_test$stouffer <- stouffer(p_val)
    return(null_test)
    print(null_test$stouffer)
}






