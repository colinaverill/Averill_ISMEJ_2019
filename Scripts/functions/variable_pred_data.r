#Give this function a model, complete cased data used to fit the model, and a variable name.
#Returns a data set with the variable of interest preserved, and every other covariate meaned.
variable_pred_data <- function(some_data, model,variable){
  pred.data <- some_data[,names(model$coefficients)[2:length(names(model$coefficients))]]
  of_interest <- pred.data[,c(variable)]
  to_mean <- pred.data[,names(pred.data) != variable]
  for(i in 1:ncol(to_mean)){
    to_mean[,i] <- mean(to_mean[,i])
  }
  out <- cbind(of_interest,to_mean)
  colnames(out)[1] <- variable
  #deal with new way we are doing the time interaction.
  if('time_int' %in% colnames(out)){out$time_int <- out$doy * out$time}
  return(out)
}
