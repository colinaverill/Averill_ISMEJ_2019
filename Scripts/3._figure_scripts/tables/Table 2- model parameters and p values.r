#Generate a table of parameter estimates and p-values for each model.
#st, env or st.env models
#load all fitted models
rm(list=ls())
source('paths.r')
library(data.table)

tal <- readRDS(tal_model_output.path)
tal.st <- tal$st.mrm
tal.env <- tal$env.mrm
tal.st.env <- tal$st.env.mrm

ted <- readRDS(ted_model_output.path)
ted.st <- ted$st.mrm
ted.env <- ted$env.mrm
ted.st.env <- ted$st.env.mrm


tal.st.p <- data.frame(tal.st$coef); colnames(tal.st.p) <- c('NA Fungi st parm','NA Fungi st p.val')
tal.env.p <- data.frame(tal.env$coef); colnames(tal.env.p) <- c('NA Fungi env parm','NA Fungi env p.val')
tal.st.env.p <- data.frame(tal.st.env$coef); colnames(tal.st.env.p) <- c('NA Fungi st.env parm','NA Fungi st.env p.val')

ted.st.p <- data.frame(ted.st$coef)    ; colnames(ted.st.p)     <- c('Global Fungi st parm','Global Fungi st p.val')
ted.env.p <- data.frame(ted.env$coef)   ; colnames(ted.env.p)    <- c('Global Fungi env parm','Global Fungi env p.val')
ted.st.env.p <- data.frame(ted.st.env$coef); colnames(ted.st.env.p) <- c('Global Fungi st.env parm','Global Fungi st.env p.val')


par.list <- list(tal.st.p,tal.env.p,tal.st.env.p,ted.st.p,ted.env.p,ted.st.env.p)
for(i in 1:length(par.list)){
  par.list[[i]]$var <- rownames(par.list[[i]])
}

out <- merge(par.list[[1]],par.list[[2]], all = T)
for(i in 3:length(par.list)){
  out <- merge(out,par.list[[i]], all = T)
}

#reorder the rows. There is for some reason not a straightforward solution here.
#I guess I could have transposed the thing.
out <- data.table(out)
row.order1 <- c('Int','space','time','doy','MAT','MAP','MAT_CV','MAP_CV','pC','cn','pH','moist')
row.order2 <- c(1:length(row.order1))
to_order <- data.frame(row.order1,row.order2)
colnames(to_order)[1] <- c('var')
out <- merge(out,to_order)
out <- out[order(row.order2),]
out[,row.order2 := NULL]

#save output.
write.csv(out,'/fs/data3/caverill/Microbial_Space_Time_data/model_output_table.csv')
