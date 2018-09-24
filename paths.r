#paths.r file for Averill et al. ISMEJ in revision.

#high-level directory structure.----
#NEFI_data - master data directory.
host <- system('hostname', intern=T)
#data directory conditional to which computer you are working on.
data.dir <- '/projectnb/talbot-lab-data/caverill/Averill_ISMEJ_2018_data/'
#conditional data directory assignment.
if(host == 'pecan2'){data.dir <- '/fs/data3/caverill/Averill_ISMEJ_2018_data/'}
system(paste0('mkdir -p ',data.dir))

#original_data: files received from authors or downloaded from the internet.
raw.dir <- paste0(data.dir,'original_data/')
system(paste0('mkdir -p ',raw.dir))

#pecan_gen: files generated on pecan2.
pecan.dir <- paste0(data.dir,'pecan_gen/')
system(paste0('mkdir -p ',pecan.dir))

#scc_gen: files generated on the scc/geo/whatever.
scc.dir <- paste0(data.dir,'scc_gen/')
system(paste0('mkdir -p ',scc.dir))

#Tedersoo file paths.----
#original raw data.
 ted_raw_otu.path <- paste0(raw.dir,'alldata.biom_rdp_tedersoo_otu.txt')
 ted_raw_map.path <- paste0(raw.dir,'merging seq and site data.csv')
ted_raw_time.path <- paste0(raw.dir,'tedersoo2014_dates.csv')

#tedersoo pecan gen data
ted_clean_map.path <- paste0(pecan.dir,'map_tedersoo2014_filtered.rds')
ted_clean_otu.path <- paste0(pecan.dir,'otu_tedersoo2014_norm.rds')
#post model fitting data for figures and tables.
    ted_model_output.path <- paste0(pecan.dir,'tedersoo_R1_data_fitted_st.env.rds')

#tedersoo bootstrap run on scc.
ted_bootstrap_output.path <- paste0(scc.dir,'tedersoo2014_bootstrap_data.rds')


#Talbot 2014 Files.----
#raw files.
tal_raw_otu.path <- paste0(raw.dir,'DOB_Soil_OTU_Table_11Feb13_UNITETAX_with_taxonomy.csv')
tal_raw_map.path <- paste0(raw.dir,'Talbot2014_mapping_with_spatial.rds')
tal_ref_sim.path <- paste0(raw.dir,"Talbot2014_brayCurtis500Avg_All.csv")
#pecan_gen: clean for analysis paths.
tal_clean_map.path <- paste0(pecan.dir,'tal_map_filtered.rds')
tal_clean_otu.path <- paste0(pecan.dir,'tal_otu_filtered.rds')
#post model fitting data for figures and tables.
    tal_model_output.path <- paste0(pecan.dir,'talbot_R1_data_fitted_st.env.rds')
#scc_gen: bootstrap results.
tal_bootstrap_output.path <- paste0(scc.dir,'talbot2014_bootstrap_data.rds')

#Kivlin 2016 Files.----
#original_data: raw files.
kiv_raw_bac_otu.path <- paste0(raw.dir,'BacteriaOTUTable_2016_04_15.csv')
kiv_raw_bac_map.path <- paste0(raw.dir,'BacteriaENVNoPrimaryNoFeb2012_2015_08_02.csv')
kiv_raw_fun_otu.path <- paste0(raw.dir,'FungiDeSeq2NoSampleSingletonsNoPlants2015_11_04.csv')
kiv_raw_fun_map.path <- paste0(raw.dir,'ENVfungiNoPrimaryNoFeb2012_2015_10_16.csv')
#pecan_gen: data formatting.
kiv_clean_bac_otu.path <- paste0(pecan.dir,'kivlin.bacteria_otu_formatted.rds')
kiv_clean_bac_map.path <- paste0(pecan.dir,'kivlin.bacteria_map_formatted.rds')
kiv_clean_fun_otu.path <- paste0(pecan.dir,'kivlin.fungi_otu_formatted.rds')
kiv_clean_fun_map.path <- paste0(pecan.dir,'kivlin.fungi_map_formatted.rds')
#pecan_gen: models and parameters.
kiv_bac_figure_data.path <- paste0(pecan.dir,'kiv_bac_figure_data.rds')
kiv_fun_figure_data.path <- paste0(pecan.dir,'kiv_fun_figure_data.rds')
      kiv_bac_param.path <- paste0(pecan.dir,'kiv_bac_param.rds')
      kiv_fun_param.path <- paste0(pecan.dir,'kiv_fun_param.rds')
      
#pecan_gen: bootstrap results.
kiv_bac_bootstrap.path <- paste0(pecan.dir,'kiv_bac_bootstrap.rds')
kiv_fun_bootstrap.path <- paste0(pecan.dir,'kiv_fun_bootstrap.rds')
kiv_bac_sub100_bootstrap.path <- paste0(pecan.dir,'kiv_bac_sub100_bootstrap.rds')
kiv_fun_sub100_bootstrap.path <- paste0(pecan.dir,'kiv_fun_sub100_bootstrap.rds')

