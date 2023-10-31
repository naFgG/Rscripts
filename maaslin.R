rm(list = ls())
library(Maaslin2)

input_data <- system.file("extdata", "HMP2_taxonomy.tsv", package="Maaslin2") 
input_metadata <- system.file("extdata", "HMP2_metadata.tsv", package="Maaslin2") 

# get the pathway (functional) data - place holder
download.file("https://raw.githubusercontent.com/biobakery/biobakery_workflows/master/examples/tutorial/stats_vis/input/pathabundance_relab.tsv", "./pathabundance_relab.tsv")

df_input_data <- read.table(file=input_data, header=T, sep="\t", row.names=1, stringsAsFactors=F)
df_input_metadata <- read.table(file= input_metadata, header=T, sep="\t", row.names=1, stringsAsFactors=F)
df_input_path <- read.csv("./pathabundance_relab.tsv", sep="\t", stringsAsFactors=F, row.names=1)

# the association between microbial species abundance vs. IBD diagnosis and dysbiosis scores 
#> In this case, the example input data has been pre-normalized and pre-filtered, 
#> so we turn off the default normalization and prevalence filtering as well
fit_data <- Maaslin2(input_data=input_data, 
                     input_metadata=input_metadata, 
                     min_prevalence=0,
                     normalization="NONE",
                     output="demo_output", 
                     fixed_effects=c("diagnosis", "dysbiosis"),
                     reference=c("diagnosis,nonIBD"))


#This can also be done with with the HUMAnN 3 untiliy `humann_split_stratified_table`
unstrat_pathways <- function(dat_path){
  temp <- dat_path[!grepl("\\|",rownames(dat_path)),]
  return(temp)
}

df_input_path <- unstrat_pathways(df_input_path)
fit_func <- Maaslin2(input_data=df_input_path, 
                    input_metadata=df_input_metadata, 
                    output="demo_functional", 
                    fixed_effects=c("diagnosis", "dysbiosis"),
                    reference=c("diagnosis,nonIBD"),
                    min_abundance=0.0001,
                    min_prevalence=0.1)
