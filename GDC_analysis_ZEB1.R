library(readr)
library(dplyr)
library(ggplot2)

manifest_path <- '/Users/Seeta/Documents/GitHub/INTD561_Paper/gdc_sample_sheet.2023-11-17.tsv'
main_folder_path <- '/Users/Seeta/Documents/GitHub/INTD561_Paper/expression_data/'

manifest <- read_tsv(manifest_path)

print(colnames(manifest))

results <- data.frame(File_Name = character(), FPKM_Unstranded = numeric(), Sample_Type = factor(), stringsAsFactors = FALSE)

file_name_col <- "File Name"
sample_type_col <- "Sample Type"

for(i in 1:nrow(manifest)) {
  file_name <- manifest[[file_name_col]][i]
  sample_type <- manifest[[sample_type_col]][i]
  
  file_path <- file.path(main_folder_path, file_name)
  
  if(file.exists(file_path)) {
    expression_data <- read_tsv(file_path, skip = 1, col_names = TRUE)
    
    FPKM_value <- expression_data %>% 
      filter(!grepl("^N_", gene_name)) %>% 
      filter(gene_name == 'ZEB1-AS1') %>%
      pull(fpkm_unstranded)
    
    if (length(FPKM_value) > 0) {
      results <- rbind(results, data.frame(File_Name = file_name, FPKM_Unstranded = FPKM_value, Sample_Type = sample_type))
    }
  }
}

ggplot(results, aes(x = Sample_Type, y = FPKM_Unstranded, fill = Sample_Type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'FPKM Unstranded for ZEB1-AS1', x = 'Sample Type', y = 'FPKM Unstranded')

ggsave('/Users/Seeta/Documents/GitHub/INTD561_Paper/ZEB1-AS1_FPKM.png', width = 10, height = 6)


for(i in 1:nrow(manifest)) {
  file_name <- manifest[[file_name_col]][i]
  sample_type <- manifest[[sample_type_col]][i]
  
  file_path <- file.path(main_folder_path, file_name)
  
  if(file.exists(file_path)) {
    expression_data <- read_tsv(file_path, skip = 1, col_names = TRUE)
    
    FPKM_value <- expression_data %>% 
      filter(!grepl("^N_", gene_name)) %>%
      filter(gene_name == 'NUDT3') %>%
      pull(fpkm_unstranded)
    
    if (length(FPKM_value) > 0) {
      results <- rbind(results, data.frame(File_Name = file_name, FPKM_Unstranded = FPKM_value, Sample_Type = sample_type))
    }
  }
}

ggplot(results, aes(x = Sample_Type, y = FPKM_Unstranded, fill = Sample_Type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'FPKM Unstranded for NUDT3', x = 'Sample Type', y = 'FPKM Unstranded')

ggsave('/Users/Seeta/Documents/GitHub/INTD561_Paper/NUDT3_FPKM.png', width = 10, height = 6)
