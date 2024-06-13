# package 
library(tidyverse)

# Fonction to convert the DADA2 taxonomic fasta file to two mothur files : the taxa and the fasta file 
split_dada2_taxonomy <- function(dada2_file, fasta_output, taxa_output) {
  # Lire le fichier FASTA
  fasta_lines <- readLines(dada2_file, warn = FALSE)
  
  # Checking and correction of the number of lines
  if (length(fasta_lines) %% 2 != 0) {
    fasta_lines <- c(fasta_lines, "")
  }
  
  # List initialization 
  ids <- c()
  sequences <- c()
  taxa <- c()
  
  # screening of the FASTA file
  for (i in seq(1, length(fasta_lines), by = 2)) {
    # Extraire l'en-tête et la séquence
    header <- fasta_lines[i]
    sequence <- fasta_lines[i + 1]
    
    # extraction of the id and taxonomy from the header
    taxon <- gsub("^>", "", header)
    taxon_parts <- strsplit(taxon, ";")[[1]]
    id <- taxon_parts[length(taxon_parts)]
    taxonomy <- paste(taxon_parts[-length(taxon_parts)], collapse = ";")
    
    # add the id and taxonomy to the lists
    ids <- c(ids, id)
    sequences <- c(sequences, sequence)
    taxa <- c(taxa, taxonomy)
  }
  
  # dataframe for sequences
  fasta_df <- data.frame(
    id = ids,
    sequence = sequences,
    stringsAsFactors = FALSE
  )
  
  # dataframe for taxonomy
  taxa_df <- data.frame(
    id = ids,
    taxonomy = taxa,
    stringsAsFactors = FALSE
  )
  
  # FASTA file writting
  writeLines(
    paste0(">", fasta_df$id, "\n", fasta_df$sequence),
    con = fasta_output
  )
  
  # TSV taxonomy writting
  write.table(taxa_df, taxa_output, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# Path to DADA2 taxonomy
dada2_file <- "refDADA2.fasta"

# Path to output files
fasta_output <- "ref_mothur.fasta"
taxa_output <- "ref_mothur.tax"

# function calling 
split_dada2_taxonomy(dada2_file, fasta_output, taxa_output)
