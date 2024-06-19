# Load the necessary libraries
library(tidyverse)

# Function to read the DADA2 FASTA file and split it into two files
split_dada2_taxonomy <- function(dada2_file, fasta_output, taxa_output) {
  # Read the FASTA file
  fasta_lines <- readLines(dada2_file, warn = FALSE)
  
  # Check and add a new line at the end if necessary
  if (length(fasta_lines) %% 2 != 0) {
    fasta_lines <- c(fasta_lines, "")
  }
  
  # Initialize lists to store information
  ids <- c()
  sequences <- c()
  taxa <- c()
  
  # Loop through the FASTA file lines
  for (i in seq(1, length(fasta_lines), by = 2)) {
    # Extract the header and sequence
    header <- fasta_lines[i]
    sequence <- fasta_lines[i + 1]
    
    # Extract the taxonomy and ID from the header
    taxon <- gsub("^>", "", header)
    taxon_parts <- strsplit(taxon, ";")[[1]]
    id <- taxon_parts[length(taxon_parts)]
    taxonomy <- paste(taxon_parts[-length(taxon_parts)], collapse = ";")
    # Add the semicolon at the end of the taxonomy
    taxonomy <- paste0(taxonomy, ";")
    
    # Add the information to the lists
    ids <- c(ids, id)
    sequences <- c(sequences, sequence)
    taxa <- c(taxa, taxonomy)
  }
  
  # Create a DataFrame for the sequences
  fasta_df <- data.frame(
    id = ids,
    sequence = sequences,
    stringsAsFactors = FALSE
  )
  
  # Create a DataFrame for the taxonomy
  taxa_df <- data.frame(
    id = ids,
    taxonomy = taxa,
    stringsAsFactors = FALSE
  )
  
  # Write the sequences in FASTA format
  writeLines(
    paste0(">", fasta_df$id, "\n", fasta_df$sequence),
    con = fasta_output
  )
  
  # Write the taxonomy in TSV format
  write.table(taxa_df, taxa_output, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# Path to your DADA2 taxonomy file
dada2_file <- "path/to/your/dada2_file.fasta"

# Paths to the output files
fasta_output <- "path/to/output/mothur_fasta.fasta"
taxa_output <- "path/to/output/mothur_taxa.tax"

# Call the function to split the file
split_dada2_taxonomy(dada2_file, fasta_output, taxa_output)
