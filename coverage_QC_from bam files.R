library(tidyverse)
library(Rsamtools)
library(GenomicAlignments)
library(GenomicRanges)


# Load the BAM file
bamfile <- BamFile("sample.bam")

# Get alignment statistics
stats <- quickBamFlagSummary(bamfile)
print(stats)


#Alignment Summary

alignments <- readGAlignments(bamfile)


# Define a function to count duplicate reads
count_duplicates <- function(alignments) {
  # Extract the flags from the GAlignments object
  flags <- mcols(alignments)$flag
  # Check for duplicates based on the flag
  sum((flags & 0x400) != 0)
}

# Duplicate Reads


duplicate_reads <- count_duplicates(alignments)
print(paste("Number of duplicate reads:", duplicate_reads))

#Coverage analysis


granges <- GRanges(seqnames = seqnames(seqinfo(bamfile)),
                   ranges = IRanges(start = 1, end = seqlengths(bamfile)))
coverage <- coverage(readGAlignments(bamfile))
mean_coverage <- mean(coverage)
median_coverage <- median(coverage)
print(paste("Mean coverage:", mean_coverage))
print(paste("Median coverage:", median_coverage))

#visualizing coverage
# Extract coverage values from GRangesList
coverage_values <- unlist(coverage)


meancov <- data.frame(mean_coverage)

meancov %>% 
  ggplot(aes(x=mean_coverage))+
  geom_histogram(binwidth = 1, fill="blue", color="black", alpha=.7)+
  #geom_density()+
  theme_minimal()+
  labs(title = "Coverage Distribution", x = "Coverage", y = "Frequency")

# Create a function to calculate mean coverage per chromosome
calculate_mean_coverage_per_chromosome <- function(coverage_list) {
  # Initialize an empty list to store mean coverage per chromosome
  mean_coverage_per_chromosome <- list()
  
  # Iterate over each chromosome in the coverage list
  for (chromosome in names(coverage_list)) {
    # Calculate the mean coverage for the current chromosome
    mean_coverage <- mean(coverage_list[[chromosome]])
    
    # Store the mean coverage in the result list
    mean_coverage_per_chromosome[[chromosome]] <- mean_coverage
  }
  
  return(mean_coverage_per_chromosome)
}

# Call the function to calculate mean coverage per chromosome
mean_coverage_per_chromosome <- calculate_mean_coverage_per_chromosome(coverage)

mean_coverage_per_chromosome <- as.data.frame(mean_coverage_per_chromosome)

mean_coverage_per_chromosome <-     mean_coverage_per_chromosome %>% 
  pivot_longer(cols = c(1:25),  names_to = "chr", values_to = "mean_cov")

mean_coverage_per_chromosome$chr <- factor(mean_coverage_per_chromosome$chr, 
                                           levels = c("chr1", "chr2", "chr3",
                                                      "chr4", "chr5", "chr6",
                                                      "chr7", "chr8", "chr9", 
                                                      "chr10", "chr11", "chr12", 
                                                      "chr13", "chr14", "chr15", 
                                                      "chr16", "chr17", "chr18",
                                                      "chr19", "chr20", "chr21",
                                                      "chr22", "chrX", "chrY", "chrMT"))
mean(mean_coverage_per_chromosome$mean_cov)

mean_coverage_per_chromosome %>% 
  filter(chr != "chrM") %>% 
  ggplot(aes(x=chr, y=mean_cov))+
  geom_bar(stat="identity",  fill="steelblue", color = "black")+
  theme_minimal()+
  geom_hline(yintercept = 3.6, linetype = "dashed")+
  labs(title = "Mean Coverage distribution per chromosome",
       x= "chromosome", y="Mean Coverage ")+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7))+
  theme(axis.text.x = element_text(angle=90, hjust=1))


