# Load the Rsamtools package
library(Rsamtools)

# Specify the path to your BAM file
bam_file <- "path/sample.bam"


# optional
# Check if the file exists
if (!file.exists(bam_file)) {
  stop("BAM file does not exist at the specified path.")
}

# Index the BAM file
tryCatch({
  indexBam(bam_file)
  message("BAM file indexed successfully.")
}, error = function(e) {
  message("Failed to index BAM file: ", e$message)
})

# Check file timestamps
file_info <- file.info(c(bam_file, paste0(bam_file, ".bai")))
print(file_info[, "mtime"])

# Try to read the BAM file header
tryCatch({
  bam_header <- scanBamHeader(bam_file)
  message("BAM file header read successfully.")
}, error = function(e) {
  message("Failed to read BAM file header: ", e$message)
})

