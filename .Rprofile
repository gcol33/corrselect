# Project-specific R profile
# Set temp directory without spaces to avoid quarto path issues on Windows
if (.Platform$OS.type == "windows") {
  temp_dir <- "C:/Temp"

  # Create directory if it doesn't exist
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  Sys.setenv(TMPDIR = temp_dir)
  Sys.setenv(TMP = temp_dir)
  Sys.setenv(TEMP = temp_dir)
}
