file_find_replace <- function(filepath, pattern, replacement) {
  file_contents <- readLines(filepath)
  updated_contents <- gsub(x = file_contents, pattern = pattern, replacement = replacement, fixed=TRUE)
  cat(updated_contents, file = filepath, sep = "\n")
}
# setwd("slides")
my_rmd_scripts <- list.files(pattern = "(qmd)$")


for (r_script in my_rmd_scripts ) 
  file_find_replace(r_script, "\\item", "* ")

for (r_script in my_rmd_scripts ) 
  file_find_replace(r_script, "\\color{blue}", " ")

for (r_script in my_rmd_scripts ) 
  file_find_replace(r_script, "\\color{red}", " ")

for (r_script in my_rmd_scripts ) 
  file_find_replace(r_script, "\\begin{itemize}", " ")