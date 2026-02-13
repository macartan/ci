## Render Experiments 2026 slides into docs/experiments_2026
##
## Usage (from project root):
##   source("docs/render_experiments_2026.R")
##
## This script:
## 1. Renders the existing Quarto slide files in `slides/`.
## 2. Copies the resulting HTML files into `docs/experiments_2026/`
##    with **simpler names**:
##       intro, experimenting, causality, analysis, design, topics.
##    (No .qmd files are moved or deleted.)

slides <- c(
  intro         = "1_intro_experiments.qmd",
  experimenting = "2_experimenting.qmd",
  causality     = "2.1__experiments_causality.qmd",
  analysis      = "experiments_analysis.qmd",
  design        = "5.1_design.qmd",
  topics        = "6_experiments_topics.qmd"
)

input_dir <- "slides"
output_dir <- file.path("docs", "experiments_2026")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

render_one <- function(qmd, short_name) {
  input <- file.path(input_dir, qmd)
  if (!file.exists(input)) {
    warning("Input file not found: ", input)
    return(invisible(FALSE))
  }

  ## Prefer quarto::quarto_render if available; fall back to rmarkdown::render
  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(input)
  } else if (requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::render(input, output_format = "html_document")
  } else {
    stop("Neither 'quarto' nor 'rmarkdown' is installed; cannot render slides.")
  }

  # Source HTML produced next to the .qmd in slides/
  html_src <- sub("\\.qmd$", ".html", basename(qmd))
  from <- file.path(input_dir, html_src)

  # Destination HTML with simplified name in docs/experiments_2026/
  html_dst <- paste0(short_name, ".html")
  to <- file.path(output_dir, html_dst)

  if (!file.exists(from)) {
    warning("Expected HTML output not found after render: ", from)
    return(invisible(FALSE))
  }

  if (file.exists(to)) {
    file.remove(to)
  }

  file.copy(from, to, overwrite = TRUE)
}

invisible(mapply(render_one, slides, names(slides)))

