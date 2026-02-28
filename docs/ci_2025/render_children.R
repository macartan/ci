slides <- c(
  "1.1_intro.qmd",
  "1.2_declaredesign.qmd",
  "2.1_causality.qmd",
  "2.2_estimands.qmd",
  "3_fisher.qmd",
  "4_bayes.qmd",
  "5.1_design.qmd",
  "5.2_evaluation.qmd",
  "6_topics_1.qmd",
  "7_topics_2.qmd",
  "8_topics_3.qmd",
  "9_topics_4.qmd"
)

slides <- "7_topics_2.qmd"
# slides <- "1.2_declaredesign.qmd"

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("Package 'quarto' is required. Install with install.packages('quarto') or install Quarto.")
}

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install with install.packages('here').")
}

input_dir  <- here::here("slides")
output_dir <- here::here("docs", "ci_2025")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

message("Rendering ", length(slides), " slide decks to ", output_dir, " ...")

for (f in slides) {
  input_path  <- file.path(input_dir, f)
  output_name <- sub("\\.qmd$", ".html", f)
  built_html  <- file.path(input_dir, output_name)
  target_html <- file.path(output_dir, output_name)

  message(" - ", f, "  ->  ", target_html)

  # Render next to the source file (Quarto CLI doesn't allow paths in output-file)
  quarto::quarto_render(
    input       = input_path,
    output_file = output_name
  )

  # Copy result into docs/ci_2025
  if (file.exists(built_html)) {
    file.copy(built_html, target_html, overwrite = TRUE)
  } else {
    warning("Expected HTML not found after render: ", built_html)
  }
}

message("Done.")

