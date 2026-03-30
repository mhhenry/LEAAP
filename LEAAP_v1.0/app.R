# app.R
# ------------------------------------------------------------------------------
# LEAAP Shiny app integrated with the Python backend.
#
# This version:
#   - runs LEAAP directly from Shiny via reticulate
#   - supports dynamic grouping_ filters from the uploaded input file
#   - adds an "All" option for group-based plots
#   - adds Alignment Rate, Phonemic Analysis, and updated Distance Summary tabs
#   - moves comparator_speaker_summary into the Tables tab
#   - prepares workbook downloads for LEAAP_error_analysis and LEAAP_alignment
#   - adds a reset-to-defaults button for grouping filters
#   - adds PNG download buttons for all figure tabs
# ------------------------------------------------------------------------------

# ----------------------------- #
# 1) Load required R packages   #
# ----------------------------- #

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(DT)
library(reticulate)
library(writexl)

# Use a local virtual environment for testing if one exists in the app folder.
# This is convenient for local development and is skipped if .venv is absent.
if (dir.exists("./.venv")) {
  try(reticulate::use_virtualenv("./.venv", required = TRUE), silent = TRUE)
}

# ------------------------------------------------------------ #
# Static resources for tutorial downloads and screenshots      #
# ------------------------------------------------------------ #

if (dir.exists("templates")) {
  addResourcePath(
    prefix = "templates",
    directoryPath = normalizePath("templates", winslash = "/", mustWork = TRUE)
  )
}

if (dir.exists("screenshots")) {
  addResourcePath(
    prefix = "screenshots",
    directoryPath = normalizePath("screenshots", winslash = "/", mustWork = TRUE)
  )
}

# --------------------------------------------- #
# 2) App-level constants and helper mappings    #
# --------------------------------------------- #

backend_candidates <- c("leaap_backend.py", "LEAAP_backend_v1.0.py")

error_type_map <- c(
  vowel_sub = "vowel substitution",
  consonant_sub = "consonant substitution",
  deletion = "deletion",
  insertion = "insertion",
  vowel_for_consonant = "vowel for consonant",
  consonant_for_vowel = "consonant for vowel"
)

error_type_levels <- c(
  "vowel substitution",
  "consonant substitution",
  "deletion",
  "insertion",
  "vowel for consonant",
  "consonant for vowel"
)

algorithm_label_map <- c(
  unweighted_lev = "Unweighted",
  weighted_lev   = "Weighted",
  PMI_lev        = "PMI"
)

error_palette <- c(
  "vowel substitution"     = "#4E79A7",
  "consonant substitution" = "#59A14F",
  "deletion"               = "#E15759",
  "insertion"              = "#F28E2B",
  "vowel for consonant"    = "#B07AA1",
  "consonant for vowel"    = "#76B7B2"
)

status_palette <- c(
  "Aligned" = "#4E79A7",
  "In Error" = "#E15759"
)

summary_card <- function(title, value, subtitle = NULL, color = "#4E79A7") {
  div(
    class = "summary-card",
    style = paste0("border-top: 5px solid ", color, ";"),
    div(class = "summary-title", title),
    div(class = "summary-value", value),
    div(class = "summary-subtitle", subtitle)
  )
}

make_filter_id <- function(label) {
  paste0("filter_", gsub("[^A-Za-z0-9_]+", "_", label))
}

format_pct_string <- function(x) {
  sprintf("%.2f%%", 100 * dplyr::coalesce(as.numeric(x), 0))
}

# Convert Python / reticulate return values into R-friendly lists/data frames.
py_dict_get_r <- function(py_dict, key, default = NULL) {
  obj <- tryCatch(py_dict$get(key), error = function(e) NULL)

  if (is.null(obj) || inherits(obj, "python.builtin.NoneType")) {
    return(default)
  }

  py_to_r(obj)
}

# Convert returned Python tabular objects into ordinary R data frames.
# This version prefers CSV conversion because it is more tolerant of mixed
# pandas dtypes than bind_rows() alone.
to_r_df <- function(x) {
  if (is.null(x) || inherits(x, "python.builtin.NoneType")) {
    return(NULL)
  }

  out <- tryCatch(py_to_r(x), error = function(e) NULL)
  if (is.data.frame(out)) {
    return(out)
  }

  csv_text <- tryCatch(py_to_r(x$to_csv(index = FALSE)), error = function(e) NULL)
  if (is.character(csv_text) && length(csv_text) == 1 && nzchar(csv_text)) {
    return(
      readr::read_csv(
        I(csv_text),
        show_col_types = FALSE,
        guess_max = 100000
      )
    )
  }

  recs <- tryCatch(py_to_r(x$to_dict(orient = "records")), error = function(e) NULL)
  if (is.list(recs)) {
    if (length(recs) == 0) {
      return(data.frame())
    }

    recs_df <- lapply(recs, function(row) {
      row_list <- as.list(row)
      row_list <- lapply(row_list, function(v) {
        if (length(v) == 0 || is.null(v)) return(NA)
        if (length(v) > 1) return(paste(v, collapse = "; "))
        v
      })
      as.data.frame(row_list, stringsAsFactors = FALSE, check.names = FALSE)
    })

    return(dplyr::bind_rows(recs_df))
  }

  stop("Could not convert the Python tabular object into an R data frame.")
}

# Convert a Python bytes object into an R raw vector so writeBin() can save it.
py_bytes_to_raw <- function(py_bytes) {
  if (is.null(py_bytes) || inherits(py_bytes, "python.builtin.NoneType")) {
    return(raw())
  }

  builtins <- reticulate::import_builtins(convert = FALSE)
  vals <- reticulate::py_to_r(builtins$list(py_bytes))

  as.raw(unlist(vals))
}

# Build comparator_speaker_summary in R for filtered views inside the app.
build_comparator_summary_r <- function(item_df, phoneme_df) {
  out_cols <- c(
    "comparator_speaker",
    "errors_per_item",
    "percentage_phonemes_in_error",
    "total_error_count",
    "percentage_vowel_for_consonant",
    "percentage_consonant_for_vowel",
    "percentage_insertion",
    "percentage_deletion",
    "percentage_vowel_sub",
    "percentage_consonant_sub"
  )

  if (is.null(item_df) || nrow(item_df) == 0) {
    return(as.data.frame(setNames(replicate(length(out_cols), character(0), simplify = FALSE), out_cols)))
  }

  item_summary <- item_df %>%
    group_by(comparator_speaker) %>%
    summarise(
      errors_per_item = round(mean(total_errors, na.rm = TRUE), 2),
      total_error_count = as.integer(sum(total_errors, na.rm = TRUE)),
      .groups = "drop"
    )

  if (is.null(phoneme_df) || nrow(phoneme_df) == 0) {
    out <- item_summary
    out$percentage_phonemes_in_error <- "0.00%"
    out$percentage_vowel_for_consonant <- "0.00%"
    out$percentage_consonant_for_vowel <- "0.00%"
    out$percentage_insertion <- "0.00%"
    out$percentage_deletion <- "0.00%"
    out$percentage_vowel_sub <- "0.00%"
    out$percentage_consonant_sub <- "0.00%"
    return(out[, out_cols])
  }

  total_slots <- phoneme_df %>%
    count(comparator_speaker, name = "total_slots")

  error_slots <- phoneme_df %>%
    filter(error_type != "alignment") %>%
    count(comparator_speaker, name = "error_slots")

  type_counts <- phoneme_df %>%
    filter(error_type != "alignment") %>%
    count(comparator_speaker, error_type, name = "n") %>%
    tidyr::pivot_wider(
      names_from = error_type,
      values_from = n,
      values_fill = 0
    )

  out <- item_summary %>%
    left_join(total_slots, by = "comparator_speaker") %>%
    left_join(error_slots, by = "comparator_speaker") %>%
    left_join(type_counts, by = "comparator_speaker") %>%
    mutate(
      total_slots = coalesce(total_slots, 0),
      error_slots = coalesce(error_slots, 0)
    )

  type_cols <- c(
    "vowel_for_consonant", "consonant_for_vowel", "insertion",
    "deletion", "vowel_sub", "consonant_sub"
  )

  for (col in type_cols) {
    if (!col %in% names(out)) out[[col]] <- 0
    out[[col]] <- coalesce(out[[col]], 0)
  }

  out <- out %>%
    mutate(
      percentage_phonemes_in_error = if_else(total_slots > 0, error_slots / total_slots, 0),
      percentage_vowel_for_consonant = if_else(total_error_count > 0, vowel_for_consonant / total_error_count, 0),
      percentage_consonant_for_vowel = if_else(total_error_count > 0, consonant_for_vowel / total_error_count, 0),
      percentage_insertion = if_else(total_error_count > 0, insertion / total_error_count, 0),
      percentage_deletion = if_else(total_error_count > 0, deletion / total_error_count, 0),
      percentage_vowel_sub = if_else(total_error_count > 0, vowel_sub / total_error_count, 0),
      percentage_consonant_sub = if_else(total_error_count > 0, consonant_sub / total_error_count, 0)
    ) %>%
    mutate(
      percentage_phonemes_in_error = format_pct_string(percentage_phonemes_in_error),
      percentage_vowel_for_consonant = format_pct_string(percentage_vowel_for_consonant),
      percentage_consonant_for_vowel = format_pct_string(percentage_consonant_for_vowel),
      percentage_insertion = format_pct_string(percentage_insertion),
      percentage_deletion = format_pct_string(percentage_deletion),
      percentage_vowel_sub = format_pct_string(percentage_vowel_sub),
      percentage_consonant_sub = format_pct_string(percentage_consonant_sub)
    )

  out[, out_cols]
}

# ---------------------------------------------- #
# 3) Load the Python backend into an environment #
# ---------------------------------------------- #

load_backend_env <- function() {
  backend_file <- backend_candidates[file.exists(backend_candidates)][1]

  if (is.na(backend_file) || is.null(backend_file)) {
    stop(
      paste0(
        "Could not find the Python backend file. Put either ",
        "'leaap_backend.py' or 'LEAAP_backend_v1.0.py' in the app folder."
      )
    )
  }

  py_env <- new.env(parent = emptyenv())
  source_python(backend_file, envir = py_env, convert = FALSE)
  py_env$backend_file <- backend_file
  py_env
}

# ----------------------------- #
# 4) Build the Shiny UI         #
# ----------------------------- #

app_theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- tagList(
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f7fb; }
      .navbar { box-shadow: 0 2px 10px rgba(0,0,0,0.06); }
      .hero-card {
        background: linear-gradient(135deg, #eef4fb 0%, #f9fbfe 100%);
        border: 1px solid #dbe6f2;
        border-radius: 18px;
        padding: 28px 30px;
        margin-bottom: 20px;
        text-align: center;
        box-shadow: 0 6px 18px rgba(0,0,0,0.05);
      }
      .hero-title {
        font-size: 30px;
        font-weight: 700;
        color: #23415f;
        margin-bottom: 8px;
      }
      .hero-subtitle {
        font-size: 16px;
        color: #4f6377;
        margin-bottom: 0;
      }
      .soft-card {
        background: white;
        border: 1px solid #e5edf5;
        border-radius: 18px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.05);
      }
      .summary-card {
        background: white;
        border-radius: 16px;
        padding: 18px 20px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.05);
        border: 1px solid #e5edf5;
        margin-bottom: 16px;
        min-height: 120px;
      }
      .summary-title {
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        color: #5f6f80;
        margin-bottom: 10px;
      }
      .summary-value {
        font-size: 28px;
        font-weight: 700;
        color: #22384d;
        line-height: 1.1;
      }
      .summary-subtitle {
        font-size: 13px;
        color: #6e7d8d;
        margin-top: 8px;
      }
      .section-title {
        font-size: 22px;
        font-weight: 700;
        color: #26445f;
        margin-bottom: 10px;
      }
      .section-text {
        font-size: 15px;
        color: #516579;
        line-height: 1.65;
      }
      .sidebar-note {
        font-size: 13px;
        color: #68798a;
      }
      .plot-caption {
        font-size: 13px;
        color: #65788a;
        margin-top: 8px;
      }
      .btn-primary { border-radius: 12px; }
      .form-control, .selectize-input { border-radius: 10px !important; }
      .tutorial-step {
        background: white;
        border-left: 6px solid #7ea7cc;
        border-radius: 14px;
        padding: 16px 18px;
        margin-bottom: 14px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.04);
      }
      .dataTables_wrapper {
        width: 100%;
        overflow-x: auto;
        margin-bottom: 20px;
      }
      .download-stack .btn {
        width: 100%;
        margin-bottom: 10px;
      }
      .figure-download-wrap .btn {
        min-width: 180px;
      }
      .tutorial-resource-card {
        background: white;
        border: 1px solid #e5edf5;
        border-radius: 18px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.05);
      }
      .tutorial-img {
        width: 100%;
        max-width: 100%;
        border: 1px solid #d9e5f0;
        border-radius: 14px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.05);
        margin-bottom: 10px;
      }
      .tutorial-caption {
        font-size: 14px;
        color: #526577;
        line-height: 1.6;
        margin-bottom: 18px;
      }
      .tutorial-download-link {
        display: inline-block;
        margin-right: 10px;
        margin-bottom: 10px;
      }
      .tutorial-subhead {
        font-size: 18px;
        font-weight: 700;
        color: #26445f;
        margin-top: 14px;
        margin-bottom: 10px;
      }
    "))
  ),

  page_navbar(
    title = div(icon("wave-square"), "Levenshtein Error Analysis & Alignments Program (LEAAP)"),
    id = "main_nav",
    theme = app_theme,

    nav_spacer(),
    
    nav_panel(
      "Analysis",
      fluidPage(
        br(),
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "hero-card",
              div(class = "hero-title", "Explore Transcription Alignments and Error Analyses with LEAAP"),
              p(
                class = "hero-subtitle",
                "Upload a transcription spreadsheet, choose Levenshtein algorithm(s), and generate downloadable error analysis workbooks plus interactive  plots and tables."
              )
            )
          )
        ),

        sidebarLayout(
          sidebarPanel(
            width = 3,

            h4("Upload and run"),
            p(
              class = "sidebar-note",
              "The uploaded spreadsheet must follow the LEAAP input template with speaker, referent_speakers, item columns, and any grouping_ columns you want to use for filtering and visualization."
            ),

            fileInput(
              "input_file",
              "Upload transcription spreadsheet",
              accept = c(".csv", ".xlsx")
            ),

            actionButton("info_file", label = NULL, icon = icon("circle-question")),
            br(), br(),

            checkboxGroupInput(
              "algorithms",
              "Algorithms to include:",
              choices = list(
                "Unweighted" = "unweighted",
                "Weighted" = "weighted",
                "PMI" = "PMI"
              ),
              selected = c("unweighted", "weighted", "PMI")
            ),

            checkboxInput(
              "include_alignment_workbook",
              "Include LEAAP_alignment workbook",
              value = TRUE
            ),
            actionButton(
              "run_analysis",
              "Run LEAAP analysis",
              icon = icon("play"),
              class = "btn-primary"
            ),
            br(), br(),
            div(
              class = "download-stack",
              uiOutput("download_buttons_ui")
            ),
            br(),
            uiOutput("grouping_controls_ui"),
            
          ),

          mainPanel(
            width = 9,

            uiOutput("summary_cards_ui"),
            br(),

            div(
              class = "soft-card",
              tabsetPanel(
                id = "results_tabs",

                tabPanel(
                  "Alignment Rate",
                  br(),
                  tabsetPanel(
                    id = "alignment_subtabs",
                    tabPanel(
                      "By Item",
                      br(),
                      plotOutput("alignment_item_plot", height = "420px"),
                      div(
                        class = "plot-caption",
                        "Items are classified as Aligned when total_errors = 0 and In Error when total_errors is at least 1."
                      ),
                      br(),
                      div(
                        class = "figure-download-wrap",
                        downloadButton("download_alignment_item_png", "Download PNG")
                      )
                    ),
                    tabPanel(
                      "By Phoneme",
                      br(),
                      plotOutput("alignment_phoneme_plot", height = "420px"),
                      div(
                        class = "plot-caption",
                        "Phoneme slots are classified as Aligned when error_type = alignment and In Error for any other error_type."
                      ),
                      br(),
                      div(
                        class = "figure-download-wrap",
                        downloadButton("download_alignment_phoneme_png", "Download PNG")
                      )
                    )
                  )
                ),

                tabPanel(
                  "Error Proportion (item)",
                  br(),
                  plotOutput("item_prop_plot", height = "520px"),
                  div(
                    class = "plot-caption",
                    "This plot summarizes item-level error counts. It is overall by default and uses the selected grouping variable only when you choose one other than All."
                  ),
                  br(),
                  div(
                    class = "figure-download-wrap",
                    downloadButton("download_item_prop_png", "Download PNG")
                  )
                ),

                tabPanel(
                  "Phonemic Analysis",
                  br(),
                  plotOutput("referent_phoneme_plot", height = "520px"),
                  div(
                    class = "plot-caption",
                    "The lollipop plot shows which referent phonemes contributed the most error tokens overall in the current filtered view."
                  ),
                  br(),
                  downloadButton("download_referent_phoneme_png", "Download PNG"),
                  br(), br(),
                  div(
                    class = "soft-card",
                    h4("Most common substitution / outcome by referent phoneme"),
                    downloadButton("download_substitution_table_xlsx", "Download Excel"),
                    br(), br(),
                    DTOutput("substitution_table")
                  )
                ),

                tabPanel(
                  "Distance Summary",
                  br(),
                  plotOutput("distance_plot", height = "460px"),
                  div(
                    class = "plot-caption",
                    "Mean distance values for the selected algorithms. This is overall by default and can be grouped using the dropdown above the filters."
                  ),
                  br(),
                  div(
                    class = "figure-download-wrap",
                    downloadButton("download_distance_png", "Download PNG")
                  )
                ),

                tabPanel(
                  "Tables",
                  br(),
                  uiOutput("table_tabs_ui")
                )
              )
            )
          )
        )
      )
    ),

    nav_panel(
      "Help",
      fluidPage(
        br(),
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "section-title", "Help"),
              p(
                class = "section-text",
                "This app runs the LEAAP backend directly from Shiny. Upload a file that follows the LEAAP input template, choose one or more algorithms, and click Run LEAAP analysis."
              ),
              tags$ul(
                tags$li("Grouping filters are detected from any columns that begin with grouping_."),
                tags$li("Use the filter dropdowns to focus the plots and tables."),
                tags$li("Choose All in the grouping selector to make plots overall by default."),
                tags$li("Download buttons appear after a successful run.")
              )
            )
          )
        )
      )
    ),

    nav_panel(
      "Tutorial",
      fluidPage(
        br(),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "section-title", "Tutorial"),
              p(
                class = "section-text",
                "LEAAP is designed to let you upload a transcription spreadsheet, choose one or more distance algorithms, and generate downloadable workbooks plus interactive summaries. The tutorial resources below are intended for first-time and non-technical users."
              ),
              p(
                class = "section-text",
                "A good first workflow is: download the blank template, look at one of the demo files, format your own file in the same way, upload it into the app, choose the algorithms you want, and then click Run LEAAP analysis."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "tutorial-resource-card",
              div(class = "tutorial-subhead", "Tutorial resources"),
              p(
                class = "section-text",
                "These files are included in the app so users can download a blank template or practice with example datasets before uploading their own files."
              ),
              
              tags$a(
                href = "templates/LEAAP_template.csv",
                download = NA,
                class = "btn btn-outline-primary tutorial-download-link",
                "Download LEAAP Import Template"
              ),
              tags$a(
                href = "templates/SSD_speech_demo.csv",
                download = NA,
                class = "btn btn-outline-primary tutorial-download-link",
                "Download Disordered Speech Demo"
              ),
              tags$a(
                href = "templates/accented_speech_demo.csv",
                download = NA,
                class = "btn btn-outline-primary tutorial-download-link",
                "Download Accented Speech Demo"
              ),
              
              br(), br(),
              
              p(
                class = "tutorial-caption",
                tags$strong("LEAAP_template.csv: "),
                "This is a generic template used for importing transcriptions to LEAAP."
              ),
              p(
                class = "tutorial-caption",
                tags$strong("SSD_speech_demo.csv: "),
                "This is a demo for disordered speakers."
              ),
              p(
                class = "tutorial-caption",
                tags$strong("accented_speech_demo.csv: "),
                "This is a demo for L1/L2 speakers."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Step 1: Start with the LEAAP import template"),
              p(
                class = "section-text",
                "The blank import template is the easiest place to begin. It gives you the expected spreadsheet structure and shows how to organize speakers, referents, grouping variables, and item columns. You can fill it in directly or compare it with one of the demo files before preparing your own data."
              ),
              
              img(
                src = "screenshots/Screenshot_1_Template.PNG",
                class = "tutorial-img"
              ),
              p(
                class = "tutorial-caption",
                "This is the generic template format for importing transcriptions into LEAAP."
              ),
              
              img(
                src = "screenshots/Screenshot_2_Grouping.PNG",
                class = "tutorial-img"
              ),
              p(
                class = "tutorial-caption",
                "LEAAP import templates allow for speakers to be grouped together using one or more optional grouping variables. For example, you could include age, sex or gender, disorder type, accent or dialect, or any other grouping variable relevant to your dataset. These should be added in columns using the format grouping_[insert variable], such as grouping_age or grouping_accent."
              ),
              
              img(
                src = "screenshots/Screenshot_3_Referents.PNG",
                class = "tutorial-img"
              ),
              p(
                class = "tutorial-caption",
                "In the LEAAP import template, you assign your referent speakers in the referent_speakers column by entering referent in the chosen talker’s row. You only need to label the talkers you want to use as referents. Leave the other rows blank in that column for comparator speakers."
              ),
              
              fluidRow(
                column(
                  width = 6,
                  img(
                    src = "screenshots/Screenshot_4_Speakers.PNG",
                    class = "tutorial-img"
                  )
                ),
                column(
                  width = 6,
                  img(
                    src = "screenshots/Screenshot_5_Items.PNG",
                    class = "tutorial-img"
                  )
                )
              ),
              p(
                class = "tutorial-caption",
                "Speakers in rows and item labels in column headers should each have unique labels. Included items can be words, sentences, or phonemes, but they must not contain spaces."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Step 2: Understand the required structure"),
              p(
                class = "section-text",
                "Each row should correspond to one speaker or talker, and each item column should contain that speaker’s transcription for that item. LEAAP compares comparator speakers to the referent speakers you designate in the referent_speakers column."
              ),
              p(
                class = "section-text",
                "Grouping variables are optional, but they are very useful if you want to filter the results or break plots out by categories such as severity, age group, dialect, language background, sex, gender, or diagnosis. If you include grouping variables, the app will automatically detect them and generate filter dropdowns for you."
              ),
              p(
                class = "section-text",
                "The app currently accepts CSV and XLSX uploads, but the included blank template is provided as a CSV because it is simple, portable, and easy to inspect."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Step 3: Run the analysis in the app"),
              p(
                class = "section-text",
                "After uploading your file, LEAAP inspects the grouping_ columns and builds the grouping controls automatically. Next, select the algorithm or algorithms you want to include: Unweighted, Weighted, PMI, or any combination of the three."
              ),
              p(
                class = "section-text",
                "If you leave LEAAP_alignment checked, the app will also generate the wide human-readable alignment workbook. This is useful when you want to inspect full alignments item by item. If you only want the core summary workbook, you can uncheck that option."
              ),
              p(
                class = "section-text",
                "Once you click Run LEAAP analysis, the app generates plots, tables, and downloadable workbooks. The LEAAP_error_analysis workbook includes comparator_speaker_summary, item_error_counts, and phoneme_alignments. The LEAAP_alignment workbook adds the wide full alignment display."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Step 4: Filter and interpret the results"),
              p(
                class = "section-text",
                "Use the grouping dropdown labeled Use this grouping for group plots to decide whether charts are shown overall or split into multiple panels. If you keep it on All, the plots summarize the currently filtered dataset as a whole. If you choose a grouping variable, the plots break out by that grouping."
              ),
              p(
                class = "section-text",
                "The grouping filters below that dropdown narrow the current view. These filters affect the interactive tables, the figure displays, and filtered downloads such as figure PNGs and the phonemic-analysis substitution table download. They do not change the original uploaded data; they only change what you are currently viewing in the app."
              ),
              p(
                class = "section-text",
                "The reset button returns all grouping filters to All, which is helpful if you want to quickly go back to the full dataset."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Tutorial 1: Disordered Speech Use Case"),
              p(
                class = "section-text",
                "Use SSD_speech_demo.csv if you want to see an example where speakers are grouped by clinical or severity-related variables. This demo is useful for learning how LEAAP can summarize disordered speech data, compare comparator speakers to referent speakers, and visualize item and phoneme-level error patterns."
              ),
              p(
                class = "section-text",
                "A helpful first exercise is to upload the demo file, select all three algorithms, keep LEAAP_alignment turned on, run the analysis, and then explore how the plots change when you set the group plots dropdown to a severity-related grouping variable."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Tutorial 2: Accented Speech Use Case"),
              p(
                class = "section-text",
                "Use accented_speech_demo.csv if you want to see an example involving L1/L2 or accent-based comparisons. This demo is useful for learning how LEAAP can be applied outside clinical speech data, including datasets where the main interest is phoneme-level mismatch patterns across language background or accent group."
              ),
              p(
                class = "section-text",
                "A good exercise is to upload the accented-speech demo, run the analysis with more than one algorithm selected, and then compare the Alignment Rate, Error Proportion, and Distance Summary views while switching between overall and grouped displays."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "tutorial-subhead", "Troubleshooting tips"),
              p(
                class = "section-text",
                "If the app does not detect your grouping variables, make sure the column names begin exactly with grouping_. If referent comparisons do not look right, check that the intended referent speakers are marked with referent in the referent_speakers column. If item labels contain spaces or are duplicated, the app may not behave as expected."
              ),
              p(
                class = "section-text",
                "When in doubt, compare your file against the blank template or one of the demo files. Matching the structure of those examples is usually the fastest way to fix an import problem."
              )
            )
          )
        )
      )
    ),

    nav_panel(
      "About",
      fluidPage(
        br(),
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "soft-card",
              div(class = "section-title", "About LEAAP"),
              p(
                class = "section-text",
                "This application is a user-friendly front end for edit distance based transcription analysis. It is intended to make alignment output and error summaries easier for researchers and clinicians to explore."
              ),
              p(
                class = "section-text",
                "Please cite the application in your work with the following: Henry, M., Henry, J., & Bent, T. (XXXX). Levenshtein Error Analysis and Alignments Program (LEAAP). [computer program]. Retrieved XX Month XXXX from https://www.LEAAP.org/."
              )
            )
          )
        )
      )
    )
  )
)

# ----------------------------- #
# 5) Server logic               #
# ----------------------------- #

server <- function(input, output, session) {

  backend <- tryCatch(load_backend_env(), error = function(e) NULL)
  backend_error <- if (is.null(backend)) {
    "The Python backend could not be loaded."
  } else {
    NULL
  }

  rv <- reactiveValues(
    grouping_info = NULL,
    results_py = NULL,
    results_r = NULL,
    comparator_summary_df = NULL,
    item_df = NULL,
    phoneme_df = NULL,
    full_alignments_df = NULL,
    metadata = NULL,
    error_workbook_path = NULL,
    alignment_workbook_path = NULL
  )

  observeEvent(input$info_file, {
    showModal(modalDialog(
      title = "Upload help",
      "Upload the LEAAP input spreadsheet. The app will inspect the grouping_ columns first, then run the full analysis when you click Run LEAAP analysis.",
      easyClose = TRUE
    ))
  })

  # -------------------------------------------------------------- #
  # Inspect grouping_ columns when a file is uploaded              #
  # -------------------------------------------------------------- #

  observeEvent(input$input_file, {
    rv$grouping_info <- NULL

    if (is.null(input$input_file)) {
      return(NULL)
    }

    if (is.null(backend)) {
      showNotification(backend_error, type = "error", duration = 8)
      return(NULL)
    }

    tryCatch({
      grouping_info_py <- backend$get_grouping_options(input$input_file$datapath)
      grouping_info <- py_to_r(grouping_info_py)

      if (!is.null(grouping_info$grouping_labels) && length(grouping_info$grouping_labels) > 0) {
        grouping_info$grouping_labels <- as.character(grouping_info$grouping_labels)
      } else {
        grouping_info$grouping_labels <- character(0)
      }

      if (is.null(grouping_info$grouping_map)) {
        grouping_info$grouping_map <- list()
      }

      if (is.null(grouping_info$grouping_levels)) {
        grouping_info$grouping_levels <- list()
      }

      rv$grouping_info <- grouping_info
    }, error = function(e) {
      rv$grouping_info <- list(
        grouping_labels = character(0),
        grouping_map = list(),
        grouping_levels = list()
      )

      showNotification(
        paste("Could not inspect grouping columns:", e$message),
        type = "warning",
        duration = 8
      )
    })
  })

  # -------------------------------------------------------------- #
  # Dynamic grouping controls                                      #
  # -------------------------------------------------------------- #

  output$grouping_controls_ui <- renderUI({
    if (is.null(input$input_file)) {
      return(
        div(
          class = "soft-card",
          h4("Grouping filters"),
          p(class = "sidebar-note", "Upload a file to detect any grouping_ columns.")
        )
      )
    }

    if (is.null(rv$grouping_info)) {
      return(
        div(
          class = "soft-card",
          h4("Grouping filters"),
          p(class = "sidebar-note", "Grouping information will appear after the uploaded file is inspected.")
        )
      )
    }

    labels <- rv$grouping_info$grouping_labels
    levels_map <- rv$grouping_info$grouping_levels

    div(
      class = "soft-card",
      h4("Grouping filters"),
      if (length(labels) == 0) {
        p(class = "sidebar-note", "No grouping_ columns were found in the uploaded file.")
      } else {
        tagList(
          selectInput(
            "primary_grouping",
            "Use this grouping for group plots",
            choices = c("All", labels),
            selected = "All"
          ),
          lapply(labels, function(lbl) {
            lvl_choices <- sort(unique(as.character(levels_map[[lbl]])))
            selectInput(
              inputId = make_filter_id(lbl),
              label = lbl,
              choices = c("All", lvl_choices),
              selected = "All"
            )
          }),
          actionButton(
            "reset_grouping_filters",
            "Reset grouping filters to defaults",
            icon = icon("rotate-left")
          )
        )
      }
    )
  })

  observeEvent(input$reset_grouping_filters, {
    req(rv$grouping_info)

    updateSelectInput(session, "primary_grouping", selected = "All")

    for (lbl in rv$grouping_info$grouping_labels) {
      updateSelectInput(session, make_filter_id(lbl), selected = "All")
    }
  })

  # -------------------------------------------------------------- #
  # Run the Python backend                                         #
  # -------------------------------------------------------------- #

  observeEvent(input$run_analysis, {
    req(input$input_file)

    if (is.null(backend)) {
      showNotification(backend_error, type = "error", duration = 8)
      return(NULL)
    }

    if (length(input$algorithms) == 0) {
      showNotification(
        "Please choose at least one algorithm before running the analysis.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }

    showNotification("Running LEAAP analysis...", type = "message", duration = 2)

    tryCatch({
      results_py <- backend$analyze_file(
        input_file = input$input_file$datapath,
        selected_algorithms = as.list(input$algorithms),
        include_phoneme_alignments = TRUE,
        include_full_alignments = isTRUE(input$include_alignment_workbook)
      )

      rv$results_py <- results_py
      rv$comparator_summary_df <- to_r_df(results_py$get("comparator_speaker_summary"))
      rv$item_df <- to_r_df(results_py$get("item_error_counts"))
      rv$phoneme_df <- to_r_df(results_py$get("phoneme_alignments"))
      rv$full_alignments_df <- to_r_df(results_py$get("full_alignments"))
      rv$metadata <- py_dict_get_r(results_py, "metadata", default = NULL)

      if (is.null(rv$comparator_summary_df) && !is.null(rv$item_df)) {
        rv$comparator_summary_df <- build_comparator_summary_r(rv$item_df, rv$phoneme_df)
      }

      rv$results_r <- list(
        metadata = rv$metadata,
        comparator_speaker_summary = rv$comparator_summary_df,
        item_error_counts = rv$item_df,
        phoneme_alignments = rv$phoneme_df,
        full_alignments = rv$full_alignments_df
      )

      error_raw <- py_bytes_to_raw(
        backend$build_download_workbook(
          results_py,
          include_phoneme_alignments = TRUE,
          include_full_alignments = FALSE
        )
      )

      rv$error_workbook_path <- file.path(
        tempdir(),
        paste0("LEAAP_error_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      )
      writeBin(error_raw, rv$error_workbook_path, useBytes = TRUE)

      if (isTRUE(input$include_alignment_workbook) && !is.null(rv$full_alignments_df)) {
        align_raw <- py_bytes_to_raw(
          backend$build_alignment_workbook(results_py)
        )

        rv$alignment_workbook_path <- file.path(
          tempdir(),
          paste0("LEAAP_alignment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        )
        writeBin(align_raw, rv$alignment_workbook_path, useBytes = TRUE)
      } else {
        rv$alignment_workbook_path <- NULL
      }

      showNotification(
        "LEAAP analysis completed successfully.",
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      rv$results_py <- NULL
      rv$results_r <- NULL
      rv$comparator_summary_df <- NULL
      rv$item_df <- NULL
      rv$phoneme_df <- NULL
      rv$full_alignments_df <- NULL
      rv$metadata <- NULL
      rv$error_workbook_path <- NULL
      rv$alignment_workbook_path <- NULL

      showNotification(
        paste("Analysis failed:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # -------------------------------------------------------------- #
  # Download buttons appear only after successful analysis         #
  # -------------------------------------------------------------- #

  output$download_buttons_ui <- renderUI({
    if (is.null(rv$item_df)) {
      return(NULL)
    }

    tagList(
      downloadButton("download_error_workbook", "Download LEAAP_error_analysis"),
      div(
        class = "sidebar-note",
        "LEAAP_error_analysis includes comparator_speaker_summary, item_error_counts, and phoneme_alignments."
      ),
      br(),
      if (!is.null(rv$alignment_workbook_path)) {
        tagList(
          downloadButton("download_alignment_workbook", "Download LEAAP_alignment"),
          div(
            class = "sidebar-note",
            "LEAAP_alignment visually displays each comparator item's alignment with the referent item, including error types and the selected levenshtein distance values."
          ),
          br(),
        )
      }
    )
  })

  # -------------------------------------------------------------- #
  # Filter data dynamically using grouping filters                 #
  # -------------------------------------------------------------- #

  filtered_item_data <- reactive({
    req(rv$item_df)

    df <- rv$item_df

    if (!is.null(rv$grouping_info) && length(rv$grouping_info$grouping_labels) > 0) {
      for (lbl in rv$grouping_info$grouping_labels) {
        col_name <- rv$grouping_info$grouping_map[[lbl]]
        input_id <- make_filter_id(lbl)

        if (!is.null(input[[input_id]]) && input[[input_id]] != "All") {
          df <- df %>% filter(.data[[col_name]] == input[[input_id]])
        }
      }
    }

    df
  })

  filtered_phoneme_data <- reactive({
    req(rv$phoneme_df)

    df <- rv$phoneme_df

    if (!is.null(rv$grouping_info) && length(rv$grouping_info$grouping_labels) > 0) {
      for (lbl in rv$grouping_info$grouping_labels) {
        col_name <- rv$grouping_info$grouping_map[[lbl]]
        input_id <- make_filter_id(lbl)

        if (!is.null(input[[input_id]]) && input[[input_id]] != "All") {
          df <- df %>% filter(.data[[col_name]] == input[[input_id]])
        }
      }
    }

    df
  })

  filtered_comparator_summary <- reactive({
    req(filtered_item_data())
    build_comparator_summary_r(filtered_item_data(), filtered_phoneme_data())
  })

  current_group_col <- reactive({
    if (is.null(rv$grouping_info) || length(rv$grouping_info$grouping_labels) == 0) {
      return(NULL)
    }

    req(input$primary_grouping)

    if (identical(input$primary_grouping, "All")) {
      return(NULL)
    }

    rv$grouping_info$grouping_map[[input$primary_grouping]]
  })

  current_group_label <- reactive({
    if (is.null(rv$grouping_info) || length(rv$grouping_info$grouping_labels) == 0) {
      return("All")
    }

    req(input$primary_grouping)
    input$primary_grouping
  })

  # -------------------------------------------------------------- #
  # Convert item-level error columns to a long format for plotting #
  # -------------------------------------------------------------- #

  item_errors_long <- reactive({
    req(filtered_item_data())

    error_cols <- names(error_type_map)
    missing_error_cols <- setdiff(error_cols, names(filtered_item_data()))

    validate(
      need(
        length(missing_error_cols) == 0,
        paste("Missing error count columns:", paste(missing_error_cols, collapse = ", "))
      )
    )

    filtered_item_data() %>%
      pivot_longer(
        cols = all_of(error_cols),
        names_to = "error_key",
        values_to = "error_count"
      ) %>%
      mutate(
        error_type = factor(
          unname(error_type_map[error_key]),
          levels = error_type_levels
        )
      )
  })

  # -------------------------------------------------------------- #
  # Summary tiles                                                  #
  # -------------------------------------------------------------- #

  output$summary_cards_ui <- renderUI({
    req(filtered_item_data())

    df <- filtered_item_data()
    group_col <- current_group_col()

    items_in_view <- nrow(df)
    comparators_in_view <- n_distinct(df$comparator_speaker)
    referents_in_view <- n_distinct(df$referent_speaker)
    groups_in_view <- if (is.null(group_col)) 1 else n_distinct(df[[group_col]])

    fluidRow(
      column(
        width = 3,
        summary_card(
          title = "Items",
          value = comma(items_in_view),
          subtitle = "Rows currently represented in item_error_counts",
          color = "#4E79A7"
        )
      ),
      column(
        width = 3,
        summary_card(
          title = "Comparators",
          value = comma(comparators_in_view),
          subtitle = "Unique comparator speakers in the current filtered view",
          color = "#59A14F"
        )
      ),
      column(
        width = 3,
        summary_card(
          title = "Referents",
          value = comma(referents_in_view),
          subtitle = "Unique referent speakers in the current filtered view",
          color = "#F28E2B"
        )
      ),
      column(
        width = 3,
        summary_card(
          title = current_group_label(),
          value = comma(groups_in_view),
          subtitle = if (is.null(group_col)) {
            "Overall view"
          } else {
            "Unique grouping values in the current filtered view"
          },
          color = "#B07AA1"
        )
      )
    )
  })

  # -------------------------------------------------------------- #
  # Plot builders used for both display and download              #
  # -------------------------------------------------------------- #

  make_alignment_item_plot <- function() {
    df <- filtered_item_data()
    if (is.null(df) || nrow(df) == 0) stop("No item rows are available.")
    
    group_col <- current_group_col()
    
    if (is.null(group_col)) {
      plot_df <- df %>%
        mutate(status = if_else(total_errors == 0, "Aligned", "In Error")) %>%
        count(status, name = "n") %>%
        mutate(
          prop = n / sum(n),
          label = paste0(status, "\n", percent(prop, accuracy = 0.1)),
          facet_group = "All"
        )
    } else {
      plot_df <- df %>%
        mutate(
          status = if_else(total_errors == 0, "Aligned", "In Error"),
          facet_group = as.character(.data[[group_col]])
        ) %>%
        count(facet_group, status, name = "n") %>%
        group_by(facet_group) %>%
        mutate(
          prop = n / sum(n),
          label = paste0(status, "\n", percent(prop, accuracy = 0.1))
        ) %>%
        ungroup()
    }
    
    p <- ggplot(plot_df, aes(x = 1, y = n, fill = status)) +
      geom_col(width = 1, color = "white", linewidth = 0.8) +
      coord_polar(theta = "y") +
      xlim(0.5, 1.5) +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        size = if (is.null(group_col)) 4 else 3.2
      ) +
      scale_fill_manual(values = status_palette, drop = FALSE) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_void(base_size = 12) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#edf3f9", color = "#d8e4f0"),
        aspect.ratio = 1
      )
    
    if (!is.null(group_col)) {
      p <- p + facet_wrap(~ facet_group, scales="free_y")
    }
    
    p
  }

  make_alignment_phoneme_plot <- function() {
    df <- filtered_phoneme_data()
    if (is.null(df) || nrow(df) == 0) stop("No phoneme rows are available.")
    
    group_col <- current_group_col()
    
    if (is.null(group_col)) {
      plot_df <- df %>%
        mutate(status = if_else(error_type == "alignment", "Aligned", "In Error")) %>%
        count(status, name = "n") %>%
        mutate(
          prop = n / sum(n),
          label = paste0(status, "\n", percent(prop, accuracy = 0.1)),
          facet_group = "All"
        )
    } else {
      plot_df <- df %>%
        mutate(
          status = if_else(error_type == "alignment", "Aligned", "In Error"),
          facet_group = as.character(.data[[group_col]])
        ) %>%
        count(facet_group, status, name = "n") %>%
        group_by(facet_group) %>%
        mutate(
          prop = n / sum(n),
          label = paste0(status, "\n", percent(prop, accuracy = 0.1))
        ) %>%
        ungroup()
    }
    
    p <- ggplot(plot_df, aes(x = 1, y = n, fill = status)) +
      geom_col(width = 1, color = "white", linewidth = 0.8) +
      coord_polar(theta = "y") +
      xlim(0.5, 1.5) +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        size = if (is.null(group_col)) 4 else 3.2
      ) +
      scale_fill_manual(values = status_palette, drop = FALSE) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_void(base_size = 12) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#edf3f9", color = "#d8e4f0"),
        aspect.ratio = 1
      )
    
    if (!is.null(group_col)) {
      p <- p + facet_wrap(~ facet_group, scales="free_y")
    }
    
    p
  }

  make_item_prop_plot <- function() {
    df_long <- item_errors_long()
    if (is.null(df_long) || nrow(df_long) == 0) stop("No rows match the current filters.")

    group_col <- current_group_col()

    plot_df <- if (is.null(group_col)) {
      df_long %>%
        group_by(error_type) %>%
        summarise(n = sum(error_count, na.rm = TRUE), .groups = "drop") %>%
        mutate(facet_group = "All")
    } else {
      df_long %>%
        group_by(.data[[group_col]], error_type) %>%
        summarise(n = sum(error_count, na.rm = TRUE), .groups = "drop") %>%
        mutate(facet_group = as.character(.data[[group_col]]))
    }

    plot_df <- plot_df %>%
      group_by(facet_group) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()

    p <- ggplot(plot_df, aes(x = error_type, y = n, fill = error_type)) +
      geom_col(width = 0.78) +
      geom_text(aes(label = percent(prop, accuracy = 0.1)), vjust = -0.35, size = 3.2) +
      scale_fill_manual(values = error_palette, drop = FALSE) +
      labs(
        x = NULL,
        y = "Count of differences",
        fill = "Difference type"
      ) +
      theme_bw(base_size = 12) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "#edf3f9", color = "#d8e4f0"),
        panel.grid.minor = element_blank()
      ) +
      expand_limits(y = max(plot_df$n, na.rm = TRUE) * 1.12)

    if (!is.null(group_col)) {
      p <- p + facet_wrap(~ facet_group, scales = "free_y")
    }

    p
  }

  make_referent_phoneme_plot <- function() {
    df <- filtered_phoneme_data()
    if (is.null(df) || nrow(df) == 0) stop("No phoneme rows are available.")

    plot_df <- df %>%
      filter(error_type != "alignment") %>%
      mutate(referent_phoneme = trimws(as.character(referent_phoneme))) %>%
      filter(!is.na(referent_phoneme), referent_phoneme != "") %>%
      count(referent_phoneme, name = "error_count") %>%
      arrange(error_count)

    if (nrow(plot_df) == 0) stop("No non-alignment referent phoneme errors are available.")

    ggplot(plot_df, aes(x = reorder(referent_phoneme, error_count), y = error_count)) +
      geom_segment(
        aes(xend = reorder(referent_phoneme, error_count), y = 0, yend = error_count),
        linewidth = 1.1, color = "#4E79A7", alpha = 0.85
      ) +
      geom_point(size = 2.8, color = "#4E79A7") +
      coord_flip() +
      labs(
        x = "Referent phoneme",
        y = "Error count"
      ) +
      theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  }

  make_distance_plot <- function() {
    df <- filtered_item_data()
    if (is.null(df) || nrow(df) == 0) stop("No item rows are available.")

    metric_cols <- intersect(
      c("unweighted_lev", "weighted_lev", "PMI_lev"),
      names(df)
    )

    if (length(metric_cols) == 0) {
      stop("No distance columns are available for the selected algorithms.")
    }

    group_col <- current_group_col()

    plot_df <- df %>%
      pivot_longer(
        cols = all_of(metric_cols),
        names_to = "algorithm_key",
        values_to = "distance_value"
      ) %>%
      mutate(
        algorithm = factor(
          unname(algorithm_label_map[algorithm_key]),
          levels = c("Unweighted", "Weighted", "PMI")
        )
      )

    if (is.null(group_col)) {
      plot_df <- plot_df %>%
        group_by(algorithm) %>%
        summarise(mean_distance = mean(distance_value, na.rm = TRUE), .groups = "drop")

      ggplot(plot_df, aes(x = algorithm, y = mean_distance, fill = algorithm)) +
        geom_col(width = 0.72, show.legend = FALSE) +
        geom_text(aes(label = number(mean_distance, accuracy = 0.01)), vjust = -0.35, size = 4) +
        labs(x = NULL, y = "Mean distance value") +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    } else {
      plot_df <- plot_df %>%
        mutate(group_value = as.character(.data[[group_col]])) %>%
        group_by(group_value, algorithm) %>%
        summarise(mean_distance = mean(distance_value, na.rm = TRUE), .groups = "drop")

      if (nrow(plot_df) == 0) {
        stop("No distance values are available for the current filter settings.")
      }

      ggplot(plot_df, aes(x = reorder(group_value, mean_distance), y = mean_distance, fill = algorithm)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.7) +
        coord_flip() +
        labs(x = NULL, y = "Mean distance value", fill = "Algorithm") +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    }
  }

  # -------------------------------------------------------------- #
  # Alignment Rate pies                                            #
  # -------------------------------------------------------------- #

  output$alignment_item_plot <- renderPlot({
    make_alignment_item_plot()
  })

  output$alignment_phoneme_plot <- renderPlot({
    make_alignment_phoneme_plot()
  })

  # -------------------------------------------------------------- #
  # Error proportion (item)                                        #
  # -------------------------------------------------------------- #

  output$item_prop_plot <- renderPlot({
    make_item_prop_plot()
  })

  # -------------------------------------------------------------- #
  # Phonemic Analysis                                              #
  # -------------------------------------------------------------- #

  output$referent_phoneme_plot <- renderPlot({
    make_referent_phoneme_plot()
  })

  substitution_table_data <- reactive({
    req(filtered_phoneme_data())
    
    sub_df <- filtered_phoneme_data() %>%
      filter(error_type != "alignment") %>%
      mutate(
        referent_phoneme = trimws(as.character(referent_phoneme)),
        comparator_display = trimws(as.character(comparator_phoneme)),
        comparator_display = if_else(
          is.na(comparator_display) | comparator_display == "",
          "∅",
          comparator_display
        )
      ) %>%
      filter(!is.na(referent_phoneme), referent_phoneme != "") %>%
      count(referent_phoneme, comparator_display, error_type, name = "count") %>%
      group_by(referent_phoneme) %>%
      mutate(total_for_referent = sum(count, na.rm = TRUE)) %>%
      arrange(referent_phoneme, desc(count), comparator_display) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      transmute(
        referent_phoneme,
        most_common_outcome = comparator_display,
        error_type,
        count,
        percent_of_errors = percent(count / total_for_referent, accuracy = 0.1)
      )
    
    sub_df
  })
  
  output$substitution_table <- renderDT({
    req(substitution_table_data())
    
    validate(
      need(
        nrow(substitution_table_data()) > 0,
        "No non-alignment referent phoneme errors are available."
      )
    )
    
    datatable(
      substitution_table_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  # -------------------------------------------------------------- #
  # Distance Summary                                               #
  # -------------------------------------------------------------- #

  output$distance_plot <- renderPlot({
    make_distance_plot()
  })

  # -------------------------------------------------------------- #
  # Tables                                                         #
  # -------------------------------------------------------------- #

  output$table_tabs_ui <- renderUI({
    req(filtered_item_data())

    tabsetPanel(
      id = "table_subtabs",

      tabPanel(
        "comparator_speaker_summary",
        br(),
        div(
          class = "soft-card",
          h4("comparator_speaker_summary"),
          DTOutput("comparator_summary_table")
        )
      ),

      tabPanel(
        "item_error_counts",
        br(),
        div(
          class = "soft-card",
          h4("item_error_counts"),
          DTOutput("item_table")
        )
      ),

      tabPanel(
        "phoneme_alignments",
        br(),
        div(
          class = "soft-card",
          h4("phoneme_alignments"),
          DTOutput("phoneme_table")
        )
      ),

      if (!is.null(rv$full_alignments_df)) {
        tabPanel(
          "full_alignments",
          br(),
          div(
            class = "soft-card",
            h4("full_alignments"),
            DTOutput("full_alignments_table")
          )
        )
      }
    )
  })

  output$comparator_summary_table <- renderDT({
    req(filtered_comparator_summary())

    datatable(
      filtered_comparator_summary(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  output$item_table <- renderDT({
    req(filtered_item_data())

    datatable(
      filtered_item_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  output$phoneme_table <- renderDT({
    req(filtered_phoneme_data())

    datatable(
      filtered_phoneme_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  output$full_alignments_table <- renderDT({
    req(rv$full_alignments_df)

    validate(need(nrow(rv$full_alignments_df) > 0, "No full alignment rows are available."))

    datatable(
      rv$full_alignments_df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = FALSE,
        ordering = FALSE,
        searching = FALSE
      ),
      rownames = FALSE,
      colnames = rep("", ncol(rv$full_alignments_df))
    )
  })

  # -------------------------------------------------------------- #
  # Download handlers                                              #
  # -------------------------------------------------------------- #

  output$download_error_workbook <- downloadHandler(
    filename = function() {
      paste0("LEAAP_error_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$error_workbook_path)
      file.copy(rv$error_workbook_path, file, overwrite = TRUE)
    }
  )

  output$download_alignment_workbook <- downloadHandler(
    filename = function() {
      paste0("LEAAP_alignment_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$alignment_workbook_path)
      file.copy(rv$alignment_workbook_path, file, overwrite = TRUE)
    }
  )

  output$download_alignment_item_png <- downloadHandler(
    filename = function() {
      paste0("LEAAP_alignment_rate_by_item_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_alignment_item_plot(), width = 7, height = 5, dpi = 300, units = "in")
    }
  )

  output$download_alignment_phoneme_png <- downloadHandler(
    filename = function() {
      paste0("LEAAP_alignment_rate_by_phoneme_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_alignment_phoneme_plot(), width = 7, height = 5, dpi = 300, units = "in")
    }
  )

  output$download_item_prop_png <- downloadHandler(
    filename = function() {
      paste0("LEAAP_error_proportion_item_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_item_prop_plot(), width = 8, height = 6, dpi = 300, units = "in")
    }
  )

  output$download_referent_phoneme_png <- downloadHandler(
    filename = function() {
      paste0("LEAAP_phonemic_analysis_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_referent_phoneme_plot(), width = 8, height = 7, dpi = 300, units = "in")
    }
  )

  output$download_distance_png <- downloadHandler(
    filename = function() {
      paste0("LEAAP_distance_summary_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_distance_plot(), width = 8, height = 6, dpi = 300, units = "in")
    }
  )
  
  output$download_substitution_table_xlsx <- downloadHandler(
    filename = function() {
      paste0(
        "LEAAP_phonemic_analysis_substitution_table_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    },
    content = function(file) {
      req(substitution_table_data())
      
      validate(
        need(
          nrow(substitution_table_data()) > 0,
          "No substitution table rows are available for download."
        )
      )
      
      writexl::write_xlsx(
        list(
          most_common_substitution_by_referent_phoneme = substitution_table_data()
        ),
        path = file
      )
    }
  )
}

# ----------------------------- #
# 6) Run the application        #
# ----------------------------- #

shinyApp(ui = ui, server = server)
