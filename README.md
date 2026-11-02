# LEAAP

**L**evenshtein **E**rror **A**nalysis & **A**lignments **P**rogram (**LEAAP**) is a Shiny-based application with a Python backend for comparing phonetic transcriptions using edit-distance methods. It was designed to support speech research and clinical workflows by making transcription comparison more transparent, reproducible, and accessible to non-programmers.

LEAAP allows users to upload a spreadsheet of transcriptions, define one or more referent speakers, select one or more distance algorithms, and generate downloadable workbooks, figures, and interactive tables. The program is especially useful for applications involving speech sound disorders (SSD), accented speech, and other contexts where researchers or clinicians want to quantify how productions differ from a relevant comparison group.

Please cite the application in your work with the following: Henry, M., Henry, J., & Bent, T. (2026). Levenshtein Error Analysis and Alignments Program (LEAAP) (Version 1.1) [computer program]. Retrieved from https://mhhenry-leaap.share.connect.posit.cloud

---

## Features

- Upload transcription spreadsheets in **CSV** or **XLSX** format
- Compare one or more **comparator speakers** to one or more **referent speakers**
- Automatically detect optional `grouping_` variables for filtering and grouped visualization
- Compute multiple distance metrics:
  - **Unweighted Levenshtein distance**
  - **Weighted Levenshtein distance**
  - **PMI-based distance**
- Generate detailed outputs at multiple levels:
  - **Comparator speaker summaries**
  - **Item-level error counts**
  - **Phoneme-level alignments**
  - **Full alignment workbook**
- Explore results through:
  - Alignment-rate plots
  - Error proportion plots
  - Phonemic analysis plots
  - Distance summary plots
  - Interactive searchable tables
- Download:
  - Excel workbooks
  - Figure PNGs
  - Summary tables

---

## Repository Contents

This repository currently includes:

- `LEAAP v1.0.R`  
  Main Shiny application file.

- `leaap_lackend.py`  
  Python backend used by the Shiny app to run alignment, classification, and distance calculations.

- `templates/`
  - `LEAAP_template.csv` — blank import template
  - `SSD_speech_demo.csv` — demo dataset for disordered speech
  - `accented_speech_demo.csv` — demo dataset for accented speech

---

## How LEAAP Works

1. The user uploads a LEAAP-formatted spreadsheet.
2. The app checks for optional `grouping_` columns and builds filter menus automatically.
3. The user selects one or more distance algorithms and whether to generate the full alignment workbook.
4. The Shiny app sends the file and settings to the Python backend.
5. The Python backend:
   - reads and validates the input file
   - identifies referent and comparator speakers
   - reshapes the data into a long format
   - aligns comparator and referent transcriptions
   - classifies differences at each phoneme slot
   - computes item-level distance values
6. The backend returns structured output tables to R/Shiny.
7. Shiny converts those outputs into interactive tables, plots, downloadable workbooks, and figure exports.

---

## Input File Structure

LEAAP expects a spreadsheet with the following structure:

### Required columns
- `speaker`  
  Unique speaker label for each row.
- `referent_speakers`  
  Mark rows containing referent speakers with `referent`. Leave comparator rows blank or use another label.

### Optional grouping columns
Any column beginning with `grouping_` will be detected automatically and used to build filter menus in the app.

Examples:
- `grouping_age`
- `grouping_severity`
- `grouping_diagnosis`
- `grouping_accent`

### Item columns
All remaining columns are treated as transcription items (for example, words or sentences). Each row should contain that speaker’s transcription for each item.

---

## Example Input Files

### `LEAAP_template.csv`
A blank template showing the expected file structure for importing new datasets.

### `SSD_speech_demo.csv`
A demo dataset illustrating how LEAAP can be used with disordered speech data.

### `accented_speech_demo.csv`
A demo dataset illustrating how LEAAP can be used with accented speech data.

---

## Outputs

LEAAP generates the following main outputs:

### `comparator_speaker_summary`
Speaker-level summaries including:
- mean errors per item
- percentage of phonemes in error
- total error count
- percentage of each error type

### `item_error_counts`
Item-level output including:
- comparator and referent information
- total error counts
- counts for each error type
- selected distance metrics

### `phoneme_alignments`
Phoneme-level alignments including:
- aligned referent and comparator phonemes
- phoneme position
- error type
- slot-level penalties

### `full_alignments`
Wide human-readable alignment display showing:
- referent phoneme sequence
- comparator phoneme sequence
- error labels
- penalty rows
- final distance values

---

## Error Types

LEAAP currently classifies aligned slots as:

- `alignment`
- `vowel_sub`
- `consonant_sub`
- `deletion`
- `insertion`
- `vowel_for_consonant`
- `consonant_for_vowel`

These categories are used in both exported tables and app visualizations.

---

## Distance Metrics

### Unweighted Levenshtein
Treats all mismatches equally.

### Weighted Levenshtein
Applies different penalties to different kinds of differences.

### PMI-based distance
Uses observed phoneme-pair distributions in the dataset to assign penalties.

---

## Running the App

### Requirements
LEAAP uses both **R** and **Python**.

#### R packages
Install required R packages such as:

- `shiny`
- `bslib`
- `ggplot2`
- `dplyr`
- `tidyr`
- `readr`
- `scales`
- `DT`
- `reticulate`
- `writexl`

#### Python packages
Install required Python packages such as:

- `pandas`
- `numpy`
- `xlsxwriter`

### Launching locally
Open `LEAAP v1.0.R` in RStudio and run the app.

If you are using a local virtual environment for Python, configure it through `reticulate` as needed.

---

## Help / Tutorial Screenshots

The application help and tutorial pages use screenshots to illustrate the expected import format and workflow. Add the screenshots to your repository and update the paths below as needed.

## Disclaimer

LEAAP is provided for research, educational, and clinical support purposes only. It is intended to assist with transcription comparison, alignment, and error analysis, but it does not replace clinical judgment, diagnostic decision-making, or independent review of results. Uploaded files are processed only for the purpose of generating the requested analyses. Files may be temporarily stored on the server during the session and are not intended to be used for other purposes or shared with third parties except as necessary for secure hosting and app operation. Users are responsible for ensuring that uploaded materials comply with all applicable privacy and confidentiality requirements. 

LEAAP is provided “as is” and without warranty of any kind, express or implied. The developers make no guarantee that the application will be uninterrupted, error-free, secure, or that all results will be accurate in every case. Use of LEAAP is at the user’s own risk, and the developers are not liable for any loss of data, damages, or other consequences arising from use of the app. This disclaimer may be updated from time to time.


```markdown
