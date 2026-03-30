Sys.setenv(
  RETICULATE_PYTHON = "/Users/malachihenry/Desktop/Programming/R/ShinyApps/LEAAP/LEAAP_repository/LEAAP_v1.0/.venv/bin/python"
)

library(rsconnect)

rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "LEAAP v1.0.R",
  appMode = "shiny",
  python = Sys.getenv("RETICULATE_PYTHON"),
  forceGeneratePythonEnvironment = TRUE
)

file.exists("manifest.json")
