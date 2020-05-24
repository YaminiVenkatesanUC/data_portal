ECONOMIC_LABEL <- paste0(
  "<h4 align=\"left\"><strong>Economic</strong></h3>"
)

ECONOMIC_LABEL_SELECTED <- paste0(
  "<h4 align=\"left\"><span style=\"color: #005ea5;\"><strong>Economic</strong></span></h3>",
  " <hr>"
)

SOCIAL_LABEL <- paste0(
  "<h4 align=\"left\"><span style=\"color: #005ea5;\"><strong>Social</strong></span></h4>",
  "<p align=\"left\">Social indictors to come.</p>",
  "<p align=\"left\"><strong>Last updated: not yet published</strong></p>"
)

HEALTH_LABEL <- paste0(
  "<h4 align=\"left\"><span style=\"color: #005ea5;\"><strong>Health</strong></span></h4>",
  "<p align=\"left\">Health indictors to come.</p>",
  "<p align=\"left\"><strong>Last updated: not yet published</strong></p>"
)

ABOUT_TEXT <- "This data portal is developed and maintained by Stats NZ to report on New Zealand economy, social and health as well as some international data since the COVID-19 outbreak. The data is sourced from Stats NZ, Reserve Bank NZ, NZ banks and other international sources. We are not responsible for the quality of the external data sources."

DATA_DEFINITION_FIELDS <- c(
  "class",
  "type",
  "indicator_name",
  "value_names",
  "group_names",
  "load_function"
)

INDICATOR_DEFINITION_FIELDS <- c(
  "class",
  "type",
  "indicator_name",
  "source",
  "download",
  "plot_function",
  "international"
)

CONFIG_FIELDS <- c(
  "production",
  "data_directory"
)

DATA_STORE_FILENAME <- "data_store.RDS"

DUPLICATE_INDICATOR_ERROR <- "Two indicators with the same class, type and name:\n"

DEV_MODE_WARNING <- "Running in development mode - switch to production before deployment."

DATA_DEFINITION_FILENAME <- "config/data_definitions.json"

INDICATOR_DEFINITION_FILENAME <- "config/indicator_definitions.json"

TOOL_TIP_SUFFIX <- c("Kg", "Tonne", "Tonnes", "Litres", "Ton")

TOOL_TIP_PREFIX <- c("$")
