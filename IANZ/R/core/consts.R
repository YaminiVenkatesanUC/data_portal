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
  "download",
  "plot_function",
  "international"
)

CONFIG_FIELDS <- c(
  "production",
  "default_parameters",
  "primary_color",
  "title",
  "about_modal_html",
  "download_modal_html",
  "indicator_definitions"
)

DUPLICATE_INDICATOR_ERROR <- "Two indicators with the same class, type and name:\n"

DEV_MODE_WARNING <- "Running in development mode - switch to production before deployment."

TOOL_TIP_SUFFIX <- c("Kg", "Tonne", "Tonnes", "Litres", "Ton", "metres")

TOOL_TIP_PREFIX <- c("$")
