linterList <- list(
  useRelPaths = lintr::absolute_path_linter,
  useArrowAssignment = lintr::assignment_linter,
  closedCurly = lintr::closed_curly_linter,
  spaceCommas = lintr::commas_linter,
  noCommentedCode = lintr::commented_code_linter,
  infixSpaces = lintr::infix_spaces_linter,
  lineLength = lintr::line_length_linter(100),
  spacesOnly = lintr::no_tab_linter,
  T_and_F_symbol_linter = lintr::T_and_F_symbol_linter,
  function_left_parentheses_linter = lintr::function_left_parentheses_linter,
  open_curly_linter = lintr::open_curly_linter,
  paren_brace_linter = lintr::paren_brace_linter,
  absolute_path_linter = lintr::absolute_path_linter,
  pipe_continuation_linter = lintr::pipe_continuation_linter,
  spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
  trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
  trailing_whitespace_linter = lintr::trailing_whitespace_linter,
  undesirable_operator_linter = lintr::undesirable_operator_linter
)

lint_file <- function(filename) {
  return (lintr::lint(filename, linters = linterList))
}