as.data.frame.gbifid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "gbifid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}
