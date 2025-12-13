# html_flora_list_encoding_fixer.R


lines <- readLines("R/html_flora_list.R", encoding = "UTF-8")
lines <- gsub('¥"', '"', lines, fixed = TRUE)
lines <- gsub('¥n', '\\n', lines, fixed = TRUE)
writeLines(lines, "R/html_flora_list_fixed.R", useBytes = TRUE)
