


#' Generate Package Documentation with Examples and Knit to HTML
#'
#' @param package_name The name of the package (character string)
#' @param output_rmd The output Rmd filename (default: "package_docs_with_examples.Rmd")
#' @param output_html The output HTML filename (default: "package_docs_with_examples.html")
#'
#' @return None. The Rmd and HTML files are generated in the current working directory.
#' @export
#'
#' @examples
#' package_docs_with_examples("TateyamaVegetation")
#'
package_docs_with_examples <- function(package_name,
                                       output_rmd = "package_docs_with_examples.Rmd",
                                       output_html = "package_docs_with_examples.html") {
  # 必要パッケージ確認
  if (!requireNamespace("tools", quietly = TRUE)) stop("tools package required")
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("rmarkdown package required")

  # .Rd ファイルの取得
  rd_files <- tools::Rd_db(package_name)

  # YAML ヘッダー
  header <- c(
    "---",
    paste0("title: \"Package ", package_name, " Documentation\""),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    number_sections: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, error = TRUE, warning = TRUE, message = TRUE)",
    "```",
    ""
  )

  # ドキュメント生成
  chunks <- lapply(rd_files, function(rd_obj) {
    # 関数名の取得
    fun_name <- tools:::.Rd_get_metadata(rd_obj, "name")

    # ドキュメントテキスト取得と強調記号除去
    rd_text <- capture.output(Rd2txt(rd_obj))
    rd_text <- gsub("_[\b]", "", rd_text)  # _ 削除
    rd_text <- gsub("^#+", "", rd_text)    # ###削除

    # .Rdのexampleコードを抽出
    ex_file <- tempfile(fileext = ".R")
    suppressWarnings(Rd2ex(rd_obj, ex_file))
    example_code <- if (file.exists(ex_file)) paste(readLines(ex_file), collapse = "\n") else ""

    # example() 実行チャンク
    safe_example_call <- paste0("try(example('", fun_name, "', package = '", package_name, "', echo=TRUE, ask=FALSE), silent = TRUE)")

    # セクションをまとめる
    c(
      paste0("## Function: `", fun_name, "`"),
      "",
      "**Documentation:**",
      "",
      paste0("```", "\n", paste(rd_text, collapse = "\n"), "\n```"),
      "",
      if (nchar(example_code) > 0) c("**Examples (from .Rd):**", paste0("```r\n", example_code, "\n```")) else "",
      "",
      "**Executed Example:**",
      paste0("```{r ", fun_name, "_example, eval=TRUE, error=TRUE}"),
      safe_example_call,
      "```",
      ""
    )
  })

  # Rmd 全体を書き出し
  rmd_contents <- c(header, unlist(chunks))
  writeLines(rmd_contents, con = output_rmd)
  message("✅ Rmd生成完了: ", output_rmd)

  # Knit実行
  message("🧶 RmdをHTMLに変換中...")
  rmarkdown::render(input = output_rmd, output_file = output_html, quiet = TRUE)
  message("✅ HTML生成完了: ", output_html)
}


