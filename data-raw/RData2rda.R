# RData2rda.R
data(package = "TateyamaVegetation")


#' RDataのファイル名とオブジェクト名の整合性
#'
#' @param dir 　対象ディレクトリ
#'
#' @returns
#' @export
#'
#' @examples
#' wd<-"data/"
#' (res<-check_data_object_names(wd))
#' (f1 <- paste0(wd,res$File[!res$Match],".RData"))
#' (f2 <- paste0(wd,res$Object[!res$Match],".RData"))
#' file.rename(f1,f2)
#'  (res<-check_data_object_names(wd))
#'
#'
#'
check_data_object_names <- function(dir = "data") {
  files <- list.files(dir, pattern = "\\.rda$|\\.RData$", full.names = TRUE)
  result <- data.frame(
    File = character(),
    Object = character(),
    Match = logical(),
    stringsAsFactors = FALSE
  )

  for (f in files) {
    e <- new.env()
    objs <- load(f, envir = e)
    for (obj in objs) {
      file_base <- tools::file_path_sans_ext(basename(f))
      result <- rbind(result, data.frame(
        File = file_base,
        Object = obj,
        Match = (file_base == obj),
        stringsAsFactors = FALSE
      ))
    }
  }

  result <- result[order(result$Match, result$File), ]
  rownames(result) <- NULL
  return(result)
}

#' Title
#'
#' @param dir
#'
#' @returns
#' @export
#'
#' @examples
#' wd<-"data"
#' convert_RData_to_rda(wd)
#'
convert_RData_to_rda <- function(dir = "data") {
  files <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)

  for (f in files) {
    cat("変換中:", f, "\n")

    # 一時環境に読み込む（既存のオブジェクトを汚さないため）
    e <- new.env()
    objs <- load(f, envir = e)

    # 保存対象オブジェクトの確認
    for (obj in objs) {
      rda_path <- file.path(dir, paste0(obj, ".rda"))
      save(list = obj, envir = e, file = rda_path, compress = "gzip")
      cat("  →", rda_path, "に保存しました\n")
    }
  }

  cat("✅ 変換完了\n")
}


#' Title
#'
#' @param data_dir
#' @param output_file
#'
#' @returns
#' @export
#'
#' @examples
#' generate_roxygen_for_rda()
#'
generate_roxygen_for_rda <- function(data_dir = "data", output_file = "R/rda.R") {
  files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)

  # 結果格納ベクトル
  doc_lines <- character()

  for (f in files) {
    e <- new.env()
    objs <- load(f, envir = e)

    for (obj in objs) {
      # オブジェクトの簡易情報取得
      obj_class <- class(e[[obj]])[1]
      obj_summary <- paste(capture.output(str(e[[obj]], max.level = 1)), collapse = "\n#' ")

      doc <- c(
        "",
        sprintf("#' %s", obj),
        "#'",
        "#' @docType data",
        sprintf("#' @name %s", obj),
        sprintf("#' @usage data(%s)", obj),
        sprintf("#' @format An object of class `%s`.", obj_class),
        "#' @keywords datasets",
        "#' @source automatically generated skeleton",
        paste0("\"", obj, "\"")
      )

      doc_lines <- c(doc_lines, doc)
    }
  }

  # 書き出し
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(doc_lines, con = output_file)
  cat("✅ Roxygen2 スケルトンを", output_file, "に書き出しました。\n")
}

