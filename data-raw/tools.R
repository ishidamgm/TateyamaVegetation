library(devtools)
# gitHub ####
# https://github.com/ishidamgm/TateyamaVegetation

devtools::install_github(ishidamgm/TateyamaVegetation)

library(TateyamaVegetation)
help(package="TateyamaVegetation")

data(package="TateyamaVegetation")


devtools::document()

devtools::install()

package_docs_with_examples("TateyamaVegetation")

#　Index.html gitHub に便利####
# https://ishidamgm.github.io/TateyamaVegetation/reference/index.html
pkgdown::build_site()




#####
devtools::document()
devtools::install()
?Fig_FrequencyCoverage

# EXIFR ####
library(exifr)

exif <- read_exif("~/2025-08-20 13.00.59.jpg")
names(exif)
t(as.data.frame(exif))
exif$Label

t.<-exif$DateTimeOriginal
tag.<-paste(unlist(exif$Subject),collapse = "_")
paste0(t.,tag.,".jpg")



wd.old<-getwd()
setwd("/home/i/16T/立山植生モニタリング調査_写真")

f<-dir(wd)
length(f)
EXIF<-read_exif(f)
str(EXIF)
na.omit(EXIF$DateTimeOriginal)
# パッケージを読み込みます
library(ggplot2)
library(lubridate)

# NAを削除して新しいデータフレームを作成します

dt<- EXIF$DateTimeOriginal

# 日付データをPOSIXct形式に変換します
EXIF$DateTimeOriginal_POSIX <- ymd_hms(EXIF$DateTimeOriginal)


EXIF_clean <- EXIF[!is.na(EXIF$DateTimeOriginal_POSIX),]





# NAの行を削除して新しいデータフレームを作成します
#EXIF_clean <- na.omit(EXIF)

summary(EXIF_clean$DateTimeOriginal_POSIX )

sum(is.na(EXIF_clean$DateTimeOriginal_POSIX))

ggplot(EXIF_clean, aes(x = DateTimeOriginal_POSIX)) +
  geom_histogram(binwidth = 30*24*3600, fill = "steelblue", color = "white") +
  labs(title = "写真撮影頻度（月単位）",
       x = "撮影月",
       y = "写真の枚数") +
  theme_minimal()

-----
  library(dplyr)
library(lubridate)

EXIF_clean %>%
  mutate(year = year(DateTimeOriginal_POSIX)) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "写真撮影頻度（年別）",
       x = "撮影年",
       y = "写真の枚数") +
  theme_minimal()


------------------

  # Create the histogram plot, automatically ignoring NA values
  ggplot(EXIF_clean, aes(x = DateTimeOriginal_POSIX)) +
  geom_histogram(binwidth = 3600, fill = "steelblue", color = "white") +
  labs(title = "時間帯別写真撮影頻度",
       x = "撮影日時",
       y = "写真の枚数") +
  theme_minimal()


####
dir()
dir.create(seq(1998,2025,1))
EXIF
#　フォルダ分け　#####
# 必要パッケージ
library(dplyr)
library(lubridate)
library(stringr)
library(fs)
library(tools)
library(purrr)


# === 設定 ===
dest_base <- "./organized"  # 仕向け先の親フォルダ
do_move   <- FALSE                      # 最初は FALSE（dry-run）。OKなら TRUE に。

# === 前処理：撮影日時 → POSIXct, 年度の計算（4月起点） ===
exif2 <- EXIF %>%
  mutate(
    ts  = suppressWarnings(lubridate::ymd_hms(DateTimeOriginal, tz = "Asia/Tokyo")),
    ts  = ifelse(is.na(ts), NA, ts) %>% as.POSIXct(origin = "1970-01-01", tz = "Asia/Tokyo"),
    # 年度 = 3か月引いて年を取る（Jan–Mar を前年に）
    fiscal_year = lubridate::year(ts %m-% months(3)),
    # 目的地のサブフォルダ名（例：FY2024）
    fy_dir = paste0("", fiscal_year),　　　#"FY"

    # 元ファイルのフルパス（SourceFileにフルパスがあるならそれを優先）
    src_path = dplyr::if_else(
      file.exists(SourceFile), SourceFile,
      file.path(Directory, FileName)
    ),

    # 拡張子・ベース名
    ext  = tolower(file_ext(FileName)),
    base = file_path_sans_ext(FileName),

    # 同一時刻重複対策も兼ねた、推奨ファイル名（日時 + 元名）
    ts_tag = ifelse(is.na(ts), "unknown", format(ts, "%Y-%m-%d_%H%M%S")),
    # 同一 ts_tag & base が複数ある場合に連番を付与
    .by = NULL
  )

# 重複連番の付与
exif2 <- exif2 %>%
  group_by(ts_tag, base) %>%
  mutate(dup_idx = row_number(),
         suffix  = if_else(n() == 1, "", sprintf("-%02d", dup_idx))) %>%
  ungroup()

# 目的地のフルパス（年度フォルダ配下）
exif2 <- exif2 %>%
  mutate(
    dest_dir  = ifelse(is.na(ts), file.path(dest_base, "FY_unknown"),
                       file.path(dest_base, fy_dir)),
    dest_name = ifelse(is.na(ts),
                       paste0("unknown_", base, suffix, ".", ext),
                       paste0(ts_tag, "_", base, suffix, ".", ext)),
    dest_path = file.path(dest_dir, dest_name)
  )

# === dry-run 確認 ===
exif2 %>%
  select(src_path, dest_path) %>%
  head(10) %>%
  print(n = Inf)

# 想定外（撮影日時NA・ファイル欠損など）の件数チェック
table("NA DateTime" = is.na(exif2$ts))
sum(!file.exists(exif2$src_path))  # 存在しない元ファイル数

# === 実行（OKなら do_move <- TRUE に） ===
if (do_move) {
  # 年度フォルダを一括作成
  dir_create(unique(exif2$dest_dir))

  # 上書きしないでコピー（移動にしたい場合は file_move）
  pwalk(
    exif2 %>% select(src_path, dest_path),
    ~ fs::file_copy(..1, ..2, overwrite = FALSE)
  )
  message("Done: copied into ", dest_base)
}




# data ############
vv               # A tibble: 7,673 × 10
vv4              # A data.frame:
vv5              # A tibble: 7,673 × 10
VT               # A list of tibbles: 5 × 10
VTdk             # A list of tibbles: 5 × 10
VC               # A list of tibbles:  × 10
VCrepo           # A list of tibbles:  × 10
vc5              # A tibble: 764 × 25


dbind<-bind_rows(VT,.id="plot")
VV<-split(vv, vv$plot)
