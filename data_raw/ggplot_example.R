library(ggplot2)
library(patchwork)

p <- ggplot(mtcars, aes(disp, wt)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = FALSE, formula = y~x-1) +
  scale_x_continuous(limits = c(0, 500)) +
  scale_y_continuous(limits = c(0, 6)) +
  labs(title = "original")
p1 <- p + xlim(c(0, 250)) + labs(title = "xlim(c(0, 250))")
p2 <- p + coord_cartesian(xlim = c(0, 250)) + labs(title = "coord_cartesian(xlim = c(0, 250))")
p / (p1 | plot_spacer()) / (p2 | plot_spacer())

p / p /p
p |(p1 | plot_spacer())


ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_vline(xintercept = c(2.5, 3.5, 4.5,100), linetype = "dashed", color = "gray") +
  geom_hline(yintercept = c(15, 25), linetype = "dotted", color = "gray")+
  xlim(c(0,10))


ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_hline(yintercept = seq(0,20,1), linetype = "dotted", color = "gray")+
  theme_classic()


# ggplot treemap + contour
library(ggplot2)
library(ggrepel)
library(akima)

# データ例
df <- data.frame(
  x = runif(30, 0, 100),
  y = runif(30, 0, 100),
  z = runif(30, 0, 100),
  dbh = runif(30, 10, 50),
  id = paste0(100:129),
  species = sample(c("A1", "B2", "C3"), 30, replace = TRUE),
  vigor = sample(1:5, 30, replace = TRUE)
)

# akimaで補間
interp_res <- with(df, akima::interp(x, y, z, nx = 100, ny = 100))

interp_df <- expand.grid(x = interp_res$x, y = interp_res$y)
interp_df$z <- as.vector(interp_res$z)

# treemap_ggplot関数に等高線を入れる
treemap_ggplot <- function(df, contour_df) {
  ggplot() +
    geom_contour(data = contour_df, aes(x = x, y = y, z = z), color = "blue") +
    geom_point(data = df, aes(x = x, y = y, size = dbh), shape = 21, fill = "lightblue", color = "black") +
    geom_text_repel(data = df, aes(x = x, y = y, label = id), size = 3, nudge_y = 5) +
    geom_text(data = df, aes(x = x, y = y, label = species), size = 3, hjust = -0.2, vjust = 0.2) +
    geom_text(data = df, aes(x = x, y = y, label = vigor), size = 2, hjust = 1.2, vjust = 1.2, color = "gray40") +
    scale_size(range = c(2, 8)) +
    theme_minimal() +
    labs(title = "樹木位置図 + 等高線", x = "X", y = "Y")
}

# 描画
treemap_ggplot(df, interp_df)


# Mimatsu ####
#' Akimaのinterpで出力したコンタデータをggplotのgeom_contour
#' で描画できるデータフレームに変換する関数
#'
#' @param ras 　Akimaのinterpで出力したコンタデータ
#'
#' @return　　　ggplotのgeom_contourで描画できるデータフレーム　
#'
#'
#' @export
#'
#' @examples
#' plot.name<-"Mimatsu"
#' interp_res <-rasXYZ[[which(names(rasXYZ)==plot.name)]]
#'
#'
ggplot_contour_data <- function(interp_res=interp_res){
  #interp_res <- rasXYZ[[which(names(rasXYZ)==plot.name)]]
  interp_df <- expand.grid(x = interp_res$x, y = interp_res$y)
  interp_df$z <- as.vector(interp_res$z)
  return(interp_df)
}






df <- subset(dd4,plot==plot.name & !is.na(d06))

interp_res <- rasXYZ[[which(names(rasXYZ)==plot.name)]]
interp_df <- expand.grid(x = interp_res$x, y = interp_res$y)
interp_df$z <- as.vector(interp_res$z)

ggplot() +
  geom_contour(data = interp_df, aes(x = x, y = y, z = z), color = "blue")


# treemap_ggplot関数に等高線を入れる
treemap_ggplot <- function(df, contour_df) {
  ggplot() +
    geom_contour(data = contour_df, aes(x = x, y = y, z = z), color = "blue") +
    geom_point(data = df, aes(x = x, y = y, size = dbh), shape = 21, fill = "lightblue", color = "black") +
    geom_text_repel(data = df, aes(x = x, y = y, label = id), size = 3, nudge_y = 5) +
    geom_text(data = df, aes(x = x, y = y, label = species), size = 3, hjust = -0.2, vjust = 0.2) +
    geom_text(data = df, aes(x = x, y = y, label = vigor), size = 2, hjust = 1.2, vjust = 1.2, color = "gray40") +
    scale_size(range = c(2, 8)) +
    theme_minimal() +
    labs(title = "樹木位置図 + 等高線", x = "X", y = "Y")
}

# 描画
treemap_ggplot(df, interp_df)

