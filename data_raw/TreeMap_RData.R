# 必要なパッケージ
library(akima)
library(ggplot2)
library(dplyr)
library(ggrepel) #install.packages("ggrepel")


# 例: 樹木位置データ (x, y) と標高データ (z)
set.seed(123)
trees <- data.frame(
  x = runif(50, 0, 100),
  y = runif(50, 0, 100),
  z = rnorm(50, mean = 100, sd = 10)
)

# akimaで補間
interp_res <- with(trees, akima::interp(x, y, z, nx = 100, ny = 100))

# 補間結果をデータフレーム化
interp_df <- expand.grid(
  x = interp_res$x,
  y = interp_res$y
)
interp_df$z <- as.vector(interp_res$z)
contour(interp_df$x,interp_df$y,interp_df$z)



# ggplotで等高線と樹木位置を描画
ggplot() +
  geom_contour(data = interp_df, aes(x = x, y = y, z = z), color = "blue") +
  geom_point(data = trees, aes(x = x, y = y), color = "red") +
  theme_minimal() +
  labs(title = "樹木位置図と等高線", x = "X座標", y = "Y座標")



library(ggplot2)
library(ggrepel)

# ダミーデータ（実データで置き換え）
df <- data.frame(
  x = runif(30, 0, 100),
  y = runif(30, 0, 100),
  dbh = runif(30, 10, 50),
  id = paste0(100:129),
  species = sample(c("A1", "B2", "C3"), 30, replace = TRUE),
  vigor = sample(1:5, 30, replace = TRUE)
)

# プロット

ggplot(df, aes(x, y)) +
  geom_point(aes(size = dbh), shape = 21, fill = "lightblue", color = "black") +
  geom_text_repel(aes(label = id), size = 3, nudge_y = 5) +  # IDを上に
  geom_text(aes(label = species), size = 3, hjust = -0.2, vjust = 0.2) +  # 樹種
  geom_text(aes(label = vigor), size = 2, hjust = 1.2, vjust = 1.2, color = "gray40") +  # 活力度
  scale_size(range = c(2, 8)) +
  theme_minimal() +
  labs(title = "樹木位置図（ラベル付き）", x = "X", y = "Y")

# treemap2  ####



## contour ####
cont<-ggplot() +
  geom_contour(data = interp_df, aes(x = x, y = y, z = z), color = "blue")

cont

#' Title
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#' x = runif(30, 0, 100),
#' y = runif(30, 0, 100),
#' z = runif(30, 0, 100),
#' dbh = runif(30, 10, 50),
#' id = paste0(100:129),
#' species = sample(c("A1", "B2", "C3"), 30, replace = TRUE),
#' vigor = sample(1:5, 30, replace = TRUE)
#' )
#' treemap_ggplot(df)
#'
#' .<-ForestTrees
#' pn <- "Mimatsu"
#' .<-.[.$plot==pn,]
#' names(.)
#'
#'
#' #'
treemap_ggplot <-function(df){
  ggplot(df, aes(x, y)) +
    geom_point(aes(size = dbh), shape = 21, fill = "lightblue", color = "black") +
    geom_text_repel(aes(label = id), size = 3, nudge_y = 5) +  # IDを上に
    geom_text(aes(label = species), size = 3, hjust = -0.2, vjust = 0.2) +  # 樹種
    geom_text(aes(label = vigor), size = 2, hjust = 1.2, vjust = 1.2, color = "gray40") +  # 活力度
    scale_size(range = c(2, 8)) +
    theme_minimal() +
    labs(title = "樹木位置図（ラベル付き）", x = "X", y = "Y")
}

ggplot(df, aes(x, y)) +
  geom_point(aes(size = dbh), shape = 21, fill = "lightblue", color = "black")+
  geom_text_repel(aes(label = id), size = 3, nudge_y = 5) +  # IDを上に

treemap <- treemap_ggplot(df)

# 樹木位置+等高線

#treemap+cont


########################
library(ggplot2)
library(ggrepel)
library(metR)



#' Title
#'
#' @param tree_df
#' @param contour_df
#' @param path_df
#' @param show_contour
#' @param show_path
#' @param contour_label
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
plot_forest_map <- function(tree_df,
                            contour_df = NULL,
                            path_df = NULL,
                            show_contour = FALSE,
                            show_path = FALSE,
                            contour_label = FALSE) {

  # ベースプロット
  p <- ggplot() +
    geom_point(data = tree_df, aes(x = x, y = y, size = dbh), shape = 21, fill = "lightblue", color = "black") +
    geom_text_repel(data = tree_df, aes(x = x, y = y, label = id), size = 3, nudge_y = 5) +
    geom_text(data = tree_df, aes(x = x, y = y, label = species), size = 3, hjust = -0.2, vjust = 0.2) +
    geom_text(data = tree_df, aes(x = x, y = y, label = vigor), size = 2, hjust = 1.2, vjust = 1.2, color = "gray40") +
    scale_size(range = c(2, 8)) +
    theme_minimal() +
    labs(title = "樹木位置図", x = "X", y = "Y")

  # 等高線
  if (show_contour && !is.null(contour_df)) {
    p <- p + geom_contour(data = contour_df, aes(x = x, y = y, z = z), color = "blue")
    if (contour_label) {
      p <- p + metR::geom_text_contour(data = contour_df, aes(x = x, y = y, z = z), size = 3, color = "blue")
    }
  }

  # 小川・歩道
  if (show_path && !is.null(path_df)) {
    p <- p + geom_path(data = path_df, aes(x = x, y = y), color = "gray40", linetype = "dashed")
  }

  return(p)
}

#####
# データ例
set.seed(123)
tree_df <- data.frame(
  x = runif(30, 0, 100),
  y = runif(30, 0, 100),
  dbh = runif(30, 10, 50),
  id = paste0(100:129),
  species = sample(c("A1", "B2", "C3"), 30, replace = TRUE),
  vigor = sample(1:5, 30, replace = TRUE)
)

# 等高線データ（akimaで作成）
library(akima)
interp_res <- with(tree_df, akima::interp(x, y, dbh, nx = 100, ny = 100))
contour_df <- expand.grid(x = interp_res$x, y = interp_res$y)
contour_df$z <- as.vector(interp_res$z)

# 小川・歩道データ
path_df <- data.frame(x = seq(0, 100, length.out = 20), y = 50 + 10 * sin(seq(0, 2*pi, length.out = 20)))

# プロット
plot_forest_map(tree_df, contour_df, path_df, show_contour = TRUE, show_path = TRUE, contour_label = TRUE)


x<-rep(seq(45,5,-10),5)
y<-rep(seq(55,95,10),each=5)
d0<-subplot_xy.RData
i<-d0$調査地=="Mimatsu"
d0$X[i]<-x
d0$Y[i]<-y
text(d0$X[i],d0$Y[i],d0$subplot[i],col="red")
subplot_xy<-d00[,-c(4,6,7)]
names(subplot_xy)<-c("plot","x","y","subplot")
#save(subplot_xy,file="subplot_xy.RData")
load("subplot_xy.RData")

#' Title
#'
#' @param plot.name
#'
#' @return
#' @export
#'
#' @examples
#' plot(1:100,type="n")
#' subplot_xy_text(plot.name="Mimatsu",col="red",cex=3)
#' subplot_xy_text(plot.name="Arimine",col="Blue",cex=2)
#' subplot_xy_text(plot.name="Kaminokodaira",col="Magenta",cex=2)
#'
subplot_xy_text<-function(plot.name="Mimatsu",...){
  .<-subplot_xy
  i<-.$plot==plot.name
  text(.$x[i],.$y[i],.$subplot[i],...)
}

PlotProfile<-read.csv("plot profile 2024.csv",fileEncoding = "cp932",skip=1)
# save(PlotProfile,file="PlotProfile.RData")

ForestTrees<-dd4
# save(ForestTrees,file="ForestTrees.RData")


f.<-dir(pattern="*@.")
d.<-c()
for(i in 1:length(f.)){
  d.<-rbind(d.,data.frame(plot=f.[i],read.csv(f.[1],fileEncoding = "cp932")))
}
d.[,1]<-gsub("^@|\\.csv$", "", d.[,1])
names(d.)
LandMarks<-d.
names(LandMarks)
# save(LandMarks,file="LandMarks.RData")


# subplot_xy.RData　 ####
# Mimatsu Kaminokodaira のサブプロットデータ修正　
setwd("C:\\Users\\ishid\\Dropbox\\00D\\00\\立山植生モニタリング事業\\第05期\\2024\\植生調査\\_樹木位置図")
dir()
d0<-read.csv("立山植生モニタリング_植生サブプロット_xy-訂正版2.csv",as.is=TRUE,fileEncoding="SHIFT-JIS")
names(d0)
.<-d0[,c(1,2,3,5)]
names(.)<-names(subplot_xy)
head(.)


subplot_xy<-.
# save(subplot_xy,file="subplot_xy.RData")
.<-subplot_xy
..<-subset(.,plot=="Mimatsu")
50-..$x
.$x[.$plot=="Mimatsu"]<-50-.$x[.$plot=="Mimatsu"]
subplot_xy<-.
# save(subplot_xy,file="data/subplot_xy.RData")

.<-subplot_xy
..<-subset(.,plot=="Kaminokodaira")
50-..$x
.$subplot[.$plot=="Kaminokodaira"]<-1:25

subplot_xy<-.
# save(subplot_xy,file="data/subplot_xy.RData")


