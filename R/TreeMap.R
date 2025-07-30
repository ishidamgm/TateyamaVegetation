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

#' Title
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#'
#' #  x11()
#' treemap("Bijodaira")
#' treemap("Bunazaka")
#' treemap("Bunadaira")
#' treemap("Kaminokodaira")
#' treemap("Matsuotoge")
#' treemap("Kagamiishi")
#'
#' #'
#' treemap("Mimatsu")
#'  treemap("Mimatsu")+ ylim(c(45, 105))
#'

#'
#' treemap("Arimine")
#'  treemap("Arimine")+ xlim(c(-5, 55))+ ylim(c(-5, 60))
#'
#'
#'
#'
#'
#' #'
treemap <-function(plot.name="Mimatsu"){
  # contour
  interp_res <-rasXYZ[[which(names(rasXYZ)==plot.name)]]
  interp_df<-ggplot_contour_data(interp_res)
  # Trees location
  df <- subset(ForestTrees,plot==plot.name & !is.na(d06))

  #grid
  .<-plt[plt$na==plot.name,]
  x_grid<-seq(0,.$px,.$step)
  y_grid<-seq(0,.$py,.$step)
  v.grid<- geom_vline(xintercept =  x_grid, linetype = "dashed", color = "red")
  h.grid<- geom_hline(yintercept =  y_grid, linetype = "dashed", color = "red")

  # subplot
    .<-subset(subplot_xy,plot==plot.name)
    subplot<- geom_text(data=.,aes(label = subplot), size = 20, color="orange") #, alpha = 0.5

  #topo
    walk1 <- NULL; walk2 <- NULL
    stream1 <- NULL; stream2 <- NULL
    .<-topo
    .<-subset(.,plot==plot.name)
    stream1 <- geom_path(data=subset(.,sp=="stream1"),aes(x = x, y = y), linewidth = 2, color="skyblue")
    stream2 <- geom_path(data=subset(.,sp=="stream2"),aes(x = x, y = y), linewidth = 2, color="skyblue")
    walk1   <- geom_path(data=subset(.,sp=="walk1"),aes(x = x, y = y), linewidth = 2, color="red",linetype = "dashed")
    walk2   <- geom_path(data=subset(.,sp=="walk2"),aes(x = x, y = y), linewidth = 2, color="red",linetype = "dashed")

  # ggplot
  ggplot(df, aes(x, y)) +
    geom_contour(data = interp_df, aes(x = x, y = y, z = z), color = "gray")+
    v.grid+h.grid+
    subplot+
    geom_point(aes(size =d06), shape = 21, fill = "lightblue", color = "black") +
    geom_text_repel(aes(label = lb), size = 3, nudge_y = 5) +  # IDを上に
    geom_text(aes(label = sp), size = 3, hjust = -0.2, vjust = 0.2) +  # 樹種
    geom_text(aes(label = f06), size = 2.5, hjust = 1.2, vjust = 1.2, color = "red", fontface = "bold") +  # 活力度
    stream1+stream2+ walk1+ walk2 +
    scale_size(range = c(2, 8)) +
    theme_classic() +
    labs(title =plot.name, x = "X (m)", y = "Y (m)")

}



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

#' Title
#'
#' @param plot.name
#'
#' @return
#' @export
#'
#' @examples
#' (p<-treemap(plot.name="Mimatsu"))
#' p+subplot_xy_ggtext("Mimatsu")
#'
subplot_xy_ggtext<-function(plot.name="Mimatsu"){
  .<-subset(subplot_xy,plot==plot.name)
  #ggplot(., aes(x, y))+
    geom_text(data=.,aes(label = subplot), size = 20, color="orange", alpha = 0.5)
}

#' Title
#'
#' @param plot.name
#'
#' @return
#' @export
#'
#' @examples
#' x11()
#' pn<-"Mimatsu"
#'  pn<-"Bijodaira"
#' (p<-treemap(plot.name=pn))
#' p+topo_gg(pn)
#' p+stream1
topo_gg<-function(plot.name="Mimatsu"){
  walk1 <- NULL; walk2 <- NULL
  stream1 <- NULL; stream2 <- NULL
  .<-topo
  .<-subset(.,plot==plot.name)
  stream1 <- geom_path(data=subset(.,sp=="stream1"),aes(x = x, y = y), linewidth = 2, color="skyblue", alpha = 0.5)
  stream2 <- geom_path(data=subset(.,sp=="stream2"),aes(x = x, y = y), linewidth = 2, color="skyblue", alpha = 0.5)
  walk1   <- geom_path(data=subset(.,sp=="walk1"),aes(x = x, y = y), linewidth = 2, color="red",linetype = "dashed", alpha = 0.5)
  walk2   <- geom_path(data=subset(.,sp=="walk2"),aes(x = x, y = y), linewidth = 2, color="red",linetype = "dashed", alpha = 0.5)

  ggplot() + stream1+stream2+ walk1+ walk2
}



#' Akimaのinterpで出力したコンタデータをggplotのgeom_contour
#' で描画できるデータフレームに変換する関数
#'
#'
#'
#' @param interp_res  Akimaのinterpで出力したリスト型のコンタデータ
#'
#' @return　　ggplotのgeom_contourで描画できるデータフレーム　
#' @export
#'
#' @examples
#' plot.name<-"Bunazaka" #"Mimatsu"
#' interp_res <-rasXYZ[[which(names(rasXYZ)==plot.name)]]
#' interp_df<-ggplot_contour_data(interp_res)
#' ggplot() +
#' geom_contour(data = interp_df, aes(x = x, y = y, z = z), color = "blue")
#'
ggplot_contour_data <- function(interp_res=interp_res){
  #interp_res <- rasXYZ[[which(names(rasXYZ)==plot.name)]]
  interp_df <- expand.grid(x = interp_res$x, y = interp_res$y)
  interp_df$z <- as.vector(interp_res$z)
  return(interp_df)
}





