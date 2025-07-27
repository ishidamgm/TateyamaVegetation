#' 　NAをNULLに変換する関数
#'　ggplotで樹木位置図を描く場合，小川や歩道のデータがない場合でもエラーにしない
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' stream1<-NULL ; stream2<-NULL
#' dat<-data.frame(x=1:50,y=2*(1:50))
#' g<-ggplot(data=dat,aes(x=x,y=y))
#' stream1<-geom_line()
#'   g+stream1
#'   g+stream1  +stream2
#'   g+ safe_add(stream1)  +　safe_add(stream2)

safe_add <- function(x) {
  if (is.ggproto(x) || inherits(x, "Layer") || inherits(x, "gg")) {
    x
  } else {
    NULL
  }
}
