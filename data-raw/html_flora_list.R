# html_flora_list.R
################  和名-学名-イタリック_APG.R
#### 学名のhtmイタリック書式

#rm(list=ls())

#l<-read.csv("種名APG.csv",as.is=TRUE,fileEncoding="cp932")
#APG<-l
# save(APG,data)

#' html書式　属名・種小名をイタリックにする
#'
#' @param sn0
#'
#' @returns
#' @export
#'
#' @examples
#' html_science_name("Quercus serrata Thunb. ex. Muuray")
#'
html_science_name<-function(sn0){
  ## sn0<-"Quercus serrata Thunb. ex. Muuray"
  cu<-c("var.","subsp.","f\\.","subvar.")
  s<-strsplit(sn0," ")[[1]]
  for(i in 1:length(cu)){
    j<-grep(cu[i],s)
    if(is.integer(j))s[j+1]<-paste("<i>",s[j+1],"</i>",sep="")
  }
  paste("<i>",s[1]," ",s[2],"</i> ",paste(s[c(-1,-2)],collapse =" "),sep="")
}




#' html科名ラベル
#' html植物目録作成用
#' 種名と科名を列に含んだデータフレームを入力し科名のhtmlラベルを出力する
#'
#' @param species_list
#'
#' @returns
#' @export
#'
#' @examples
#' spj <- c("ブナ","コナラ","スギ","アカミノイヌツゲ","イタヤカエデ","アオカラムシ","ミズナラ")
#' species_list <- APG[match(spj,APG$種名),]
#'
#' html_family_label(species_list)
#'
html_family_label <- function(species_list){
  paste("<tr>",
        "<td height=30 valign=\"bottom\">",
        "<font size=+1; face='Arial'>",
        paste(species_list$科名, "&emsp;", species_list$科名.和., sep=""),
        "</font>",
        "</td>",
        "</tr>\n", sep="")
}





#' html植物目録の作成
#'
#' @param spj　和名
#' @param memo　メモ
#'
#'
#' @returns htmlリスト
#'
#' @export
#'
#' @examples
#' spj <- c("ブナ","コナラ","スギ","アカミノイヌツゲ","イタヤカエデ","アオカラムシ","ミズナラ")
#' memo<-c("位山","平井","平井","平井","キャンパス","位山","名古屋","位山")
#' filename="flora_list_test.htm"
#'
#' (FloraList.<- FloraListMaker(spj,memo))
#'
#' #美松調査区の植物目録
#' names(vv)
#' names(APG)
#' spj<-unique(subset(vv,plot=="Mimatsu")$sp)[1:106] #野帳から出現種を抽出
#'
#' spj[is.na(match(spj,APG$種名))]
#'
#' memo.<-rep("Mimatsu",length(spj))
#' (FloraList.<- FloraListMaker(spj,memo=memo.))
#'
#' #　htmlに保存　ブラウザ用
#' #cat(FloraList., file=filename)
#'
FloraListMaker<-function(spj,memo){
  spi<-match(spj,APG$種名)
  sn<-APG$学名[spi]
  spl<-APG[spi,];
  i <- order(spl$ID)
  spl<-spl[i,]
  memo<-memo[i]

  fn<-length(unique(spl$科名));gn<-length(unique(spl$属名));spn<-length(unique(spl$種名))
  (z_<-paste(fn,"科 ",gn,"属 ",spn,"種",sep=""))  #　分類群の数

  ###　目録表題 ####

  FloraList_Title <- paste(
    "<div style=\"text-align:left; margin-top:20px;\">",
    "<font size=\"+2\" face=\"ＭＳ ゴシック,平成ゴシック\">植物目録</font><br><br>",
    "<font size=\"+1\" face=\"ＭＳ ゴシック,平成ゴシック\">", z_, "</font><br><br>",
    "</div>"
  )


  ###　目録 ####
  l <- ""
  for(ii in 1:nrow(spl)){#

    if(ii==1 | (ii!=1 && spl$科名[ii]!=spl$科名[ii-1]) ) {fl.<-paste0(html_family_label(spl[ii,]))} else {fl.<-""}

    sp.<-paste(
      "<tr>","<td>","<font size=-1 face =\"ＭＳ 明朝,平成明朝\">",spl$種名[ii],"</font>","</td>",
      "<td>","<font size=-1 face =\"Times New Roman\">",html_science_name(spl$学名[ii]),
      "</font>","</td>","</tr>",sep="")

    if(is.null(memo[ii])){memo.<-""}else{
      memo.<-paste("<tr>","<td>","</td>","<td>","<font size=\"-3\" face =\"ＭＳ 明朝,平成明朝\">","　",memo[ii],"</font>","</td>","</tr>",sep="")
    }

    l<-paste(l,fl.,sp.,memo.)
  }


  paste(FloraList_Title,"<pre><table>",l,"</table></pre>",sep="")
}





#' 立山植生モニタリング調査の植物目録
#'
#' @returns
#' @export
#'
#' @examples
#' head(flora)
#' flora[flora$美松!="",c("spj","sp")]
#' sp.<-c("ブナ","ハイマツ","リュウキュウマツ","イソマツ")
#' cat("以下はfloraリストにありません")
#' data.frame(sp=sp.[!is.element(sp.,flora$spj)])
#'
#'
RData_flora<-function(){

}


#' 立山植生モニタリング調査の植物目録
#'
#' @returns
#' @export
#'
#' @examples
#' head(flora)
#' flora[flora$美松!="",c("spj","sp")]
#' sp.<-c("ブナ","ハイマツ","リュウキュウマツ","イソマツ")
#' cat("以下はfloraリストにありません")
#' data.frame(sp=sp.[!is.element(sp.,flora$spj)])
#'
#'
RData_flora<-function(){

}


#' APG植物目録()
#' 米倉浩司・梶田忠 (2007-) 植物和名ー学名インデックスYList
#' を利用させていただきました
#'
#' @returns
#'
#' @export
#'
#' @examples
#' head(APG)
#' sp.<-c("ブナ","ハイマツ","リュウキュウマツ","イソマツ")
#' cat("以下はAPGリストにありません")
#' data.frame(sp=sp.[!is.element(sp.,APG$種名)])
#'
#'
RData_APG<-function(){

}



