########
#ライブラリのインストール
library("dplyr") #データベースの操作をするためのパッケージ（filterなど）
library("stringr") #文字列処理のパッケージ

#対象ファイルリストの読み込み
zahyo <- read.csv("projection-list-edit.txt",fileEncoding="cp932",header=FALSE,colClasses=rep("character",2)) #投影法リスト
kekka <- read.csv("result20250421.csv",fileEncoding="utf8",header=FALSE,colClasses=c(rep("character",3),"numeric")) #前のステップの結果

#文字列の調整
zahyo[,1] <- sub(".xml",".zip",zahyo[,1]) #投影法リストのファイル名をZIPにして今回のリストに合わせます
zahyo[,2] <- gsub(" ","",zahyo[,2]) #前回作ったときにスペースを入れてしまったので削除しておく。


#今後の解析処理用に結果・地番地区名を調整（府中市（東京都と広島県）、伊達市（北海道と福島県）のように同名の市区町村があるので市区町村コードと地番地区をつなげる文字列にする。
kekka[,5] <- paste(kekka[,1],"-",kekka[,3],sep="")

#座標をjoin（ベースは解析結果）
joinTable <- left_join(kekka,zahyo,by=c("V2"="V1"))

#書き出し処理に向けて地番地区（町字）リストmachiListを作成
machiList <- unique(joinTable[,c(1,3,5)]) #東京と広島の府中市、北海道と福島の伊達市に注意

#一行目はタイトル
write(paste("市区町村コード","地番区域","ファイル数","公共座標ファイル数","公共座標ファイル割合（％）","筆数（ポリゴン数）","公共座標筆数（ポリゴン数）","公共座標筆（ポリゴン数）割合（％）",sep=","),"2025-chizuXML公共座標率.csv",append=TRUE)

#各市区町村・地番地区（町字について処理）
for(i in 1:nrow(machiList)){
  #全体から当該地番地区を抽出
  if(exists("target")){rm(target)}
  target <- filter(joinTable, V5 == machiList[i,3] )

  ##地番地区の関係ファイル数での数、割合
  if(exists("stat1")){rm(stat1)}
  if(exists("niniFileN")){rm(niniFileN)}
  if(exists("kokyoFileN")){rm(kokyoFileN)}
  if(exists("fileN")){rm(fileN)}
  stat1 <- 
    target %>% 
    group_by(V2.y) %>%
    summarise(fileN=n())

  if(nrow(filter(stat1,V2.y=="任意座標系"))==1){
    niniFileN <- filter(stat1,V2.y=="任意座標系")[1,2]
  }else{
    niniFileN <- 0
  }

  fileN <- sum(stat1[,2])
  kokyoFileN <- fileN - niniFileN
  
  ##筆数（ポリゴン数）での数、割合
  if(exists("stat2")){rm(stat2)}
  if(exists("niniFudeN")){rm(niniFudeN)}
  if(exists("kokyoFudeN")){rm(kokyoFudeN)}
  if(exists("fudeN")){rm(fudeN)}

  stat2 <- 
    target %>% 
    group_by(V2.y) %>%
    summarise(fudeN=sum(V4))

  if(nrow(filter(stat2,V2.y=="任意座標系"))==1){
    niniFudeN <- filter(stat2,V2.y=="任意座標系")[1,2]
  }else{
    niniFudeN <- 0
  }

  fudeN <- sum(stat2[,2])
  kokyoFudeN <- fudeN -niniFudeN

  #書き出し
  write(paste(machiList[i,1],machiList[i,2],fileN,kokyoFileN,round(kokyoFileN/fileN*100,1),fudeN,kokyoFudeN,round(kokyoFudeN/fudeN*100,1),sep=","),"2025-chizuXML公共座標率.csv",append=TRUE)
}





