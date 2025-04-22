#ライブラリのインストール
library("dplyr") #データベースの操作をするためのパッケージ（filterなど）
library("stringr") #文字列処理のパッケージ

#対象ファイルリストの読み込み
listDataPath <- 'src'
dataList <- list.files(listDataPath, pattern="search-list.csv")

#各リストをターゲットとして、統計をだす処理をする。
for (i in 1:length(dataList)){
  if(exists("target")){rm(target)}
  target <- dataList[i]

  if(exists("searchList")){rm(searchList)}
  searchList <- read.csv(paste(listDataPath,"/",target,sep=""),fileEncoding="cp932",header=TRUE,colClasses=rep("character",6))
  searchList <- select(searchList,1,3)

  #ＸＭＬファイル（ＸＭＬファイルのＺＩＰファイル）ごとに処理するため一覧を取得
  if(exists("files")){rm(files)}
  files <- unique(searchList[,1])

  # 各ＸＭＬファイルごとに統計を取る。
  for(j in 1:length(files)){
    if(exists("subList")){rm(subList)}
    subList <- filter(searchList, ZIPファイル名==files[j])

    #各ＸＭＬファイル中の地番区域ごとに筆数を調べる
    if(exists("stat1")){rm(stat1)}
    stat1 <- 
      subList %>% 
      group_by(地番区域) %>%
      summarise(n=n())

    for (k in 1:nrow(stat1)){
      write(paste(substr(searchList[j,1],1,5),",",files[j],",",stat1[k,1],",",stat1[k,2],sep=""),"result20250421.csv",append=TRUE)
    }
  }
}



