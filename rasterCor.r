#install.packages("spatialEco")

#ライブラリの導入
library(spatialEco)
library(jsonlite)

#xの取得（DepthMapデータ)
x <- read.csv("Agent_Analysis.csv")
x <- x$Gate.Counts

#yの取得（Unityデータ)
y <- read.csv("FootPrint.csv")
# nrow(y)
# ncol(y)

#x座標の作成
coX1 <- data.frame(coX = 1:ncol(y))
coX2 <- data.frame(coX = 1:ncol(y))
for(i in 2:nrow(y)){
    coX1 <- cbind(coX1, coX2)
}

coX1 <- flatten(coX1)
head(coX1)
#y座標の作成
coY1 <- data.frame(coY = 1:nrow(y))
coY2 <- data.frame(coY = 1:nrow(y))
for(i in 2:ncol(y)){
    coY1 <- rbind(coY1, coY2)
}

#点座標の作成
co = cbind(coX1, coY1)
#xのデータリファイン
x <- gsub(-1, 0, x)

#yのリスト化解除
y <- unlist(y)
co <- unlist(co)

coFoot <- cbind(y, co)
#write.csv(coFoot, file = "coFoot.csv")
#要素数の確認
# length(x)
# length(y)

# showClass(x)
# showClass(y)

# r.cor <- rasterCorrelation(x, y, s = 5, type = "spearman")
# plot(r.cor)
