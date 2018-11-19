#ライブラリの導入

#yの取得（Unityデータ)
y <- read.csv("corValue.csv")
nrow(y)
ncol(y)

#x座標の作成
coX1 <- data.frame(coX = rep(1, each = nrow(y)))
for(i in 2:ncol(y)){
    coX2 <- data.frame(coX = rep(i, each = nrow(y)))
    coX1 <- rbind(coX1, coX2)
}


#y座標の作成
coY1 <- data.frame(coY = nrow(y):1)
coY2 <- data.frame(coY = nrow(y):1)
for(i in 2:ncol(y)){
    coY1 <- rbind(coY1, coY2)
}

#点座標の作成
co = cbind(coX1, coY1)

#yのリスト化解除
y <- unlist(y)

coFoot <- cbind(co, y)
head(coFoot)
write.csv(coFoot, file = "coFoot.csv")