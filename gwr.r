library(spgwr)
library(spatialEco)
library(raster)
library(tictoc)


################
### 関数の定義箇所
################
makeCo<- function(dimx, dimy) {
    #x座標の作成
    coX1 <- data.frame(LON = rep(1, each = dimy))
    for(i in 2:dimx){
        coX2 <- data.frame(LON = rep(i, each = dimy))
        coX1 <- rbind(coX1, coX2)
    }

    #y座標の作成
    coY1 <- data.frame(LAT = dimy:1)
    coY2 <- data.frame(LAT = dimy:1)
    for(i in 2:dimx){
        coY1 <- rbind(coY1, coY2)
    }

    #点座標の作成
    co = cbind(coX1, coY1)
    return(co)
}


#計算時間の計測開始
tic("Calculation time is")

#ラスタ画像の読み取り
A <- raster(file.path("Depthmap_Foot.tif"))
B <- raster(file.path("Unity_Foot.tif"))

#ラスタ画像の範囲を合わせる
extent(A) <- c(1, 647, 1, 1027)

AA <- values(A)
AA[AA< 0] <- NA

UNITY <- values(B)

id <- 1:length(AA)

UNITY <- cbind(id, UNITY)
AA <- cbind(id, AA)

#大きさを取得
dimA <- dim(A)
dimB <- dim(B)

#座標の作成
co <- makeCo(dimA[1], dimA[2])
co <- cbind(id, co)

#データの結合
pt1 <- merge(co, AA)
pt <- merge(pt1, UNITY)

length(pt$id)

#AAデータの欠損値の置換
pt <- na.omit(pt)
length(pt$id)

head(pt)

print("Preprocessing is finished")

#バンド幅
# bw <- gwr.sel(AA ~ UNITY , data=pt, coords=cbind(pt$LON, pt$LAT))
# bw

gauss <- gwr(AA ~ UNITY, data=pt,coords=cbind(pt$LON, pt$LAT), bandwidth=5)
print("--------------------")
print("Result")
gauss
print("--------------------")
print("SDF")
gauss$SDF
print("fin")
print("--------------------")
toc()