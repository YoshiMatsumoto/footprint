# library(spatialEco)
library(raster)
library(spgwr)

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



#ラスタ画像の読み取り
A <- raster(file.path("Unity_Foot.tif"))
B <- raster(file.path("Depthmap_Foot.tif"))

UNITY <- values(A)
AA <- values(B)

#大きさを取得
dimA <- dim(A)
dimB <- dim(B)

#データをクリーン
UNITY[is.na(UNITY)] <- 0
AA[is.na(AA)] <- 0

#座標の作成
co <- makeCo(dimA[1], dimA[2])

#データの結合
pt <- cbind(co, UNITY, AA)
# ptB <- cbind(co, values(B))

head(pt)

print("Preprocessing is finished")

#バンド幅
bw <- gwr.sel(AA ~ UNITY , data=pt, coords=cbind(pt$LON, pt$LAT))
bw

# gauss <- gwr(AA ~ UNITY, data=pt,coords=co, bandwidth=bw)
# gauss
print("fin")