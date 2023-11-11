
# 3 kriter için Tekdüze dağılımdan 50 tane ve Weibull dağılımdan 50 tane olmak üzere 100 tane rastgele ağırlık vektörü üretin 

uniform <- data.frame("z1" = c(runif(50, min = 0, max = 1)), 
                      "z2" = c(runif(50, min = 0, max = 1)),
                      "z3" = c(runif(50, min = 0, max = 1)))

weibull <- data.frame("z1" = c(rweibull(50, 0.3, 0.1)), 
                      "z2" = c(rweibull(50, 0.3, 0.1)),
                      "z3" = c(rweibull(50, 0.3, 0.1)))

# Her vektörde ağırlıkların toplamı 1 olmalıdır

uniform_normalized <- uniform / rowSums(uniform)
weibull_normalized <- weibull / rowSums(weibull)

data <- rbind(uniform_normalized, weibull_normalized)

# Ürettiğiniz 100 tane vektöre Komşuluğun Dışındaki En Uzak Nokta Yöntemini uygulayarak filtreleyin. Ağırlıksız Öklit mesafesi kullanın.

alınan_noktalar <- data.frame()
alınan_noktalar <- rbind(alınan_noktalar,data[1,])   # seed point i aldığımız noktalara ekledik. 

l2_norm <- function(x,y){
  (abs(x-y))^2
}

# İterasyon 1 
a <- data.frame()
for (i in 1:100){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:82

b <- data.frame()
for (i in 1:82){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # seed point e en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:2

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

#iterasyon 2
a <- data.frame()
for (i in 1:81){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:72

b <- data.frame()
for (i in 1:72){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:3

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# iterasyon 3
a <- data.frame()
for (i in 1:71){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:62

b <- data.frame()
for (i in 1:62){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:4

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# iterasyon 4
a <- data.frame()
for (i in 1:61){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:49

b <- data.frame()
for (i in 1:49){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:5

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 5
a <- data.frame()
for (i in 1:48){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:43

b <- data.frame()
for (i in 1:43){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:6

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 6
a <- data.frame()
for (i in 1:42){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:40

b <- data.frame()
for (i in 1:40){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:7

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 7
a <- data.frame()
for (i in 1:39){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:35

b <- data.frame()
for (i in 1:35){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:8

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 8
a <- data.frame()
for (i in 1:34){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:30

b <- data.frame()
for (i in 1:30){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:9

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 9
a <- data.frame()
for (i in 1:29){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[9,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:23

b <- data.frame()
for (i in 1:23){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[9,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:10

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 10
a <- data.frame()
for (i in 1:22){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[10,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:19

b <- data.frame()
for (i in 1:19){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[9,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[10,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:11

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 11
a <- data.frame()
for (i in 1:18){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[11,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:14

b <- data.frame()
for (i in 1:14){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[9,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[10,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[11,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:12

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 12
a <- data.frame()
for (i in 1:13){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[12,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:10

b <- data.frame()
for (i in 1:10){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[9,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[10,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[11,], data[i,]))) +  sqrt(rowSums(l2_norm(alınan_noktalar[12,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:13

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 13
a <- data.frame()
for (i in 1:9){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[13,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 
row.names(data) <- 1:3

b <- data.frame()
for (i in 1:3){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[1,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[2,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[3,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[4,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[5,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[6,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[7,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[8,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[9,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[10,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[11,], data[i,]))) +  sqrt(rowSums(l2_norm(alınan_noktalar[12,], data[i,]))) + sqrt(rowSums(l2_norm(alınan_noktalar[13,], data[i,])))
  b <- rbind(b,l2)
}
alınan_noktalar <- rbind(alınan_noktalar,data[which.max(b[,1]),])  # alınan noktalardaki çözümlere olan toplamda en uzak çözümü alınan noktalar data frame ine ekledik. 
row.names(alınan_noktalar) <- 1:14

data <- data[-c(which.max(b[,1])), ]   # data'dan artık alınan noktalar a aldığımız noktayı çıkarabiliriz.

# Iterasyon 14
a <- data.frame()
for (i in 1:2){
  l2 <- sqrt(rowSums(l2_norm(alınan_noktalar[14,], data[i,])))
  a <- rbind(a,l2)
}
data <- data[-c(which(a < 0.25)), ]      # alınan noktalara eklenen yeni çözüm ile aralarındaki uzaklıkları 0.25'ten küçük olanları çıkardık. 

# Geriye çözüm kalmadı. Sonuçta aldığımız noktalar 14 çözümü içermektedir.


# Elde ettiğiniz vektörlerin grafiğini çizip tüm olurlu ağırlık uzayını iyi temsil edip etmediklerini yorumlayın

plot(alınan_noktalar)
plot(data)

plot_ly(x=data$z1, y =data$z2, z=data$z3, type = "scatter3d", mode = "markers")
plot_ly(x=alınan_noktalar$z1, y =alınan_noktalar$z2, z=alınan_noktalar$z3, type = "scatter3d", mode = "markers")









