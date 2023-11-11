
# 400 satırlı 4 sütunlu bir data frame oluşturuldu. Sütunlardaki kriterlerden ilk ikisi
# maksimizasyon iken kalanları minimizasyondur. Aşağıda rastgele sayılardan oluşturulan veri seti verilmiştir. 

data <- data.frame("z1(max)" = c(runif(400, min = 1, max = 100)), 
                   "z2(max)" = c(runif(400, min = 7, max = 49)),
                   "z3(min)" = c(runif(400, min = 65, max = 900)),
                   "z4(min)" = c(runif(400, min = 150, max = 750)))

# "Elitist Non-Dominated Sorting Genetic Algorithm" R paketi nondominated çözümlere sorting uygulamak için kullanılmıştır.
library(nsga2R)
data_set <- data.frame(-data$z1.max., -data$z2.max.,data$z3.min.,data$z4.min.)  # bu kütüphanede kullandığımız fonksiyon amaç fonksiyonları minimize edecek şekilde tanımlandığı için ilk iki sütun negatif olarak dönüştürülüp devam edilir.
ranks <- fastNonDominatedSorting(data_set)

# Sonuçta rankler sırasıyla etkinlik seviyelerinin data setimizdeki indis değerlerini çıkarmaktadır.
# Aşağıda oluşturulan 7 sorting grubunun data setindeki karşılıklarını oluşturan kodlar verilmiştir.

efficient1 <- data.frame(data[ranks[[1]],])
efficient2 <- data.frame(data[ranks[[2]],])
efficient3 <- data.frame(data[ranks[[3]],])
efficient4 <- data.frame(data[ranks[[4]],])
efficient5 <- data.frame(data[ranks[[5]],])
efficient6 <- data.frame(data[ranks[[6]],])
efficient7 <- data.frame(data[ranks[[7]],])

# data setinde üretilen 400 çözüm için ideal ve nadir çözümler aşağıdaki kodla bulunabilir.
# ideal çözümler:

ideal <- data.frame("z1ideal" = max(data$z1.max.),
                    "z2ideal" = max(data$z2.max.),
                    "z3ideal" = min(data$z3.min.), 
                    "z4ideal" = min(data$z4.min.))

# nadir çözümler (400 çözüm için efficient set olarak yukarıda bulunan efficient1 değerleri kullanılır) :

efficient_set <- efficient1
nadir <- data.frame("z1nad" = min(efficient_set$z1.max.),
                    "z2nad" = min(efficient_set$z2.max.),
                    "z3nad" = max(efficient_set$z3.min.), 
                    "z4nad" = max(efficient_set$z4.min.))

# İlk etkinlik seviyesindeki çözümlerin kriter değerleri prosedür 2 kullanılarak normalize edilmiştir.

prosedur2_max <- function (x){
  (x-min(x))/(max(x)-min(x))
}
prosedur2_min <- function (x){
  (max(x)-x)/(max(x)-min(x))
}
normalized <- data.frame("z1norm" = prosedur2_max(efficient1$z1.max.),"z2norm" = prosedur2_max(efficient1$z2.max.),
                         "z3norm" = prosedur2_min(efficient1$z3.min.),"z4norm" = prosedur2_min(efficient1$z4.min.))
normalized_ideal <- data.frame("z1*norm" = 1, "z2*norm" = 1,"z3*norm" = 1,"z4*norm" = 1)
normalized_nadir <- data.frame("z1nad_norm" = 0, "z2nad_norm" = 0,"z3nad_norm" = 0,"z4nad_norm" = 0)

# ilk etkinlik seviyesindeki çözümler, ideal ve nadir çözümler normalize edildikten sonra aralarındaki uzaklık hesaplanır.
# ideal ile arasındaki uzaklıklar:

l1_norm <- function(x,y){
  abs(x-y)
} 
l1_uzaklık_ideal <- data.frame()
for (i in 1:53) {  
  l1 <- rowSums(l1_norm(normalized[i,],normalized_ideal))
  l1_uzaklık_ideal <- rbind(l1_uzaklık_ideal, l1)
  colnames(l1_uzaklık_ideal) <- c("l1 uzaklık")
  }
# l1 norma göre ideale en yakın çözüm: 
which.min(l1_uzaklık_ideal$`l1 uzaklık`)     #sonuca göre 1. çözüm ideale en yakın l1 normuna göre.


l2_norm <- function(x,y){
  (abs(x-y))^2
}
l2_uzaklık_ideal <- data.frame()
for (i in 1:53) {  
  l2 <- sqrt(rowSums(l2_norm(normalized[i,],normalized_ideal)))
  l2_uzaklık_ideal <- rbind(l2_uzaklık_ideal, l2)
  colnames(l2_uzaklık_ideal) <- c("l2 uzaklık")
}
# l2 norma göre ideale en yakın çözüm: 
which.min(l2_uzaklık_ideal$`l2 uzaklık`)     #sonuca göre 11. çözüm ideale en yakın l2 normuna göre.


l_inf_norm <- function(x,y){
  max(abs(x-y))
}
l_inf_uzaklık_ideal <- data.frame()
for (i in 1:53) {  
  l_inf <- l_inf_norm(normalized[i,],normalized_ideal)
  l_inf_uzaklık_ideal <- rbind(l_inf_uzaklık_ideal, l_inf)
  colnames(l_inf_uzaklık_ideal) <- c("l inf uzaklık")
}
# l infinity norma göre ideale en yakın çözüm: 
which.min(l_inf_uzaklık_ideal$`l inf uzaklık`)     #sonuca göre 22. çözüm ideale en yakın linf normuna göre.


# nadir ile arasındaki uzaklıklar:

l1_uzaklık_nadir <- data.frame()
for (i in 1:53) {  
  l1 <- rowSums(l1_norm(normalized[i,],normalized_nadir))
  l1_uzaklık_nadir <- rbind(l1_uzaklık_nadir, l1)
  colnames(l1_uzaklık_nadir) <- c("l1 uzaklık")
}
# l1 norma göre nadire en uzak çözüm: 
which.max(l1_uzaklık_nadir$`l1 uzaklık`)     #sonuca göre 1. çözüm nadire en uzak l1 normuna göre.


l2_uzaklık_nadir <- data.frame()
for (i in 1:53) {  
  l2 <- rowSums(l2_norm(normalized[i,],normalized_nadir))
  l2_uzaklık_nadir <- rbind(l2_uzaklık_nadir, l2)
  colnames(l2_uzaklık_nadir) <- c("l2 uzaklık")
}
# l2 norma göre nadire en uzak çözüm: 
which.max(l2_uzaklık_nadir$`l2 uzaklık`)     #sonuca göre 1. çözüm nadire en uzak l2 normuna göre.


l_inf_uzaklık_nadir <- data.frame()
for (i in 1:53) {  
  l_inf <- l_inf_norm(normalized[i,],normalized_nadir)
  l_inf_uzaklık_nadir <- rbind(l_inf_uzaklık_nadir, l_inf)
  colnames(l_inf_uzaklık_nadir) <- c("l inf uzaklık")
}
# l infinity norma göre nadire en uzak çözüm: 
which.max(l_inf_uzaklık_nadir$`l inf uzaklık`)     #sonuca göre 7. çözüm nadire en uzak linf normuna göre.
