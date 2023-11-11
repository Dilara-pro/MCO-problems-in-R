
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

efficient1 <- data.frame(data[ranks[[1]],])


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

# İlk etkinlik seviyesindeki çözümlerin kriter değerleri prosedür 2 kullanılarak normalize edilmiştir. Entropi için normalize edilirken logaritma alındığı için min değerden 0.1 kadarlık daha düşüğü, max değerden ise 0.1 kadarlık daha büyüğü alınarak hesaplanır. 

prosedur2_max <- function (x){
  (x-(min(x)-0.1))/(max(x)-min(x))
}
prosedur2_min <- function (x){
  ((max(x)+0.1)-x)/(max(x)-min(x))
}
normalized <- data.frame("z1norm" = prosedur2_max(efficient1$z1.max.),"z2norm" = prosedur2_max(efficient1$z2.max.),
                         "z3norm" = prosedur2_min(efficient1$z3.min.),"z4norm" = prosedur2_min(efficient1$z4.min.))
normalized_ideal <- data.frame("z1*norm" = 1, "z2*norm" = 1,"z3*norm" = 1,"z4*norm" = 1)
normalized_nadir <- data.frame("z1nad_norm" = 0, "z2nad_norm" = 0,"z3nad_norm" = 0,"z4nad_norm" = 0)

# normalize edilmiş veri setinin kriterlerinin entropi ağırlıkları aşağıda hesaplanmıştır.

a <- data.frame(log(normalized[,]))

Entropi_1 <- -(log(53)^(-1))*(sum(normalized[,1]*a[,1]))
Entropi_2 <- -(log(53)^(-1))*(sum(normalized[,2]*a[,2]))
Entropi_3 <- -(log(53)^(-1))*(sum(normalized[,3]*a[,3]))
Entropi_4 <- -(log(53)^(-1))*(sum(normalized[,4]*a[,4]))

Toplam <- Entropi_1 + Entropi_2 + Entropi_3 + Entropi_4

Agırlık_1 <- (1-Entropi_1)/(4-Toplam)
Agırlık_2 <- (1-Entropi_2)/(4-Toplam)
Agırlık_3 <- (1-Entropi_3)/(4-Toplam)
Agırlık_4 <- (1-Entropi_4)/(4-Toplam)

sum(Agırlık_1,Agırlık_2,Agırlık_3,Agırlık_4)   #Ağırlık toplamları 1 olduğundan zaten normalize haldedir. Buradan kontrolü yapılır.

#Agırlıklı toplam yöntemi ile en iyiden en kötüye altenatifler sıralanmıştır. 
Agırlıklı_toplam <- data.frame(Agırlık_1*normalized[,1],Agırlık_2*normalized[,2],
                               Agırlık_3*normalized[,3],Agırlık_4*normalized[,4])

R_values <-rowSums(Agırlıklı_toplam)
Best_to_worst_solutions <- data.frame(sort(R_values),order(R_values))

# Critic yönteme göre kriter ağılıkları hesaplanır.

r_jk <- cor(normalized)    # kriter çiftleri arasında korelasyon katsayısı
corr_values <- rowSums((1-r_jk))

Weights <- data.frame("c1" = var(normalized[,1])*corr_values[1],
                      "c2" = var(normalized[,2])*corr_values[2],
                      "c3" = var(normalized[,3])*corr_values[3],
                      "c4" = var(normalized[,4])*corr_values[4])
row.names(Weights) <- c("W_j")
rowSums(Weights)        # row sum larına baktığımızda toplamlarının 1 etmediğini görebiliriz. Bu nedenle normalize edilmelidir.

Normalized_Weights <- Weights/rowSums(Weights)
rowSums(Normalized_Weights)

Agırlıklı_toplam2 <- data.frame(Normalized_Weights[1,1]*normalized[,1],Normalized_Weights[1,2]*normalized[,2],
                                Normalized_Weights[1,3]*normalized[,3],Normalized_Weights[1,4]*normalized[,4])
R_values2 <- rowSums(Agırlıklı_toplam2)
Best_to_worst_solutions_critic <- data.frame(sort(R_values2),order(R_values2))

# Entropi ile critic metodları arasındaki sıralamaların ilişkisi spearman rank corr ile ölçülebilir. 
Spearman_corr <- cor(Best_to_worst_solutions[,2], Best_to_worst_solutions_critic[,2], method = "spearman")
Spearman_corr

# Kriterlerin önem sırası en önemliden en az önemliye doğru 1-2-3-4 şeklindeyken çözümlerin Rank Sum ağırlıkları hesaplanmıştır.


W_RS_z1 <- (4-1+1) / ((4-1+1)+(4-2+1)+(4-3+1)+(4-4+1))
W_RS_z2 <- (4-2+1) / ((4-1+1)+(4-2+1)+(4-3+1)+(4-4+1))
W_RS_z3 <- (4-3+1) / ((4-1+1)+(4-2+1)+(4-3+1)+(4-4+1))
W_RS_z4 <- (4-4+1) / ((4-1+1)+(4-2+1)+(4-3+1)+(4-4+1))
sum(W_RS_z1,W_RS_z2,W_RS_z3,W_RS_z4)

Agırlıklı_toplam3 <- data.frame(W_RS_z1*normalized[,1],W_RS_z2*normalized[,2],
                                W_RS_z3*normalized[,3],W_RS_z4*normalized[,4])

R_values3 <- rowSums(Agırlıklı_toplam3)
Best_to_worst_solutions_RS <- data.frame(sort(R_values3),order(R_values3))


# Kriterlerin önem sırası en önemliden en az önemliye doğru 1-2-3-4 şeklindeyken çözümleri Rank Order Centroid ağırlıkları hesaplanmıştır. 

ROC_z1 <- (1/4)* (1/1 + 1/2 + 1/3 + 1/4)
ROC_z2 <- (1/4)* (1/2 + 1/3 + 1/4)
ROC_z3 <- (1/4)* (1/3 + 1/4)
ROC_z4 <- (1/4)* (1/4)
sum(ROC_z1, ROC_z2, ROC_z3, ROC_z4)

Agırlıklı_toplam4 <- data.frame(ROC_z1*normalized[,1],ROC_z2*normalized[,2],
                                ROC_z3*normalized[,3],ROC_z4*normalized[,4])

R_values4 <- rowSums(Agırlıklı_toplam4)
Best_to_worst_solutions_ROC <- data.frame(sort(R_values4),order(R_values4))

# RS ile ROC metodları arasındaki sıralamaların ilişkisi spearman rank corr ile ölçülebilir. 
Spearman_corr <- cor(Best_to_worst_solutions_RS[,2], Best_to_worst_solutions_ROC[,2], method = "spearman")
Spearman_corr






