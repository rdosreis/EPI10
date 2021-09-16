d2j <- c(3,0,1,1,1,1)
dj <- c(3,2,1,1,1,1)
nj <- c(29,23,19,16,15,14)
n1j <- c(15,13,10,8,8,8)
n2j <- c(14,10,9,8,7,6)
         

D2j.barra <- (dj*n2j)/nj
round(D2j.barra,3)
round(d2j - D2j.barra,3)
VD2j <- (n1j * n2j * dj * (nj - dj))/((nj^2)*(nj - 1))
round(VD2j,3)

(sum(d2j - D2j.barra)^2)/sum(VD2j)
sum(D2j.barra)
