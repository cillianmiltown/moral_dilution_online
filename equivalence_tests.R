library(TOSTER)

d <- .15

equivalence_means_test <- function(k1,k2,d){
  TOSTtwo(m1=mean(k1), m2=mean(k2),
          sd1=sd(k1), sd2=sd(k2),
          n1=length(k1), n2=length(k2),
          low_eqbound_d=-d, high_eqbound_d=d, alpha = 0.05)
}



g1 <- x$M1[which(x$condition=="non-diagnostic")]
g2 <- x$M1[which(x$condition=="diagnostic")]

TOSTER::powerTOSTpaired(g1, g2, low_eqbound_dz = -d, high_eqbound_dz = d)

help(power_t_TOST)

TOSTER::powerTOSTpaired(.05,.8, low_eqbound_dz = -d, high_eqbound_dz = d)


equivalence_means_test(g1,g2,d)
