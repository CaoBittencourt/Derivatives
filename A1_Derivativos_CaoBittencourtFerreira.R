# A1 - DERIVATIVOS

setwd('C:/Users/Sony/Documents/GitHub/Derivativos')
source('funcoes_prova.R')

pkg <- c('derivmkts')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})


# Precificação binomial ---------------------------------------------------
# Parâmetros
strike <- 95
s <- 100
dividend <- 0.015
dividend <- 0.15
r <- 0.06
u <- 1.0202
d <- 1/u
years <- 1
h <- 0.01
steps <- years/h

# Probabilidade neutra ao risco 
# q <- (exp((r - dividend)*h) - d)/(u - d)

# Árvore binomial
derivmkts::binomplot(putopt = T,
                     american = T,
                     
                     s = s,
                     k = strike,
                     r = r,
                     d = dividend,
                     tt = 1,
                     nstep = steps,
                     v = log(u)/sqrt(h),
                     
                     specifyupdn = F,
                     plotvalues = F,
                     plotarrows = F)

derivmkts::binomopt(putopt = T,
                    american = T,
                    
                    s = s,
                    k = strike,
                    r = r,
                    d = dividend,
                    tt = years,
                    nstep = steps,
                    v = log(u)/sqrt(h),
                    
                    specifyupdn = F,
                    returnparams = T,
                    returngreeks = T,
                    returntrees = T) -> binom_option

binom_option$params
binom_option$price

# Árvore do preço do ativo (primeiros 3 períodos)
binom_option$stree[1:4,1:4]
round(binom_option$stree[1:4,1:4],2)
# Árvore do preço da opção (primeiros 3 períodos)
binom_option$oppricetree[1:4,1:4]
round(binom_option$oppricetree[1:4,1:4],2)

binom_option$exertree

# Delta
binom_option$greeks
# Cálculo manual: Delta = vp(dividendos)*(cu - cd)/(su - sd)
# cu - cd
(binom_option$oppricetree[1,2] - binom_option$oppricetree[2,2]) / 
  (binom_option$stree[1,2] - binom_option$stree[2,2])*vp(taxa = dividend, tn = h) -> delta
  # su - sd
  

delta <- round(delta, 4)









# Parte I, Questão 1  ---------------------------------------------------
# Parâmetros
strike <- 95
s <- 100
dividend <- 0.015
dividend <- 0.15
r <- 0.06
u <- 1.0202
d <- 1/u
years <- 1
h <- 0.01
steps <- years/h

# Probabilidade neutra ao risco 
# q <- (exp((r - dividend)*h) - d)/(u - d)

# Árvore binomial
derivmkts::binomplot(putopt = T,
                     american = T,
                     
                     s = s,
                     k = strike,
                     r = r,
                     d = 0.015,
                     tt = 1,
                     nstep = steps,
                     v = log(u)/sqrt(h),
                     
                     specifyupdn = F,
                     plotvalues = F,
                     plotarrows = F)

derivmkts::binomplot(putopt = T,
                     american = T,
                     
                     s = s,
                     k = strike,
                     r = r,
                     d = 0.15,
                     tt = 1,
                     nstep = steps,
                     v = log(u)/sqrt(h),
                     
                     specifyupdn = F,
                     plotvalues = F,
                     plotarrows = F)


derivmkts::binomopt(putopt = T,
                    american = T,
                    
                    s = s,
                    k = strike,
                    r = r,
                    d = 0.015,
                    tt = years,
                    nstep = steps,
                    v = log(u)/sqrt(h),
                    
                    specifyupdn = F,
                    returnparams = T,
                    returngreeks = T,
                    returntrees = T) -> binom_option_dy1


derivmkts::binomopt(putopt = T,
                    american = T,
                    
                    s = s,
                    k = strike,
                    r = r,
                    d = 0.15,
                    tt = years,
                    nstep = steps,
                    v = log(u)/sqrt(h),
                    
                    specifyupdn = F,
                    returnparams = T,
                    returngreeks = T,
                    returntrees = T) -> binom_option_dy2


sum(binom_option_dy1$exertree)
sum(binom_option_dy2$exertree)



# Parte II, Questão 3 ---------------------------------------------------
# Parâmetros
strike3 <- 35
s3 <- 40
dividend3 <- 5
r3 <- 0.05
v3 <- 0.20
years3 <- 1
steps3 <- 3
h3 <- years/steps3
u3 <- exp(v3*sqrt(h3))
d3 <- 1/u3

# Ajuste do preço pelo dividendo futuro
D <- vp(tn = 9/12, taxa = r3)*dividend3
s3_d <- s3 - D

# Probabilidade neutra ao risco 
# q3 <- (exp((r3 - dividend3)*h3) - d3)/(u3 - d3)

# Árvore binomial
derivmkts::binomplot(putopt = F,
                     american = T,
                     
                     s = s3_d,
                     k = strike3,
                     r = r3,
                     d = 0,
                     tt = years3,
                     nstep = steps3,
                     # nstep = 100,
                     v = v3,
                     # up = u3,
                     # dn = d3, 
                     
                     specifyupdn = F,
                     plotvalues = F,
                     plotarrows = F)

derivmkts::binomopt(putopt = F,
                    american = T,
                    
                    s = s3_d,
                    k = strike3,
                    r = r3,
                    d = 0,
                    tt = years3,
                    nstep = steps3,
                    # nstep = 100,
                    v = v3,
                    # up = u3,
                    # dn = d3, 
                    
                    specifyupdn = F,
                    returnparams = T,
                    returngreeks = T,
                    returntrees = T) -> binom_option3

binom_option3$params
binom_option3$price

# Árvore do preço do ativo (primeiros 3 períodos)
binom_option3$stree[1:4,1:4]
round(binom_option3$stree[1:4,1:4],2)
# Árvore do preço da opção (primeiros 3 períodos)
binom_option3$oppricetree[1:4,1:4]
round(binom_option3$oppricetree[1:4,1:4],2)

binom_option$exertree

# Delta
binom_option$greeks
# Cálculo manual: Delta = vp(dividendos)*(cu - cd)/(su - sd)
# cu - cd
(binom_option$oppricetree[1,2] - binom_option$oppricetree[2,2]) / 
  (binom_option$stree[1,2] - binom_option$stree[2,2])*vp(taxa = dividend, tn = h) -> delta
# su - sd


delta <- round(delta, 4)












# Parte II, Questão 2 -----------------------------------------------------
# vf(taxa = .03, tn = 3, divisor_conversao = 12, val = 1.25) < 1.29
vp(taxa = 0.03, tn  = 3, divisor_conversao = 12)