install.packages("pwr")
library("pwr")

# Cohen's d of 0.2, 0.5, 0.8 are considered small, medium and large effect sizes respectively.
# Cohen's f of 0.1,

# d = .2    f = .1    f2 = .01
# d = .5    f = .25   f2 = .0625
# d = .8    f = .4    f2 = .16



pwr::pwr.f2.test(u = 1, f2 = .01,   sig.level =  .05, power = .8)
pwr::pwr.f2.test(u = 1, f2 = .0625, sig.level =  .05, power = .8)
pwr::pwr.f2.test(u = 1, f2 = .16,   sig.level =  .05, power = .8)


pwr::pwr.anova.test(k=2,f=.1,  sig.level = .05, power=.8)
pwr::pwr.anova.test(k=4,f=.25, sig.level = .05, power=.8)
pwr::pwr.anova.test(k=4,f=.4,  sig.level = .05, power=.8)





# f2 = .02
# f2 = .15
# f2 = .35


# small effect (f 2 = .02) a minimum sample of N 400 participants is required. Detecting a medium effect (f 2 = .15) requires a sample of N = 54, and detecting a large effect (f 2 = .35) requires a sample of N = 24.
