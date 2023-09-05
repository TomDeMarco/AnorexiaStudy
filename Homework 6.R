
Anorexia = read.table('Anorexia.txt', header = TRUE)


Anorexia
#Exercise 1)
#HO mu = 90, ha: mu< 90 (after)
t.test(Anorexia$After, alternative = 'less', mu = 90, conf.level = .95)

#We reject the null hypothesis because the p value is less then the alpha level meaning that the likelihood of the true mean being 90 is very low.

#Exercise 2
CB = Anorexia[Anorexia$Therapy == 'Cog/Behav',]
t.test(CB$Y, alternative = 'greater',mu=0)

# for this test the p-test is less then alpha, signaling that the true mean is greater then 0, making rejecting the null hypothesis the most probable option

#Exercise 3

Gains.Fam <- Anorexia$Y[ Anorexia$Therapy == "Family" ]
Gains.Cont <- Anorexia$Y[ Anorexia$Therapy == "Control" ]

t.test(Gains.Fam, Gains.Cont, alternative="greater")

#We can conclude that the family treatment is effective because the null hypothesis that the means is equal has less then .05(alpha) percent chance. The therapy does change the mean.

#Exercise 4)

perm_fun <- function(x, nA, nB)
{
  n <- nA + nB
  idx_b <- sample(1:n, nB)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

Y1 = Anorexia$Y[ Anorexia$Therapy != "Cog/Behav"]
Gains.Fam <- Anorexia$Y[ Anorexia$Therapy == "Family" ]
Gains.Cont <- Anorexia$Y[ Anorexia$Therapy == "Control" ]
nA = length(Gains.Fam)
nB = length(Gains.Cont)


perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Y1, nA, nB)
}

# Calculate the "p-value"
mean_diff = mean(Gains.Fam)-mean(Gains.Cont)
mean(perm_diffs > mean_diff)

#Question 5)

  
#a)

qqnorm(Gains.Fam, main = "Family Group")
qqline(Gains.Fam)

qqnorm(Gains.Cont, main = "Control")
qqline(Gains.Cont)

#After creating a qq plot for both the control and Family group, they both to be relatively normal with a few points deviating from the straight diagonal line, but for the most part they retain a normal distribution 

#b)
var.test(Gains.Cont, Gains.Fam)
#the p test returns a value greater then alpha so we conclude there is not enough evidence to reject the null hypothesis 

#c)
t.test(Gains.Cont, Gains.Fam, var.equal = TRUE)
#The null hypothesis assumes there is no difference between the control and family therapy mean. The p test of the null hypothesis had a probability less then alpha , meaning that we have strong evidence to reject the null hypothesis and conclude family therapy helps.

