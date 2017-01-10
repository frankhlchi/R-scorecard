coe = (fit3$coefficients)
p <- 20/log(2)
q <- 600-20*log(2.5)/log(2)

base <- q + p*as.numeric(coe[1])

AccountBalanceSCORE = p*as.numeric(coe[2])*AccountBalancewoe$woe[1]

p*as.numeric(coe[2])*AccountBalancewoe$WOE[1]*-1
p*as.numeric(coe[2])*AccountBalancewoe$WOE[2]*-1
p*as.numeric(coe[2])*AccountBalancewoe$WOE[3]*-1
p*as.numeric(coe[2])*AccountBalancewoe$WOE[4]*-1

p*as.numeric(coe[3])*Durationwoe$WOE[1]*-1
p*as.numeric(coe[3])*Durationwoe$WOE[2]*-1
p*as.numeric(coe[3])*Durationwoe$WOE[3]*-1

p*as.numeric(coe[4])*PaymentStatusofPreviousCreditwoe$WOE[1]*-1
p*as.numeric(coe[4])*PaymentStatusofPreviousCreditwoe$WOE[2]*-1
p*as.numeric(coe[4])*PaymentStatusofPreviousCreditwoe$WOE[3]*-1
p*as.numeric(coe[4])*PaymentStatusofPreviousCreditwoe$WOE[4]*-1
p*as.numeric(coe[4])*PaymentStatusofPreviousCreditwoe$WOE[5]*-1

for(i in 1:10){
  print(p*as.numeric(coe[5])*Purposewoe$WOE[i])*-1
}

p*as.numeric(coe[6])*CreditAmountwoe$WOE[1]*-1
p*as.numeric(coe[6])*CreditAmountwoe$WOE[2]*-1

p*as.numeric(coe[7])*ValueSavingswoe$WOE[1]*-1
p*as.numeric(coe[7])*ValueSavingswoe$WOE[2]*-1
p*as.numeric(coe[7])*ValueSavingswoe$WOE[3]*-1
p*as.numeric(coe[7])*ValueSavingswoe$WOE[4]*-1

p*as.numeric(coe[8])*Lengthofcurrentemploymentwoe$WOE[1]*-1
p*as.numeric(coe[8])*Lengthofcurrentemploymentwoe$WOE[2]*-1
p*as.numeric(coe[8])*Lengthofcurrentemploymentwoe$WOE[3]*-1
p*as.numeric(coe[8])*Lengthofcurrentemploymentwoe$WOE[4]*-1

p*as.numeric(coe[9])*Agewoe$WOE[1]*-1
p*as.numeric(coe[9])*Agewoe$WOE[2]*-1

p*as.numeric(coe[10])*ForeignWorkerwoe$WOE[1]*-1
p*as.numeric(coe[10])*ForeignWorkerwoe$WOE[2]*-1
