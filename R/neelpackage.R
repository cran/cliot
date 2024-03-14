kidney_risk_num <- function(egfr, sex, acr, age, alb, phos, bicarb, calc, units = ""){
  x <- 0
  if(units == "SI"){
    alb = as.numeric(alb)
    alb <- alb / 10
    phos = as.numeric(phos)
    phos <- 3.0974 * phos
    calc = as.numeric(calc)
    calc <- 4.0078 * calc
  }
  egfr = as.numeric(egfr)
  if(!is.na(egfr) & !is.na(sex)& !is.na(acr)& !is.na(age)& !is.na(alb)& !is.na(phos)& !is.na(bicarb)& !is.na(calc)){
    if(egfr >=10 & egfr <=14){
      x <- x -35
    }
    else if(egfr >=15 & egfr <=19){
      x <- x -30
    }
    else if(egfr >=20 & egfr <=24){
      x <- x -25
    }
    else if(egfr >=25 & egfr <=29){
      x <- x -20
    }
    else if(egfr >=30 & egfr <=34){
      x <- x -15
    }
    else if(egfr >=35 & egfr <=39){
      x <- x -10
    }
    else if(egfr >=40 & egfr <=44){
      x <- x -5
    }
    else if(egfr >=50 & egfr <=54){
      x <- x +5
    }
    else if(egfr >=55 & egfr <=59){
      x <- x +10
    }
    sex = as.character(sex)
    if(sex == 'M'){
      x <- x -2
    }
    acr = as.numeric(acr)
    if(acr >=30 & acr <=300){
      x <- x -14
    }
    else if(acr >300){
      x <- x -22
    }
    age = as.numeric(age)
    if(age <30){
      x <- x -4
    }
    else if(age >=30 & age <= 39){
      x <- x -2
    }
    else if(age >=50 & age <= 59){
      x <- x +2
    }
    else if(age >=60 & age <= 69){
      x <- x +4
    }
    else if(age >=70 & age <= 79){
      x <- x +6
    }
    else if(age >=80 & age <= 89){
      x <- x +8
    }
    else if(age >=90){
      x <- x +10
    }
    alb = as.numeric(alb)
    if(alb <=2.5){
      x <- x -5
    }
    else if(alb >=3.1 & alb <= 3.5){
      x <- x +2
    }
    else if(alb >=3.6){
      x <- x +4
    }
    phos = as.numeric(phos)
    if(phos <3.5){
      x <- x +3
    }
    else if(phos >=4.6 & phos <=5.5 ){
      x <- x -3
    }
    else if(phos >=5.5){
      x <- x -5
    }
    bicarb = as.numeric(bicarb)
    if(bicarb < 18){
      x <- x -7
    }
    else if(bicarb >=18 & bicarb <= 22){
      x <- x -4
    }
    else if(bicarb >=23 & bicarb <= 25){
      x <- x -1
    }
    calc = as.numeric(calc)
    if(alb <=8.5){
      x <- x -3
    }
    else if(alb >=9.6){
      x <- x +2
    }
    return(x)
  }
  else
    return(NA)
}

kid_risk_perc<-function(egfr, sex, acr, age, alb, phos, bicarb, calc, units = ""){
  x<- kidney_risk_num(egfr, sex, acr, age, alb, phos, bicarb, calc, units)
  if(!is.na(x)){
    x <- as.numeric(x)
    y <- 0
    if(x < -41)
      y = 90
    else if(x == -41)
      y = 89
    else if(x == -40)
      y = 86.9
    else if(x == -39)
      y = 84.1
    else if(x == -38)
      y = 81
    else if(x == -37)
      y = 77.8 #typo in original source?
    else if(x == -36)
      y = 74.4
    else if(x== -35)
      y = 70.9
    else if(x == -34)
      y = 67.3
    else if(x == -33)
      y = 63.6
    else if(x == -32)
      y = 59.9
    else if(x== -31)
      y = 56.3
    else if(x == -30)
      y = 52.8
    else if(x == -29)
      y = 49.3
    else if(x == -28)
      y = 45.9
    else if(x == -27)
      y = 42.7
    else if(x == -26)
      y = 39.6
    else if(x == -25)
      y = 36.6
    else if(x == -24)
      y = 33.8
    else if(x == -23)
      y = 31.2
    else if(x == -22)
      y = 28.7
    else if(x == -21)
      y = 26.4
    else if(x == -20)
      y = 24.2
    else if(x == -19)
      y = 22.2
    else if(x == -18)
      y = 20.3
    else if(x == -17)
      y = 18.6
    else if(x == -16)
      y = 17.0
    else if(x == -15)
      y = 15.5
    else if(x == -14)
      y = 14.1
    else if(x == -13)
      y = 12.9
    else if(x == -12)
      y = 11.7
    else if(x == -11)
      y = 10.7
    else if(x == -10)
      y = 9.7
    else if(x == -9)
      y = 8.8
    else if(x == -8)
      y = 8.0
    else if(x == -7)
      y = 7.3
    else if(x == -6)
      y = 6.6
    else if(x == -5)
      y = 6.0
    else if(x == -4)
      y = 5.5
    else if(x >= -3)
      y = 5.0
    return(y)
  }
  else
    return(NA)
}

meld_xi<-function(sbill, screat){
  if(!is.na(sbill)&!is.na(screat)){
    sbill <- as.numeric(sbill)
    screat <- as.numeric(screat)
    x <- 5.112 + log(sbill) + 11.76 * log(screat) + 9.44
    return(x)
  }
  else
    return(NA)
}

fib4<-function(age, ast, alt, plt){
  if(!is.na(age) & !is.na(ast) & !is.na(alt)&!is.na(plt)){
    age <- as.numeric(age)
    ast <- as.numeric(ast)
    alt <- as.numeric(alt)
    plt <- as.numeric(plt)
    x <- (age * ast) / (plt * sqrt(alt))
    return(x)
  }
  else
    return(NA)
}

apri<-function(ast, plt){
  if(!is.na(ast)&!is.na(plt)){
    ast <- as.numeric(ast)
    plt <- as.numeric(plt)
    x <- (ast / 40) / (plt * 100)
    return(x)
  }
  else
    return(NA)
}

albi<-function(sbill, salb, units = ""){
  if(units == "SI"){
    sbill <- as.numeric(sbill)
    sbill <- sbill /17.1
    salb <- as.numeric(salb)
    salb <- salb/ 10
  }
  if(!is.na(sbill)&!is.na(salb)){
    sbill <- as.numeric(sbill)
    salb <- as.numeric(salb)
    x <- (log10(sbill * 17.1) *0.66)  + (-0.085 * (salb * 10))
    return(x)
  }
  else
    return(NA)
}

fli<-function(trigly, bmi, waist, ggt, units = ""){
  if(units == "SI"){
    trigly <- as.numeric(trigly)
  trigly <- trigly * 88.4
  }
  if(!is.na(trigly) & !is.na(bmi) & !is.na(waist)&!is.na(ggt)){
    trigly <- as.numeric(trigly)
    bmi <- as.numeric(bmi)
    ggt <- as.numeric(ggt)
    waist <- as.numeric(waist)
    y <-  0.953 * log(trigly) + 0.139 * bmi + 0.718 * log(ggt) + 0.053 * waist - 15.745
    x <- (exp(1)^y)/(1+exp(1)^y) * 100
    return(x)
  }
  else
    return(NA)
}

eAG <- function(a1c){
  a1c = as.numeric(a1c)
  if(!is.na(a1c))
    return((28.7 * a1c) - 46.7)
  else
    return(NA)
}

homair <- function(insulin,glucose, units = ""){
  if(units == "SI"){
    insulin = as.numeric(insulin)
    insulin <- insulin / 6
    glucose = as.numeric(glucose)
    glucose <- glucose * 18
  }
  insulin = as.numeric(insulin)
  glucose = as.numeric(glucose)
  if(!is.na(insulin) & !is.na(glucose))
    return((insulin * glucose)/405)
  else
    return(NA)
}

bard_score <- function(bmi, ast, alt, diabetes){
  bmi <- as.numeric(bmi)
  ast <- as.numeric(ast)
  alt <- as.numeric(alt)
  diabetes <- as.numeric(diabetes)
  if(is.na(diabetes) | is.na(bmi) | is.na(ast) | is.na(alt)){
    return(NA)
  }
  y <- ast/alt
  if(is.na(y)){
    return(NA)
  }
  x<- 0
  if(bmi >= 28){
    x <- x + 1
  }
  if((y) >= 0.8){
    x <- x + 2
  }
  if(diabetes == 1){
    x <- x + 1
  }
  return(x)
}

bard_score_interpret <- function(bmi, ast, alt, diabetes){
  x<- bard_score(bmi, ast, alt, diabetes)
  if(is.na(x))
    return(NA)
  if(x >1){
    y<- "high"
  }
  else{
    y<- "low"
  }
  return(y)
}

ast_alt_ratio_interpret <- function(ast, alt){
  ast <- as.numeric(ast)
  alt <- as.numeric(alt)
  if(is.na(ast) | is.na(alt)){
    return(NA)
  }
  y <- ast/alt
  if(is.na(y)){
    return(NA)
  }
  x<- 0
  if((y) >= 0.8){
    x <- 1
  }
  return(x)
}

nafld_score <- function(age, bmi, diabetes, ast, alt, platelet, albumin, units = ""){
  age = as.numeric(age)
  bmi = as.numeric(bmi)
  diabetes = as.numeric(diabetes)
  ast = as.numeric(ast)
  alt = as.numeric(alt)
  platelet = as.numeric(platelet)
  albumin = as.numeric(albumin)
  if(units == "SI"){
  albumin = albumin / 10
  }
  if(is.na(age) | is.na(bmi)| is.na(diabetes)| is.na(ast)| is.na(alt)| is.na(platelet)| is.na(albumin)){
    return(NA)
  }
  x<- -1.675 + (0.037 * age) + (0.094 * bmi) + (1.13 * diabetes) + (0.99 * (ast/alt)) - (0.013 * platelet) - (0.66 * albumin)
  return(x)
}

rfactor <- function(alt, alp){
  alt = as.numeric(alt)
  alp = as.numeric(alp)
  if(is.na(alp) | is.na(alt)){
    return(NA)
  }
  x<- (alt/40)/(alp/120)
  return(x)
}

rfactor_interpret <- function(alt, alp){
  x<- rfactor(alt, alp)
  x = as.numeric(x)
  if(is.na(x)){
    return(NA)
  }
  if(x <2){
    return("cholestatic")
  }
  else if(x>5){
    return("hepatocellular")
  }
  else{
    return("indeterminate")
  }
}

aha_stroke <- function(bp, afib, sugar, bmi, diet, chol, dm, physical, hx, smoke){
  bp <- as.numeric(bp)
  afib <- as.numeric(afib)
  sugar <- as.numeric(sugar)
  bmi <- as.numeric(bmi)
  diet <- as.numeric(diet)
  chol <- as.numeric(chol)
  dm <- as.numeric(dm)
  physical <- as.numeric(physical)
  hx <- as.numeric(hx)
  smoke <- as.numeric(smoke)
  if(is.na(bp)){
    bp <- 1
  }
  if(is.na(afib)){
    afib <- 1
  }
  if(is.na(sugar)){
    sugar <- 1
  }
  if(is.na(bmi)){
    bmi <- 1
  }
  if(is.na(diet)){
    diet <- 1
  }
  if(is.na(chol)){
    chol <- 1
  }
  if(is.na(dm)){
    dm <- 1
  }
  if(is.na(physical)){
    physical <- 1
  }
  if(is.na(hx)){
    hx <- 1
  }
  if(is.na(smoke)){
    smoke <- 1
  }
  x <- 0
  if(bp == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(afib == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(sugar == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(bmi == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(diet == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(chol == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(dm == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(physical == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(hx == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  if(smoke == 1){
    x = x + 1
  }
  else{
    x = x - 1
  }
  return(x)
}
