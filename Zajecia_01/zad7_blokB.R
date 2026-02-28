podatek_Belki = function(przychod, koszt, typ_aktywa){
  zysk = przychod - koszt
  if(zysk <= 0){
    return(0)
  }
  else if(typ_aktywa == "akcje" || typ_aktywa == "obligacje"){
    podatek = 0.19
    zysk = zysk*(1- podatek)
    return(zysk)
  }
  else if(typ_aktywa == "kryptowaluty"){
    if(zysk <= 85528){
      podatek = 0.18
    } 
    else{
      podatek = 0.32
    }
    zysk = zysk*(1- podatek)
    return(zysk)
  }
  return("Nieznany typ aktywa")
}
print(podatek_Belki(15000, 10000, "akcje"))
print(podatek_Belki(8000, 10000, "akcje"))
print(podatek_Belki(150000, 50000, "kryptowaluty"))
print(podatek_Belki(1500, 1000, "lokata"))
