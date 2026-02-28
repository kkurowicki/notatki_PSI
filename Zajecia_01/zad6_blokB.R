#zad 6.
  ocena_kredytowa = function(dochod, zadluzenie){
    proc = zadluzenie/dochod * 100
    if(proc < 30){
      return("KREDYT PRZYZNANY")
    }
    else if(proc >= 30 && proc <= 50){
      return("WYMAGA WERYFIKACJI")
    }
    else if(proc > 50){
      return("KREDYT ODRZUCONY")
    }
  }
  print(ocena_kredytowa(10000, 2000))
  print(ocena_kredytowa(10000, 4000))
  print(ocena_kredytowa(10000, 6000))  
  