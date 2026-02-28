kalkulator = function(a,b, operacja){
  if(operacja == "*"){
    wynik = a * b 
  }
  else if(operacja == "+"){
     wynik = a + b
  } 
  else if(operacja == "-"){
    wynik = a - b
  }
  else if(operacja == "/" && b == 0){
    wynik = "Nie mozna dzielic przez 0"
  }
  else if(operacja == "/" && b != 0){
    wynik = a/b
  }
  else{
    wynik = "nieznana operacja"
  }
  return(wynik)
}
print( kalkulator(1,0,"/") )
  