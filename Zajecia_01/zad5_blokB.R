#zad 5
przyznaj_nagrode = function(){
  x = sample(1:6, 1, TRUE)
  if(x == 6){
    return("Super bonus")
  }
  if( x >= 4 && x <= 5){
    return("Nagroda standardowa")
  }
  if( x <= 3){
    return("Brak nagrody...")
  }
}
print(przyznaj_nagrode())
  

