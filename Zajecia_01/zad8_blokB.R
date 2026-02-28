typ_gospodarstwa = function(dochod, wydatki,dzieci, miasto){
  if(wydatki > dochod){
    return("Trudna sytuacja")
  }
  else if(wydatki <= dochod && dzieci >= 2){
    return("Przeciętna sytuacja")
  }
  else if(wydatki <= 0.8 * dochod && miasto == "małe miasto"){
    return("Stabilna sytuacja")
  }
  else{
    return("nie wiadomo jaka sytuacja")
  }
}
print(typ_gospodarstwa(4000, 4500, 1, "duże miasto"))
print(typ_gospodarstwa(5000, 4800, 2, "duże miasto"))
print(typ_gospodarstwa(5000, 3500, 0, "małe miasto"))