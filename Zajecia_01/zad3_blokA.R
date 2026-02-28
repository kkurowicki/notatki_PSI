kostka = function(n){
  wyniki = sample(1:6,size = n, replace = TRUE)
  return (wyniki)
}
#print(kostka(10))
#print(kostka(100))
print(table(kostka(10000)))


      