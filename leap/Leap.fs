module Leap

let leapYear (year: int): bool = 
    let div n = (year % n = 0)
    (div 400) || ((div 4) && not (div 100))
    //(year % 400 = 0) || ((year % 100 <> 0) && (year % 4 = 0))  
