module Clock

let create hours minutes = 
    
    //convert negative minutes to possitive
    // -20 = 23:40 = 24*60 - 20 minutes
    let rec absMinutes = function
        | m when m >= 0 -> m
        | m -> absMinutes (m + 24 * 60)

    let totalM = (hours * 60 + minutes) |> absMinutes //convert to minutes
    
    let m = totalM % 60
    let h = (totalM / 60) % 24

    (h, m)
    
let add minutes clock = 
    let (h, m) = clock
    create h (m + minutes)

let subtract minutes clock = 
    let (h, m) = clock
    create h (m - minutes)

let display clock = 
    let (h, m) = clock
    sprintf "%02i:%02i" h m