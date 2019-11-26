module Clock

let create hours minutes = 
    
    //convert negative minutes to possitive
    // -20 = 23:40 = 24*60 - 20 minutes
    let rec absMinutes m = 
        if m >= 0 then m 
        else absMinutes (m + 24 * 60)
    
    let totalM = (hours * 60 + minutes) |> absMinutes //convert to minutes
    
    let m = totalM % 60
    let h = (totalM / 60) % 24

    (h, m)
    
let add minutes (h, m) = create h (m + minutes)

let subtract minutes clock = add (0 - minutes) clock

let display (h, m) = sprintf "%02i:%02i" h m