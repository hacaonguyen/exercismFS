module SpaceAge

// TODO: define the Planet type
type Planet =
    | Earth //= 100000000UL //1.0
    | Mercury //= 24084670UL //0.2408467
    | Venus //= 61519726UL //0.61519726
    | Mars //= 188081580UL //1.8808158 
    | Jupiter //= 1186261500UL //11.862615
    | Saturn //= 2944749800UL //29.447498
    | Uranus //= 8401684600UL//84.016846
    | Neptune //= 16479132000UL //164.79132

let age (planet: Planet) (seconds: int64): float =     
    let factor = function 
    | Earth -> 1.0
    | Mercury -> 0.2408467 
    | Venus -> 0.61519726
    | Mars -> 1.8808158
    | Jupiter -> 11.862615 
    | Saturn -> 29.447498
    | Uranus -> 84.016846 
    | Neptune -> 164.79132    
    float (seconds) / float (31557600) / (factor planet)