namespace Domain
open System.Collections.Generic

module Capital =
    
    let normalDistRandom mean stdDev = 
        let rand = new System.Random()
        let rec polarBoxMullerDist () = seq {
                let rec getRands () =
                    let u = (2.0 * rand.NextDouble()) - 1.0
                    let v = (2.0 * rand.NextDouble()) - 1.0
                    let w = u * u + v * v
                    if w >= 1.0 then
                        getRands()
                    else
                        u, v, w
                let u, v, w = getRands()
            
                let scale = System.Math.Sqrt(-2.0 * System.Math.Log(w) / w)
                let x = scale * u
                let y = scale * v
                yield mean + (x * stdDev); yield mean + (y * stdDev); yield! polarBoxMullerDist ()
            }
        polarBoxMullerDist ()



    type Quantity = int
    
    type Category = string
    
    and Resource = {
        category : Category
        needsPerHour :  Map<Resource, Quantity>
        owns : Map<Resource, Quantity>
        producesPerHour : Map<Resource, Quantity>
    }

    let rec epoch (a:Resource) : Resource = failwith "not implemented"
    
    



    let thing cat = { category = cat; needsPerHour = Map.empty; owns = Map.empty; producesPerHour = Map.empty}

    let water = thing "water"
    let iron = thing "iron"
    let air = thing "air"
    let time = thing "time"
    let carbonDioxide = thing "carbon dioxide"

    let tree =  { 
        category = "tree"; 
        needsPerHour = Map.ofList [carbonDioxide,1]
        owns = Map.empty 
        producesPerHour = Map.ofList [air,1]
    }

    let humanTtlHours = normalDistRandom 60.0 60.0 
                                |> Seq.map (fun years -> years * 365.0 * 24.0)
                                |> Seq.map int

    let human ttl =  { 
        category = "human"; 
        needsPerHour = Map.ofList [air,1; water, 1; time, 1]
        owns = Map.ofList [time,ttl]
        producesPerHour = Map.ofList [carbonDioxide,1]
    }

    let earth = { 
        category = "earth"; 
        needsPerHour = Map.empty; 
        owns = Map.ofList [tree, 100; water, 1000; iron, 20; air, 1000]
        producesPerHour = Map.empty
    }

    
