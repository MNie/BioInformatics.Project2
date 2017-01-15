namespace BioInformatics.Project2.Core.SequenceGapAligner
open BioInformatics.Project2.Core.Sequence
open System.Collections.Generic

type SequenceGapAligner(theSame: float, different: float, gapPenalty: int -> float) =
    member val TheSame = theSame with get
    member val Different = different with get
    member val GapPenalty = gapPenalty with get

    member this.Align(first: Sequence, second: Sequence) =
        let maxIter = if first.Data.Length > second.Data.Length then first.Data.Length else second.Data.Length
        let allAreThatLong i =
            first.Data.Length > i && second.Data.Length > i
        let valuesAreTheSame index =
            first.Data.[index] = second.Data.[index]
        
        let whatResult =
            [0..maxIter]
            |> Seq.mapi(fun i _ -> 
                if allAreThatLong i then
                    if first.Data.[i] = '-' then 1
                    elif second.Data.[i] = '-' then 2
                    elif valuesAreTheSame i then 0
                    else -1
                else -1
                )
            |> Seq.toArray

        let calculateTypeAndOccurences =
            let result = new List<KeyValuePair<int, int>>()
            let mutable counter = 1
            for i = 1 to maxIter do
                if whatResult.[i-1] = whatResult.[i] then
                    counter <- counter + 1
                else
                    result.Add(KeyValuePair(whatResult.[i-1], counter))
                    counter <- 1
            result

        calculateTypeAndOccurences
        |> Seq.map(fun x -> 
                if x.Key = -1 then (x.Value |> float) * this.Different
                elif x.Key = 0 then (x.Value |> float) * this.TheSame
                else -(this.GapPenalty (x.Value))
            )
        |> Seq.sum
