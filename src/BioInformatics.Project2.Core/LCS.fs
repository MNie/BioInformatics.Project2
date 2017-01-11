namespace BioInformatics.Project2.Core.LCS
open BioInformatics.Project2.Core.Sequence
open System

type LCS() =
    let rec count (first: string) (second: string) (firstIndex: int) (secondIndex: int) =
        if firstIndex = 0 || secondIndex = 0 then 0
        elif first.[firstIndex - 1] = second.[secondIndex - 1] then 1 + (count first second (firstIndex - 1) (secondIndex - 1))
        else Math.Max( (count first second firstIndex (secondIndex - 1)), (count first second (firstIndex - 1) secondIndex))
    member this.Count(first: Sequence, second: Sequence) =
        count first.Data second.Data first.Data.Length second.Data.Length       