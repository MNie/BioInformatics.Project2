namespace BioInformatics.Project2.Core.SequenceAligner
open BioInformatics.Project2.Core.Sequence
open System
type SequenceAligner(insert, delete, substract) =
    member val Insert = insert with get
    member val Delete = delete with get
    member val Substract = substract with get

    member this.Align(first: Sequence, second: Sequence) =
        let maxJ = second.Data.Length
        let maxI = first.Data.Length
        let score = Array2D.create maxI maxJ 0.0

        let calculateFirstColumn (index: int): double =
            score.[0, index - 1] + (this.Insert second.Data.[index]) 

        let calculateSub indexI indexJ =
            let sub = this.Substract(first.Data.[indexI], second.Data.[indexJ])
            score.[indexI - 1, indexJ - 1] + sub

        let calculateDel indexI indexJ =
            let del = this.Delete(first.Data.[indexI])
            score.[indexI - 1, indexJ] + del

        let calculateIns indexI indexJ =
            let ins = this.Insert(second.Data.[indexJ])
            score.[indexI, indexJ - 1] + ins

        let calculateSingleScore indexI indexJ: double =
            let sub = calculateSub indexI indexJ
            let del = calculateDel indexI indexJ
            let ins = calculateIns indexI indexJ
            Math.Max(sub, Math.Max(del, ins))

        for i = 1 to maxI - 1 do
            score.[0, i] <- (calculateFirstColumn i)
        for i = 1 to maxI - 1 do
            score.[i, 0] <- score.[i-1, 0] + this.Delete(first.Data.[i])
            for j = 1 to maxJ - 1 do
                score.[i, j] <- (calculateSingleScore i j)
        score.[maxI - 1, 0..]

type SequenceGapAligner(theSame: float, different: float, gapPenalty: int -> float) =
    member val TheSame = theSame with get
    member val Different = different with get
    member val GapPenalty = gapPenalty with get

    member this.Align(first: Sequence, second: Sequence) =
        let maxIter = if first.Data.Length > second.Data.Length then first.Data.Length else second.Data.Length
        let allAreThatLong i =
            first.Data.Length > i && second.Data.Length > i
        let itIsAGap index =
            first.Data.[index] = '-' || second.Data.[index] = '-'
        let valuesAreTheSame index =
            first.Data.[index] = second.Data.[index]
        
        let mutable nCounter = 0
        let calculateCurrentValue action =
            let gapScore = if nCounter > 0 then -(this.GapPenalty nCounter) else 0.0
            nCounter <- 0
            action + gapScore

        [0..maxIter]
        |> Seq.mapi(fun i _ -> 
            if allAreThatLong i then
                if itIsAGap i then 
                    nCounter <- nCounter + 1
                    0.0
                elif valuesAreTheSame i then 
                    calculateCurrentValue this.TheSame
                else
                    calculateCurrentValue this.Different
            else
                calculateCurrentValue this.Different
            )
        |> Seq.sum