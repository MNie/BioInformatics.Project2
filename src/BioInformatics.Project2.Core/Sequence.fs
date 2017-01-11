namespace BioInformatics.Project2.Core.Sequence
open System
type SequenceType = RNA = 0 | DNA = 1

type Sequence (seqType: SequenceType, data: string) =
    member val Type = seqType with get
    member val Data = data with get

type SequenceParser() =
    let ifCorrectReturnIt (alphabet: string) (data: string) (seqType: SequenceType) =
        let isCorrect = 
            data
            |> Seq.filter(fun x -> x <> '\n')
            |> Seq.map string
            |> Seq.forall alphabet.Contains
        if isCorrect then Option.Some (Sequence(seqType, data))
        else Option.None
    
    let ifDnaReturnIt data = 
        let dnaChars = "GCAT"
        ifCorrectReturnIt dnaChars data SequenceType.DNA

    let ifRnaReturnIt data = 
        let dnaChars = "GCAU"
        ifCorrectReturnIt dnaChars data SequenceType.RNA

    member this.Parse(data: string, seqType: SequenceType) = 
        match seqType with
        | SequenceType.DNA -> ifDnaReturnIt data
        | SequenceType.RNA -> ifRnaReturnIt data
        | _ -> Option.None 

type SequenceProvider() =
    member this.Provide (data: string) =
        let splitedData = data.Split '\n'
        let sequenceData = splitedData.[1..] |> String.concat "\n"
        let seqType = Enum.Parse(typedefof<SequenceType>, splitedData.[0]) :?> SequenceType
        SequenceParser().Parse(sequenceData, seqType) 



