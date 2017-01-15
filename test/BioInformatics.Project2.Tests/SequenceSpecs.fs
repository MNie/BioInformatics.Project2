module Tests.Sequence

open Expecto
open BioInformatics.Project2.Core.Sequence
open BioInformatics.Project2.Core.SequenceAligner
open BioInformatics.Project2.Core.LCS
[<Tests>]
let tests =
    testList "All" [
        testList "Sequence" [
            testList "SequenceParser" [
                testCase "when trying to parse DNA" <| fun _ ->
                    let parser = SequenceParser()
                    let data = "GGG\nGATAAA"
                    let result = parser.Parse(data, SequenceType.DNA)

                    Expect.isSome result "should be true because passed data is correct"
                    Expect.equal result.Value.Type SequenceType.DNA "should be of type DNA"
                    Expect.equal result.Value.Data data "should parse data to seq data"

                testCase "when trying to parse RNA" <| fun _ ->
                    let parser = SequenceParser()
                    let data = "GGG\nGAUAAA"
                    let result = parser.Parse(data, SequenceType.RNA)

                    Expect.isSome result "should be true because passed data is correct"
                    Expect.equal result.Value.Type SequenceType.RNA "should be of type RNA"
                    Expect.equal result.Value.Data data "should parse data to seq data"

                testCase "when trying to parse RNA" <| fun _ ->
                    let parser = SequenceParser()
                    let data = "GGG\nGATAAA"
                    let result = parser.Parse(data, SequenceType.RNA)

                    Expect.isNone result "should be false because passed data is dna not rna"
            ]

            testList "SequenceProvider" [
                testCase "when trying to provide DNA" <| fun _ ->
                    let provider = SequenceProvider()
                    let seqData = "GGG\nGATAAA"
                    let data = sprintf "DNA\n%s" seqData
                    let result = provider.Provide(data)

                    Expect.isSome result "should be true because passed data is correct"
                    Expect.equal result.Value.Type SequenceType.DNA "should be of type DNA"
                    Expect.equal result.Value.Data seqData "should parse data to seq data"

                testCase "when trying to provide RNA" <| fun _ ->
                    let provider = SequenceProvider()
                    let seqData = "GGG\nGAUAAA"
                    let data = sprintf "RNA\n%s" seqData
                    let result = provider.Provide(data)

                    Expect.isSome result "should be true because passed data is correct"
                    Expect.equal result.Value.Type SequenceType.RNA "should be of type RNA"
                    Expect.equal result.Value.Data seqData "should parse data to seq data"

                testCase "when trying to parse RNA" <| fun _ ->
                    let provider = SequenceProvider()
                    let seqData = "GGG\nGATAAA"
                    let data = sprintf "RNA\n%s" seqData
                    let result = provider.Provide(data)

                    Expect.isNone result "should be false because passed data is dna not rna"
            ]
        ]

        testList "SequenceAligner" [
            testCase "when trying to align sequences" <| fun _ ->
                let first = Sequence(SequenceType.DNA, "GGGAT")
                let second = Sequence(SequenceType.DNA, "GG--T")
                let sub (elemFirst, elemSecond) =
                    if elemFirst = elemSecond then 2.0
                    else -1.0
                let ins elem =
                    -2.0
                let del elem =
                    -2.0
                let aligner = SequenceAligner(ins, del, sub)
                let result = aligner.Align(first, second)

                Expect.containsAll result [|-8.0;-4.0;-3.0;-2.0;2.0|] "should return valid values in 5 row"
        ]

        testList "SequenceGapAligner" [
            testCase "when trying to align sequences by gap penalty function" <| fun _ ->
                let first = Sequence(SequenceType.DNA, "-AGAG-TCAATCCATAG")
                let second = Sequence(SequenceType.DNA, "CAGAGGTCCATC-ATG-")
                let theSame =
                    2.0
                let different =
                    0.0
                let gapPenalty (element: int) = 
                    (element |> float) + 2.0

                let aligner = SequenceGapAligner(theSame, different, gapPenalty)
                let result = aligner.Align(first, second)

                Expect.equal result 10.0 "should return exactly 10"

            testCase "when trying to align sequences by gap penalty function" <| fun _ ->
                let first = Sequence(SequenceType.DNA, "------AGAGTCAATCCATAG")
                let second = Sequence(SequenceType.DNA, "CAGAGG----TCCATCATG--")
                let theSame =
                    2.0
                let different =
                    0.0
                let gapPenalty (element: int) = 
                    (element |> float) + 2.0

                let aligner = SequenceGapAligner(theSame, different, gapPenalty)
                let result = aligner.Align(first, second)

                Expect.equal result -8.0 "should return exactly -8"
        ]

        testList "LCS" [
            testCase "when trying to get longest subsequence from ABCDGH and AEDFHR" <| fun _ ->
                let first = Sequence(SequenceType.DNA, "ABCDGH")
                let second = Sequence(SequenceType.DNA, "AEDFHR")
                let lcs = LCS()
                let result = lcs.Count(first, second)

                Expect.equal result 3 "should return 3"

            testCase "when trying to get longest subsequence from AGGTAB and GXTXAYB" <| fun _ ->
                let first = Sequence(SequenceType.DNA, "GXTXAYB")
                let second = Sequence(SequenceType.DNA, "GTAB")
                let lcs = LCS()
                let result = lcs.Count(first, second)

                Expect.equal result 4 "should return 4"
        ]
    ]

[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args