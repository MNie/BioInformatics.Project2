module Tests.Sequence

open Expecto
open BioInformatics.Project2.Core.Sequence
[<Tests>]
let tests =
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

[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args