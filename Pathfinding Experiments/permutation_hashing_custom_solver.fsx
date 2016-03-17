// Well, MiniZinc did not serve me well. I'll roll my own solver for this.

let multinomials = [|14;1;1|]

let perm_ar = 
    [|
    for i=0 to multinomials.Length-1 do
        for j=1 to multinomials.[i] do
            yield i
    |]

#r "../packages/Combinatorics.1.0.3.2/lib/net40/Combinatorics.dll"
open Combinatorics.Collections

let p = Permutations(perm_ar,GenerateOption.WithoutRepetition) // Lazily generates permutations without repetition.

let perms =
    [|
    for x in p do
        let x = x |> Seq.toArray
        yield x |> Seq.toArray |> Array.map float32
    |]

#r "../packages/MathNet.Numerics.3.11.0/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.3.11.0/lib/net40/MathNet.Numerics.FSharp.dll"

open MathNet
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

Control.NativeProviderPath <- __SOURCE_DIRECTORY__ + "/../packages/MathNet.Numerics.MKL.Win-x64.2.0.0/build/x64"
Control.UseNativeMKL()

let m = Matrix<float32>.Build.Random(5, 5)
let v = Vector<float32>.Build.Random(5)
m*v

// Actually, before I go down this route, let me see if I can figure out a generator for multinomials.