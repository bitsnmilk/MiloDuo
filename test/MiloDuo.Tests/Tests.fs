module MiloDuo.UnitTests

open Xunit
open MiloDuo

[<Fact>]
let ``MiloDuo adds 5 and 3``() =
    let expected = 8
    let actual = foo 5 3
    Assert.Equal(expected, actual)

