let data =
  Node.Fs.readFileAsUtf8Sync("02/input.txt")
  |> Js.String.split(",")
  |> Array.map(int_of_string);

module Operations = {
  let make =
    fun
    | `Add => (
        (arr, p1, p2, r) =>
          arr
          |> Array.mapi((index, val_) =>
               index === r ? arr[p1] + arr[p2] : val_
             )
      )
    | `Mult => (
        (arr, p1, p2, r) =>
          arr
          |> Array.mapi((index, val_) =>
               index === r ? arr[p1] * arr[p2] : val_
             )
      );
};

module Part1 = {
  let rec modify = (arr, i) =>
    switch (arr[i]) {
    | 1 =>
      Operations.make(`Add, arr, arr[i + 1], arr[i + 2], arr[i + 3])
      ->modify(i + 4)
    | 2 =>
      Operations.make(`Mult, arr, arr[i + 1], arr[i + 2], arr[i + 3])
      ->modify(i + 4)
    | _ => arr[0]
    };

  let make = () => data->modify(0) |> Js.log2("Day 2 - part 1: ");
};