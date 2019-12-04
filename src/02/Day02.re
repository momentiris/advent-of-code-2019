let input =
  Node.Fs.readFileAsUtf8Sync("02/input.txt")
  |> Js.String.split(",")
  |> Array.map(int_of_string);

let mInput = (input, v1, v2) =>
  input |> Array.mapi((i, v) => i === 1 ? v1 : i === 2 ? v2 : v);

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

let rec program = (arr, i) =>
  switch (arr[i]) {
  | 1 =>
    Operations.make(`Add, arr, arr[i + 1], arr[i + 2], arr[i + 3])
    ->program(i + 4)
  | 2 =>
    Operations.make(`Mult, arr, arr[i + 1], arr[i + 2], arr[i + 3])
    ->program(i + 4)
  | _ => arr[0]
  };

module Part1 = {
  let make = () =>
    input->mInput(12, 2)->program(0) |> Js.log2("Day 2 - part 1: ");
};

module Part2 = {
  let make = () =>
    input
    |> Array.mapi((index_, _) =>
         input
         |> Array.mapi((index, _val_) =>
              input->mInput(index, index_)->program(0) === 19690720
                ? Js.log2("Day 2 - part 2: ", 100 * index + index_) : ()
            )
       );
};