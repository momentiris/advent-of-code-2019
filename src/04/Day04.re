let rInput =
  Node.Fs.readFileAsUtf8Sync("04/input.txt")
  |> Js.String.split("-")
  |> Array.map(int_of_string);

let input =
  Belt.Array.range(rInput[0], rInput[1]) |> Array.map(string_of_int);

let isAscending = str =>
  switch (str |> Js.String.match([%re "/^0*1*2*3*4*5*6*7*8*9*$/"])) {
  | Some(_) => true
  | _ => false
  };

let doubles = str => str |> Js.String.match([%re "/([1-9])\1+/g"]);

module Part1 = {
  let make = input =>
    input
    |> Js.Array.filter(p =>
         p->doubles->Belt.Option.getWithDefault([||])->Array.length > 0
         && p->isAscending
       )
    |> Array.length
    |> Js.log;
};

module Part2 = {
  let make = input =>
    input
    |> Js.Array.filter(p =>
         p->doubles->Belt.Option.getWithDefault([|"0"|])
         |> Js.Array.some(x => x |> String.length === 2)
         && p->isAscending
       )
    |> Array.length
    |> Js.log;
};

Part2.make(input);