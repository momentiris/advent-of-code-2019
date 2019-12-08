let [|first, second|] =
  Node.Fs.readFileAsUtf8Sync("03/input.txt")
  |> Js.String.split("\n")
  |> Array.map(x => x |> Js.String.split(","));

type coordinate = {
  x: int,
  y: int,
  steps: int,
  md: int,
};

let getManhattanDistance = (a, b) => {
  ...b,
  md: Js.Math.abs_int(a.x - b.x) + Js.Math.abs_int(a.y - b.y),
};

let directionToCoordinateValue = (direction, array) => {
  let last = array[array->Array.length - 1];
  switch (direction) {
  | "U" => {...last, y: last.y + 1, steps: last.steps + 1}
  | "D" => {...last, y: last.y - 1, steps: last.steps + 1}
  | "R" => {...last, x: last.x + 1, steps: last.steps + 1}
  | "L" => {...last, x: last.x - 1, steps: last.steps + 1}
  };
};

let instructionToCoordinates = (instruction, arr) => {
  let direction = instruction |> Js.String.substrAtMost(~from=0, ~length=1);
  let steps = instruction |> Js.String.substr(~from=1) |> int_of_string;

  steps
  |> Array.make(steps)
  |> Array.iter(_ => {
       let new_ = directionToCoordinateValue(direction, arr);
       Js.Array.push(new_, arr)->ignore;
     });
};

module Part1 = {
  let w1 = ref([|{x: 0, y: 0, steps: 0, md: 0}|]);
  let w2 = ref([|{x: 0, y: 0, steps: 0, md: 0}|]);

  let make = () => {
    for (x in 0 to first->Array.length - 1) {
      instructionToCoordinates(first[x], w1^);
    };
    for (x in 0 to second->Array.length - 1) {
      instructionToCoordinates(second[x], w2^);
    };

    let answer =
      Js.Array.reduce(
        (acc, curr) => {
          let matchIndex =
            w2^ |> Js.Array.findIndex(y => curr.x === y.x && curr.y === y.y);

          let clone = acc->Array.copy;

          switch (matchIndex > 0) {
          | true =>
            clone
            |> Js.Array.push({
                 x: curr.x,
                 y: curr.y,
                 steps: curr.steps + w2^[matchIndex].steps,
                 md: 0,
               })
            |> ignore;

            clone;
          | _ => clone
          };
        },
        [||],
        w1^,
      );

    answer;
  };
};
module Part2 = {
  let make = () => {
    Part1.make()
    |> Array.map(x =>
         getManhattanDistance({x: 0, y: 0, steps: 0, md: 0}, x)
       )
    |> Js.Array.sortInPlaceWith((a, b) => a > b ? 1 : (-1))
    |> Js.log2("Day 3");
  };
};
Part1.make();