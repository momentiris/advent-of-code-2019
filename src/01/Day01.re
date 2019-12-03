let data =
  Node.Fs.readFileAsUtf8Sync("./01/input.txt")
  |> Js.String.split("\n")
  |> Array.map(int_of_string)
  |> Array.to_list;

let fuelFromMass = m => m / 3 - 2;

let rec calcTotalFuel = d => {
  let fuel = fuelFromMass(d);
  fuel <= 0 ? 0 : fuel + calcTotalFuel(fuel);
};

module Part1 = {
  let make = () =>
    data
    |> List.fold_left((acc, el) => acc + el->fuelFromMass, 0)
    |> Js.log2("Day 1 - part 1: ");
};

module Part2 = {
  let make = () =>
    data
    |> List.fold_left((acc, el) => acc + el->calcTotalFuel, 0)
    |> Js.log2("Day 1 - part 2: ");
};