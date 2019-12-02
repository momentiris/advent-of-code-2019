let fuelFromMass = m => m / 3 - 2;

let sum =
  Input.data |> List.fold_left((acc, el) => acc + el->fuelFromMass, 0);

Js.log2("and the answer is: ", sum);