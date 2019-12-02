open First;

let rec calcTotalFuel = d => {
  let fuel = fuelFromMass(d);
  fuel <= 0 ? 0 : fuel + calcTotalFuel(fuel);
};

let answer =
  Input.data |> List.fold_left((acc, el) => acc + el->calcTotalFuel, 0);

Js.log2("and the answer is: ", answer);