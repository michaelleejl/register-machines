open Lang
open Expanded
open Checked

let translate_body = function
  | ChAdd (r, k) -> EAdd (r, k)
  | ChSub (r, k1, k2) -> ESub (r, k1, k2)
  | ChHalt -> EHalt
  | ChExecute { machine; arguments; next } ->
      EExecute { machine; arguments; next }
  | ChClear (r, k) -> EClear (r, k)
  | ChJump k -> EJump k

let translate { label; body } = Expanded.{ label; body = translate_body body }

let rec expand_machine { name; parameters; definition } =
  Expanded.
    { name; parameters; definition = expand_definition parameters definition }

and expand_definition parameters = function
  | ChStruct { machines; registers; instructions } ->
      {
        machines = List.map expand_machine machines;
        registers;
        instructions = List.map translate instructions;
      }
  | ChApply { name = machine; arguments } ->
      {
        machines = [];
        registers = [];
        instructions =
          [
            {
              label = "_l0";
              body = EExecute { machine; arguments; next = "_l1" };
            };
            { label = "_l1"; body = EHalt };
          ];
      }
  | ChSeq (d1, d2) ->
      let first = "First" in
      let second = "Second" in
      let m1 =
        Expanded.
          {
            name = first;
            parameters;
            definition = expand_definition parameters d1;
          }
      in
      let m2 =
        Expanded.
          {
            name = second;
            parameters;
            definition = expand_definition parameters d2;
          }
      in
      {
        machines = [ m1; m2 ];
        registers = [];
        instructions =
          [
            {
              label = "_l0";
              body =
                EExecute
                  { machine = first; arguments = parameters; next = "_l1" };
            };
            {
              label = "_l1";
              body =
                EExecute
                  { machine = second; arguments = parameters; next = "_l2" };
            };
            { label = "_l2"; body = EHalt };
          ];
      }
  | ChIf (r, d1, d2) ->
      let tbranch = "Then" in
      let ebranch = "Else" in
      let m1 =
        Expanded.
          {
            name = tbranch;
            parameters;
            definition = expand_definition parameters d1;
          }
      in
      let m2 =
        Expanded.
          {
            name = ebranch;
            parameters;
            definition = expand_definition parameters d2;
          }
      in
      {
        machines = [ m1; m2 ];
        registers = [];
        instructions =
          [
            { label = "_l0"; body = ESub (r, "_l1", "_l2") };
            {
              label = "_l1";
              body =
                EExecute
                  { machine = tbranch; arguments = parameters; next = "_l3" };
            };
            {
              label = "_l2";
              body =
                EExecute
                  { machine = ebranch; arguments = parameters; next = "_l3" };
            };
            { label = "_l3"; body = EHalt };
          ];
      }
  | ChWhile (r, d) ->
      let name = "Body" in
      let m =
        Expanded.
          { name; parameters; definition = expand_definition parameters d }
      in
      {
        machines = [ m ];
        registers = [];
        instructions =
          [
            { label = "_l0"; body = ESub (r, "_l1", "_l2") };
            {
              label = "_l1";
              body =
                EExecute
                  { machine = name; arguments = parameters; next = "_l0" };
            };
            { label = "_l2"; body = EHalt };
          ];
      }

let program ({ machines = ms; registers; instructions } : program) =
  let registers =
    List.map
      (fun (r : register) -> Expanded.{ name = r.name; value = r.value })
      registers
  in
  Expanded.
    {
      machines = List.map expand_machine ms;
      registers;
      instructions = List.map translate instructions;
    }
