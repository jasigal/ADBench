open Torch
open Bigarray

let () =
  Stdio.printf "cuda available: %b\n%!" (Cuda.is_available ());
  Stdio.printf "cudnn available: %b\n%!" (Cuda.cudnn_is_available ());
  let x = 1252375.822111831046640872955322265625 in
  let t = Torch.Tensor.full
    ~size:[1]
    ~fill_value:(Torch.Scalar.f x)
    ~options:(Torch_core.Kind.T Torch_core.Kind.f64, Torch.Device.Cpu) in
  let () = match Tensor.kind t with
    | T Float -> Stdio.printf "float\n"
    | T Double -> Stdio.printf "double\n"
    | _ -> ()
  in
  Stdio.printf "t: %.30f\n" (Tensor.get_float1 t 0);
  let tt = Tensor.float_vec ?kind:(Some `double) [ x ] in
  let () = match Tensor.kind tt with
    | T Float -> Stdio.printf "float\n"
    | T Double -> Stdio.printf "double\n"
    | _ -> ()
  in
  Tensor.print tt;
  let aaa = Genarray.create float64 c_layout [|1|] in
  Genarray.set aaa [|0|] x;
  Stdio.printf "aaa: %.30f\n" (Genarray.get aaa [|0|]);
  let ttt = Tensor.of_bigarray aaa in
  Stdio.printf "ttt: %.30f\n" (Tensor.get_float1 (Tensor.add ttt ttt) 0);
;;
