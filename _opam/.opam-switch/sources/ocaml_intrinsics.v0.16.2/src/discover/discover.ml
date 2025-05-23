open Configurator.V1

let prog_popcnt =
  {|
int main(int argc, char ** argv)
{
  return __builtin_popcount(argc);
}
|}
;;

let _prog_lzcnt =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_lzcnt_u64(argc);
}
|}
;;

let _prog_tzcnt =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_tzcnt_u64(argc);
}
|}
;;

let prog_crc32 =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_crc32di(argc,argc);
}
|}
;;

let prog_crc32_on_32bit_target =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_crc32si(argc,argc);
}
|}
;;

let prog_sse42 =
  {|
int main(int argc, char ** argv)
{
#ifndef __SSE4_2__
#error "SSE4.2 Not supported"
#endif
  return 0;
}
   |}
;;

let prog_sse41 =
  {|
int main(int argc, char ** argv)
{
#ifndef __SSE4_1__
#error "SSE4.1 Not supported"
#endif
  return 0;
}
   |}
;;

let prog_prefetchw =
  {|
int main(int argc, char ** argv)
{
  __builtin_prefetch(argv, 1, 3);
  return 0;
}
|}
;;

let prog_prefetchwt1 =
  {|
int main(int argc, char ** argv)
{
  __builtin_prefetch(argv, 1, 2);
  return 0;
}
|}
;;

let prog_arm_crc32 =
  {|
#include <arm_acle.h>

int main(int argc, char ** argv)
{
  return __crc32cw(argc, argc);
  return __crc32cd(argc, argc);
}
|}
;;

let prog_bmi2 =
  {|
int main(int argc, char ** argv)
{
#ifndef __BMI2__
#error "BMI2 Not supported"
#endif
  return 0;
}
|}
;;

let () =
  let output = ref "" in
  main
    ~name:"discover"
    ~args:[ "-o", Set_string output, "FILENAME output file" ]
    (fun _c ->
      let flags =
        List.filter_map
          (fun (flag, prog) ->
           let test_flag flag prog =
            let f, oc = Filename.open_temp_file "baseconf" ".c" in
            Fun.protect ~finally:(fun () -> close_out oc; Sys.remove f)
              (fun () ->
                Out_channel.(output_string oc prog; flush oc);
                Sys.command (Printf.sprintf "cc %s %s -o /dev/null >/dev/null 2>&1" f flag) = 0)
            in
            match test_flag flag prog with
            | true -> Some flag
            | false -> None)
          [ "-mpopcnt", prog_popcnt (* ; "-mlzcnt", prog_lzcnt
                                     * ; "-mbmi", prog_tzcnt *)
          ; "-mcrc32", prog_crc32
          ; "-mcrc32", prog_crc32_on_32bit_target
          ; "-mbmi2", prog_bmi2
          ; "-msse4.2", prog_sse42
          ; "-msse4.1", prog_sse41
          ; "-mprfchw", prog_prefetchw
          ; "-mprefetchwt1", prog_prefetchwt1
          ; "-march=armv8-a+crc", prog_arm_crc32
          ]
        |> List.sort_uniq String.compare
      in
      Flags.write_sexp !output flags)
;;
