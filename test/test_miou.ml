let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue int)
        (Unix.getpid ()) Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let truncated_request01 =
  {|--------------------------eb790219f130e103|}
  ^ "\r"
  ^ {|
Content-Disposition: form-data; name="text"|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
default|}
  ^ "\r"
  ^ {|
--------------------------eb790219f130e103|}
  ^ "\r"
  ^ {|
Content-Disposition: form-data; name="file1"; filename="a.html"|}
  ^ "\r"
  ^ {|
Content-Type: text/html|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
<!DOCTYPE html><title>Content of a.html.</title>|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
--------------------------eb790219f130e103|}
  ^ "\r"
  ^ {|
Content-Disposition: form-data; name="file2"; filename="a.txt"|}
  ^ "\r"
  ^ {|
Content-Type: text/plain|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
Conten|}

let truncated_request02 =
  {|--------------------------eb790219f130e103
Content-Disposition: form-data; name="text"

default
--------------------------eb790219f130e103
Content-Disposition: form-data; name="file1"; filename="a.html"
Content-Type: text/html

<!DOCTYPE html><title>Content of a.html.</title>

--------------------------eb790219f130e103
Content-Disposition: form-data; name="file2"; filename="a.txt"
Content-Type: text/plain

Conten|}

open Multipart_form_miou

let always v _ = v

let test01 =
  Alcotest.test_case "truncated flow (with CRLF)" `Quick @@ fun () ->
  Miou.run @@ fun () ->
  let content_type =
    "multipart/form-data; boundary=------------------------eb790219f130e103\r\n"
  in
  let content_type =
    match Multipart_form.Content_type.of_string content_type with
    | Ok v -> v
    | Error (`Msg err) -> failwith err in
  let body = Bounded_stream.of_list [ truncated_request01 ] in
  let `Parse prm, _ =
    Multipart_form_miou.stream ~identify:(always ()) body content_type in
  match Miou.await_exn prm with
  | Ok _ -> Alcotest.(check pass) "Truncated request" () ()
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %s" err

let test02 =
  Alcotest.test_case "truncated flow (without CRLF)" `Quick @@ fun () ->
  Miou.run @@ fun () ->
  let content_type =
    "multipart/form-data; boundary=------------------------eb790219f130e103\r\n"
  in
  let content_type =
    match Multipart_form.Content_type.of_string content_type with
    | Ok v -> v
    | Error (`Msg err) -> failwith err in
  let body = Bounded_stream.of_list [ truncated_request02 ] in
  Bounded_stream.close body ;
  let `Parse prm, _ =
    Multipart_form_miou.stream ~identify:(always ()) body content_type in
  match Miou.await_exn prm with
  | Ok _ -> Alcotest.fail "Unexpected valid input"
  | Error (`Msg "Invalid multipart/form") ->
      Alcotest.(check pass) "truncated input" () ()
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %s." err

let fail_if_locked f =
  let fail_prm =
    Miou.async @@ fun () ->
    Miou_unix.sleep 1. ;
    Alcotest.failf "Bounded_stream is locked" in
  match Miou.await_first [ fail_prm; Miou.async f ] with
  | Error exn -> Miou.reraise exn
  | Ok () -> ()

(* check that bs of size n actually hold n item *)
let test_bs_01 =
  Alcotest.test_case "bounded stream" `Quick @@ fun () ->
  Miou_unix.run @@ fun () ->
  let open Bounded_stream in
  let f () =
    let n = 1 in
    let bs = create n in
    put bs 0 ;
    ignore (get bs) ;
    close bs ;
    Alcotest.(check pass) "ok bs" () () in
  fail_if_locked f

(* Bounded_stream.put wait if bs is full
   and fail if stream is closed while waiting (previously it was not waked up)
   so users of Bounded_stream must be carefull to close only after all puts are done.
   This would fail:
     let prm0 = Miou.async @@ fun () -> put bs 0 in
     let prm1 = Miou.async @@ fun () -> put bs 1 in
     let prm2 = Miou.async @@ fun () -> close bs in
     let l = [ prm0; prm1; prm2 ] in
     Miou.await_all l
     |> List.iter (function Error exn -> Miou.reraise exn | Ok () -> ()) ;
   *)
let test_bs_02 =
  Alcotest.test_case "bounded stream" `Quick @@ fun () ->
  Miou_unix.run @@ fun () ->
  let open Bounded_stream in
  let f () =
    let n = 1 in
    let bs = create (n + 1) in
    let prm_put =
      Miou.async @@ fun () ->
      put bs 0 ;
      put bs 1 ;
      close bs in
    let prm_get = Miou.async @@ fun () -> ignore (get bs) in
    let l = [ prm_put; prm_get ] in
    Miou.await_all l
    |> List.iter (function Error exn -> Miou.reraise exn | Ok () -> ()) ;
    Alcotest.(check pass) "ok bs" () () in
  fail_if_locked f

let () =
  Alcotest.run "multipart_form_miou"
    [
      ("truncated", [ test01; test02 ]);
      ("bounded_stream", [ test_bs_01; test_bs_02 ]);
    ]
