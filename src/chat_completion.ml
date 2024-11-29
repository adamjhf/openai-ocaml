open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let endpoint = "/v1/chat/completions"

type role =
  [ `System
  | `User
  | `Assistant
  ]

let yojson_of_role = function
  | `System -> `String "system"
  | `User -> `String "user"
  | `Assistant -> `String "assistant"
;;

type message =
  { content : string
  ; role : role
  }
[@@deriving yojson_of]

(** raw API request:
 * @param k for continuation to avoid redefining labeled parameters
 *)
let send_raw_k
  k
  (client : Client.t)
  ?(model = client.model)
  ?max_tokens
  ~messages
  ?temperature
  ?top_p
  ?stream
  ?n
  ?stop
  ?frequency_penalty
  ?logit_bias
  ?presence_penalty
  ?user
  ()
  =
  let temperature = Json.to_field_opt "temperature" (fun f -> `Float f) temperature in
  let top_p = Json.to_field_opt "top_p" (fun f -> `Float f) top_p in
  let n = Json.to_field_opt "n" (fun i -> `Int i) n in
  let stream = Json.to_field_opt "stream" (fun b -> `Bool b) stream in
  let stop = Json.to_field_opt "stop" (fun l -> `List l) stop in
  let max_tokens = Json.to_field_opt "max_tokens" (fun i -> `Int i) max_tokens in
  let presence_penalty =
    Json.to_field_opt "presence_penalty" (fun f -> `Float f) presence_penalty
  in
  let frequency_penalty =
    Json.to_field_opt "frequency_penalty" (fun f -> `Float f) frequency_penalty
  in
  let logit_bias = Json.to_field_opt "logit_bias" (fun x -> `Assoc x) logit_bias in
  let user = Json.to_field_opt "user" (fun s -> `String s) user in
  let body =
    List.filter
      (fun (_, v) -> v <> `Null)
      [ "model", `String model
      ; "messages", `List (List.map yojson_of_message messages)
      ; temperature
      ; top_p
      ; n
      ; stream
      ; stop
      ; max_tokens
      ; presence_penalty
      ; frequency_penalty
      ; logit_bias
      ; user
      ]
    |> fun l -> Yojson.Safe.to_string (`Assoc l)
  in
  let headers =
    [ "content-type", "application/json"
    ; "Authorization", String.concat " " [ "Bearer"; client.api_key ]
    ]
  in
  let%lwt resp =
    Ezcurl_lwt.post
      ~client:client.c
      ~headers
      ~content:(`String body)
      ~url:(client.url ^ endpoint)
      ~params:[]
      ()
  in
  k resp
;;

let extract_content body =
  let json = Yojson.Safe.from_string body in
  Json.(
    member "choices" json
    |> function
    | [%yojson? [ res ]] ->
      res
      |> member "message"
      |> member "content"
      |> to_string
      |> String.trim
      |> Lwt.return
    | _ -> Lwt.fail_with @@ Printf.sprintf "Unexpected response: %s" body)
;;

let send =
  send_raw_k
  @@ function
  | Ok { body; _ } -> extract_content body
  | Error (_code, e) -> Lwt.fail_with e
;;
