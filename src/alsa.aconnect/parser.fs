namespace alsa.aconnect

type AlsaClientPortConnection =
  | From of clientId: int * portId: int * attributes: (string * string) array
  | To of clientId: int * portId: int * attributes: (string * string) array
  
type AlsaClientPort =
  {
    id: int
    label: string
    connections: AlsaClientPortConnection array
  }
  
type AlsaClient =
  {
    id: int
    label: string
    attributes: (string * string) array
    ports: AlsaClientPort array    
  }
  
module parser =
  open FParsec
  
  let internal ws = spaces
  let internal str s = pstring s
  let internal attribute<'a> = 
    tuple2 
      (many1Satisfy (fun c -> c <> '=' && c <> ',' && not (System.Char.IsWhiteSpace(c))) .>> str "=")
      (many1Satisfy (fun c -> c <> ',' && c <> ']') .>> ws)
  let internal connectionAttribute<'a> =
    tuple2
      (many1Satisfy (fun c -> c <> ':' && not (System.Char.IsWhiteSpace c)) .>> str ":")
      (many1Satisfy (fun c -> c <> ',' && c <> ']') .>> ws)
      
  let internal attributes<'a> = between (str "[") (str "]") (sepBy attribute (str ","))
  let internal connectionAttributes<'a> = between (str "[") (str "]") (sepBy connectionAttribute (str ":"))
  let internal clientLine<'a> =
    pipe4
      (ws >>. str "client" >>. ws >>. pint32 .>> str ":" .>> ws)
      (between (str "'") (str "'") (many1Satisfy ((<>) ''')) .>> ws)
      (opt attributes |>> function Some a -> a | None -> [])
      ws
      (fun id label attrs _ ->
        {
          id = id
          label = label.TrimEnd()
          attributes = attrs |> List.toArray
          ports = [||]
        }
      )
            
  let internal portLine = 
    ws >>. pint32 .>> ws 
    >>= fun portId -> 
      between (str "'") (str "'") (many1Satisfy ((<>) ''')) .>> ws 
      |>> fun label ->
        {
          id = portId
          label = label.TrimEnd()
          connections = [||]
        }
            
  let internal connectionLine connectionType =
    
    (ws >>. str connectionType >>. ws >>. (str "To:" <|> str "From:")) .>> ws  
    >>= fun connectType -> 
      pint32 .>> str ":" .>>. pint32 .>>. (opt connectionAttributes |>> function Some a -> a | None -> []) 
      |>> fun ((toClientId, toPortId), attributes) -> 
        match connectType with 
        | "To:" -> To(toClientId, toPortId, attributes |> List.toArray)
        | "From:" -> From(toClientId, toPortId, attributes |> List.toArray)
        | _ -> failwith "Unknown connection type"

  let internal portEntries =
    many (
      portLine
      >>= fun port ->
        many(
          choice [
            attempt (connectionLine "Connecting")
            attempt (connectionLine "Connected")
          ]
        )
        .>> ws
        |>> fun entries -> { port with connections = entries |> List.toArray }
    )
    
  let internal clientEntry<'a> = 
      clientLine
      >>= fun client -> 
        portEntries
        |>> fun ports -> { client with ports = ports |> List.toArray }

  let internal parser : Parser<_,unit> = many (clientEntry .>> ws) |>> List.toArray
  
  /// Parses the output of `aconnect -l` into Ok(AlsaClient array) or Error(message, parserError).
  let parse (text:string) =
    match run parser text with
    | Success(clients, _, _) -> Result.Ok clients
    | Failure(message, parserError, userState) -> Result.Error(message, string parserError)
  