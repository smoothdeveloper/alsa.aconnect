module [<NUnit.Framework.TestFixture>] alsa.aconnect.tests

open NUnit.Framework

let [<Test>] trimsLabels () =
  let input = """
  client 0: 'System   ' [type=kernel]
    0 'Timer           '
    1 'Announce        '
    """
  let actual =
    match parser.parse input with
    | Ok(clients) -> clients
    | Error(result, error) -> failwith $"{result} {error}"
  let expected =
    [|
      {
        id = 0
        label = "System"
        attributes = [|"type","kernel"|]
        ports = [|
          {
            id=0
            label="Timer"
            connections=[||]
          }
          {
            id=1
            label="Announce"
            connections=[||]
          }
        |]
      }
    |]
  
  Assert.That(actual, Is.EqualTo(expected : obj))


let [<Test>] parseAll () =
  let input = """client 0: 'System' [type=kernel]
    0 'Timer           '
    1 'Announce        '
client 14: 'Midi Through' [type=kernel]
    0 'Midi Through Port-0'
client 16: 'Virtual Raw MIDI 0-0' [type=kernel,card=0]
    0 'VirMIDI 0-0     '
	Connecting To: 129:0
client 17: 'Virtual Raw MIDI 0-1' [type=kernel,card=0]
    0 'VirMIDI 0-1     '
	Connecting To: 129:1
client 18: 'Virtual Raw MIDI 0-2' [type=kernel,card=0]
    0 'VirMIDI 0-2     '
client 19: 'Virtual Raw MIDI 0-3' [type=kernel,card=0]
    0 'VirMIDI 0-3     '
	Connecting To: 129:2
client 20: 'Scarlett 18i20 USB' [type=kernel,card=1]
    0 'Scarlett 18i20 USB MIDI 1'
client 32: 'SireneMIDI' [type=kernel,card=4]
    0 'SireneMIDI MIDI IN'
	Connected From: 129:4
client 36: 'FS-1-WL USB-MIDI' [type=kernel,card=5]
    0 'FS-1-WL USB-MIDI MIDI 1'
	Connecting To: 129:3
client 128: 'Client-128' [type=user,pid=1727]
    0 'Virtual RawMIDI '
client 129: 'Pure Data' [type=user,pid=2067]
    0 'Pure Data Midi-In 1'
	Connected From: 16:0
    1 'Pure Data Midi-In 2'
	Connected From: 17:0
    2 'Pure Data Midi-In 3'
	Connected From: 19:0
    3 'Pure Data Midi-In 4'
	Connected From: 36:0
    4 'Pure Data Midi-Out 1'
	Connecting To: 32:0
    5 'Pure Data Midi-Out 2'
    6 'Pure Data Midi-Out 3'
    7 'Pure Data Midi-Out 4'
    8 'Pure Data Midi-Out 5'
client 130: 'WebMIDI output' [type=user,pid=2938]
    0 'Output connection'
client 131: 'WebMIDI input' [type=user,pid=2938]
    0 'Input connection'
client 132: 'WebMIDI output' [type=user,pid=2938]
    0 'Output connection'
client 133: 'WebMIDI input' [type=user,pid=2938]
    0 'Input connection'
"""
  let actual =
    match parser.parse input with
    | Ok(clients) -> clients
    | Error(result, error) -> failwith $"{result} {error}"
  let expected =
    [|
      { id = 0
        label = "System"
        attributes = [|("type", "kernel")|]
        ports =
        [|
          { id = 0; label = "Timer"; connections = [||] }
          { id = 1; label = "Announce"; connections = [||] }
        |]
      }
      { id = 14
        label = "Midi Through"
        attributes = [|("type", "kernel")|]
        ports = [|{ id = 0; label = "Midi Through Port-0"; connections = [||] }|]
      }
      { id = 16
        label = "Virtual Raw MIDI 0-0"
        attributes = [|("type", "kernel"); ("card", "0")|]
        ports = [|{ id = 0; label = "VirMIDI 0-0"; connections = [|To (129, 0)|] }|]
      }
      { id = 17
        label = "Virtual Raw MIDI 0-1"
        attributes = [|("type", "kernel"); ("card", "0")|]
        ports = [|{ id = 0; label = "VirMIDI 0-1"; connections = [|To (129, 1)|] }|] }
      { id = 18
        label = "Virtual Raw MIDI 0-2"
        attributes = [|("type", "kernel"); ("card", "0")|]
        ports = [|{ id = 0; label = "VirMIDI 0-2"; connections = [||] }|] }
      { id = 19
        label = "Virtual Raw MIDI 0-3"
        attributes = [|("type", "kernel"); ("card", "0")|]
        ports = [|{ id = 0; label = "VirMIDI 0-3"; connections = [|To (129, 2)|] }|] }
      { id = 20
        label = "Scarlett 18i20 USB"
        attributes = [|("type", "kernel"); ("card", "1")|]
        ports = [|{ id = 0; label = "Scarlett 18i20 USB MIDI 1"; connections = [||] }|] }
      { id = 32
        label = "SireneMIDI"
        attributes = [|("type", "kernel"); ("card", "4")|]
        ports = [|{ id = 0; label = "SireneMIDI MIDI IN"; connections = [|From (129, 4)|] }|] }
      { id = 36
        label = "FS-1-WL USB-MIDI"
        attributes = [|("type", "kernel"); ("card", "5")|]
        ports = [|{ id = 0; label = "FS-1-WL USB-MIDI MIDI 1"; connections = [|To (129, 3)|] }|] }
      { id = 128
        label = "Client-128"
        attributes = [|("type", "user"); ("pid", "1727")|]
        ports = [|{ id = 0; label = "Virtual RawMIDI"; connections = [||] }|] }
      { id = 129
        label = "Pure Data"
        attributes = [|("type", "user"); ("pid", "2067")|]
        ports =
         [|
           { id = 0; label = "Pure Data Midi-In 1"; connections = [|From (16, 0)|] }
           { id = 1; label = "Pure Data Midi-In 2"; connections = [|From (17, 0)|] }
           { id = 2; label = "Pure Data Midi-In 3"; connections = [|From (19, 0)|] }
           { id = 3; label = "Pure Data Midi-In 4"; connections = [|From (36, 0)|] }
           { id = 4; label = "Pure Data Midi-Out 1"; connections = [|To (32, 0)|] }
           { id = 5; label = "Pure Data Midi-Out 2"; connections = [||] }
           { id = 6; label = "Pure Data Midi-Out 3"; connections = [||] }
           { id = 7; label = "Pure Data Midi-Out 4"; connections = [||] }
           { id = 8; label = "Pure Data Midi-Out 5"; connections = [||] }
          |]
      }
      { id = 130
        label = "WebMIDI output"
        attributes = [|("type", "user"); ("pid", "2938")|]
        ports = [|{ id = 0; label = "Output connection"; connections = [||] }|]
      }
      { id = 131
        label = "WebMIDI input"
        attributes = [|("type", "user"); ("pid", "2938")|]
        ports = [|{ id = 0; label = "Input connection"; connections = [||] }|] }
      { id = 132
        label = "WebMIDI output"
        attributes = [|("type", "user"); ("pid", "2938")|]
        ports = [|{ id = 0; label = "Output connection"; connections = [||] }|] }
      { id = 133
        label = "WebMIDI input"
        attributes = [|("type", "user"); ("pid", "2938")|]
        ports = [|{ id = 0; label = "Input connection"; connections = [||] }|] }
    |]
  
  Assert.That(actual, Is.EqualTo(expected : obj))
