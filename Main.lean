import Lean.Data.Json

def allowedTypeNames := [
  "Uint8",
  "Uint16",
  "Uint32",
  "Sint8",
  "Sint16",
  "Sint32",
  "SDL_Window",
  "SDL_Event",
  "SDL_Rect",
  "SDL_Renderer"
]

def allowedFuncNames := [
  "SDL_Init",
  "SDL_CreateWindow",
  "SDL_GetWindowSurface",
  "SDL_UpdateWindowSurface",
  "SDL_Delay",
  "SDL_CreateRenderer",
  "SDL_RenderSetLogicalSize",
  "SDL_RenderClear",
  "SDL_RenderPresent",
  "SDL_RenderDrawRect",
  "SDL_RenderFillRect",
  "SDL_SetRenderDrawColor",
  "SDL_PollEvent"
]

def readFile? (filePath: System.FilePath): IO (Except IO.Error String) := do
  try
    let contents <- IO.FS.readFile filePath
    return Except.ok contents
  catch e => return Except.error e

def getJsonObjPropVal? (x: Lean.Json) (propName: String) :=
  if let Lean.Json.obj obj := x then
    let prop? := obj.toArray.find? (fun x => x.fst == propName)
    if let .some prop := prop? then
      some prop.snd
    else none
  else none

def getJsonObjPropValStr? (x: Lean.Json) (propName: String): Option String :=
  (getJsonObjPropVal? x propName) >>= (fun x => x.getStr?.toOption)

def getJsonObjPropValArr? (x: Lean.Json) (propName: String): Option (Array Lean.Json) :=
  (getJsonObjPropVal? x propName) >>= (fun x => x.getArr?.toOption)

def getFuncName? (x : Lean.Json): Option String := 
  let tagPropVal? := getJsonObjPropValStr? x "tag"
  if tagPropVal? == "function" then
    getJsonObjPropValStr? x "name"
  else none

def getFuncReturnType? (x : Lean.Json): Option String := 
  let tagPropVal? := getJsonObjPropValStr? x "tag"
  if tagPropVal? == "function" then
    getJsonObjPropValStr? x "name"
  else none

def getTypeName? (x : Lean.Json): Option String := 
  let tagPropVal? := getJsonObjPropValStr? x "tag"
  if tagPropVal? == "typedef" || tagPropVal? == "struct" then
    getJsonObjPropValStr? x "name"
  else none

def shouldGenFfiCode (x : Lean.Json): Bool :=
  if let .some funcName := (getFuncName? x) then
    allowedFuncNames.contains funcName
  else if let .some typeName := (getTypeName? x) then
    allowedTypeNames.contains typeName
  else
    False

partial def genFfiType (x: Lean.Json): String :=
  let tag := (getJsonObjPropValStr? x "tag").get!
  match tag with
  | ":int" => "Int32"
  | "uint8_t" => "UInt8"
  | "uint16_t" => "UInt16"
  | "uint32_t" => "UInt32"
  | "int8_t" => "SInt8"
  | "int16_t" => "SInt16"
  | "int32_t" => "SInt32"
  | ":void" => "Unit"
  | ":char" => "Char"
  | ":pointer" =>
    let internalType := (getJsonObjPropVal? x "type").get!
    let internalTypeStr := genFfiType internalType
    s!"Ptr {internalTypeStr}"
  | "struct" =>
    let name := (getJsonObjPropValStr? x "name").get!
    name
  | ":struct" =>
    let name := (getJsonObjPropValStr? x "name").get!
    name
  | ":union" =>
    let name := (getJsonObjPropValStr? x "name").get!
    name
  | _ =>
    tag
    --panic! s!"Unknown FFI type: {tag}"

def genParam (x: Lean.Json): String :=
  let name := (getJsonObjPropValStr? x "name").get!
  let type := (getJsonObjPropVal? x "type").get!
  let genType := genFfiType type

  s!"({name}: {genType})"

-- TODO: Use string builder?
def genFfiCode? (x : Lean.Json): Option String :=
  let tagPropVal? := getJsonObjPropValStr? x "tag"
  if tagPropVal? == "function" then
    let namePropVal := (getJsonObjPropValStr? x "name").get!
    let paramsStr := (getJsonObjPropValArr? x "parameters").get!.toList
      |>.map genParam
      |> String.intercalate (s := ", ")
    let returnTypeName := (getJsonObjPropVal? x "return-type").get! |> genFfiType

    s!"@[extern \"{namePropVal}\"]
constant {namePropVal} : {paramsStr} -> IO {returnTypeName}"
  else if tagPropVal? == "typedef" then
    let namePropVal := (getJsonObjPropValStr? x "name").get!
    let type := (getJsonObjPropVal? x "type").get! |> genFfiType
    s!"abbrev {namePropVal} := {type}"
  else if tagPropVal? == "struct" then
    let namePropVal := (getJsonObjPropValStr? x "name").get!
    s!"structure {namePropVal}"
  else none

def genBindingCode? (x: Lean.Json): Except String String :=
  match x with
  | .arr jsonArr =>
    let genCodeParts := jsonArr.filter shouldGenFfiCode
      |>.map genFfiCode?
      |>.filter Option.isSome
      |>.map Option.get!
      |>.toList
    .ok (String.intercalate (s := "\n\n") genCodeParts)
  | _ => .error "JSON isn't an array."

def main : IO Unit := do
  -- Load the contents of the c2ffi output JSON file.
  let .ok jsonStr <- readFile? "SDL2_c2ffi_output.json"
    | IO.println "Failed to load JSON file."

  -- Parse the JSON.
  let .ok json := Lean.Json.parse jsonStr
    | IO.println "Failed to parse JSON."

  -- Traverse the JSON and generate Lean binding code.
  let .ok bindingCode := genBindingCode? json
    | IO.println "Failed to generate binding code from JSON."

  IO.println bindingCode