type Note =
    | A
    | Bb
    | B
    | C
    | Db
    | D
    | Eb
    | E
    | F
    | Gb
    | G
    | Ab

type Interval = Note -> Note


type ChordQuality =
    | Major
    | Minor 
    | Diminished 

type FlatThird = ChordQuality -> ChordQuality //?

type ScaleDegree =
    | I of Note
    | II of Note
    | III of Note
    | IV of Note
    | V of Note
    | VI of Note
    | VII of Note

type Triad = Note * Note * Note

type FourNotes = Triad * Note

type FiveNotes = FourNotes * Note 

type Chord = Triad | FourNotes | FiveNotes

type PentatonicScale = ScaleDegree list -> ScaleDegree list option 

//type RootThirdFifth = (ScaleDegree * ChordQuality) list  -> (Note * Note * Note) list 

type CreateChord = ScaleDegree list -> Chord  

type Scale = Interval list -> Note -> ScaleDegree list 
    
type RelativeNaturalMinor = ScaleDegree list -> ScaleDegree list 

let flat: Interval =
    function
    | A -> Ab
    | Bb -> A
    | B -> Bb
    | C -> B
    | Db -> C
    | D -> Db
    | Eb -> D
    | E -> Eb
    | F -> E
    | Gb -> F
    | G -> Gb
    | Ab -> G
    
let half: Interval =
    function
    | A -> Bb
    | Bb -> B
    | B -> C 
    | C -> Db
    | Db -> D
    | D -> Eb
    | Eb -> E
    | E -> F
    | F -> Gb
    | Gb -> G
    | G -> Ab
    | Ab -> A

let whole: Interval =
    function
    | A -> B
    | Bb -> C
    | B -> Db 
    | C -> D
    | Db -> Eb
    | D -> E
    | Eb -> F
    | E -> Gb
    | F -> G
    | Gb -> Ab
    | G -> A
    | Ab -> Bb

let majorScale = [ whole; whole; half; whole; whole; whole; half ]
let diminshedScale = [whole; half; whole; half; whole; half; whole]
let minorScale = [ whole; half; whole; whole; half; whole; whole ]

let qualityOfMajorScale = [ Major; Minor; Minor; Major; Major; Minor; Diminished; Major ]
let qualityOfMinorScale = [ Minor; Diminished; Major; Minor; Minor; Major; Major; Minor]

// type MajorChord = Interval list -> Major 

let buildScale: Scale =
    fun intervals tonic -> 
    intervals
    |> List.fold(fun notes interval ->
        notes @ [ match (notes |> List.last) with
                  | I tonic -> yield II (interval tonic)                   
                  | II supertonic -> yield III (interval supertonic)                 
                  | III mediant -> yield IV( interval mediant)                   
                  | IV subdominant -> yield V (interval subdominant)                   
                  | V dominant -> yield VI (interval dominant)                   
                  | VI submediant -> yield VII (interval submediant)                   
                  | VII leadingtone -> yield I (interval leadingtone)
                 ])
                 [ I tonic]
                 
let value =
    function
    | I tonic -> tonic
    | II supertonic -> supertonic
    | III mediant -> mediant
    | IV subdominant -> subdominant
    | V dominant -> dominant
    | VI submediant -> submediant
    | VII leadingtone -> leadingtone


let rootThirdFifth =
    function
    | [I tonic; II _; III mediant; IV _; V dominant; VI _; VII _; I _] ->
        (tonic,mediant,dominant)

let buildMajorScale = buildScale majorScale
let buildMinorScale = buildScale minorScale
let buildDiminishedScale = buildScale diminshedScale

let getI  =
    function 
    | [x; _; _; _; _; _; _; _] -> Some x
    | _ -> None 

let getII  =
    function 
    | [_; x; _; _; _; _; _; _] -> Some x
    | _ -> None 

    
let getIII =
    function
    | [_; _; x; _; _; _; _; _] -> Some x
    | _ -> None
    
let getIV =
    function
    | [_; _; _; x; _; _; _; _] -> Some x
    | _ -> None
    
let getV =
    function
    | [_; _; _; _; x; _; _; _] -> Some x
    | _ -> None
    
let getVI =
    function
    | [_; _; _; _; _; x; _; _] -> Some x
    | _ -> None
    
let getVII =
    function
    | [_; _; _; _; _; _; x; _] -> Some x
    | _ -> None 
        
let buildRootThirdFifth scaleDegreeWithQuality =
    let (scaleDegree, chordQuality) = scaleDegreeWithQuality
    let note = scaleDegree |> value
    match chordQuality with
    | Major -> buildMajorScale note |> rootThirdFifth
    | Minor -> buildMinorScale note |> rootThirdFifth
    | Diminished -> buildDiminishedScale note |> rootThirdFifth
    


let buildChordForScaleDegree scale degreeFromString =
    match degreeFromString with
    | "I" -> scale |> getI
    | "II" -> scale |> getII
    | "III" -> scale |> getIII
    | "IV" -> scale |> getIV
    | "V" -> scale |> getV
    | "VI" -> scale |> getVI
    | "VII" -> scale |> getVII
    | _ -> None 
    |> Option.map buildRootThirdFifth


let noteFromString =
    function 
    | "C" -> Some C
    | "C#" | "Db" -> Some Db
    | "D" -> Some D
    | "D#" | "Eb" -> Some Eb
    | "E" -> Some E
    | "F" -> Some F
    | "F#" | "Gb" -> Some Gb
    | "G" -> Some G
    | "G#" | "Ab" -> Some Ab
    | "A" -> Some A
    | "A#" | "Bb" -> Some Bb
    | "B" -> Some B
    | _ -> None


let relativeNaturalMinorScale: RelativeNaturalMinor =
    fun scale ->
        buildScale minorScale (scale.[5] |> value)
        
let majorPentatonicScale: PentatonicScale =
    fun scale ->
        match scale with
        | [I tonic; II supertonic; III mediant; IV _; V dominant; VI submediant; VII _; I _] ->
            Some [I tonic; II supertonic; III mediant; V dominant; VI submediant]
        | _ -> None
        
let minorPentatonicScale: PentatonicScale =
    fun scale ->
        match scale with
        | [I tonic; II _; III mediant; IV subdominant ; V dominant; VI _; VII leadingtone; I _] ->
            Some [I tonic; III mediant; IV subdominant; V dominant; VII leadingtone]
        | _ -> None 

let CMajor = C |> buildMajorScale
let CMajorWithChordQuality = (CMajor, qualityOfMajorScale) ||> List.zip
let buildChordsFromProgression = buildChordForScaleDegree CMajorWithChordQuality
["I";"IV";"V"] |> List.map buildChordsFromProgression |> ignore