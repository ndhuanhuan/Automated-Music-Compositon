module Basic where
import Data.Ratio
import Data.List hiding (transpose)
import Data.Char(toLower, toUpper)
import System.Random
import Dist
import System.IO
import qualified Data.MarkovChain as M

infixr 5 :+:, :=:

type Oct = Int                          --Octave, the range of music notes of a scale. In the case of piano, there're total 7 octaves.
type Pitch = (PitchC, Oct)              --Pitch has a pitch class and octave number, basically every note has a unique pitch
type Duration   = Rational              --Duration, the amount of time a note or rest would last
type AbsolutePitch = Int                --Absolute Pitch. Every note has an unique value describing its location in the entire music range
type Volume = Int                       --Volume

--Basic Data Type	
--Building blocks of music 
data Basics a  =  Note Duration a        --Note has a duration and a pitch  
                  |  Rest Duration          
     deriving (Show, Eq, Ord)
     
     
--Pitch Class
--Basically, every pitch in an octave will have a falling tone and a rising tone
--That is a flat variation and a sharp variation
data PitchC  =   Cf | C | Cs | Df | D | Ds 
                 | Ef | E | Ff | Es | F | Fs
                 | Gf | G | Gs | Af | A | As 
                 | Bf | B | Bs
     deriving (Show, Eq, Ord, Read, Enum, Bounded)
     
     
--Scale Data Type
data Scale        = Major | Minor         -- diatonic scale, major and minor
 deriving (Show, Eq, Ord)
     
--Control Data Type 
data Ctrl =
          Tempo       Rational           -- tempo describe the pace of music
       |  Transpose   AbsolutePitch      -- transposition means moving a collection of notes by a constant pitch interval(AbsolutePitch)
       |  Instrument  InstrumentMIDI     -- instrument label
       |  KeySignature  PitchC Scale     -- key signature conventions.This would looks something like "C Major"
  deriving (Show, Eq, Ord)               -- more info: http://en.wikipedia.org/wiki/Key_signature


--Instrument Data Type(General MIDI). 
--ref:http://en.wikipedia.org/wiki/General_MIDI
data InstrumentMIDI =
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta 
  |  Glockenspiel           | MusicBox               | Vibraphone  
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan 
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1   
  |  SynthBass2             | Violin                 | Viola  
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax 
  |  TenorSax               | BaritoneSax            | Oboe  
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle 
  |  Shanai                 | TinkleBell             | Agogo  
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  |  Custom String
  deriving (Show, Eq, Ord)
   
   
   
--Music Data Type
data Music a  = 
       Prim (Basics a)                  -- primitive value,      either a note of a rest 
    |  Music a :+: Music a              -- play sequentially
    |  Music a :=: Music a              -- play simultaneously
    |  Mod Ctrl (Music a)               -- modifier
     deriving (Show)
   

--functions that help to write certain kind of musical values
note            :: Duration -> a -> Music a
note d p        = Prim (Note d p)                           --describe the pitch and duration of a note

rest            :: Duration -> Music a                      --rest only has a duration
rest d          = Prim (Rest d)

tempo           :: Duration -> Music a -> Music a           --describe the pace of music
tempo d m       = Mod (Tempo d) m

transpose       :: AbsolutePitch -> Music a -> Music a      --describe the pitch interval of music transposition
transpose a m   = Mod (Transpose a) m

instrument      :: InstrumentMIDI -> Music a -> Music a     --describe the kind of instrument this music will be played on
instrument i m  = Mod (Instrument i) m

keysignature    :: PitchC -> Scale -> Music a -> Music a    --designate a set of notes that will be either sharpened or flattened from their nature tone
keysignature pc s m  = Mod (KeySignature pc s) m


--To specify a note, one need to provide its pitch class, on which octave does it land, and its duration 
cf,c,cs,df,d,ds,ef,e,es,ff,f,
  fs,gf,g,gs,af,a,as,bf,b,bs:: 
    Oct -> Duration -> Music Pitch
cf   o d = note d (Cf,   o); c    o d = note d (C,    o)
cs   o d = note d (Cs,   o); df   o d = note d (Df,   o)
d    o d = note d (D,    o); ds   o d = note d (Ds,   o)
ef   o d = note d (Ef,   o); e    o d = note d (E,    o)
es   o d = note d (Es,   o); ff   o d = note d (Ff,   o)
f    o d = note d (F,    o); fs   o d = note d (Fs,   o)
gf   o d = note d (Gf,   o); g    o d = note d (G,    o)
gs   o d = note d (Gs,   o); af   o d = note d (Af,   o)
a    o d = note d (A,    o); as   o d = note d (As,   o)  
bf   o d = note d (Bf,   o); b    o d = note d (B,    o)
bs   o d = note d (Bs,   o)


--to specify note or rest duration, one need to provide its value
--ref: http://en.wikipedia.org/wiki/Note_value 
bn, wn, hn, qn, en, sn, tn, sfn, dwn, dhn, 
    dqn, den, dsn, dtn :: Duration

bnr, wnr, hnr, qnr, enr, snr, tnr, dwnr, dhnr, 
     dqnr, denr, dsnr, dtnr :: Music Pitch

bn    = 2;     bnr    = rest bn     -- double whole note has value 2
wn    = 1;     wnr    = rest wn     -- whole note has value 1
dwn   = 3/2;   dwnr   = rest dwn    -- dotted whole has value 1 + 1/2
hn    = 1/2;   hnr    = rest hn     -- half note has value 1/2
dhn   = 3/4;   dhnr   = rest dhn    -- dotted half has value 1/2 + 1/4 
qn    = 1/4;   qnr    = rest qn     -- quarter note has value 1/4
dqn   = 3/8;   dqnr   = rest dqn    -- dotted quarter has value 1/4 + 1/8
en    = 1/8;   enr    = rest en     -- eighth note has value 1/8
den   = 3/16;  denr   = rest den    -- dotted eighth has value 1/8 + 1/16
sn    = 1/16;  snr    = rest sn     -- sixteenth note has value 1/16
dsn   = 3/32;  dsnr   = rest dsn    -- dotted sixteenth has value 1/16 + 1/32
tn    = 1/32;  tnr    = rest tn     -- thirty-second note has value 1/32
dtn   = 3/64;  dtnr   = rest dtn    -- dotted thirty-second has value 1/32 + 1/64
sfn   = 1/64;  sfnr   = rest sfn    -- sixty-forth note has value 1/64

--convert Pitch class to a Int
--According to music theory, each pitch class associates with a numeric number.
--ref:http://composertools.com/Theory/PCSets/PCSets1.htm
pcToInt     :: PitchC -> Int
pcToInt pc  = case pc of
  Cf  -> -1;  C  -> 0;   Cs  -> 1;     -- There are some overlapping numbers.It's natural.
  Df  -> 1;   D  -> 2;   Ds  -> 3;     --eg. In music theory, C sharp is enharmonic to D flat
  Ef  -> 3;   E  -> 4;   Es  -> 5;   
  Ff  -> 4;   F  -> 5;   Fs  -> 6;   
  Gf  -> 6;   G  -> 7;   Gs  -> 8;  
  Af  -> 8;   A  -> 9;   As  -> 10;  
  Bf  -> 10;  B  -> 11;  Bs  -> 12; 
  
  
--convert Pitch to Absolute Pitch
absolutePitch           :: Pitch -> AbsolutePitch
absolutePitch (pc,oct)  = 12*oct + pcToInt pc


--convert Absolute Pitch to Pitch 
pitch     :: AbsolutePitch -> Pitch
pitch ap  = 
    let (oct, n) = divMod ap 12
    in  ([C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n, oct)

    
--transposes pitches:
trans      :: Int -> Pitch -> Pitch
trans i p  = pitch (absolutePitch p + i)

--convert Int to Music Pitch
intToMusicPitch  :: Duration -> Int -> Music Pitch
intToMusicPitch d i = note d (pitch i)

--convert Pitches to Absolute Pitches
ptoAbspithes :: [Pitch ] -> [AbsolutePitch ]
ptoAbspithes ps = map absolutePitch ps

--convert Absolute Pitches to Pitches
abspToPitches :: [AbsolutePitch ] -> [Pitch ]
abspToPitches as = map pitch as

--convert Ints to Music Pitches
intToMusicPitches :: Duration-> [Int] -> Music Pitch
intToMusicPitches d (x:xs)= (note d (pitch x)):+:(intToMusicPitches d xs)
intToMusicPitches d []    = rest 0

intToMusicPitches' :: [Duration]-> [Int] -> Music Pitch
intToMusicPitches' (d:ds) (x:xs)= (note d (pitch x)):+:(intToMusicPitches' ds xs)
intToMusicPitches' _ []    = rest 0
intToMusicPitches' [] _    = rest 0

--line convert a list of notes into a line of music
--chord convert notes into a chord
line, chord :: [Music a] -> Music a
line  l = foldr (:+:) (rest 0) l   --line is played sequentially
chord  l = foldr (:=:) (rest 0) l   --chord is played simultaneously

--transform line to a list
lineToList                    :: Music a -> [Music a]
lineToList (Prim (Rest 0))    = []
lineToList (n :+: ns)         = n : lineToList ns

--delay a music by adding a rest
delayMusic      :: Duration -> Music a -> Music a
delayMusic d m  = rest d :+: m

--repeat music for specific times
repeatMusic      :: Int -> Music a -> Music a
repeatMusic 0 m  = rest 0
repeatMusic n m  = m :+: repeatMusic (n-1) m

--compute duration
compDuration                       :: Music a -> Duration
compDuration   (Prim (Note d _))     = d
compDuration  (Prim (Rest d))       = d
compDuration   (m1 :+: m2)           = compDuration   m1   +   compDuration   m2
compDuration   (m1 :=: m2)           = compDuration   m1 `max` compDuration   m2
compDuration   (Mod (Tempo r) m)  = compDuration   m / r
compDuration   (Mod _ m)          = compDuration   m

--reverses any Music value 
revMusic               :: Music a -> Music a
revMusic n@(Prim _)    = n
revMusic (Mod c m)  = Mod c (revMusic m)
revMusic (m1 :+: m2)   = revMusic m2 :+: revMusic m1
revMusic (m1 :=: m2)   =  
   let  d1 = compDuration m1
        d2 = compDuration m2
   in if d1>d2  then revMusic m1 :=: (rest (d1-d2) :+: revMusic m2)
                else (rest (d2-d1) :+: revMusic m1) :=: revMusic m2
