import Basic
import Algo
import Dist
--cascade recursion algorithm examples-------------------------------------------------
--how to use: prelude > final
--            prelude > final'
--changing parameters can create some fancy cascade music melodies
run       = cas (transpose 5) (delayMusic tn) 8 (c 4 tn)
cascade   = cas (transpose 4) (delayMusic en) 8 run
cascades  = cas  id           (delayMusic sn) 2 cascade
final = cascades :+: revMusic cascades
run'       = cas (delayMusic tn) (transpose 5) 8 (c 4 tn)
cascade'   = cas (delayMusic en) (transpose 4) 8 run'
cascades'  = cas (delayMusic sn)  id           2 cascade'
final'     = cascades' :+: revMusic cascades'



--Sorting algorithm based composing examples-----------------------------------------------
--how to use: prelude > exp1
--            prelude > exp2
--            prelude > exp3 
ts1 = [55,61,64,55,61,64,57,62,66,57,62,66,55,62,66,55,62,66,55,61,64,55,61,64,57,64,69,57,64,69]

exp1 = instrument MusicBox (tempo 1(insertGen sn ts1 300))           --insert sort composing example
exp2 = instrument MusicBox (tempo 1(msortGen' sn ts1 300))           --merge sort composing example
exp3 = instrument MusicBox (tempo 1 (selectSortGen sn ts1))          --select sort composing example




--Random Permutation Composition example---------------------------------------------------
--how to use: prelude > exp'
exp' = instrument MusicBox (tempo 1 (finalPermuGen sn (take 9 ts1) 300)) 





--Euclid's algorithm composing examples------------------------------------------------------------
--how to use: prelude > ee1
--            prelude > ee2
ee1 = selfGCDGen sn 21 50
ee2 = selfGCDGen sn 30 1000





--Chinese Restaurant composing algorithm examples--------------------------------------------------
--how to use: prelude > eeee1

eeee1 = chiRestGen sn sGen 50




--self-similar composing examples------------------------------------------------------
--how to use: prelude > ss1
--            prelude > ss2
--            ...
--            prelude > ss6    
m0   :: [AbsNote]
m0   = [(1,2),(1,0),(1,5),(1,7)]

m1   :: [AbsNote]
m1   = [(0.5,0),(0.5,0),(1,0)]

m2    :: [AbsNote]
m2    = [(hn,5),(qn,6),(qn,10),(hn,6)]

m3    :: [AbsNote]
m3    = [(en,3),(en,8),(hn,22),(qn,4),(qn,7),(qn,21)]


ss1  = finalSS m0 4 50 (1/8)
ss2  = finalSS m1 4 50 (1/2)
ss3  = finalSS m2 3 50 2
ss4  = finalSS m3 4 45 (1/500)




--Random music composing(distribution based) examples----------------------------------
--how to use: prelude > sss1
--            prelude > sss2
--            ...
--            prelude > sss4 

sss1= m1'				-- uniform distribution example
sss2= m2'				-- linear distribution example
sss3= m3' 0.1           -- exponential distribution example. You can try other parameters such as 2, 10 ,50 
sss4= m4' 1.4 0.5       -- Normal distribution example




--Markov chain composing examples-----------------------------------------------------
--how to use: prelude > ssss1
--            prelude > ssss2
--            ...
--            prelude > ssss5

-- some sample training sequences
ps0,ps1,ps2 :: [Pitch]
ps0  = [(C,4), (D,4), (E,4)]
ps1  = [(C,4), (D,4), (E,4), (F,4), (G,4), (A,4), (B,4)]
ps2  = [  (C,4), (E,4), (G,4), (E,4), (F,4), (A,4), (G,4), (E,4),
          (C,4), (E,4), (G,4), (E,4), (F,4), (D,4), (C,4)]

bach = [(G,4), (Cs,5), (E,5), (G,4), (Cs,5), (E,5),
        (G,4), (Cs,5), (E,5), (G,4), (Cs,5), (E,5),
        (A,4), (D,5), (Fs,5), (A,4), (D,5), (Fs,5),
        (A,4), (D,5), (Fs,5), (A,4), (D,5), (Fs,5),
        (G,4), (D,5), (Fs,5), (G,4), (D,5), (Fs,5),
        (G,4), (D,5), (Fs,5), (G,4), (D,5), (Fs,5)]

--examples
ssss1 = markovChain' [ps1, reverse ps1 ] 1
ssss2 = markovChain' [ps0, ps2] 1
ssss3 = markovChain ps2 1
ssss4 = markovChain' [bach, reverse bach] 1
ssss5 = markovChain bach 1




