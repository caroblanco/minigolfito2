module Lib where
import Text.Show.Functions

bart = ("Bart","Homero",(25,60))
todd = ("Todd","Ned",(15,80))
rafa = ("Rafa","Gorgory",(10,1))
nombre (n,_,_) = n
padre (_,p,_) = p
habilidad (_,_,h) = h

type Jugador = (String, String, Habilidad)
type Habilidad = (Fuerza, Precision)
type Fuerza = Int
type Precision = Int

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b | f a >= f b = a
                 | otherwise = b

-------1
type Tiro = (Velocidad,Precision, Altura)
type Velocidad = Int
type Altura = Int

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = crearTiro 10 0 (((*2).snd) habilidad)

madera :: Palo
madera habilidad = crearTiro 100 5 (((div 2).snd) habilidad)

hierro :: Int -> Palo
hierro n habilidad = crearTiro (((*n).fst)habilidad) (min 0 (n-3)) (((div n).snd) habilidad) 

crearTiro :: Int -> Int -> Int -> Tiro
crearTiro velocidad altura precision = (velocidad, precision, altura)

palos = putter : madera : map hierro [1 .. 10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador


type Obstaculo = ( Tiro -> Bool , Tiro -> Tiro )

puedeSuperar :: Tiro -> Obstaculo -> Bool
puedeSuperar tiro obstaculo = (fst.obstaculo) tiro


laguna largo = ((\(v,_,a)-> v>80 && between 10 50 a),(\(v,p,a) -> (v,p,a `div` largo)))

tunelConRampita = ((\(_,p,a) -> p>90 && a==0), (\(v,_,a) -> (v*2,100,0)) )

hoyo = ((\(v,p,a) -> between 5 20 v && p>95 && a==0), (\ _ -> (0,0,0)))


palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles persona obstaculo = filter (condicionFiltrar obstaculo persona) palos

condicionFiltrar :: Obstaculo -> Jugador -> Palo -> Bool
condicionFiltrar obstaculo jugador palo = puedeSuperar (golpe jugador palo) obstaculo


nombresDeLosQuePuedenSuperarTodos :: [Jugador] -> [Obstaculo] -> [String]
nombresDeLosQuePuedenSuperarTodos jugadores obstaculos = filter fst (superanTodos jugadores obstaculos)

superanTodos :: [Jugador] -> [Obstaculo] -> [Jugador]
superanTodos