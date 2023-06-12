{- Ejercicio 2 -}
{- ================Televisor========================= -}
data PulgadasTele = VeintiCuatro | TreintayDos | Cuarenta | Setenta
data TipoPantalla = LED | QLED | OLED
{- ================HomeTheater================================== -}
type Potencia = Int
data TipoDeSalida = Mono | Estereo | CincoPUno
{- ==================Dispositivo================================ -}
type Precio = Int
data Dispositivo = Televisor PulgadasTele TipoPantalla Precio | HomeTheater Potencia TipoDeSalida Precio
{- ============================================================= -}
{- Defino variable para ejemplo -}
stock = [Televisor Cuarenta QLED 100, Televisor Cuarenta OLED 300, Televisor Setenta QLED 200, HomeTheater 50 Estereo 2000, HomeTheater 200 CincoPUno 600]


{- Instancias=================================================-}
instance Eq PulgadasTele where
    VeintiCuatro == VeintiCuatro = True
    TreintayDos == TreintayDos = True
    Cuarenta == Cuarenta = True
    Setenta == Setenta = True
    _ == _ = False
{-======================================================  -}
--b)
cuantosTelevisores :: [Dispositivo] -> PulgadasTele -> Int
cuantosTelevisores [] _ = 0
cuantosTelevisores (dispositivo:ld) pulgadas = case dispositivo of
    (Televisor p_tele _ _) | p_tele == pulgadas -> 1 + cuantosTelevisores ld pulgadas
    _ -> cuantosTelevisores ld pulgadas
--ejemplo: cuantosTelevisores stock Cuarenta = 2

--b)
{- Instancias -}
instance Eq Dispositivo where
    Televisor pulgadas1 _ precio1 == Televisor pulgadas2 _ precio2 = (precio1 == precio2) && (pulgadas1 == pulgadas2)
    HomeTheater _ _ p1 == HomeTheater _ _ p2 = p1 == p2
    _ == _ = False
--Ejemplo: Televisor VeintiCuatro LED 50  == Televisor VeintiCuatro QLED 50 = True
-- HomeTheater 1 Mono 100 == HomeTheater 2 Estereo 100 = True

--d)
--Defino variable para ejemplo
ejemplo_hay = [Televisor VeintiCuatro LED 50, Televisor VeintiCuatro LED 50, Televisor Setenta LED 20] -- false porque si hay ...
ejemplo_noHay = [Televisor VeintiCuatro LED 50,Televisor VeintiCuatro LED 10,Televisor VeintiCuatro LED 50] -- true porquen no hay ...
noHayDosIguales :: [Dispositivo] -> Bool
noHayDosIguales [] = True
noHayDosIguales [d1] = True 
noHayDosIguales (d1:d2:dispositivos) | d1 == d2 = False
                                    |otherwise = noHayDosIguales (d2:dispositivos)

{- ============================Ejercicio 3==================================== -}

{- NotasDelcuatri -}
type Nombre = String
type PrimerParcial = Int
type SegundoParcial = Int
type Recuperatorio = Int
data NotasDelCuatri = NotasDelAlumno Nombre PrimerParcial SegundoParcial Recuperatorio NotasDelCuatri  | NoHayMasNotas
{- Condicion -}
data Condicion = Regular | Libre | Promocional

--b)

esLibreAlumno :: NotasDelCuatri -> Nombre -> Bool
esLibreAlumno NoHayMasNotas _ = False
esLibreAlumno (NotasDelAlumno nombre parcial1 parcial2 recup _notas) n | nombre == n = (parcial1 < 6 || parcial2 < 6) && (recup < 6) 
                                                                    |otherwise = esLibreAlumno _notas n
--esLibreAlumno notasDelProfe "Armando Esteban Quito" --True 
--esLibreAlumno notasDelProfe "Tomas Albino Blanco" --False

--c)
notasDelProfe = NotasDelAlumno "Tomas Albino Blanco" 7 4 6 (NotasDelAlumno "Armando Labolita" 8 8 0 (NotasDelAlumno "Armando Esteban Quito" 6 5 5 NoHayMasNotas) )
promediar :: Int -> Int -> Int -> Int
promediar p1 p2 r | (p1>=6 && p2>=6) || (p1 < 6 && p2 < 6) = div (p1+p2) 2
                  | p1 < 6 = div (p2+r) 2
                  | p2 < 6 = div (p1+r) 2

devolverPromedio :: NotasDelCuatri -> String -> Maybe Int
devolverPromedio NoHayMasNotas _ = Nothing
devolverPromedio (NotasDelAlumno nombre parcial1 parcial2 recup _notas) n | nombre == n = Just (promediar parcial1 parcial2 recup) 
                                                                        |otherwise = devolverPromedio _notas n

--devolverPromedio notasDelProfe "Tomas Albino Blanco" --Just 6 
--Main> devolverPromedio notasDelProfe "Armando Labolita" --Just 8
--Main> devolverPromedio notasDelProfe "Armando Esteban Quito" --Just 5 
--Main> devolverPromedio notasDelProfe "Armando Esteban" --Nothing

{- Ejercicio 1 -}
data Color = Rojo | Verde| Azul | Negro | Blanco 

-- Programar la funcion usando pattern matching

mismoColor :: Color -> Color -> Bool
mismoColor Rojo Rojo = True
mismoColor Verde Verde = True
mismoColor Azul Azul = True
mismoColor Negro Negro = True
mismoColor Blanco Blanco = True
mismoColor _ _ = False

--b)
{- Sinonimos de tipo -}
type Nombre = String
type Costo = Int
type Dano = Int
type Resistencia = Int
{- Definicion de Tipos -}
data CartaMagic = CartaDeTerreno Nombre Color | CartaDeCriatura Nombre Costo Dano Resistencia deriving Eq


--c)

cuantoDano :: CartaMagic -> Int
cuantoDano (CartaDeTerreno _ _) = 0
cuantoDano (CartaDeCriatura _ _ dano _) = dano
--d)
instance Eq Color where
    Rojo == Rojo = True
    Verde == Verde = True
    Azul == Azul = True
    Negro == Negro = True
    Blanco == Blanco = True
    _ == _ = False
instance Ord CartaMagic where
    (CartaDeCriatura n1 c1 dano1 r1) > (CartaDeCriatura n2 c2 dano2 r2) = cuantoDano(CartaDeCriatura n1 c1 dano1 r1) > cuantoDano(CartaDeCriatura n2 c2 dano2 r2)

{- Ejercicio 2 -}

soloTerreno :: [CartaMagic] -> Color -> [Nombre]
soloTerreno [] _ = []
soloTerreno (carta:ns) c = case carta of
    CartaDeTerreno nombre color | color == c -> nombre : soloTerreno ns c
    _ -> soloTerreno ns c 

--b)[(CartaDeTerreno "Lobo Piola" Rojo) ,(CartaDeTerreno "Alaskan" Rojo),(CartaDeCriatura "Oso Pardo" 100 17 89)]

--c) soloTerreno [(CartaDeTerreno "Lobo Piola" Rojo) ,(CartaDeTerreno "Alaskan" Rojo),(CartaDeCriatura "Oso Pardo" 100 17 89)] Rojo = ["Lobo Piola","Alaskan"]

{- Ejercicio 3 -}
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)

la_menores :: (Ord b) => ListaAsoc a b -> b -> ListaAsoc a b
la_menores Vacia _ = Vacia
la_menores (Nodo a b lista) x | x > b = Nodo a b (la_menores lista x)
                            |otherwise = la_menores lista x