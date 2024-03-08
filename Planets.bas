ON ERROR: ON ERROR OFF: MODE 26:PRINT REPORT$;" at line ";ERL'';"r1%=";r1%;" r2%=";r2%;" p(p,5)=";p(p,5);" r3%=";r3%;" r4%=";r4%:END
MODE 27
MODE 26:OFF
REM Number of planets in Solar System
solarplnum%=10

REM Number of Satellites around each planet, excluding Mercury and Venus
eartsatnum%=1
marssatnum%=2
jupisatnum%=22:REM 16 satellites and 6 ring boundaries
satusatnum%=31:REM 18 satellites and 13 ring boundaries
uransatnum%=25:REM 15 satellites and 10 ring boundaries
neptsatnum%=8
plutsatnum%=1

DIM p(solarplnum%,6),p$(solarplnum%)
DIM s(2,6),s$(2)
DIM eartsat(1,6),eartsat$(1)
DIM marssat(2,6),marssat$(2)
DIM jupisat(jupisatnum%,6),jupisat$(jupisatnum%)
DIM satusat(satusatnum%,6),satusat$(satusatnum%)
DIM uransat(uransatnum%,6),uransat$(uransatnum%)
DIM neptsat(neptsatnum%,6),neptsat$(neptsatnum%)
DIM plutsat(1,6),plutsat$(1)
DIM ll(2,2),mm(2),nn(2),oo(2,2),pp(2),qq(2)
orgx%=640
orgy%=480
orgxdif%=-640
orgydif%=-480
planetnames%=1
orbitdraw%=1
RESTORE
COLOUR 1,0,255,0
COLOUR 2,255,255,0
COLOUR 3,80,80,80

FOR n=1 TO solarplnum%
 READ p$(n),p(n,1),p(n,2),sss,p(n,4),p(n,5),p(n,6)
 p(n,3)=0
NEXT n

FOR n=1 TO eartsatnum%
 READ eartsat$(n),eartsat(n,1),eartsat(n,2),sss,eartsat(n,4),eartsat(n,5),eartsat(n,6)
 eartsat(n,3)=0
NEXT n

FOR n=1 TO marssatnum%
 READ marssat$(n),marssat(n,1),marssat(n,2),sss,marssat(n,4),marssat(n,5),marssat(n,6)
 marssat(n,3)=0
 marssat(n,1)=marssat(n,1)+p(4,4)
NEXT n

FOR n=1 TO jupisatnum%
 READ jupisat$(n),jupisat(n,1),jupisat(n,2),sss,jupisat(n,4),jupisat(n,5),jupisat(n,6)
 jupisat(n,3)=0
 jupisat(n,1)=jupisat(n,1)+p(5,4)
NEXT n

FOR n=1 TO satusatnum%
 READ satusat$(n),satusat(n,1),satusat(n,2),sss,satusat(n,4),satusat(n,5),satusat(n,6)
 satusat(n,3)=0
 satusat(n,1)=satusat(n,1)+p(6,4)
NEXT n
 
FOR n=1 TO uransatnum%
 READ uransat$(n),uransat(n,1),uransat(n,2),sss,uransat(n,4),uransat(n,5),uransat(n,6)
 uransat(n,3)=0
 uransat(n,1)=uransat(n,1)+p(7,4)
NEXT n

FOR n=1 TO neptsatnum%
 READ neptsat$(n),neptsat(n,1),neptsat(n,2),sss,neptsat(n,4),neptsat(n,5),neptsat(n,6)
 neptsat(n,3)=0
 neptsat(n,1)=neptsat(n,1)+p(8,4)
NEXT n
REM 
REM FOR n=1 TO plutsatnum%
REM  READ plutsat$(n),plutsat(n,1),plutsat(n,2),sss,plutsat(n,4),plutsat(n,5),plutsat(n,6)
REM  plutsat(n,3)=0
REM NEXT n

S1=1:S2=2
ORIGIN orgx%,orgy%
z=1/13
t=100
e=0
i=1
frames%=0
timer%=TIME
REPEAT
frames%=frames%+1
VDU 5
WAIT:SYS 6,112,S1:SYS 6,113,S2:CLS:SWAP S1,S2
GCOL 2
CIRCLEFILL 0,0,0.7*z
FOR p=1 TO 10
p(p,3)=p(p,3)+(t/p(p,2))

r1% = p(p,1)*z
r2% = p(p,1)*z*COSRADi

IF orbitdraw%=1 THEN
  IF r1%<1600 OR r2%<1600 THEN

    GCOL 3
    IF r1%=r2% THEN
      CIRCLE 0,0,r1%
    ELSE
      IF r2%<>0 THEN
        IF r1%<>0 AND r2%<>0 THEN
          ELLIPSE 0,0,r1%,r2%,((p(p,5)*PI)/180)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

REM PLANET MATRICES

REM Transition Matrix Construction
ll(1,1)=COSRADp(p,5)
ll(1,2)=-SINRADp(p,5)
ll(2,1)=SINRADp(p,5)
ll(2,2)=COSRADp(p,5)

REM Start Vector for Transformation
mm(1)=SINRADp(p,3)*r1%
mm(2)=COSRADp(p,3)*r2%

REM Matrix Multiplication
nn()=ll().mm()

IFplanetnames%=1 THEN
  IF z>0.009 THEN
    IF z>0.2 THEN
      GCOL 1
      MOVE nn(1)+5,nn(2)+5
      PRINT p$(p)
    ELSE
      IF p>4 THEN
        GCOL 1
        MOVE nn(1)+5,nn(2)+5
        PRINT p$(p)
      ENDIF
    ENDIF
  ENDIF
ENDIF
GCOL 2
opx=nn(1)+orgx%
opy=nn(2)+orgy%
IF opx>=0 AND opx<=1280 AND opy>=0 AND opy<=1024 THEN
  MOVE SINRADp(p,3)*r1%,COSRADp(p,3)*r2%
  GCOL 0
  CIRCLE nn(1),nn(2),(p(p,4)*z)+2
  GCOL 2
  CIRCLE FILL nn(1),nn(2),p(p,4)*z:REM SINRADp(p,3)*r1%,COSRADp(p,3)*r2%,p(p,4)*z
  REM DRAW SINRADp(p,3)*r1%,COSRADp(p,3)*r2%
  
ELSE
  IF opx<0 AND opy<0 THEN
    GCOL 2
    MOVE 0+orgxdif%,0+orgydif%
    DRAW 20+orgxdif%,20+orgydif%
    MOVE 25+orgxdif%,25+orgydif%
    PRINT p$(p)
  ENDIF
  :
  IF opx>1280 AND opy>960 THEN
    GCOL 2
    MOVE 1280+orgxdif%,960+orgydif%
    DRAW 1260+orgxdif%,940+orgydif%
    MOVE (1260-(LEN(p$(p))*16))+orgxdif%,919+orgydif%
    PRINT p$(p)
  ENDIF
  :
  IF opx>0 AND opx<1280 AND opy<0 THEN
    GCOL 2
    MOVE opx+orgxdif%,0+orgydif%
    DRAW opx+orgxdif%,10+orgydif%
    MOVE opx+orgxdif%,15+orgydif%
    PRINT p$(p)
  ENDIF
  :
  IF opx>1280 AND opy>0 AND opy<960 THEN
    GCOL 2
    MOVE 1280+orgxdif%,opy+orgydif%
    DRAW 1270+orgxdif%,opy+orgydif%
    MOVE (1265-(LEN(p$(p))*16))+orgxdif%,(opy+8)+orgydif%
    PRINT p$(p)
  ENDIF
  :
  IF opx>0 AND opx<1280 AND opy>960 THEN
    GCOL 2
    MOVE opx+orgxdif%,960+orgydif%
    DRAW opx+orgxdif%,950+orgydif%
    MOVE opx+orgxdif%,945+orgydif%
    PRINT p$(p)
  ENDIF
  :
  IF opx<0 AND opy>0 AND opy<960 THEN
    GCOL 2
    MOVE 0+orgxdif%,opy+orgydif%
    DRAW 10+orgxdif%,opy+orgydif%
    MOVE 15+orgxdif%,(opy+orgydif%)+8
    PRINT p$(p)
  ENDIF
  :
  IF opx>1280 AND opy<0 THEN
    GCOL 2
    MOVE 1280+orgxdif%,0+orgydif%
    DRAW 1270+orgxdif%,10+orgydif%
    MOVE (1260-(LEN(p$(p))*16))+orgxdif%,15+orgydif%
    PRINT p$(p)
  ENDIF
  :
  IF opx<0 AND opy>960 THEN
    GCOL 2
    MOVE 0+orgxdif%,960+orgydif%
    DRAW 10+orgxdif%,950+orgydif%
    MOVE 15+orgxdif%,945+orgydif%
    PRINT p$(p)
  ENDIF
ENDIF
NEXT p
f=360/(t/365)
e=e+(1/f)
VDU 4
@%="+F8.2"
PRINT TAB(0,0);e;" years"
PRINTTAB(0,1);"FPS:";(frames%/(TIME-timer%))*100
IF INKEY(-58) THEN z=z*1.05          : REM Up Arrow
IF INKEY(-42) THEN z=z/1.05          : REM Down Arrow
IF INKEY(-122) THEN t=t*1.05:REM *1.05         : REM Right Arrow
IF INKEY(-26) THEN  t=t/1.05:REM 1.05         : REM Left Arrow
IF INKEY(-80) THEN i=i-2          : REM '"
IF INKEY(-105) THEN i=i+2         : REM /?
IF INKEY(-98) THEN orgx%=orgx%+25:orgxdif%=orgxdif%-25   : REM Z
IF INKEY(-67) THEN orgx%=orgx%-25:orgxdif%=orgxdif%+25   : REM X
IF INKEY(-88) THEN orgy%=orgy%-25:orgydif%=orgydif%+25   : REM ;:
IF INKEY(-104) THEN orgy%=orgy%+25:orgydif%=orgydif%-25  : REM .>
IF INKEY(-102) THEN PROCmercury     : REM M for Mercury
IF INKEY(-100) THEN PROCvenus       : REM V for Venus
IF INKEY(-35) THEN PROCearth        : REM E for Earth
IF INKEY(-66) THEN PROCmars         : REM A for Mars
IF INKEY(-70) THEN PROCjupiter      : REM J for Jupiter
IF INKEY(-82) THEN PROCsaturn       : REM S for Saturn
IF INKEY(-54) THEN PROCuranus       : REM U for Uranus
IF INKEY(-86) THEN PROCneptune      : REM N for Neptune
IF INKEY(-56) THEN PROCpluto        : REM P for Pluto
IF INKEY(-36) THEN INPUTTAB(0,1)"Time : "t : REM T for time set
IF INKEY(-99) THEN
  CASE planetnames% OF
    WHEN 1
      planetnames%=0
    WHEN 0
      planetnames%=1
  ENDCASE
ENDIF
IF INKEY(-55) THEN
  CASE orbitdraw% OF
    WHEN 1
      orbitdraw%=0
    WHEN 0
      orbitdraw%=1
  ENDCASE
ENDIF

ORIGIN orgx%,orgy%
IF i<0 THEN i=0
IF i>90 THEN i=90
:
m=(1280/z)
PRINT TAB(0,58);z
PRINT TAB(0,57);t
@%="+F8.1"
PRINT TAB(0,59);m;" million km";
UNTIL 0=1

REM Planets
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt AngSun  Tilt AngEqu
DATA Mercury,     58.34322,  87.969,     0.38,   0.0049,    7,           0
DATA Venus,       107.71056, 224.701,    0.95,   0.0121,    3.39,        0
DATA Earth,       149.598,   365.256,    1,      0.012756,  0,           0
DATA Mars,        227.38896, 686.980,    0.53,   0.0068,    1.85,        0
DATA Jupiter,     777.9096,  4332.59,    11.2,   0.143,     1.30,        0
DATA Saturn,      1427.16492,10759.20,   9.5,    0.12,      2.49,        0
DATA Uranus,      2872.2816, 30684.9,    4,      0.052,     0.77,        0
DATA Neptune,     4502.8998, 60190.3,    3.8,    0.048,     1.77,        0
DATA Pluto,       5909.121,  90465.0,    0.18,   0.003,     17.2,        0
DATA Planet X,    9800.0000, 219000.0,   0,      0.04,      0,           0

REM  Satellites
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
REM  EARTH's Satellites
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA Moon,         0.384400,  27.321661, 0,      0.003476,  23.45,      3.1
REM  MARS's Satellites
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA Phobos,       0.009270,   0.3189,   0,      0.000023,   1.1,       4.1
DATA Deimos,       0.023400,   1.2624,   0,      0.000013,   1.8,       4.2
REM  JUPITER's Satellites
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA Metis,        0.127960,   0.295,    0,      0.000040,   0,         5.1
DATA Adrastea,     0.128980,   0.298,    0,      0.000020,   0,         5.2
DATA Amalthea,     0.181300,   0.498,    0,      0.000175,   0.45,      5.3
DATA Thebe,        0.221900,   0.675,    0,      0.000100,   0.9,       5.4
DATA Io,           0.421600,   1.769,    0,      0.003637,   0.04,      5.5
DATA Europa,       0.670900,   3.551,    0,      0.003130,   0.47,      5.6
DATA Ganymede,     1.070000,   7.155,    0,      0.005268,   0.21,      5.7
DATA Callisto,     1.880000,  16.689,    0,      0.004806,   0.51,      5.8
DATA Leda,        11.094000, 238.7,      0,      0.000010,  26.1,       5.9
DATA Himalia,     11.480000, 250.6,      0,      0.000170,  27.6,       5.10
DATA Lysithea,    11.720000, 259.2,      0,      0.000024,  29.0,       5.11
DATA Elara,       11.737000, 259.7,      0,      0.000080,  24.8,       5.12
DATA Ananke,      21.200000, 631,        0,      0.000020, 147,         5.13
DATA Carme,       22.600000, 692,        0,      0.000030, 164,         5.14
DATA PasiphaMODE,    23.500000, 735,        0,      0.000036, 145,         5.15
DATA Sinope,      23.700000, 758,        0,      0.000028, 153,         5.16
REM JUPITER's Rings
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA ,             0.100000,   0,        0,      0.000001,   1.30,      5.R1close
DATA ,             0.122800,   0,        0,      0.000001,   1.30,      5.R1far
DATA ,             0.122800,   0,        0,      0.000001,   1.30,      5.R2close
DATA ,             0.129200,   0,        0,      0.000001,   1.30,      5.R2far
DATA ,             0.129200,   0,        0,      0.000001,   1.30,      5.R3close
DATA ,             0.214200,   0,        0,      0.000001,   1.30,      5.R3far
REM SATURN's Satellites
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA Pan,          0.133600,   0.570,    0,      0.000020,   0.00,       6.1
DATA Atlas,        0.137670,   0.602,    0,      0.000034,   0.30,       6.2
DATA Prometheus,   0.139350,   0.613,    0,      0.000100,   0.00,       6.3
DATA Pandora,      0.141700,   0.629,    0,      0.000088,   0.10,       6.4
DATA Janus,        0.151470,   0.695,    0,      0.000110,   0.10,       6.5
DATA Epimetheus,   0.151420,   0.694,    0,      0.000190,   0.30,       6.6
DATA Mimas,        0.185540,   0.942,    0,      0.000395,   1.52,       6.7
DATA Enceladus,    0.238040,   1.370,    0,      0.000495,   0.02,       6.8
DATA Tethys,       0.294670,   1.888,    0,      0.001046,   1.86,       6.9 
DATA Telesto,      0.294670,   1.888,    0,      0.000025,   2.00,       6.10
DATA Calypso,      0.294670,   1.888,    0,      0.000016,   2.00,       6.11
DATA Dione,        0.377420,   2.737,    0,      0.001120,   0.02,       6.12
DATA Helene,       0.377420,   2.737,    0,      0.000035,   0.20,       6.13
DATA Rhea,         0.527040,   4.518,    0,      0.001528,   0.35,       6.14
DATA Titan,        1.221860,  15.945,    0,      0.005150,   0.33,       6.15
DATA Hyperion,     1.481100,  21.277,    0,      0.000280,   0.43,       6.16
DATA Iapetus,      3.561300,  79.331,    0,      0.001436,   7.52,       6.17
DATA PhCOSbe,       12.954000, 550.400,    0,      0.000220, 175.00,       6.18
REM SATURN's Rings
REM  Name                       Distance  Orbit Time  Nil  Diameter  Tilt Angle  PlNo
DATA ,                          0.067000, 0,          0,   0.00001,  2.49,    19:6.R1
DATA ,                          0.073200, 0,          0,   0.00001,  2.49,    20:6.R2
DATA ,                          0.087500, 0,          0,   0.00001,  2.49,    21:6.R3
DATA ,                          0.092200, 0,          0,   0.00001,  2.49,    22:6.R4
DATA ,                          0.117500, 0,          0,   0.00001,  2.49,    23:6.R5
DATA ,                          0.119000, 0,          0,   0.00001,  2.49,    24:6.R6
DATA ,                          0.121000, 0,          0,   0.00001,  2.49,    25:6.R7
DATA ,                          0.133500, 0,          0,   0.00001,  2.49,    26:6.R8
DATA ,                          0.136530, 0,          0,   0.00001,  2.49,    27:6.R9
DATA ,                          0.135200, 0,          0,   0.00001,  2.49,    28:6.R10
DATA ,                          0.140600, 0,          0,   0.00001,  2.49,    29:6.R11
DATA ,                          0.170000, 0,          0,   0.00001,  2.49,    30:6.R12
DATA ,                          0.230000, 0,          0,   0.00001,  2.49,    31:6.R13
REM URANUS's Satellites
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA Cordelia,    0.049471,  0.330,      0,      0.000026,  0,          7.1
DATA Ophelia,     0.053796,  0.372,      0,      0.000030,  0,          7.2
DATA Bianca,      0.059173,  0.433,      0,      0.000042,  0,          7.3
DATA Cressida,    0.061777,  0.463,      0,      0.000062,  0,          7.4
DATA Desdemona,   0.062676,  0.475,      0,      0.000054,  0,          7.5
DATA Juliet,      0.064352,  0.493,      0,      0.000084,  0,          7.6
DATA Portia,      0.066085,  0.513,      0,      0.000108,  0,          7.7
DATA Rosalind,    0.069941,  0.558,      0,      0.000054,  0,          7.8
DATA Belinda,     0.075258,  0.622,      0,      0.000066,  0,          7.9
DATA Puck,        0.086000,  0.762,      0,      0.000154,  0,          7.10
DATA Miranda,     0.129400,  1.414,      0,      0.000472,  0,          7.11
DATA Ariel,       0.191000,  2.520,      0,      0.001158,  0,          7.12
DATA Umbriel,     0.266300,  4.144,      0,      0.001169,  0,          7.13
DATA Titania,     0.435000,  8.706,      0,      0.001578,  0,          7.14
DATA Oberon,      0.583500, 13.463,      0,      0.001523,  0,          7.15
REM URANUS's Rings
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA ,      0.041837,  0,          0,      0.00001,   0.06,       16:7.R1
DATA ,      0.042234,  0,          0,      0.00001,   0.05,       17:7.R2
DATA ,      0.042570,  0,          0,      0.00001,   0.03,       18:7.R3
DATA ,      0.044718,  0,          0,      0.00001,   0.01,       19:7.R4
DATA ,      0.045661,  0,          0,      0.00001,   0.005,      20:7.R5
DATA ,      0.047175,  0,          0,      0.00001,   0.001,      21:7.R6
DATA ,      0.047630,  0,          0,      0.00001,   0.11,       22:7.R7
DATA ,      0.048320,  0,          0,      0.00001,   0.04,       23:7.R8
DATA ,      0.050023,  0,          0,      0.00001,   0.0,        24:7.R9
DATA ,      0.051149,  0,          0,      0.00001,   0.005,      25:7.R10
REM NEPTUNE's Satellites                 
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA Naiad,       0.048000,  0.296,      0,      0.000054,  0,          8.1
DATA Thalassa,    0.050000,  0.312,      0,      0.000080,  0,          8.2
DATA Despina,     0.052500,  0.333,      0,      0.000180,  0,          8.3
DATA Galatea,     0.062000,  0.429,      0,      0.000150,  0,          8.4
DATA Larissa,     0.073600,  0.554,      0,      0.000192,  0,          8.5
DATA Proteus,     0.117600,  1.121,      0,      0.000416,  4.5,        8.6
DATA Triton,      0.354800,  5.877,      0,      0.002705,159.9,        8.7
DATA Nereid,      5.514000,360.160,      0,      0.000240, 27.2,        8.8
REM NEPTUNE's Rings - Galle, Le Verrier, 'Plateau', Adams
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number
DATA ,       0.041900,  0,          0,      0.00001,   1.77,       9:8.R1
DATA ,  0.053200,  0,          0,      0.00001,   1.77,      10:8.R2
DATA ,   0.055000,  0,          0,      0.00001,   1.77,      11:8.R3
DATA ,            0.062000,  0,          0,      0.00001,   1.77,      12:8.R4
DATA ,       0.062900,  0,          0,      0.00001,   1.77,      13:8.R5
REM PLUTO's Satellites                   
REM  Name         Distance   Orbit Time  Unused  Diameter   Tilt Angle  Planet Number


REM Individual planetary systems.

DEFPROCmercury
ENDPROC

DEFPROCvenus
ENDPROC

DEFPROCearth
ENDPROC

DEFPROCmars
maxsolar%=0
z=15000
frames%=0
timer%=TIME
REPEAT
  frames%=frames%+1
  VDU 5
  WAIT:SYS 6,112,S1:SYS 6,113,S2:CLS:SWAP S1,S2
  GCOL 2
  CIRCLEFILL 0,0,p(4,4)*z
  FOR p1=1 TO marssatnum%
    IF p1<17 THEN
      marssat(p1,3)=marssat(p1,3)+(t/marssat(p1,2))
    ENDIF
    r3% = marssat(p1,1)*z
    r4% = marssat(p1,1)*z*COSRADi
    IF orbitdraw%=1 THEN
    IF r3%<1600 OR r4%<1600 THEN
      GCOL 3
      IF p1>16 THEN GCOL 2
        IF r3%=r4% THEN
          CIRCLE 0,0,r3%
        ELSE
          IF r4%<>0 THEN
            IF r3%<>0 AND r4%<>0 THEN
              ELLIPSE 0,0,r3%,r4%,((marssat(p1,5)*PI)/180)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    ENDIF
    oo(1,1)=COSRADmarssat(p1,5)
    oo(1,2)=-SINRADmarssat(p1,5)
    oo(2,1)=SINRADmarssat(p1,5)
    oo(2,2)=COSRADmarssat(p1,5)
    pp(1)=SINRADmarssat(p1,3)*r3%
    pp(2)=COSRADmarssat(p1,3)*r4%
    qq()=oo().pp()
    IF planetnames%=1 THEN
      GCOL 1
      MOVE qq(1)+5,qq(2)+5
      PRINT marssat$(p1)
    ENDIF
    opx1=qq(1)+orgx%
    opy1=qq(2)+orgy%
    IF opx1>=0 AND opx1<=1280 AND opy1>=0 AND opy1<=1024 AND p1<17 THEN
      GCOL 0
      CIRCLE qq(1),qq(2),(marssat(p1,4)*z)+2
      GCOL 2
      CIRCLE FILL qq(1),qq(2),marssat(p1,4)*z
    ENDIF
  NEXT p1
  MOVE 0,0
  MOVE p(4,4)*z,0
  PLOT &B5,-p(4,4)*z,0
  GCOL 0,0
  CIRCLE 0,0,p(4,4)*z
  
  f=360/(t/365)
  e=e+(1/f)
  VDU 4
  @%="+F8.2"
  PRINT TAB(0,0);e;" years"
  PRINTTAB(0,1);"FPS:";(frames%/(TIME-timer%))*100
  
  IF INKEY(-58) THEN z=z*1.05          : REM Up Arrow
  IF INKEY(-42) THEN z=z/1.05          : REM Down Arrow
  IF INKEY(-122) THEN t=t*1.05:REM *1.05         : REM Right Arrow
  IF INKEY(-26) THEN  t=t/1.05:REM 1.05         : REM Left Arrow
  IF INKEY(-80) THEN i=i-2          : REM '"
  IF INKEY(-105) THEN i=i+2         : REM /?
  IF INKEY(-17) THEN maxsolar%=1:i=1:z=1/13: REM Q to return to solar system screen
  IF INKEY(-102) THEN PROCmercury     : REM M for Mercury
  IF INKEY(-100) THEN PROCvenus       : REM V for Venus
  IF INKEY(-35) THEN PROCearth        : REM E for Earth
  IF INKEY(-66) THEN PROCmars         : REM A for Mars
  IF INKEY(-70) THEN PROCjupiter      : REM J for Jupiter
  IF INKEY(-82) THEN PROCsaturn       : REM S for Saturn
  IF INKEY(-54) THEN PROCuranus       : REM U for Uranus
  IF INKEY(-86) THEN PROCneptune      : REM N for Neptune
  IF INKEY(-56) THEN PROCpluto        : REM P for Pluto
  IF INKEY(-99) THEN
    CASE planetnames% OF
      WHEN 1
        planetnames%=0
      WHEN 0
        planetnames%=1
    ENDCASE
  ENDIF
  IF INKEY(-55) THEN
    CASE orbitdraw% OF
     WHEN 1
        orbitdraw%=0
      WHEN 0
        orbitdraw%=1
    ENDCASE
  ENDIF
  IF i<0 THEN i=0
  IF i>90 THEN i=90

  m=(1280/z)
  PRINT TAB(0,58);z
  PRINT TAB(0,57);t
  @%="+F8.1"
  PRINT TAB(0,59);m;" million km";
  
UNTIL maxsolar%=1
ENDPROC

DEFPROCjupiter
maxsolar%=0
z=20

REPEAT

  VDU 5
  WAIT:SYS 6,112,S1:SYS 6,113,S2:CLS:SWAP S1,S2
  GCOL 2
  CIRCLEFILL 0,0,p(5,4)*z
  
  FOR p1=1 TO jupisatnum%
    IF p1<17 THEN
      jupisat(p1,3)=jupisat(p1,3)+(t/jupisat(p1,2))
    ENDIF
    
    r3% = jupisat(p1,1)*z
    r4% = jupisat(p1,1)*z*COSRADi

    IF orbitdraw%=1 THEN
    IF r3%<1600 OR r4%<1600 THEN
      GCOL 3
      IF p1>16 THEN GCOL 2
      IF r3%=r4% THEN
        CIRCLE 0,0,r3%
      ELSE
        IF r4%<>0 THEN
          IF r3%<>0 AND r4%<>0 THEN
            ELLIPSE 0,0,r3%,r4%,((jupisat(p1,5)*PI)/180)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    ENDIF
    
    REM SATELLITE MATRICES

    REM Transition Matrix Construction
    oo(1,1)=COSRADjupisat(p1,5)
    oo(1,2)=-SINRADjupisat(p1,5)
    oo(2,1)=SINRADjupisat(p1,5)
    oo(2,2)=COSRADjupisat(p1,5)

    REM Start Vector for Transformation
    pp(1)=SINRADjupisat(p1,3)*r3%
    pp(2)=COSRADjupisat(p1,3)*r4%

    REM Matrix Multiplication
    qq()=oo().pp()

    IF planetnames%=1 THEN
      GCOL 1
      MOVE qq(1)+5,qq(2)+5
      PRINT jupisat$(p1)
    ENDIF
    
    GCOL 2
    opx1=qq(1)+orgx%
    opy1=qq(2)+orgy%
    
    IF opx1>=0 AND opx1<=1280 AND opy1>=0 AND opy1<=1024 AND p1<17 THEN
      GCOL 0
      CIRCLE qq(1),qq(2),(jupisat(p1,4)*z)+2
      GCOL 2
      CIRCLE FILL qq(1),qq(2),jupisat(p1,4)*z
    ENDIF

  NEXT p1
  
  MOVE 0,0
  MOVE p(5,4)*z,0
  PLOT &B5,-p(5,4)*z,0
  GCOL 0,0
  CIRCLE 0,0,p(5,4)*z
  
  f=360/(t/365)
  e=e+(1/f)
  VDU 4
  @%="+F8.2"
  PRINT TAB(0,0);e;" years"
  
  IF INKEY(-58) THEN z=z*1.05          : REM Up Arrow
  IF INKEY(-42) THEN z=z/1.05          : REM Down Arrow
  IF INKEY(-122) THEN t=t*1.05:REM *1.05         : REM Right Arrow
  IF INKEY(-26) THEN  t=t/1.05:REM 1.05         : REM Left Arrow
  IF INKEY(-80) THEN i=i-2          : REM '"
  IF INKEY(-105) THEN i=i+2         : REM /?
  IF INKEY(-17) THEN maxsolar%=1:i=1:z=1/13: REM Q to return to solar system screen
  IF INKEY(-49) THEN z=1328         : REM 1 for rings
  IF INKEY(-50) THEN z=229          : REM 2 for inner planets
  IF INKEY(-18) THEN z=39           : REM 3 for middle planets
  IF INKEY(-19) THEN z=20           : REM 4 for far planets
  IF INKEY(-102) THEN PROCmercury     : REM M for Mercury
  IF INKEY(-100) THEN PROCvenus       : REM V for Venus
  IF INKEY(-35) THEN PROCearth        : REM E for Earth
  IF INKEY(-66) THEN PROCmars         : REM A for Mars
  IF INKEY(-70) THEN PROCjupiter      : REM J for Jupiter
  IF INKEY(-82) THEN PROCsaturn       : REM S for Saturn
  IF INKEY(-54) THEN PROCuranus       : REM U for Uranus
  IF INKEY(-86) THEN PROCneptune      : REM N for Neptune
  IF INKEY(-56) THEN PROCpluto        : REM P for Pluto
  IF INKEY(-99) THEN
    CASE planetnames% OF
      WHEN 1
        planetnames%=0
      WHEN 0
        planetnames%=1
    ENDCASE
  ENDIF
  IF INKEY(-55) THEN
    CASE orbitdraw% OF
     WHEN 1
        orbitdraw%=0
      WHEN 0
        orbitdraw%=1
    ENDCASE
  ENDIF
  IF i<0 THEN i=0
  IF i>90 THEN i=90

  m=(1280/z)
  PRINT TAB(0,58);z
  PRINT TAB(0,57);t
  @%="+F8.1"
  PRINT TAB(0,59);m;" million km";
  
UNTIL maxsolar%=1
ENDPROC

DEFPROCsaturn
maxsolar%=0
z=36

REPEAT

  VDU 5
  WAIT:SYS 6,112,S1:SYS 6,113,S2:CLS:SWAP S1,S2
  GCOL 2
  CIRCLEFILL 0,0,p(6,4)*z
  GCOL 2
  CIRCLE 0,0,p(6,4)*z
  
  FOR p1=1 TO satusatnum%
    IF p1<19 THEN
      satusat(p1,3)=satusat(p1,3)+(t/satusat(p1,2))
    ENDIF
    
    r3% = satusat(p1,1)*z
    r4% = satusat(p1,1)*z*COSRADi
    
    IF orbitdraw%=1 THEN
      IF r3%<1600 OR r4%<1600 THEN
        GCOL 3
        REM 21,24,26,27
        IF p1>18 THEN GCOL 2
          IF p1<>21 AND p1<>24 AND p1<>26 AND p1<>27 THEN
            IF r3%=r4% THEN
              CIRCLE 0,0,r3%
            ELSE
              IF r4%<>0 THEN
                IF r3%<>0 AND r4%<>0 THEN
                  ELLIPSE 0,0,r3%,r4%,((satusat(p1,5)*PI)/180)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    REM SATELLITE MATRICES

    REM Transition Matrix Construction
    oo(1,1)=COSRADsatusat(p1,5)
    oo(1,2)=-SINRADsatusat(p1,5)
    oo(2,1)=SINRADsatusat(p1,5)
    oo(2,2)=COSRADsatusat(p1,5)

    REM Start Vector for Transformation
    pp(1)=SINRADsatusat(p1,3)*r3%
    pp(2)=COSRADsatusat(p1,3)*r4%

    REM Matrix Multiplication
    qq()=oo().pp()

    IF planetnames%=1 THEN    
      GCOL 1
      MOVE qq(1)+5,qq(2)+5
      IF planetnames%=1 THEN PRINT satusat$(p1)
    ENDIF
    
    GCOL 2
    opx1=qq(1)+orgx%
    opy1=qq(2)+orgy%
    
    IF opx1>=0 AND opx1<=1280 AND opy1>=0 AND opy1<=1024 AND p1<17 THEN
      GCOL 0
      CIRCLE qq(1),qq(2),(satusat(p1,4)*z)+2
      GCOL 2
      CIRCLE FILL qq(1),qq(2),satusat(p1,4)*z
    ENDIF

  NEXT p1
  
  MOVE 0,0
  MOVE p(6,4)*z,0
  PLOT &B5,-p(6,4)*z,0
  GCOL 0,0
  CIRCLE 0,0,p(6,4)*z
  
  f=360/(t/365)
  e=e+(1/f)
  VDU 4
  @%="+F8.2"
  PRINT TAB(0,0);e;" years"
  
  IF INKEY(-58) THEN z=z*1.05          : REM Up Arrow
  IF INKEY(-42) THEN z=z/1.05          : REM Down Arrow
  IF INKEY(-122) THEN t=t*1.05:REM *1.05         : REM Right Arrow
  IF INKEY(-26) THEN  t=t/1.05:REM 1.05         : REM Left Arrow
  IF INKEY(-80) THEN i=i-2          : REM '"
  IF INKEY(-105) THEN i=i+2         : REM /?
  IF INKEY(-17) THEN maxsolar%=1:i=1:z=1/13: REM Q to return to solar system screen
  IF INKEY(-49) THEN z=1300         : REM 1 for rings
  IF INKEY(-50) THEN z=717          : REM 2 for inner planets
  IF INKEY(-18) THEN z=36           : REM 3 for far planets
  IF INKEY(-102) THEN PROCmercury     : REM M for Mercury
  IF INKEY(-100) THEN PROCvenus       : REM V for Venus
  IF INKEY(-35) THEN PROCearth        : REM E for Earth
  IF INKEY(-66) THEN PROCmars         : REM A for Mars
  IF INKEY(-70) THEN PROCjupiter      : REM J for Jupiter
  IF INKEY(-82) THEN PROCsaturn       : REM S for Saturn
  IF INKEY(-54) THEN PROCuranus       : REM U for Uranus
  IF INKEY(-86) THEN PROCneptune      : REM N for Neptune
  IF INKEY(-56) THEN PROCpluto        : REM P for Pluto
  IF INKEY(-99) THEN
    CASE planetnames% OF
      WHEN 1
        planetnames%=0
      WHEN 0
        planetnames%=1
    ENDCASE
  ENDIF
  IF INKEY(-55) THEN
    CASE orbitdraw% OF
     WHEN 1
        orbitdraw%=0
      WHEN 0
        orbitdraw%=1
    ENDCASE
  ENDIF
  IF i<0 THEN i=0
  IF i>90 THEN i=90

  m=(1280/z)
  PRINT TAB(0,58);z
  PRINT TAB(0,57);t
  @%="+F8.1"
  PRINT TAB(0,59);m;" million km";
  
UNTIL maxsolar%=1
ENDPROC

DEFPROCuranus
maxsolar%=0
z=741

REPEAT

  VDU 5
  WAIT:SYS 6,112,S1:SYS 6,113,S2:CLS:SWAP S1,S2
  GCOL 2
  CIRCLEFILL 0,0,p(7,4)*z
  GCOL 2
  CIRCLE 0,0,p(7,4)*z
  
  FOR p1=1 TO uransatnum%
    IF p1<16 THEN
      uransat(p1,3)=uransat(p1,3)+(t/uransat(p1,2))
    ENDIF
    
    r3% = uransat(p1,1)*z
    r4% = uransat(p1,1)*z*COSRADi
    
    IF orbitdraw%=1 THEN
    IF r3%<1600 OR r4%<1600 THEN
      GCOL 3
      REM 21,24,26,27
      IF p1>15 THEN GCOL 2
        IF r3%=r4% THEN
          CIRCLE 0,0,r3%
        ELSE
          IF r4%<>0 THEN
            IF r3%<>0 AND r4%<>0 THEN
              ELLIPSE 0,0,r3%,r4%,((uransat(p1,5)*PI)/180)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    ENDIF
    
    REM SATELLITE MATRICES

    REM Transition Matrix Construction
    oo(1,1)=COSRADuransat(p1,5)
    oo(1,2)=-SINRADuransat(p1,5)
    oo(2,1)=SINRADuransat(p1,5)
    oo(2,2)=COSRADuransat(p1,5)

    REM Start Vector for Transformation
    pp(1)=SINRADuransat(p1,3)*r3%
    pp(2)=COSRADuransat(p1,3)*r4%

    REM Matrix Multiplication
    qq()=oo().pp()

    IF planetnames%=1 THEN    
      GCOL 1
      MOVE qq(1)+5,qq(2)+5
      IF planetnames%=1 THEN PRINT uransat$(p1)
    ENDIF
    
    GCOL 2
    opx1=qq(1)+orgx%
    opy1=qq(2)+orgy%
    
    IF opx1>=0 AND opx1<=1280 AND opy1>=0 AND opy1<=1024 AND p1<17 THEN
      GCOL 0
      CIRCLE qq(1),qq(2),(uransat(p1,4)*z)+2
      GCOL 2
      CIRCLE FILL qq(1),qq(2),uransat(p1,4)*z
    ENDIF

  NEXT p1
  
  MOVE 0,0
  MOVE p(7,4)*z,0
  PLOT &B5,-p(7,4)*z,0
  GCOL 0,0
  CIRCLE 0,0,p(7,4)*z
  
  f=360/(t/365)
  e=e+(1/f)
  VDU 4
  @%="+F8.2"
  PRINT TAB(0,0);e;" years"
  
  IF INKEY(-58) THEN z=z*1.05          : REM Up Arrow
  IF INKEY(-42) THEN z=z/1.05          : REM Down Arrow
  IF INKEY(-122) THEN t=t*1.05:REM *1.05         : REM Right Arrow
  IF INKEY(-26) THEN  t=t/1.05:REM 1.05         : REM Left Arrow
  IF INKEY(-80) THEN i=i-2          : REM '"
  IF INKEY(-105) THEN i=i+2         : REM /?
  IF INKEY(-17) THEN maxsolar%=1:i=1:z=1/13: REM Q to return to solar system screen
  IF INKEY(-49) THEN z=1300         : REM 1 for rings
  IF INKEY(-50) THEN z=717          : REM 2 for inner planets
  IF INKEY(-18) THEN z=36           : REM 3 for far planets
  IF INKEY(-102) THEN PROCmercury     : REM M for Mercury
  IF INKEY(-100) THEN PROCvenus       : REM V for Venus
  IF INKEY(-35) THEN PROCearth        : REM E for Earth
  IF INKEY(-66) THEN PROCmars         : REM A for Mars
  IF INKEY(-70) THEN PROCjupiter      : REM J for Jupiter
  IF INKEY(-82) THEN PROCsaturn       : REM S for Saturn
  IF INKEY(-54) THEN PROCuranus       : REM U for Uranus
  IF INKEY(-86) THEN PROCneptune      : REM N for Neptune
  IF INKEY(-56) THEN PROCpluto        : REM P for Pluto
  IF INKEY(-99) THEN
    CASE planetnames% OF
      WHEN 1
        planetnames%=0
      WHEN 0
        planetnames%=1
    ENDCASE
  ENDIF
  IF INKEY(-55) THEN
    CASE orbitdraw% OF
     WHEN 1
        orbitdraw%=0
      WHEN 0
        orbitdraw%=1
    ENDCASE
  ENDIF
  IF i<0 THEN i=0
  IF i>90 THEN i=90

  m=(1280/z)
  PRINT TAB(0,58);z
  PRINT TAB(0,57);t
  @%="+F8.1"
  PRINT TAB(0,59);m;" million km";
  
UNTIL maxsolar%=1
ENDPROC

DEFPROCneptune
maxsolar%=0
z=82

REPEAT

  VDU 5
  WAIT:SYS 6,112,S1:SYS 6,113,S2:CLS:SWAP S1,S2
  GCOL 2
  CIRCLEFILL 0,0,p(8,4)*z
  GCOL 0
  CIRCLE 0,0,p(8,4)*z
 
  FOR p1=1 TO neptsatnum%
    IF p1<9 THEN
      neptsat(p1,3)=neptsat(p1,3)+(t/neptsat(p1,2))
    ENDIF
    
    r3% = neptsat(p1,1)*z
    r4% = neptsat(p1,1)*z*COSRADi

    IF orbitdraw%=1 THEN
    IF r3%<1600 OR r4%<1600 THEN
      GCOL 3
      REM 21,24,26,27
      IF p1>8 THEN GCOL 2
        IF r3%=r4% THEN
          CIRCLE 0,0,r3%
        ELSE
          IF r4%<>0 THEN
            IF r3%<>0 AND r4%<>0 THEN
              ELLIPSE 0,0,r3%,r4%,((neptsat(p1,5)*PI)/180)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    ENDIF
    
    REM SATELLITE MATRICES

    REM Transition Matrix Construction
    oo(1,1)=COSRADneptsat(p1,5)
    oo(1,2)=-SINRADneptsat(p1,5)
    oo(2,1)=SINRADneptsat(p1,5)
    oo(2,2)=COSRADneptsat(p1,5)

    REM Start Vector for Transformation
    pp(1)=SINRADneptsat(p1,3)*r3%
    pp(2)=COSRADneptsat(p1,3)*r4%

    REM Matrix Multiplication
    qq()=oo().pp()

    IF planetnames%=1 THEN    
      GCOL 1
      MOVE qq(1)+5,qq(2)+5
      IF planetnames%=1 THEN PRINT neptsat$(p1)
    ENDIF
    
    GCOL 2
    opx1=qq(1)+orgx%
    opy1=qq(2)+orgy%
    
    IF opx1>=0 AND opx1<=1280 AND opy1>=0 AND opy1<=1024 AND p1<17 THEN
      GCOL 0
      CIRCLE qq(1),qq(2),(neptsat(p1,4)*z)+2
      GCOL 2
      CIRCLE FILL qq(1),qq(2),neptsat(p1,4)*z
    ENDIF

  NEXT p1
  
  MOVE 0,0
  MOVE p(8,4)*z,0
  PLOT &B5,-p(8,4)*z,0
  GCOL 0,0
  CIRCLE 0,0,p(8,4)*z
  
  f=360/(t/365)
  e=e+(1/f)
  VDU 4
  @%="+F8.2"
  PRINT TAB(0,0);e;" years"
  
  IF INKEY(-58) THEN z=z*1.05          : REM Up Arrow
  IF INKEY(-42) THEN z=z/1.05          : REM Down Arrow
  IF INKEY(-122) THEN t=t*1.05:REM *1.05         : REM Right Arrow
  IF INKEY(-26) THEN  t=t/1.05:REM 1.05         : REM Left Arrow
  IF INKEY(-80) THEN i=i-2          : REM '"
  IF INKEY(-105) THEN i=i+2         : REM /?
  IF INKEY(-17) THEN maxsolar%=1:i=1:z=1/13: REM Q to return to solar system screen
  IF INKEY(-49) THEN z=1300         : REM 1 for rings
  IF INKEY(-50) THEN z=717          : REM 2 for inner planets
  IF INKEY(-18) THEN z=36           : REM 3 for far planets
  IF INKEY(-102) THEN PROCmercury     : REM M for Mercury
  IF INKEY(-100) THEN PROCvenus       : REM V for Venus
  IF INKEY(-35) THEN PROCearth        : REM E for Earth
  IF INKEY(-66) THEN PROCmars         : REM A for Mars
  IF INKEY(-70) THEN PROCjupiter      : REM J for Jupiter
  IF INKEY(-82) THEN PROCsaturn       : REM S for Saturn
  IF INKEY(-54) THEN PROCuranus       : REM U for Uranus
  IF INKEY(-86) THEN PROCneptune      : REM N for Neptune
  IF INKEY(-56) THEN PROCpluto        : REM P for Pluto
  IF INKEY(-99) THEN
    CASE planetnames% OF
      WHEN 1
        planetnames%=0
      WHEN 0
        planetnames%=1
    ENDCASE
  ENDIF
  IF INKEY(-55) THEN
    CASE orbitdraw% OF
     WHEN 1
        orbitdraw%=0
      WHEN 0
        orbitdraw%=1
    ENDCASE
  ENDIF
  IF i<0 THEN i=0
  IF i>90 THEN i=90

  m=(1280/z)
  PRINT TAB(0,58);z
  PRINT TAB(0,57);t
  @%="+F8.1"
  PRINT TAB(0,59);m;" million km";
  
UNTIL maxsolar%=1
ENDPROC

DEFPROCpluto
ENDPROC
