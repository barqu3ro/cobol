    *> 100        DIVISION 1: IDENTIFICACION DEL PROGRAMA Y AUTOR
               IDENTIFICATION DIVISION.                                                                                 97/07/04
    200        PROGRAM-ID. R09RY1RCA.                                                                                   19/12/16
    300        AUTHOR.  Randall Vallejos Rojas.                                                                         19/11/16
    400        INSTALLATION.  INSTITUTO NACIONAL DE SEGUROS.                                                            20/02/20
    500       ******************************************************************                                        20/08/15
    600       * Randall Vallejos Rojas 19/12/2016                              *                                        19/12/16
    700       * Reserva Caso a Caso RT.                                                                                 19/11/16
    800       * RequerimienTO 20162000160                                                                               20/02/20
    900       * Objetivo:                                                                                               19/11/16
   1000       *   1. Proceso se ejecuta mensualmente por demanda del usuario                                            19/12/16
   1100       *   2. Procesa todos los movimientos realizados en un mes.                                                28/02/20
   1200       *      MovimienTOs: INC.TEMP, INC.PERM, CTAS.MEDIC, INDEM.MUERTE                                          20/02/20
   1300       *   3. Si el caso no ha tenido movimienTOs lo cierra                                                      20/02/20
   1400       *   4. Acumula en nuevo registro(mes) los gasTOs anteriores mas                                           20/02/20
   1500       *      lo gastado en ese mes, generando la provisión                                                      19/12/16
   1600       *      Esto sólo si el caso está abierto.                                                                 28/02/20
   1700       * **************************************************************                                          19/10/15
   1800       * RequerimienTO 20192000012.                                                                              26/02/20
   1900       * Realizado por: Gloriana Guzmán Chavarría.                                                               26/02/19
   2000       * Fecha: 26-02-2019.                                                                                      31/01/20
   2100       * Se modifica los campos de las siguientes variables, a 11 campos                                         26/02/19
   2200       * OKMONTO, TOtal1,TOtal2, TOtal3, TOtal4, TOtal5 y TOtal6,GASTOT.                                         20/02/20
   2300       *****************************************************************                                         26/02/19
   2400       * RequerimienTO 20152001121.                                                                              20/02/20
   2500       * Realizado por: Alejandro Brenes Solano.                                                                 09/04/19
   2600       * Fecha: 09-04-2019.                                                                                      09/04/19
   2700       * Respaldo R09RY1RCA6                                                                                     09/04/19
   2800       * Version V006                                                                                            09/04/19
   2900       * No debe de considerar ni grabar los casos cuya poliza asociada                                          09/04/19
   3000       * sea 'NEMAGON'                                                                                           09/04/19
   3100       *****************************************************************                                         16/01/20
   3101       * RequerimienTO  : 20202000019 CAMBIOS PROCESO CASO A CASO                                                25/02/20
   3102       * Realizado por: Rodrigo León Murillo.                                                                    16/01/20
   3103       * Fecha: 16-01-2020.                                                                                      16/01/20
   3104       * Respaldo R09RY1RCA8                                                                                     16/01/20
   3105       * Version V008                                                                                            16/01/20
   3106       * Se modifica el programa para lo siguiente:                                                              16/01/20
   3107       * 1. Si falla la lectura en D010, cualquier file status malo                                              16/01/20
   3108       *    enTOnces se modifica el programa para que no haga STOP RUN                                           20/02/20
   3109       *    sino que grabe el caso como NO PROCESADO en un nuevo archivo                                         16/01/20
   3110       *    que se crea para esta función.                                                                       16/01/20
   3111       *    De esta manera el proceso sigue hasta que termina ya que                                             16/01/20
   3112       *    antes de este cambio estaba dejando en caso de que esTO                                              20/02/20
   3113       *    sucediera, una cantidad de registros de casos por fuera.                                             16/01/20
   3114       * 2. El proceso va a permitir reconstruir la data para poder                                              16/01/20
   3115       *   "echar para atrás" en cualquier momenTO y volver a generar                                            20/02/20
   3116       *    una corrida sin necesidad de restaurar.                                                              16/01/20
   3117       *    EsTOs daTOs de recuperación se TOmarán del BITREC                                                    20/02/20
   3118       *    el cual se va a llenar en la ejecución original.                                                     17/01/20
   3119       * 3. Se realiza además un cambio para que en el módulo                                                    16/01/20
   3120       *    B010-PROCESA-CASOS no lea TOdo el archivo de casos sino                                              20/02/20
   3121       *    que recorra un cursor con los casos que cumplan con                                                  16/01/20
   3122       *    una o varias de estas condiciones:                                                                   17/01/20
   3123       *    3.1. Casos que estén en TEMMTO es decir los que tuvieron                                             20/02/20
   3124       *         pago en el mes de estudio.                                                                      16/01/20
   3125       *    3.2. Casos abierTOs en el momenTO de ejecución.                                                      20/02/20
   3126       *    3.3. Casos que no tengan codigo aceptacion 30,31,32 ni 99.                                           10/02/20
   3128       *    3.5. Casos que no estén entre los duplicados.                                                        10/02/20
   3129       *    De esta forma el tiempo de procesamienTO va a bajar                                                  20/02/20
   3130       *    considerablemente.                                                                                   16/01/20
   3131       * 4. Agregar un parámetro a este programa para que el proceso                                             17/01/20
   3132       *    sepa si debe realizar una corrida original o si debe                                                 17/01/20
   3133       *    realizar un re-proceso o sea recuperar información anterior                                          17/01/20
   3134       *    y ejecutar de nuevo.                                                                                 17/01/20
   3135       * 5. Crear pantalla para visualizar bitácora BITREC. Desde esta                                           17/01/20
   3136       *    pantalla es desde donde se podría re-procesar.                                                       17/01/20
   3137       ******************************************************************                                        17/01/20
   3139       * Requerimiento  : Correo de Pablo Camacho del 04/05/2020                                                 05/05/20
   3140       * Realizado por  : Rodrigo León Murillo.                                                                  05/05/20
   3141       * Fecha          : 05-05-2020.                                                                            05/05/20
   3142       * Respaldo R09RY1RCA1                                                                                     05/05/20
   3143       * Version V009                                                                                            05/05/20
   3144       ******************************************************************                                        05/05/20
   3145       * Requerimiento  : 20202000226 - Pablo Camacho                                                            25/06/20
   3146       * Realizado por  : Rodrigo León Murillo.                                                                  25/06/20
   3147       * Fecha          : 23-06-2020.                                                                            25/06/20
   3148       * Respaldo R09RY1RCA2                                                                                     25/06/20
   3149       * Version V010                                                                                            25/06/20
   3150       ******************************************************************                                        25/06/20
      *>        * Webinar Git 
      *>        * Primer commit
      *>        * Fin de modificacion      
      *>        * Jorge Barquero - Webinar ISI 9-3-2021                                                               

      *>     DIVISION 1: DIVISION DE AMBIENTE                        
   3200        ENVIRONMENT DIVISION.                                                                                    97/07/04
   3300        CONFIGURATION SECTION.                                                                                   97/07/04
   3400        SOURCE-COMPUTER.   IBM-AS400.                                                                            10/02/20
   3500        OBJECT-COMPUTER.   IBM-AS400.                                                                            10/02/20
   3600        INPUT-OUTPUT SECTION.          
      *>         Archivos a utilizar                                                                          97/07/04
   3700        FILE-CONTROL.                                                                                            27/07/15
   3701            SELECT R09MAE59                                                                                      20/01/20
   3702                   ASSIGN       TO DATABASE-R09MAE59                                                             20/02/20
   3703                   ORGANIZATION IS INDEXED                                                                       20/01/20
   3704                   ACCESS MODE  IS DYNAMIC                                                                       20/01/20
   3705                   RECORD KEY IS EXTERNALLY-DESCRIBED-KEY                                                        20/01/20
   3706                   WITH DUPLICATES                                                                               20/01/20
   3707                   FILE STATUS  IS FS-R09MAE59.                                                                  20/01/20
   4500                                                                                                                 07/01/17
   4600            SELECT R08MAE                                                                                        14/01/17
   4700                   ASSIGN       TO DATABASE-R08MAE                                                               20/02/20
   4800                   ORGANIZATION IS INDEXED                                                                       14/01/17
   4900                   ACCESS MODE  IS DYNAMIC                                                                       14/01/17
   5000                   RECORD KEY IS EXTERNALLY-DESCRIBED-KEY                                                        14/01/17
   5100                   FILE STATUS  IS FS-R08MAE.                                                                    14/01/17
   5200                                                                                                                 14/01/17
   5300            SELECT TEMFMX1                                                                                       29/02/20
   5400                   ASSIGN       TO DATABASE-TEMFMX1                                                              20/02/20
   5500                   ORGANIZATION IS INDEXED                                                                       07/01/17
   5600                   ACCESS MODE  IS DYNAMIC                                                                       07/01/17
   5700                   RECORD KEY IS EXTERNALLY-DESCRIBED-KEY                                                        07/01/17
   5800                   FILE STATUS  IS FS-TEMFMX1.                                                                   07/01/17
   5900                                                                                                                 07/01/17
   6000            SELECT TEMMTO                                                                                        20/02/20
   6100                   ASSIGN       TO DATABASE-TEMMTO                                                               20/02/20
   6200                   ORGANIZATION IS INDEXED                                                                       11/01/17
   6300                   ACCESS MODE  IS DYNAMIC                                                                       11/01/17
   6400                   RECORD KEY IS EXTERNALLY-DESCRIBED-KEY                                                        11/01/17
   6500                   FILE STATUS  IS FS-TEMMTO.                                                                    20/02/20
   6600                                                                                                                 07/01/17
   6700            SELECT RY1RCA01                                                                                      30/01/17
   6800                   ASSIGN       TO DATABASE-RY1RCA01                                                             20/02/20
   6900                   ORGANIZATION IS INDEXED                                                                       11/01/17
   7000                   ACCESS MODE  IS DYNAMIC                                                                       11/01/17
   7100                   RECORD KEY IS EXTERNALLY-DESCRIBED-KEY                                                        11/01/17
   7200                   WITH DUPLICATES                                                                               17/02/17
   7300                   FILE STATUS  IS FS-RY1RCA.                                                                    11/01/17
   7400                                                                                                                 11/01/17
   7500            SELECT RY1RCD01                                                                                      17/02/17
   7600                   ASSIGN       TO DATABASE-RY1RCD01                                                             20/02/20
   7700                   ORGANIZATION IS INDEXED                                                                       11/01/17
   7800                   ACCESS MODE  IS DYNAMIC                                                                       11/01/17
   7900                   RECORD KEY IS EXTERNALLY-DESCRIBED-KEY                                                        11/01/17
   8000                   WITH DUPLICATES                                                                               17/02/17
   8100                   FILE STATUS  IS FS-RY1RCD.                                                                    11/01/17
   8200                           
      *>     DIVISIÓN 2: DIVISIÓN DE DATOS                                                                                      11/01/17
   8300        DATA DIVISION.                                                                                           97/07/04
   8400        FILE SECTION.                                                                                            97/07/04
   8401                                                                                                                 20/01/20
   8402        FD  R09MAE59.                                                                                            20/01/20
   8403        01  R09MAE59.                                                                                            20/01/20
   8404            COPY DDS-ALL-FORMATS OF R09MAE59.                                                                    20/01/20
   8500                                                                                                                 07/01/17
   8600        FD  R08MAE.                                                                                              14/01/17
   8700        01  RR08MAE.                                                                                             17/02/17
   8800            COPY DDS-ALL-FORMATS OF R08MAE.                                                                      17/02/17
   9300                                                                                                                 07/01/17
   9400        FD  TEMFMX1.                                                                                             07/01/17
   9500        01  RTEMFMX1.                                                                                            07/01/17
   9600            COPY DDS-ALL-FORMATS OF TEMFMX1.                                                                     07/01/17
   9700                                                                                                                 07/01/17
   9800        FD  TEMMTO.                                                                                              20/02/20
   9900        01  RTEMMTO.                                                                                             20/02/20
  10000            COPY DDS-ALL-FORMATS OF TEMMTO.                                                                      20/02/20
  10100                                                                                                                 11/01/17
  10200        FD  RY1RCA01.                                                                                            30/01/17
  10300        01  RRY1RCA01.                                                                                           30/01/17
  10400            COPY DDS-ALL-FORMATS OF RY1RCA01.                                                                    30/01/17
  10500                                                                                                                 11/01/17
  10600        FD  RY1RCD01.                                                                                            17/02/17
  10700        01  RRY1RCD01.                                                                                           17/02/17
  10800            COPY DDS-ALL-FORMATS OF RY1RCD01.                                                                    17/02/17
  10900                                                                                                                 07/01/17
  11000        WORKING-STORAGE SECTION.                                                                                 20/02/20
  11001        01  BITFIL-WS          PIC X(10) VALUES SPACES.                                                          24/02/20
  11002        01  IND-VALIDAR-RENTA  PIC X VALUE "S".                                                                  30/03/20
  11003        01  FECHAHOY           PIC X(08).                                                                        13/02/20
  11004        01  FECHAINICIORENTA   PIC X(08).                                                                        13/02/20
  11005        01  MESES-INICIO-RENTA PIC 9(5) COMP-3.                                                                  20/02/20
  11006        01  TIENE-PAGO-TEMP    PIC X.                                                                            13/02/20
  11007        01  TIENE-PAGO-PERM    PIC X.                                                                            13/02/20
  11008        01  TIENE-PAGO-CTAMED  PIC X.                                                                            13/02/20
  11009        01  VARERR            PIC X.                                                                             12/02/20
  11010        01  FECHAVALIDA       PIC X.                                                                             05/02/20
  11011        01  DUPLICADO         PIC X.                                                                             05/02/20
  11012        01  SALDORCD          PIC S9(14)V99 VALUE 0.                                                             30/01/20
  11013        01  SALJUDIC-WS       PIC S9(14)V99 VALUE 0.                                                             14/08/20
  11014        01  RRY1CIF.                                                                                             30/01/20
  11015            05 RY1FEC-WS      PIC X(08).                                                                         30/01/20
  11016            05 CASOSCAN-WS    PIC S9(10) VALUE 0.                                                                30/01/20
  11017            05 CASOSTOM-WS    PIC S9(10) VALUE 0.                                                                20/02/20
  11018            05 CERRADOS-WS    PIC S9(10) VALUE 0.                                                                30/01/20
  11019            05 ABIERTOS-WS    PIC S9(10) VALUE 0.                                                                20/02/20
  11020            05 CONTROVE-WS    PIC S9(10) VALUE 0.                                                                30/01/20
  11021            05 CASOSALC-WS    PIC S9(10) VALUE 0.                                                                06/02/20
  11022            05 CASOSALN-WS    PIC S9(10) VALUE 0.                                                                06/02/20
  11023            05 CASOSAPL-WS    PIC S9(10) VALUE 0.                                                                06/02/20
  11024            05 CASOSDLC-WS    PIC S9(10) VALUE 0.                                                                06/02/20
  11025            05 CASOSDLN-WS    PIC S9(10) VALUE 0.                                                                06/02/20
  11026            05 CASOSDPL-WS    PIC S9(10) VALUE 0.                                                                06/02/20
  11027            05 TOTALPRO-WS    PIC S9(14)V99 VALUE 0.                                                             20/02/20
  11028            05 TOTALPAJ-WS    PIC S9(14)V99 VALUE 0.                                                             20/02/20
  11029            05 TOTALGAS-WS    PIC S9(14)V99 VALUE 0.                                                             20/02/20
  11030            05 TOTALSAL-WS    PIC S9(14)V99 VALUE 0.                                                             20/02/20
      *>         VARIABLES FICTICIAS
                   05 TOTCOMMITS     PIC S9(14)V99 VALUE 0. 
                   05 TOTCAMBIOS     PIC S9(14)V99 VALUE 0.
  11031                                                                                                                 30/01/20
  11032        01  RR09MAE.                                                                                             20/01/20
  11033            05 R09CAS-WS PIC X(11).                                                                              20/01/20
  11034            05 R09CED-WS PIC X(15).                                                                              20/01/20
  11035            05 R09AGE-WS PIC XX.                                                                                 20/01/20
  11036            05 R09FEC-WS PIC X(08).                                                                              20/01/20
  11037            05 R09ACE-WS PIC XX.                                                                                 20/01/20
  11038            05 R09FEI-WS PIC X(08).                                                                              20/01/20
  11039            05 R09REA-WS PIC X(08).                                                                              20/01/20
  11040            05 R09CON-WS PIC XX.                                                                                 20/01/20
  11041            05 R09CTV-WS PIC X.                                                                                  27/01/20
  11042            05 R09ESS-WS PIC X.                                                                                  20/01/20
  11043            05 R09ESR-WS PIC X.                                                                                  20/01/20
  11044            05 R09EST-WS PIC X.                                                                                  20/01/20
  11045        77  EXISTE-RCA       PIC S9(11)V99 VALUE 0.                                                              21/01/20
               77  NO-EXISTE-RCA    PIC S9(11)V99 VALUE 1.
  11046        77  MONTO-WS         PIC S9(11)V99 VALUE 0.                                                              20/02/20
  11047        77  SEGUIR           PIC X VALUE "S".                                                                    20/01/20
  11048        77  SALIR-R09        PIC X VALUE "N".                                                                    20/01/20
  11049        77  SALIR-BITREC     PIC X VALUE "N".                                                                    20/01/20
  11050        77  BITCO1-WS        PIC XX.                                                                             17/01/20
  11051        77  BITES1-WS        PIC X.                                                                              17/01/20
  11052        77  BITES2-WS        PIC X.                                                                              17/01/20
  11053        77  BITCA2-WS        PIC X(11).                                                                          17/01/20
  11054        77  FE3-WS           PIC X(08).                                                                          17/01/20
  11055        77  F06-WS           PIC X(08).                                                                          17/01/20
  11056        77  F08-WS           PIC X(08).                                                                          17/01/20
  11057        77  ANO-WS           PIC X(04).                                                                          17/01/20
  11058        77  MES-WS           PIC XX.                                                                             17/01/20
  11059        77  USU-WS           PIC XX.                                                                             20/01/20
  11060        77  LECTURA-OK       PIC X VALUE "S".                                                                    17/01/20
  11061        77  MENSAJE          PIC X(50) VALUE SPACES.                                                             20/02/20
  11062        77  FECHAMOMENTO     PIC X(08) VALUE SPACES.                                                             20/02/20
  11063        77  HORAMOMENTO      PIC X(08) VALUE SPACES.                                                             20/02/20
  11300        77  LUGARERRO        PIC X(15) VALUE SPACES.                                                             20/02/20
  11400        01  HORACORR         PIC X(06).                                                                          10/07/19
  11500        01  WRY1RCA01.                                                                                           30/01/17
  11600            COPY DDS-ALL-FORMATS OF RY1RCA01.                                                                    30/01/17
  11700                                                                                                                 11/01/17
  11800        01  WRY1RCD01A.                                                                                          17/02/17
  11900            COPY DDS-ALL-FORMATS OF RY1RCD01.                                                                    17/02/17
  12000                                                                                                                 16/02/17
  12100        01  WRY1RCD01B.                                                                                          17/02/17
  12200            COPY DDS-ALL-FORMATS OF RY1RCD01.                                                                    17/02/17
  12300                                                                                                                 17/02/17
  12400        01  WRY1RCD01C.                                                                                          17/02/17
  12500            COPY DDS-ALL-FORMATS OF RY1RCD01.                                                                    17/02/17
  12600                                                                                                                 17/02/17
  12700        01  WRY1RCD01D.                                                                                          17/02/17
  12800            COPY DDS-ALL-FORMATS OF RY1RCD01.                                                                    17/02/17
  12900                                                                                                                 17/02/17
  13000        01  WRY1RCD01T.                                                                                          17/02/17
  13100            COPY DDS-ALL-FORMATS OF RY1RCD01.                                                                    17/02/17
  13500                                                                                                                 17/02/17
              *-- SE AGREGAN DOS VALORES AL PUNTERO
  13600        01  FILESTATUS.                                                                                          07/01/17
  13700            05 FS-R08MAE        PIC XX.                                                                          14/01/17
  13800            05 FS-R09MAE59      PIC XX.                                                                          14/01/17
  13900            05 FS-TEMFMX1       PIC XX.                                                                          07/01/17
  14000            05 FS-TEMMTO        PIC XX.                                                                          20/02/20
  14100            05 FS-RY1RCA        PIC XX.                                                                          11/01/17
  14200            05 FS-RY1RCD        PIC XX.                                                                          11/01/17
                   05 FS-VARIABLE1     PIC XX.
                   05 FS-VARIABLE2     PIC XX.
  14300                                                                                                                 07/01/17
  14400        01  VARIABLES.                                                                                           19/12/16
  14500            05 FCURRENT.                                                                                         20/12/16
  14600               10 CHASTA-WS               PIC s9(02).                                                            20/12/16
  14700               10 AMDHASTA-WS             PIC s9(06).                                                            20/12/16
  14800            05 FDESDE-CURRENT             PIC s9(08).                                                            20/12/16
  14900            05 FCURRENT-WS                PIC s9(08).                                                            20/12/16
  *> 15000       *-- LOS RANGOS MENSUALES DE EJECUCIÓN                                                                     29/12/16
  15100            05 FDE                        PIC X(08).                                                             19/12/16
  15200            05 FHA                        PIC X(08).                                                             19/12/16
  15300            05 VIVMUE                     PIC X.                                                                 17/01/17
  15400            05 OKCTV                      PIC X.                                                                 18/01/17
  15500            05 ENTRA                      PIC X.                                                                 14/02/20
  15600            05 OKMONTO                    PIC S9(11)V99 COMP-3.                                                  20/02/20
  15700            05 WCONDI                     PIC XX.      
      *>           Indice                                                          30/01/17
  15800            05 INDICE                     PIC 9.                                                                 10/02/20
  15900            05 FINR09                     PIC 9.                                                                 17/02/17
  16000            05 FIN                        PIC 9.                 
      *>           Gastos totales                                                17/02/17
  16100            05 GASTOT                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  16200            05 PAUMRU                     PIC 9(03)V99 COMP-3.                                                   20/02/20
  *> 16300       *------ TOtal Provision inicial                                                                           20/02/20
  16400            05 TOTAL1                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  *> 16500       *------ TOtal GasTOs Acumulados                                                                           20/02/20
  16600            05 TOTAL2                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  *> 16700       *------ TOtal GasTOs Administrativos                                                                      20/02/20
  16800            05 TOTAL3                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  *> 16900       *------ TOtal AumenTO Acumulado                                                                           20/02/20
  17000            05 TOTAL4                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  *> 17100       *------ TOtal Provisiòn Ajustada                                                                          20/02/20
  17200            05 TOTAL5                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  *> 17300       *------ TOtal Saldo de la Provisiòn                                                                       20/02/20
  17400            05 TOTAL6                     PIC 9(11)V99 COMP-3.                                                   20/02/20
  *> 17500       *--                                                                                                       14/02/17
  17600            05 WFTOPEV                    PIC X(08).                                                             20/02/20
  17700            05 WFTOPEM                    PIC X(08).                                                             20/02/20
  17800            05 WFTOPE                     PIC X(08).                                                             20/02/20
  17900            05 WMESNUM                    PIC 99.                                                                09/01/17
  18000            05 WMESESNUM                  PIC 9(2) COMP-3. 
      *>         Año y mes de corrida                                                      20/02/20
  18100            05 WMESCORRIDA                PIC X(2).                                                              20/12/16
  18200            05 WANOCORRIDA                PIC X(4).                                                              20/02/17
  18300            05 WANOMESCORR.                                                                                      20/02/17
  18400               07 MESCORR                 PIC XX.                                                                20/02/17
  18500               07 FILLER                  PIC X VALUE "-".                                                       20/02/17
  18600               07 ANOCORR                 PIC XXXX.                                                              20/02/17
  18700            05 WANOMESCORRIDA             PIC X(7). 
      *>         Usuario                                                             20/02/17
  18800            05 WUSUARIO                   PIC X(10).                                                             20/12/16
  18900            05 CAS001                     PIC X(11).                                                             16/02/17
  19000            05 WCASDUP                    PIC X(11).                                                             16/02/17
  *> 19100       *--     indicador de error 0 si está OK                                                                   12/01/17
  19200            05 WFECHA                     PIC X(8).                                                              12/01/17
  19300            05 INDICA                     PIC X(1).                                                              12/01/17
  19400            05 WR20IN                     PIC X(1).                                                              04/02/17
  19500            05 WR54EF                     PIC X(1).                                                              04/02/17
  19600            05 WCASO                      PIC X(11).                                                             27/02/17
  19700            05 NUEVO                      PIC X(1).  
      *>         Condiciones                                                             20/02/17
  19800            05 TIPCOR                     PIC X(1)   VALUE "M".                                                  20/02/17
  19900            05 CONDIC                     PIC X      VALUE "V".                                                  20/02/17
  20000            05 TIPPRO                     PIC X      VALUE "1".                                                  20/02/17
  20100            05 ACERRAR                    PIC X      VALUE " ".                                                  28/02/17

               01  POLUNITST.
                   05 LOC-UT  PIC X(2) VALUE ='01'.
                   05 MCO-UT  PIC X(2) VALUE ='01',
  20200                                                                                                                 19/12/16
  20300        01  WFCASOSW.                                                                                            19/12/16
  20400            05 WTEMC45       PIC X(11).                                                                          19/12/16
  20500            05 WFECMAX       PIC X(08).                                                                          19/12/16
  20600                                                                                                                 20/12/16
  20700        01  WPARAMETROSG.                                                                                        20/12/16
  20800            05 WANORES       PIC S9(02) COMP-3.                                                                  20/02/20
  20900            05 WMESDEF       PIC S9(02) COMP-3.                                                                  20/02/20
  21000            05 WMESVIV       PIC S9(02) COMP-3.                                                                  20/02/20
  21100            05 WACTUAL       PIC X(08).                                                                          20/12/16
  21200            05 WUSERAC       PIC X(10).                                                                          20/12/16
  21300            05 WCORRID       PIC X(08).                                                                          20/12/16
  21400            05 WFCALCU       PIC X(08).                                                                          04/01/17
  21500            05 WMESPRO       PIC X(02).                                                                          20/12/16
  21600            05 WANOPRO       PIC X(04).                                                                          20/12/16
  21700                                                                                                                 19/12/16
  21800        01  WRY1VGR.                                                                                             19/02/17
  21900            COPY DDS-ALL-FORMATS OF RY1VGR.                                                                      19/02/17
  22000                                                                                                                 19/02/17
  22100        01  RECORD_FECHAS.                                                                                       10/02/20
  22200            05 AGENCIA-REC      PIC X(2).                                                                        10/02/20
  22300            05 NUM-CASO-REC     PIC X(11).                                                                       10/02/20
  22400            05 FEC-MOVTO-REC    PIC X(8).                                                                        20/02/20
  22500            05 EST-SIMA         PIC X(1).                                                                        10/02/20
  22600            05 EST-RT           PIC X(1).                                                                        10/02/20
  22700            05 REC-SIMA         PIC X(2).                                                                        10/02/20
  22800            05 ENV-SIMA         PIC X(2).                                                                        10/02/20
  22900            05 MOT-REAPERTURA   PIC X(1).                                                                        10/02/20
  23000            05 USR-REAPERTURA   PIC X(20).                                                                       10/02/20
  23100                                                                                                                 27/10/15
  23200            EXEC SQL                                                                                             26/10/15
  23300               INCLUDE SQLCA                                                                                     26/10/15
  23400            END-EXEC.                                                                                            20/02/20
  23500                                                                                                                 26/10/15
  23600        01 WFDESDECORR          PIC X(08).                                                                       20/12/16
  23700        LINKAGE SECTION.                                                                                         19/12/16
  23800        01 WFHASTACORR          PIC X(08).                                                                       20/12/16
  23801        01 WFTIPOCORR           PIC X.                                                                           17/01/20
  23900                                                                                                                 19/12/16
  24000        PROCEDURE DIVISION USING WFHASTACORR WFTIPOCORR.                                                         17/01/20
  24100        A000-INICIO.                                                                                             19/12/16
  24200            INITIALIZE FILESTATUS.                                                                               20/02/17
  24201            CALL "F025SNAP" USING WUSUARIO                                                                       20/01/20
  24202            EVALUATE WFTIPOCORR                                                                                  17/01/20
  24203            WHEN "N"                                                                                             17/01/20
  24204                 EXEC SQL                                                                                        22/01/20
  24205                     DELETE FROM BITREC                                                                          22/01/20
  24206                     WHERE BITFE9 = :WFHASTACORR                                                                 20/02/20
  24207                 END-EXEC                                                                                        20/02/20
  24208       *         EXEC SQL                                                                                        05/03/20
  24209       *             DELETE FROM BITCAS                                                                          05/03/20
  24210       *         END-EXEC                                                                                        05/03/20
  24211                 EXEC SQL                                                                                        20/01/20
  24212                     SELECT                                                                                      20/01/20
  24215                        RY1FE3                                                                                   20/01/20
  24216                       ,RY1F06                                                                                   20/01/20
  24217                       ,RY1F08                                                                                   20/01/20
  24218                       ,RY1ANO                                                                                   20/01/20
  24219                       ,RY1ME1                                                                                   20/01/20
  24220                       ,RY1US4                                                                                   20/01/20
  24221                       ,RY1MON                                                                                   14/08/20
  24222                     INTO                                                                                        20/02/20
  24223                         :FE3-WS                                                                                 20/01/20
  24224                        ,:F06-WS                                                                                 20/01/20
  24225                        ,:F08-WS                                                                                 20/01/20
  24226                        ,:ANO-WS                                                                                 20/01/20
  24227                        ,:MES-WS                                                                                 20/01/20
  24228                        ,:USU-WS                                                                                 20/01/20
  24229                        ,:SALJUDIC-WS                                                                            14/08/20
  24230                     FROM RY1PVH                                                                                 20/01/20
  24231                 END-EXEC                                                                                        20/02/20
  24257            WHEN "R"                                                                                             17/01/20
  24258                 PERFORM RECUPERACIONDATOS                                                                       20/02/20
  24259            END-EVALUATE                                                                                         20/02/20
  24261              ACCEPT HORAMOMENTO  FROM TIME                                                                      20/02/20
  24262              ACCEPT FECHAMOMENTO FROM DATE                                                                      20/02/20
  24263              MOVE ZEROS TO TOTALPRO-WS                                                                          20/02/20
  24264                            TOTALGAS-WS                                                                          20/02/20
  24265                            TOTALPAJ-WS                                                                          20/02/20
  24266                            TOTALSAL-WS                                                                          20/02/20
  24267                            SALDORCD                                                                             03/02/20
  24268                            CASOSALC-WS                                                                          06/02/20
  24269                            CASOSALN-WS                                                                          06/02/20
  24270                            CASOSAPL-WS                                                                          06/02/20
  24271                            CASOSDLC-WS                                                                          06/02/20
  24272                            CASOSDLN-WS                                                                          06/02/20
  24273                            CASOSDPL-WS                                                                          06/02/20
  24274            EXEC SQL                                                                                             05/02/20
  24275                INSERT INTO BITREC VALUES                                                                        20/02/20
  24276                     ("PARAMETROS"                                                                               22/01/20
  24277                     ,:WFHASTACORR                                                                               22/01/20
  24278                     ," "                                                                                        22/01/20
  24279                     ," "                                                                                        22/01/20
  24280                     ," "                                                                                        22/01/20
  24281                     ," "                                                                                        22/01/20
  24282                     ," "                                                                                        22/01/20
  24283                     ," "                                                                                        22/01/20
  24284                     ," "                                                                                        22/01/20
  24285                     ," "                                                                                        22/01/20
  24286                     ," "                                                                                        22/01/20
  24287                     ,:FE3-WS                                                                                    22/01/20
  24288                     ,:F06-WS                                                                                    22/01/20
  24289                     ,:F08-WS                                                                                    22/01/20
  24290                     ,:ANO-WS                                                                                    22/01/20
  24291                     ,:MES-WS                                                                                    22/01/20
  24292                     ,:FECHAMOMENTO                                                                              20/02/20
  24293                     ,:HORAMOMENTO                                                                               20/02/20
  24294                     ,:WUSUARIO                                                                                  27/01/20
  24295                     ," "                                                                                        27/01/20
  24296                     ," "                                                                                        30/01/20
  24297                     ,:TOTALPRO-WS                                                                               20/02/20
  24298                     ,:TOTALGAS-WS                                                                               20/02/20
  24299                     ,:TOTALPAJ-WS                                                                               20/02/20
  24300                     ,:TOTALSAL-WS                                                                               20/02/20
  24301                     ,:SALDORCD                                                                                  23/07/20
  24302                     ,:SALJUDIC-WS)                                                                              14/08/20
  24303            END-EXEC                                                                                             20/02/20
  24304            MOVE WFHASTACORR  TO WFDESDECORR.                                                                    20/02/20
  24400            MOVE "01"         TO WFDESDECORR(7:2).                                                               20/02/20
  24700       *                                                                                                         20/12/16
  24701            MOVE SPACES            TO MENSAJE                                                                    20/02/20
  24702            MOVE "INICIO"          TO MENSAJE                                                                    20/02/20
  24703            PERFORM GRABA-BITACORA                                                                               22/01/20
  24800            ACCEPT AMDHASTA-WS FROM DATE.                                                                        19/12/16
  24900            MOVE 20                TO CHASTA-WS.                                                                 20/02/20
  25000            MOVE FCURRENT          TO FCURRENT-WS                                                                20/02/20
  25100       **   MOVE FDESDE-CURRENT    TO FDE.                                                                       20/02/20
  25200  RVR  **   Para que procese desde el primer desde de parámetros de                                              12/01/17
  25300       **   reserva, en vez de hechar 50 a¦os hacia atrás del current                                            12/01/17
  25400       **   procesará desde el 01/01/1967 en adelante. esTO va a crecer                                          20/02/20
  25500  ==>       MOVE "19670101"        TO FDE.                                                                       20/02/20
  25600            MOVE FCURRENT          TO FHA.                                                                       20/02/20
  25700            MOVE WFHASTACORR(1:4)  TO WANOCORRIDA.                                                               20/02/20
  25800            MOVE WFHASTACORR(5:2)  TO WMESCORRIDA.                                                               20/02/20
  25900            MOVE WMESCORRIDA       TO MESCORR.                                                                   20/02/20
  26000            MOVE WANOCORRIDA       TO ANOCORR.                                                                   20/02/20
  26100            MOVE WANOMESCORR       TO WANOMESCORRIDA.                                                            20/02/20
  26200            PERFORM A010-PROCESO.                                                                                12/12/16
  26300                                                                                                                 26/11/16
  26400        A010-PROCESO.                                                                                            12/12/16

      *>         SE LIMPIAN TABLAS DE CONTROL DE VARIABLES DEL PROCESO 
  26411            EXEC SQL                                                                                             05/02/20
  26412                DELETE FROM RY1CIF                                                                               30/01/20
  26413                WHERE RY1FEC = :WFHASTACORR                                                                      30/01/20
  26414            END-EXEC                                                                                             20/02/20
  26415            EXEC SQL                                                                                             05/02/20
  26416                DELETE FROM CASTEM                                                                               22/01/20
  26417            END-EXEC                                                                                             20/02/20
  26426            EXEC SQL                                                                                             10/07/19
  26427                DELETE FROM RY1ACT                                                                               10/07/19
  26428            END-EXEC   
  
      *>         Se agrega a la tabla de control la fecha hasta para controlar el rango de ejecucion del proceso 
  26429            EXEC SQL                                                                                             10/07/19
  26430                INSERT INTO RY1ACT                                                                               20/02/20
  26431                VALUES                                                                                           10/07/19
  26432                (:WFHASTACORR)                                                                                   10/07/19
  26433            END-EXEC                                                                                             20/02/20
  26434            EXEC SQL                                                                                             29/01/20
  26435             COMMIT                                                                                              29/01/20
  26436            END-EXEC                                                                                             20/02/20
  26500       *...... ACTUALIZA EL ESTADO DEL SIMA A RT MASIVAMENTE .....*                                              20/02/17
      *>         Variables de spool?
  26501            MOVE SPACES            TO MENSAJE                                                                    20/02/20
  26502            MOVE "INICIA RY1PBRCS" TO MENSAJE                                                                    20/02/20
  26503            PERFORM GRABA-BITACORA                                                                               06/02/20
  26600            CALL "RY1PBRCS" USING TIPCOR.                                                                        20/03/17
  26601            MOVE SPACES            TO MENSAJE                                                                    20/02/20
  26602            MOVE "FIN RY1PBRCS"    TO MENSAJE                                                                    20/02/20
  26603            PERFORM GRABA-BITACORA                                                                               06/02/20
  26700       * QUERY PARA OBTENER Y ACTUALIZAR PARAMETROS VARIOS RESERVAS                                              20/12/16
  26800            EXEC SQL                                                                                             28/02/17
  26900                 DECLARE CPARAM CURSOR FOR                                                                       27/02/17
  27000                 SELECT                                                                                          27/02/17
  27100                        CAST(ANOSRE AS NUMERIC(2)) ANOSRE,                                                       27/02/17
  27200                        CAST(RY1MDF AS NUMERIC(2)) RY1MDF,                                                       27/02/17
  27300                        CAST(RY1MER AS NUMERIC(2)) RY1MER,                                                       27/02/17
  27400                        RY1FE3, RY1US4, RY1F06, RY1F08, RY1ME1, RY1ANO                                           27/02/17
  27500                 FROM RY1PVH                                                                                     27/02/17
  27600                 WHERE RY1FPR = "20160101"                                                                       27/02/17
  27700            END-EXEC.                                                                                            20/02/20
  27800            EXEC SQL                                                                                             27/02/17
  27900                 OPEN CPARAM                                                                                     27/02/17
  28000            END-EXEC.                                                                                            20/02/20
  28100            EXEC SQL                                                                                             27/02/17
  28200                 FETCH NEXT FROM CPARAM INTO                                                                     20/02/20
  28300                       :WANORES, :WMESDEF, :WMESVIV, :WACTUAL,                                                   27/02/17
  28400                       :WUSERAC, :WCORRID, :WFCALCU, :WMESPRO, :WANOPRO                                          27/02/17
  28500            END-EXEC.  
  
                 
  28600       *==================================================                                                       09/01/17
  28700       * Obtiene los meses de gracias vivo y muerTOs para efecTOs de                                             20/02/20
  28800       * cerrar o no un caso. De momenTO están 2 meses vivos y 4 muerTO                                          20/02/20
  28900       * Resta n cantidad e meses a la fecha de corrida.                                                         21/02/17
  29000       *--> Resta Meses a una Fecha y obtiene nueva fecha                                                        21/02/17
  29001            MOVE SPACES            TO MENSAJE                                                                    20/02/20
  29002            MOVE "INICIA RY1FTOPE" TO MENSAJE                                                                    20/02/20
  29003            PERFORM GRABA-BITACORA                                                                               06/02/20
  29100            CALL "RY1FTOPE" USING WFHASTACORR WFTOPEV WFTOPEM.                                                   20/02/20
  29101            MOVE SPACES            TO MENSAJE                                                                    20/02/20
  29102            MOVE "FIN RY1FTOPE"    TO MENSAJE                                                                    20/02/20
  29103            PERFORM GRABA-BITACORA                                                                               06/02/20
  29200       *==================================================                                                       09/01/17
  29300       *ACT - F.ACTUAL, USUARIO, F.CORRIDA, FCALCUL, MES.PROC, ANO.PROC.                                         20/12/16
  29400            EXEC SQL                                                                                             27/02/17
  29500                 UPDATE RY1PVH                                                                                   27/02/17
  29600                 SET  RY1FE3 = :FCURRENT-WS,                                                                     27/02/17
  29700                      RY1US4 = :WUSUARIO,                                                                        27/02/17
  29800                      RY1F06 = :FCURRENT-WS,                                                                     27/02/17
  29900                      RY1F08 = :WFHASTACORR,                                                                     27/02/17
  30000                      RY1ME1 = :WMESCORRIDA,                                                                     27/02/17
  30100                      RY1ANO = :WANOCORRIDA                                                                      27/02/17
  30200                 WHERE RY1FPR = "20160101"                                                                       27/02/17
  30300            END-EXEC.                                                                                            20/02/20
  30400            EXEC SQL                                                                                             27/02/17
  30500                 CLOSE CPARAM                                                                                    27/02/17
  30600            END-EXEC.                                                                                            20/02/20
  30700       * QUERY PARA OBTENER RY1VGR, VALORES GENERALES DE RESERVA ACTUAL                                          19/02/17
  30800            EXEC SQL                                                                                             27/02/17
  30900                 DECLARE CPARVGR CURSOR FOR                                                                      27/02/17
  31000                 SELECT RY1FE6, RY1FE7, RY1ITM, RY1PAT,                                                          27/02/17
  31100                        RY1IPM, RY1PAP, RY1CTM, RY1PCM,                                                          27/02/17
  31200                        RY1IXM, RY1PIM, RY1CA1, RY1CSA,                                                          27/02/17
  31300                        RY1PO3, RY1FE9, RY1F01, RY1US2                                                           27/02/17
  31400                 FROM RY1VGR                                                                                     27/02/17
  31500                 WHERE RY1FE6 <= :WFHASTACORR  AND                                                               27/02/17
  31600                       RY1FE7 >= :WFHASTACORR                                                                    27/02/17
  31700            END-EXEC.                                                                                            20/02/20
  31800            EXEC SQL                                                                                             27/02/17
  31900                 OPEN CPARVGR                                                                                    27/02/17
  32000            END-EXEC.                                                                                            20/02/20
  32100            EXEC SQL                                                                                             27/02/17
  32200                 FETCH NEXT FROM CPARVGR INTO                                                                    20/02/20
  32300                       :RY1FE6, :RY1FE7, :RY1ITM, :RY1PAT,                                                       27/02/17
  32400                       :RY1IPM, :RY1PAP, :RY1CTM, :RY1PCM,                                                       27/02/17
  32500                       :RY1IXM, :RY1PIM, :RY1CA1, :RY1CSA,                                                       27/02/17
  32600                       :RY1PO3, :RY1FE9, :RY1F01, :RY1US2                                                        27/02/17
  32700            END-EXEC.                                                                                            20/02/20
  32800       *=======================================================*                                                 20/02/17
  32900       *=======================================================*                                                 20/02/17
  33000       * LIMPIA LOS ARCHIVOS DE TRABAJO y LOS ARCHIVOS DESTINO *                                                 20/02/17
  33100       *=======================================================*                                                 20/02/17
  33101            MOVE SPACES                      TO MENSAJE                                                          20/02/20
  33102            MOVE "INICIO LIMPIANDO ARCHIVOS" TO MENSAJE                                                          20/02/20
  33103            PERFORM GRABA-BITACORA                                                                               21/01/20
  33200            EXEC SQL                                                                                             28/02/17
  33300                 DELETE FROM RY1RCA                                                                              28/02/17
  33400                 WHERE  SUBSTRING(RY1FMR, 1, 4) = :WANOCORRIDA                                                   28/02/17
  33500                 AND    SUBSTRING(RY1FMR, 5, 2) = :WMESCORRIDA                                                   28/02/17
  33600            END-EXEC.                                                                                            20/02/20
  33700       *                                                                                                         28/02/17
  33800            EXEC SQL                                                                                              28/02/17
  33900                 DELETE FROM RY1RCD                                                                              28/02/17
  34000                 WHERE  SUBSTRING(RY1F03, 1, 4) = :WANOCORRIDA                                                   28/02/17
  34100                 AND    SUBSTRING(RY1F03, 5, 2) = :WMESCORRIDA                                                   28/02/17
  34200            END-EXEC.                                                                                            20/02/20
  34300       *                                                                                                         28/02/17
  34400            EXEC SQL                                                                                             28/02/17
  34500                 DELETE FROM RY1HVR                                                                              28/02/17
  34600                 WHERE  RY1AOD = :WANOCORRIDA                                                                    28/02/17
  34700                 AND    RY1MES = :WMESCORRIDA                                                                    28/02/17
  34800            END-EXEC.                                                                                            20/02/20
  34900            EXEC SQL                                                                                             28/02/17
  35000                 DELETE FROM RY2HVR                                                                              28/02/17
  35100            END-EXEC.                                                                                            20/02/20
  35200            EXEC SQL                                                                                             28/02/17
  35300                 DELETE FROM RY1DUP                                                                              28/02/17
  35400            END-EXEC.                                                                                            20/02/20
  35500            EXEC SQL                                                                                             28/02/17
  35600                 DELETE FROM FECINIRES1                                                                          28/02/17
  35700            END-EXEC.                                                                                            20/02/20
  35800            EXEC SQL                                                                                             28/02/17
  35900                 DELETE FROM FECINIRES                                                                           28/02/17
  36000            END-EXEC.                                                                                            20/02/20
  36100            EXEC SQL                                                                                             28/02/17
  36200                 DELETE FROM TEMFMX                                                                              28/02/17
  36300            END-EXEC.                                                                                            20/02/20
  36400            EXEC SQL                                                                                             28/02/17
  36500                 DELETE FROM TEMFMX1                                                                             28/02/17
  36600            END-EXEC.                                                                                            20/02/20
  36700            EXEC SQL                                                                                             28/02/17
  36800                 DELETE FROM TEMFMXX                                                                             28/02/17
  36900            END-EXEC.                                                                                            20/02/20
  37000            EXEC SQL                                                                                             28/02/17
  37100                 DELETE FROM TEMMTO                                                                              20/02/20
  37200            END-EXEC.                                                                                            20/02/20
  37300            EXEC SQL                                                                                             28/02/17
  37400                 DELETE FROM RY1PRV                                                                              28/02/17
  37500                 WHERE RY1FPV = :WFHASTACORR                                                                     28/02/17
  37600            END-EXEC.                                                                                            20/02/20
  37700            EXEC SQL                                                                                             28/02/17
  37800                 DELETE FROM RY2HVR                                                                              28/02/17
  37900                 WHERE FCORTE = :WANOMESCORRIDA                                                                  28/02/17
  38000            END-EXEC.                                                                                            20/02/20
  38001            MOVE SPACES                   TO MENSAJE                                                             20/02/20
  38002            MOVE "FIN LIMPIANDO ARCHIVOS" TO MENSAJE                                                             20/02/20
  38003            PERFORM GRABA-BITACORA                                                                               21/01/20
  38100       *=======================================================*                                                 20/02/17
  38200       *=======================================================*                                                 20/02/17
  38300       *----------------------------                                                                             08/01/17
  38400       * QUERY PARA OBTENER LOS CASOS DUPLICADOS DIFERENTE AGENCIA                                               05/03/20
  38500       * ESTOS SE EXCLUIRAN DE PROCESO                                                                           20/02/20
  38501            MOVE SPACES               TO MENSAJE                                                                 20/02/20
  38502            MOVE "OBTIENE DUPLICADOS" TO MENSAJE                                                                 20/02/20
  38503            PERFORM GRABA-BITACORA                                                                               21/01/20
  38600            EXEC SQL                                                                                             28/02/17
  38700                 INSERT INTO RY1DUP                                                                              20/02/20
  38800                 SELECT R09CAS CASNU7, COUNT(1) CASCAN                                                           20/02/20
  38900                 FROM R09MAE                                                                                     20/02/20
  39000                 WHERE R09POL <> "NEMAGON"                                                                       09/04/19
  39100                 GROUP BY R09CAS                                                                                 28/02/17
  39200                 HAVING COUNT(1) > 1                                                                             20/02/20
  39300            END-EXEC.                                                                                            20/02/20
  39301            MOVE SPACES                   TO MENSAJE                                                             20/02/20
  39302            MOVE "FIN OBTIENE DUPLICADOS" TO MENSAJE                                                             20/02/20
  39303            PERFORM GRABA-BITACORA                                                                               21/01/20
  39400       *----------------------------                                                                             08/01/17
  39500       * QUERY PARA DETERMINAR FECXXX QUE SERÁ LA DE PIVOTE PARA                                                 19/12/16
  39600       * REVISAR FECHA DE PAGO > QUE ÉSTA                                                                        19/12/16
  39700       * SELECCIONA ENTRE LA FECHA DE ACCIDENTE O LA FECHA DE AVISO                                              19/12/16
  39800       * MÁXIMA SI EXISTE EN CASO NO AMPARADO                                                                    19/12/16
  39801            MOVE SPACES                      TO MENSAJE                                                          20/02/20
  39802            MOVE "OBTENIENDO FECHAS MAXIMAS" TO MENSAJE                                                          20/02/20
  39803            PERFORM GRABA-BITACORA                                                                               21/01/20
  39900            EXEC SQL                                                                                             28/02/17
  40000                INSERT INTO FECINIRES1                                                                           20/02/20
  40100                SELECT R59CAS NUMCAS59, MAX(R59FER) FECMAX59                                                     28/02/17
  40200                FROM R59DET30                                                                                    28/02/17
  40300                WHERE R59EST <> "C" AND R59EST <> "X" AND R59FER <> " "                                          28/02/17
  40400                GROUP BY R59CAS                                                                                  28/02/17
  40500            END-EXEC.                                                                                            20/02/20
  40600            EXEC SQL                                                                                             28/02/17
  40700                INSERT INTO FECINIRES                                                                            20/02/20
  40800                SELECT  DISTINCT(C.R09CAS) NUMCAS,                                                               20/02/20
  40900                        CASE WHEN FECMAX59 > R09FEC                                                              28/02/17
  41000                             THEN FECMAX59                                                                       28/02/17
  41100                        ELSE R09FEC                                                                              28/02/17
  41200                        END  FECXXX                                                                              20/02/20
  41300                FROM R09MAE59 C LEFT JOIN FECINIRES1 ON                                                          20/02/20
  41400                C.R09CAS = NUMCAS59                                                                              28/02/17
  41500                WHERE NOT EXISTS(SELECT CASNU7 FROM RY1DUP                                                       20/02/20
  41600                                 WHERE C.R09CAS = CASNU7)                                                        20/02/20
  41700            END-EXEC.                                                                                            20/02/20
  41800            EXEC SQL                                                                                             28/02/17
  41900                DELETE FROM FECINIRES WHERE FECXXX = " "                                                         28/02/17
  42000            END-EXEC.                                                                                            20/02/20
  42001            MOVE SPACES                          TO MENSAJE                                                      20/02/20
  42002            MOVE "FIN OBTENIENDO FECHAS MAXIMAS" TO MENSAJE                                                      20/02/20
  42003            PERFORM GRABA-BITACORA                                                                               21/01/20
  42100       *===================V006==========================                                                        09/04/19
  42200       * SQL PARA ELIMINAR CASOS ASOCIADOS A LA POLIZA                                                           09/04/19
  42300       * 'NEMAGON'                                                                                               09/04/19
  42400            EXEC SQL                                                                                             09/04/19
  42500                DELETE FROM FECINIRES WHERE                                                                      05/03/20
  42501                EXISTS                                                                                           05/03/20
  42600                (SELECT * FROM R09MAE25                                                                          05/03/20
  42700                 WHERE R09CAS = NUMCAS                                                                           05/03/20
  42701                 AND   R09POL = "NEMAGON")                                                                       05/03/20
  42800            END-EXEC                                                                                             20/02/20
  42900       *===================V006==========================                                                        09/04/19
  42901            MOVE SPACES                            TO MENSAJE                                                    20/02/20
  42902            MOVE "INICIA GRABA TEMFMXX TEMPORALES" TO MENSAJE                                                    20/02/20
  42903            PERFORM GRABA-BITACORA                                                                               21/01/20
  43000       *-- %%%%%%%%%%%%%%%%%%%%%%%%                                                                              29/12/16
  43001               EXEC SQL                                                                                          05/03/20
  43002               INSERT INTO TEMFMXX                                                                               05/03/20
  43003               WITH                                                                                              05/03/20
  43004               FMAXTEM AS                                                                                        05/03/20
  43005                 (SELECT  DISTINCT  "IT",  R14CAS, MAX(R14FRC) FECMAX                                            05/03/20
  43006                 FROM   (R09MAE59 A INNER JOIN R14DET70                                                          05/03/20
  43007                 ON      A.R09CAS = R14CAS                                                                       05/03/20
  43008                 INNER JOIN R68DET51                                                                             05/03/20
  43009                 ON      R14CAS = R68CAS AND R14REC = R68REC)                                                    05/03/20
  43010                 LEFT JOIN FECINIRES F ON R14CAS = NUMCAS                                                        05/03/20
  43012                 WHERE NOT EXISTS(SELECT CASNU7 FROM RY1DUP                                                      05/03/20
  43013                                  WHERE A.R09CAS = CASNU7) AND                                                   05/03/20
  43014                 R09FEC <> " "  AND                                                                              05/03/20
  43015                 R09FEC BETWEEN :FDE AND :FHA AND                                                                05/03/20
  43017                 R09ACE NOT IN("30", "31", "32") AND                                                             05/03/20
  43018                 R14FRC <> " " AND                                                                               05/03/20
  43019                 R14FRC <= :WFHASTACORR AND                                                                      05/03/20
  43020                 R14EST <> "2" AND                                                                               05/03/20
  43021                 R68PAG NOT IN("902", "903")                                                                     05/03/20
  43022                 GROUP BY R14CAS)                                                                                05/03/20
  43023               ,FMAXPERMH AS                                                                                     05/03/20
  43024               (SELECT  DISTINCT "IP",  R14CAS, MAX(R14FRC) FECMAX                                               05/03/20
  43025                FROM (R09MAE59 C INNER JOIN R14DET70 ON C.R09CAS =                                               05/03/20 
  43026               R14CAS                                                                                            05/03/20
  43027                INNER JOIN R68DET51 ON R14CAS = R68CAS AND R14REC =                                              05/03/20
  43028               R68REC)                                                                                           05/03/20
  43029                LEFT JOIN FECINIRES ON R14CAS = NUMCAS                                                           05/03/20
  43030                WHERE                                                                                            05/03/20
  43031                      NOT EXISTS(SELECT CASNU7 FROM RY1DUP                                                       05/03/20
  43032                                 Where c.R09CAS = CASNU7) AND                                                    05/03/20
  43033                         R09FEC <> " "                                                                           05/03/20
  43034                         AND R09FEC BETWEEN :FDE AND :FHA                                                        05/03/20
  43035                  AND                                                                                            05/03/20
  43036                    R09ACE NOT IN("30", "31", "32") AND                                                          05/03/20
  43037                    R14FRC <> " " AND R14FRC <= :WFHASTACORR AND                                                 05/03/20
  43038                    R14EST <> "2" AND R68PAG = "902"                                                             05/03/20
  43039                    GROUP BY R14CAS    )                                                                         05/03/20
  43040               ,FMAXPERM AS                                                                                      05/03/20
  43041                  (SELECT DISTINCT "IP", R33CAS, MAX(R20FIF) FecMax                                              05/03/20
  43042                   FROM R33MAE51 INNER JOIN R20DET55 ON R33FIJ = R20FIJ                                          05/03/20
  43043                   WHERE                                                                                         05/03/20
  43044                          R20FIF >= "19970916" AND                                                               05/03/20
  43045                          R20EFI IN("1", "2", "3")                                                               05/03/20
  43046                   Group by R33CAS, R20FIJ                                                                       05/03/20
  43047                  UNION                                                                                          05/03/20
  43048                  SELECT DISTINCT "IP", R33CAS, MAX(R20FIF) FecMax                                               05/03/20
  43049                  FROM R33MAE51 INNER JOIN R20DET55 ON R33FIJ = R20FIJ                                           05/03/20
  43050                  WHERE                                                                                          05/03/20
  43051                         R20FIF < "19970916" AND R20EFI = "1"                                                    05/03/20
  43052                  Group by R33CAS, R20FIJ                                                                        05/03/20
  43053                  UNION                                                                                          05/03/20
  43054                  SELECT DISTINCT "IP", R33CAS, MAX(R20FIF) FecMax                                               05/03/20
  43055                  FROM R33MAE51 INNER JOIN R20DET55 ON R33FIJ = R20FIJ                                           05/03/20
  43056                  WHERE                                                                                          05/03/20
  43057                         R20FIF < "19970916" AND R20EFI = "2"                                                    05/03/20
  43058                Group by R33CAS, R20FIJ                                                                          05/03/20
  43059                UNION                                                                                            05/03/20
  43060                SELECT  DISTINCT "IP", R33CAS, MAX(R58FEC) FecMax                                                05/03/20
  43061                FROM (R33MAE51 INNER JOIN R20DET55 ON R33FIJ = R20FIJ)                                           05/03/20
  43062                INNER JOIN R58DET10 ON R58FIJ = R20FIJ AND                                                       05/03/20
  43063                                       R58CED = R20PER                                                           05/03/20
  43064                WHERE                                                                                            05/03/20
  43065                      R58EST = "1" AND R58TIP <> "J"                                                             05/03/20
  43066                      AND R58FEC <= "20200301"                                                                   05/03/20
  43067                Group by R33CAS, R58FIJ  )                                                                       05/03/20
  43068               ,FMAXCTAH AS                                                                                      05/03/20
  43069                 (SELECT DISTINCT "CM", R14CAS, MAX(R14FRC) FECMAX                                               05/03/20
  43070                  FROM (R09MAE59 C INNER JOIN R14DET70 ON R09CAS =                                               05/03/20
  43071                R14CAS                                                                                           05/03/20
  43072                  INNER JOIN R68DET51 ON R14CAS = R68CAS AND                                                     05/03/20
  43073                        R14REC = R68REC)                                                                         05/03/20
  43074                  LEFT JOIN FECINIRES ON R14CAS = NUMCAS                                                         05/03/20
  43076                  WHERE NOT EXISTS(SELECT CASNU7 FROM RY1DUP                                                     05/03/20
  43077                                    WHERE C.R09CAS = CASNU7) AND                                                 05/03/20
  43078                   R09FEC <> " " AND R09FEC BETWEEN :FDE AND :FHA                                                05/03/20
  43080                   AND R09ACE NOT IN("30", "31", "32") AND                                                       05/03/20
  43081                   R14FRC <> " " AND R14FRC <> :WFHASTACORR                                                      05/03/20
  43082                   AND R14EST <> "2" AND R68PAG = "903"                                                          05/03/20
  43083                   GROUP BY R14CAS)                                                                              05/03/20
  43084               ,FMAXCTA AS                                                                                       05/03/20
  43085                 (SELECT  DISTINCT "CM", R49CAS, MAX(R15PAG) FECMAX                                              05/03/20
  43086                  FROM R49DET10 LEFT JOIN R15DET56 ON R49CON = R15CON                                            05/03/20
  43088                  WHERE                                                                                          05/03/20
  43089                  R49CAS <> "19850999999" AND                                                                    05/03/20
  43090                  R15EST <> "4" AND                                                                              05/03/20
  43091                  R15PAG <= :WFHASTACORR AND R15PAG <> " " AND                                                   05/03/20
  43092                  R15TIP NOT IN ("709", "710", "711")                                                            05/03/20
  43093                  GROUP BY R49CAS  )                                                                             05/03/20
  43094               ,FMAXINDM AS                                                                                      05/03/20
  43095                  (SELECT DISTINCT "IM", R33CAS, MAX(R54INC) FECMAX                                              05/03/20
  43096                   FROM  R33MAE51 INNER JOIN R54DET53 ON R33FIJ = R54FIJ                                         05/03/20
  43098                   WHERE R54EST IN("1","2","3") AND R54INC <= :FHA                                               05/03/20
  43100                   AND R54INC <> " " AND R54PAG <= :FHA                                                          05/03/20
  43101                   GROUP BY R33CAS )                                                                             05/03/20
  43102               ,FMAXINDH AS                                                                                      05/03/20
  43103                 (SELECT DISTINCT "IM", R33CAS, MAX(R58FEC) FECMAX                                               05/03/20
  43104                  FROM  R33MAE51 INNER JOIN R54DET53 ON R33FIJ = R54FIJ                                          05/03/20
  43105                  INNER JOIN R58DET10 ON R54FIJ = R58FIJ                                                         05/03/20
  43107                  WHERE R54EST IN("1", "2", "3") AND R58EST = "1"                                                05/03/20
  43108                  AND R54CE1 = R58CEB AND R58TIP <> "J"  AND                                                     05/03/20
  43109                  R58FEC <= :FHA AND R58FEC <> " "                                                               05/03/20
  43110                  GROUP BY R33CAS  )                                                                             05/03/20
  43111                SELECT * FROM FMAXTEM                                                                            05/03/20
  43112                UNION                                                                                            05/03/20
  43113                SELECT * FROM FMAXPERMH                                                                          05/03/20
  43114                UNION                                                                                            05/03/20
  43115                SELECT * FROM FMAXPERM                                                                           05/03/20
  43116                UNION                                                                                            05/03/20
  43117                SELECT * FROM FMAXCTAH                                                                           05/03/20
  43118                UNION                                                                                            05/03/20
  43119                SELECT * FROM FMAXCTA                                                                            05/03/20
  43120                UNION                                                                                            05/03/20
  43121                SELECT * FROM FMAXINDM                                                                           05/03/20
  43122                UNION                                                                                            05/03/20
  43123                SELECT * FROM FMAXINDH                                                                           05/03/20
  43124            END-EXEC.                                                                                            05/03/20
  43125       *-- %%%%%%%%%%%%%%%%%%%%%%%%                                                                              05/03/20
  43126       *                                                                                                         05/03/20
  55600            EXEC SQL                                                                                             28/02/17
  55700                DELETE FROM TEMFMXX WHERE TEMMAX = " "                                                           28/02/17
  55800            END-EXEC.                                                                                            20/02/20
  55900       *                                                                                                         27/02/17
  56000       *==================V006===========================================                                        09/04/19
  56100       * SQL PARA ELIMINAR CASOS ASOCIADOS A LA POLIZA                                                          
  
              
  56200       * 'NEMAGON'                                                                                               09/04/19
  56300            EXEC SQL                                                                                             09/04/19
  56400                DELETE FROM TEMFMXX WHERE TEMC45 IN                                                              09/04/19
  56500                (SELECT R09CAS FROM R09MAE                                                                       09/04/19
  56600                WHERE R09POL = "NEMAGON")                                                                        09/04/19

  56700            END-EXEC                                                                                             20/02/20
  56800       *==================V006==========================================                                         09/04/19
  56900            EXEC SQL                                                                                             09/04/19
  57000                 INSERT INTO TEMFMX                                                                              20/02/20
  57100                 SELECT DISTINCT TEMRUB, TEMC45, MAX(TEMMAX) FecMax                                              28/02/17
  57200                 FROM TEMFMXX                                                                                    28/02/17
  57300                 GROUP BY TEMRUB, TEMC45                                                                         28/02/17
  57400            END-EXEC.                                                                                            20/02/20
  57500       *--  Archivo de fechas definitivo  TEMFMX1                                                                18/01/17
  57600            EXEC SQL                                                                                             28/02/17
  57700                 INSERT INTO TEMFMX1                                                                             20/02/20
  57800                 SELECT Distinct "AL", TEMC45, MAX(TEMMAX)                                                       28/02/17
  57900                 FROM TEMFMX                                                                                     28/02/17
  58000                 GROUP BY TEMC45                                                                                 28/02/17
  58100            END-EXEC.                                                                                            20/02/20
  58200       *=========================================================*                                               08/01/17
  58300       *-- QUERYS PARA OBTENER LOS PAGOS REALIZADOS POR RUBRO EN UN                                              08/01/17
  58400       *-- PERIODO DADO DE INICIO A FIN DE MES                                                                   19/12/16
  58500       ** ==> ARCHIVO PAGOS INCAPACIDAD TEMPORAL                                                                 12/01/17
  58501            MOVE SPACES                   TO MENSAJE                                                             20/02/20
  58502            MOVE "GRABANDO TEMMTO TEMPOR" TO MENSAJE                                                             20/02/20
  58503            PERFORM GRABA-BITACORA                                                                               21/01/20
  58504            EXEC SQL                                                                                             05/03/20
  58505                INSERT INTO TEMMTO                                                                               05/03/20
  58506                WITH                                                                                             05/03/20
  58507                PAGOSTEM AS                                                                                      05/03/20
  58508                  (SELECT "IT" TEMRBR, R14CAS TEMC47, " " TEMFEP,                                                05/03/20
  58509                   SUM(R68MNT) TEMMNT                                                                            05/03/20
  58510                   FROM R14DET INNER JOIN R68DET ON R14CAS = R68CAS AND                                          05/03/20
  58511                        R14REC = R68REC                                                                          05/03/20
  58512                   WHERE                                                                                         05/03/20
  58513                   R14EST <> "2" AND R68PAG NOT IN("902", "903") AND                                             05/03/20
  58514                   R14FRC >= :WFDESDECORR AND R14FRC <= :WFHASTACORR                                             05/03/20
  58515                   GROUP BY R14CAS )                                                                             05/03/20
  58516                ,PAGOSPERMH AS                                                                                   05/03/20
  58517               (SELECT "IP" TEMRBR, R14CAS TEMC47, " " TEMFEP,                                                   05/03/20
  58518                SUM(R68MNT) TEMMNT                                                                               05/03/20
  58519                FROM R14DET INNER JOIN R68DET ON R14CAS                                                          05/03/20
  58520                = R68CAS AND R14REC = R68REC                                                                     05/03/20
  58522                WHERE   R14EST <> "2" AND R68PAG = "902" AND                                                     05/03/20
  58523                R14FRC >= :WFDESDECORR AND R14FRC <= :WFHASTACORR                                                05/03/20
  58524                GROUP BY R14CAS        )                                                                         05/03/20
  58525                ,PAGOSPERM AS                                                                                    05/03/20
  58526                (SELECT "IP" TEMRBR, R33CAS TEMC47, " " TEMFEP,                                                  05/03/20
  58527                 SUM(R58MON) TEMMNT                                                                              05/03/20
  58528                 FROM (R33MAE51 INNER JOIN R20DET55 ON R33FIJ = R20FIJ)                                          05/03/20
  58529                 INNER JOIN R58DET10 ON R58FIJ = R20FIJ AND R58CED =                                             05/03/20
  58530                 R20PER                                                                                          05/03/20
  58531                 WHERE                                                                                           05/03/20
  58532                       R58EST = "1" AND R58TIP <> "J" AND                                                        05/03/20
  58533                 R20EFI IN("1", "2", "3") AND                                                                    05/03/20
  58534                 R58FEC >= :WFDESDECORR AND R58FEC <= :WFHASTACORR                                               05/03/20
  58535                 GROUP BY R33CAS  )                                                                              05/03/20
  58536                ,PAGOSCTAH AS                                                                                    05/03/20
  58537                  (SELECT "CM" TEMRBR, R14CAS TEMC47, " " TEMFEP,                                                05/03/20
  58538                   SUM(R68MNT) TEMMNT                                                                            05/03/20
  58539                   FROM R14DET INNER JOIN R68DET                                                                 05/03/20
  58540                   ON R14CAS = R68CAS AND R14REC = R68REC                                                        05/03/20
  58541                   WHERE                                                                                         05/03/20
  58542                        R14EST <> "2" AND R68PAG = "903" AND                                                     05/03/20
  58543                 R14FRC >= :WFDESDECORR AND R14FRC <= :WFHASTACORR                                               05/03/20
  58544                 GROUP BY R14CAS                      )                                                          05/03/20
  58545                ,PAGOSCTA AS                                                                                     05/03/20
  58546                 (SELECT "CM" TEMRBR, R49CAS TEMC47, " " TEMFEP,                                                 05/03/20
  58547                  SUM(R49MON) TEMMNT                                                                             05/03/20
  58548                  FROM  R49DET10 INNER JOIN R15DET56 ON R49CON = R15CON                                          05/03/20
  58549                  WHERE                                                                                          05/03/20
  58550                     R15EST <> "4" AND                                                                           05/03/20
  58551                   R15PAG >= :WFDESDECORR AND R15PAG <= :WFHASTACORR                                             05/03/20
  58552                   AND R15TIP NOT IN ("709", "710", "711")                                                       05/03/20
  58553                  GROUP BY R49CAS  )                                                                             05/03/20
  58554                ,PAGOSINDM AS                                                                                    05/03/20
  58555                   (                                                                                             05/03/20
  58556                 SELECT "IM" TEMRBR, R33CAS TEMC47, " " TEMFEP,                                                  05/03/20
  58557                 SUM(R58MON) TEMMNT                                                                              05/03/20
  58558                 FROM  R33MAE51 INNER JOIN R54DET53 ON R33FIJ = R54FIJ                                           05/03/20
  58559                 INNER JOIN R58DET10 ON R54FIJ = R58FIJ                                                          05/03/20
  58560                 WHERE                                                                                           05/03/20
  58561                    R58EST = "1" AND R54CE1 = R58CEB AND                                                         05/03/20
  58562                    R58TIP <> "J" AND R54EST IN("1", "2", "3") AND                                               05/03/20
  58563                    R58FEC >= :WFDESDECORR AND R58FEC <= :WFHASTACORR                                            05/03/20
  58564                  GROUP BY R33CAS   )                                                                            05/03/20
  58565                SELECT * FROM PAGOSTEM                                                                           05/03/20
  58566                UNION                                                                                            05/03/20
  58567                SELECT * FROM PAGOSPERMH                                                                         05/03/20
  58568                UNION                                                                                            05/03/20
  58569                SELECT * FROM PAGOSPERM                                                                          05/03/20
  58570                UNION                                                                                            05/03/20
  58571                SELECT * FROM PAGOSCTAH                                                                          05/03/20
  58572                UNION                                                                                            05/03/20
  58573                SELECT * FROM PAGOSCTA                                                                           05/03/20
  58574                UNION                                                                                            05/03/20
  58575                SELECT * FROM PAGOSINDM                                                                          05/03/20
  58577            END-EXEC.                 
  70600                                                                                                                 09/04/19
  70700            EXEC SQL                                                                                             28/02/17
  70800                 COMMIT                                                                                          28/02/17
  70900            END-EXEC.                                                                                            20/02/20
  71000       * /////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ *                                           03/01/17
  71100       * | EJECUCIÓN DE CIERRE Y ACUMULACIÓN DE PAGOS, PROVICIONES | *                                           29/12/16
  71200       * \\\\\\\\\\\\\\\\\\\\\\\\\\\\\////////////////////////////// *                                           03/01/17
  71202            MOVE SPACES                        TO MENSAJE                                                        20/02/20
  71203            MOVE "OBTENIENDO CASOS A ESTUDIAR" TO MENSAJE                                                        20/02/20
  71204            PERFORM GRABA-BITACORA                                                                               21/01/20
  71246              EXEC SQL                                                                                           31/01/20
  71247                INSERT INTO CASTEM                                                                               20/02/20
  71248                SELECT R09CAS                                                                                    31/01/20
  71249                   ,R09CED                                                                                       31/01/20
  71250                   ,R09AGE                                                                                       31/01/20
  71251                   ,R09FEC                                                                                       31/01/20
  71252                   ,R09ACE                                                                                       31/01/20
  71253                   ,R09FEI                                                                                       31/01/20
  71254                   ,R09REA                                                                                       31/01/20
  71255                   ,R09CON                                                                                       31/01/20
  71256                   ,R09CTV                                                                                       31/01/20
  71257                   ,R09ESS                                                                                       31/01/20
  71258                   ,R09ESR                                                                                       31/01/20
  71259                   ,R09EST                                                                                       31/01/20
  71260                FROM R09MAE59 A                                                                                  31/01/20
  71261                WHERE                                                                                            21/01/21
  71262                    ( SELECT COUNT(*) FROM R09MAE B                                                              31/01/20
  71263                       WHERE A.R09CAS = B.R09CAS ) = 1                                                           31/01/20
  71264              AND R09ACE NOT IN ("30","31","32","99")                                                            31/01/20
  71265              AND SUBSTR(R09FEC , 1 , 6  )                                                                       04/02/20
  71266                BETWEEN "196701" AND SUBSTR(:WFHASTACORR , 1 , 6)                                                04/02/20
  71267              AND SUBSTR(R09FEC , 5 , 2  ) BETWEEN "01" AND "12"                                                 31/01/20
  71268              AND SUBSTR(R09FEC , 7 , 2  ) BETWEEN "01" AND "31"                                                 31/01/20
  71269              AND (                                                                                              11/02/20
  71270                   R09ESR = "A" OR                                                                               11/02/20
  71271                   R09CTV = "S" OR                                                                               11/02/20
  71272                   EXISTS ( SELECT * FROM TEMMTO                                                                 20/02/20
  71273                          WHERE TEMC47 = R09CAS)                                                                 11/02/20
  71274                  )                                                                                              11/02/20
  71277              END-EXEC                                                                                           20/02/20
  71279            MOVE WFHASTACORR TO RY1FEC-WS.                                                                       20/02/20
  71280            MOVE ZEROS TO CASOSCAN-WS                                                                            20/02/20
  71281                          CASOSTOM-WS                                                                            20/02/20
  71282                          CERRADOS-WS                                                                            30/01/20
  71283                          ABIERTOS-WS                                                                            20/02/20
  71284                          CONTROVE-WS                                                                            30/01/20
  71285                          TOTALPRO-WS                                                                            20/02/20
  71286                          TOTALPAJ-WS                                                                            20/02/20
  71287                          TOTALGAS-WS                                                                            20/02/20
  71288                          TOTALSAL-WS                                                                            20/02/20
  71289                          CASOSALN-WS                                                                            06/02/20
  71290                          CASOSALC-WS                                                                            06/02/20
  71291                          CASOSAPL-WS                                                                            06/02/20
  71292                          CASOSDLN-WS                                                                            06/02/20
  71293                          CASOSDLC-WS                                                                            06/02/20
  71294                          CASOSDPL-WS                                                                            06/02/20
  71295            EXEC SQL                                                                                             30/01/20
  71296                 SELECT COUNT(*) INTO :CASOSCAN-WS                                                               20/02/20
  71297                 FROM CASTEM                                                                                     30/01/20
  71298            END-EXEC                                                                                             20/02/20
  71299            EXEC SQL                                                                                             22/01/20
  71300              DECLARE CSELCASOS CURSOR FOR                                                                       22/01/20
  71301                   SELECT * FROM CASTEM                                                                          22/01/20
  71302            END-EXEC                                                                                             20/02/20
  71303            PERFORM SELECCION-CASOS-INTERES                                                                      20/01/20
  71304            OPEN INPUT TEMFMX1 TEMMTO R08MAE                                                                     20/02/20
  71400                 I-O RY1RCA01 RY1RCD01.                                                                          20/01/20
  71500            MOVE " EN OPEN " TO LUGARERRO.                                                                       20/02/20
  71501            MOVE " 1er READ "                TO LUGARERRO.                                                       25/02/20
  71600            PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT.                                                     11/01/17
  71800            MOVE 0                           TO FINR09.                                                          20/02/20
  72301            MOVE "N" TO SALIR-R09                                                                                20/02/20
  72302            MOVE SPACES                      TO MENSAJE                                                          20/02/20
  72303            MOVE "A PUNTO DE RECORRER CASOS" TO MENSAJE                                                          20/02/20
      *>             Graba en bitácora que va a iniciar la ejecución
  72304            PERFORM GRABA-BITACORA                                                                               21/01/20
  72305            PERFORM B010-PROCESA-CADA-CASO UNTIL SALIR-R09 = "S"                                                 20/01/20
  72306            MOVE SPACES                          TO MENSAJE                                                      20/02/20
  72307            MOVE "FINALIZADO RECORRIDO DE CASOS" TO MENSAJE                                                      20/02/20
  72308            PERFORM GRABA-BITACORA                                                                               12/02/20
  72309            MOVE SPACES                      TO MENSAJE                                                          20/02/20
  72310            MOVE "INICIA GRABA CIFRAS DE CONTROL" TO MENSAJE                                                     20/02/20

      *>             Graba en bitácora 
  72311            PERFORM GRABA-BITACORA                                                                               12/02/20

      *>             Realiza conteo de casos cerrados para la ejecución 
  72312            EXEC SQL                                                                                             12/02/20
  72313                  SELECT COUNT(*) INTO :CERRADOS-WS                                                              20/02/20
  72314                  FROM BITREC                                                                                    12/02/20
  72315                  WHERE BITES2 = "A"                                                                             12/02/20
  72316                  AND BITEFI = "C"                                                                               12/02/20
  72317                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72318            END-EXEC                                                                                             20/02/20
      *>             Realiza conteo de casos abiertos para la ejecución
  72319            EXEC SQL                                                                                             12/02/20
  72320                  SELECT COUNT(*) INTO :ABIERTOS-WS                                                              20/02/20
  72321                  FROM BITREC                                                                                    12/02/20
  72322                  WHERE BITES2 = "C"                                                                             12/02/20
  72323                  AND BITEFI = "A"                                                                               12/02/20
  72324                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72325            END-EXEC                                                                                             20/02/20
                       

      *>             Realiza conteo de casos ALC

  72326            EXEC SQL                                                                                             12/02/20
  72327                  SELECT COUNT(*) INTO :CASOSALC-WS                                                              20/02/20
  72328                  FROM RY1RCA, BITREC                                                                            12/02/20
  72329                  WHERE RY1CAS = BITCA2                                                                          12/02/20
  72330                  AND   RY1FMR = BITFE9                                                                          12/02/20
  72331                  AND   BITCO1 = "LC"                                                                            12/02/20
  72332                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72333            END-EXEC                                                                                             20/02/20
      *>             Realiza conteo de casos ALN
  72334            EXEC SQL                                                                                             12/02/20
  72335                  SELECT COUNT(*) INTO :CASOSALN-WS                                                              20/02/20
  72336                  FROM RY1RCA, BITREC                                                                            12/02/20
  72337                  WHERE RY1CAS = BITCA2                                                                          12/02/20
  72338                  AND   RY1FMR = BITFE9                                                                          12/02/20
  72339                  AND   BITCO1 = "LN"                                                                            12/02/20
  72340                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72341            END-EXEC                                                                                             20/02/20

      *>             Realiza conteo de casos Aplicados
  72342            EXEC SQL                                                                                             12/02/20
  72343                  SELECT COUNT(*) INTO :CASOSAPL-WS                                                              20/02/20
  72344                  FROM RY1RCA, BITREC                                                                            12/02/20
  72345                  WHERE RY1CAS = BITCA2                                                                          12/02/20
  72346                  AND   RY1FMR = BITFE9                                                                          12/02/20
  72347                  AND   BITCO1 = "PL"                                                                            12/02/20
  72348                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72349            END-EXEC                                                                                             20/02/
      *>             Realiza conteo de casos declinados       
  72350            EXEC SQL                                                                                             12/02/20
  72351                  SELECT COUNT(*) INTO :CASOSDLC-WS                                                              20/02/20
  72352                  FROM RY1RCA, BITREC                                                                            12/02/20
  72353                  WHERE RY1CAS = BITCA2                                                                          12/02/20
  72354                  AND   RY1FMR = BITFE9                                                                          12/02/20
  72355                  AND   BITCFI = "LC"                                                                            12/02/20
  72356                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72357            END-EXEC                                                                                             20/02/20

  72358            EXEC SQL                                                                                             12/02/20
  72359                  SELECT COUNT(*) INTO :CASOSDLN-WS                                                              20/02/20
  72360                  FROM RY1RCA, BITREC                                                                            12/02/20
  72361                  WHERE RY1CAS = BITCA2                                                                          12/02/20
  72362                  AND   RY1FMR = BITFE9                                                                          12/02/20
  72363                  AND   BITCFI = "LN"                                                                            12/02/20
  72364                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72365            END-EXEC                                                                                             20/02/20
  72366            EXEC SQL                                                                                             12/02/20
  72367                  SELECT COUNT(*) INTO :CASOSDPL-WS                                                              20/02/20
  72368                  FROM RY1RCA, BITREC                                                                            12/02/20
  72369                  WHERE RY1CAS = BITCA2                                                                          12/02/20
  72370                  AND   RY1FMR = BITFE9                                                                          12/02/20
  72371                  AND   BITCFI = "PL"                                                                            12/02/20
  72372                  AND BITFE9 = :WFHASTACORR                                                                      20/02/20
  72373            END-EXEC                                                                                             20/02/20
  72374            MOVE ZEROS TO TOTALPRO-WS TOTALGAS-WS                                                                20/02/20
  72375                          TOTALPAJ-WS TOTALSAL-WS                                                                20/02/20

      *>             Inserta en la tabla de control los valores recolectados en los counts anteriores
  72376            EXEC SQL                                                                                             30/01/20
  72377                  INSERT INTO RY1CIF VALUES                                                                      20/02/20
  72378                  (:RY1FEC-WS                                                                                    30/01/20
  72379                  ,:CASOSCAN-WS                                                                                  30/01/20
  72380                  ,:CASOSTOM-WS                                                                                  20/02/20
  72381                  ,:CERRADOS-WS                                                                                  30/01/20
  72382                  ,:ABIERTOS-WS                                                                                  20/02/20
  72383                  ,:CONTROVE-WS                                                                                  30/01/20
  72384                  ,:CASOSALC-WS                                                                                  06/02/20
  72385                  ,:CASOSALN-WS                                                                                  06/02/20
  72386                  ,:CASOSAPL-WS                                                                                  06/02/20
  72387                  ,:CASOSDLC-WS                                                                                  06/02/20
  72388                  ,:CASOSDLN-WS                                                                                  06/02/20
  72389                  ,:CASOSDPL-WS                                                                                  06/02/20
  72390                  ,:TOTALPRO-WS                                                                                  20/02/20
  72391                  ,:TOTALPAJ-WS                                                                                  20/02/20
  72392                  ,:TOTALGAS-WS                                                                                  20/02/20
  72393                  ,:TOTALSAL-WS  )                                                                               20/02/20
  72394            END-EXEC                                                                                             20/02/20
      *>             limpia y setea variables para ver en spool, graba en bitacora y cierra archivos
  72395            MOVE SPACES                      TO MENSAJE                                                          20/02/20
  72396            MOVE "FIN GRABA CIFRAS DE CONTROL" TO MENSAJE                                                        20/02/20
  72397            PERFORM GRABA-BITACORA                                                                               12/02/20
  72401            CLOSE TEMFMX1 TEMMTO RY1RCA01 RY1RCD01 R08MAE.                                                       20/02/20
  72402            EXEC SQL                                                                                             20/01/20
  72403                 CLOSE CSELCASOS                                                                                 20/01/20
  72404            END-EXEC                                                                                             20/02/20

      *>             Revisa los files status y finaliza el proceso con normalidad
  72500            PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT.                                                     11/01/17
  72600            DISPLAY                                                                                              28/02/17
  72700            "._______________________________________." UPON CONSOLE                                             28/02/17
  72800            DISPLAY                                                                                              28/02/17
  72900            "| R09RY1RCA - | Fin Normal del Programa |" UPON CONSOLE                                             28/02/17
  73000            DISPLAY                                                                                              28/02/17
  73100            "|_______________________________________|" UPON CONSOLE                                             28/02/17
  73200       *                                                                                                         01/03/17
  73201            MOVE SPACES                           TO MENSAJE                                                     20/02/20
  73202            MOVE "INICIANDO GENERACION DE RY1HVR" TO MENSAJE                                                     20/02/20
  73203            PERFORM GRABA-BITACORA                                                                               05/02/20
  73300            CALL "RY1PBHVR" USING WANOCORRIDA WMESCORRIDA                                                        01/03/17
  73301            MOVE SPACES                          TO MENSAJE                                                      20/02/20
  73302            MOVE "FINALIZA GENERACION DE RY1HVR" TO MENSAJE                                                      20/02/20
  73303            PERFORM GRABA-BITACORA                                                                               05/02/20
  73304            CALL "RY1F1HVR" USING WANOCORRIDA WMESCORRIDA                                                        05/02/21
  73305            MOVE SPACES                          TO MENSAJE                                                      05/02/21
  73306            MOVE "FINALIZA GENERACION DE RY2HVR" TO MENSAJE                                                      05/02/21
  73307            PERFORM GRABA-BITACORA                                                                               05/02/21
  73400            DISPLAY                                                                                              01/03/17
  73500            "._______________________________________." UPON CONSOLE                                             01/03/17
  73600            DISPLAY                                                                                              01/03/17
  73700            "| RY1PBHVR  - | Fin Normal del Programa |" UPON CONSOLE                                             01/03/17
  73800            DISPLAY                                                                                              01/03/17
  73900            "|_______________________________________|" UPON CONSOLE                                             01/03/17
  73901            ACCEPT HORACORR FROM TIME                                                                            17/01/20
  73902            EXEC SQL                                                                                             17/01/20
  73903                INSERT INTO BITARC VALUES                                                                        20/02/20
  73904                ("R09RY1RC"                                                                                      17/01/20
  73905                ,"D"                                                                                             17/01/20
  73906                ,"1"                                                                                             17/01/20
  73907                ,:WFHASTACORR                                                                                    17/01/20
  73908                ,:HORACORR                                                                                       17/01/20
  73909                ,:WUSUARIO                                                                                       17/01/20
  73910                ," ")                                                                                            17/01/20
  73911            END-EXEC                                                                                             20/02/20
  73912            MOVE SPACES                TO MENSAJE                                                                20/02/20
  73913            MOVE "PROGRAMA FINALIZADO" TO MENSAJE                                                                20/02/20
  73914            PERFORM GRABA-BITACORA                                                                               17/01/20
  73915            EXEC SQL                                                                                             30/01/20
  73916                 COMMIT                                                                                          30/01/20
  73917            END-EXEC                                                                                             20/02/20
  74200            STOP RUN.                                                                                            20/02/20
  74300        A010-PROCESO-EXIT.                                                                                       16/02/17
  74400             EXIT.                                                                                               16/02/17
  74500                                                                                                                 10/01/17
  74600        B010-PROCESA-CADA-CASO.                                                                                  30/01/17
  75100            PERFORM VALIDA-FEC-ACC                                                                               05/02/20
  75101            PERFORM VALIDA-DUPLICADOS                                                                            
      *>             El caso tiene una fecha valida y no está duplicado
  77100            IF FECHAVALIDA IS EQUAL "S" AND                                                                      05/02/20
  77200               DUPLICADO IS EQUAL "N" THEN                                                                       05/02/20
  77202                  PERFORM PROCESA-CASO                                                                           05/02/20
  77700            END-IF                                                                                               20/02/20
  77701            IF IND-VALIDAR-RENTA IS EQUAL "S" AND                                                                07/05/20
  77702             R09CTV-WS IS NOT EQUAL "S" THEN                                                                     07/05/20
  77703              IF ENTRA IS EQUAL "S" THEN                                                                         14/02/20
  77704                PERFORM VALIDAR-RENTA                                                                            13/02/20
  77705              END-IF                                                                                             20/02/20
  77706            END-IF                                                                                               20/02/20
  77707            IF ENTRA IS EQUAL "S" THEN                                                                           14/02/20
  77708                PERFORM B030-ACUMULA-MONTOS THRU B030-EXIT                                                       20/02/20
  77709            END-IF                                                                                               20/02/20
  77710            PERFORM ACTUALIZA-BITREC                                                                             06/02/20
  77711            PERFORM LEE-SIGUIENTE-CASO.                                                                          05/02/20
  78900        B010-EXIT.                                                                                               10/01/17
  79000             EXIT.                                                                                               10/01/17
  79001       *                                                                                                         05/02/20
  79002        PROCESA-CASO.                                                                                            05/02/20
  79003            PERFORM B020-REVISA-ESTADOS THRU B020-EXIT                                                           05/02/20
  79004            IF ENTRA IS NOT EQUAL "S" THEN                                                                       14/02/20
  79005                IF R09CTV-WS IS EQUAL "S" THEN                                                                   19/03/20
  79006                        MOVE R09CTV to OKCTV                                                                     29/02/20
  79007                        MOVE "S"  TO ENTRA                                                                       28/02/20
  79008                        MOVE "LC" TO R09CON-WS                                                                   28/02/20
  79009                        MOVE "A"  TO R09ESR-WS                                                                   28/02/20
  79010                END-IF                                                                                           20/02/20
  79011            END-IF.                                                                                              20/02/20
  79038        END-PROCESA-CASO.                                                                                        20/02/20
  79039                                                                                                                 05/02/20
      *>         Actualizacion de bitácora 
  79040        ACTUALIZA-BITREC.                                                                                        05/02/20
  79041            ADD 1 TO CASOSTOM-WS                                                                                 20/02/20
  79056            IF R09CTV-WS IS EQUAL "S" THEN                                                                       05/02/20
  79057                         ADD 1 TO CONTROVE-WS                                                                    20/02/20
  79058            END-IF.                                                                                              20/02/20
  79059            EVALUATE R09CON-WS                                                                                   06/02/20
  79060            WHEN "LC"                                                                                            06/02/20
  79061                         ADD 1 TO CASOSDLC-WS                                                                    20/02/20
  79062            WHEN "LN"                                                                                            06/02/20
  79063                         ADD 1 TO CASOSDLN-WS                                                                    20/02/20
  79064            WHEN "PL"                                                                                            06/02/20
  79065                         ADD 1 TO CASOSDPL-WS                                                                    20/02/20
  79066            END-EVALUATE                                                                                         20/02/2
      *>             EVALUA SI EL CASO DEBE ENTRAR EN EL PROCESO DE ACTUALIZACION DE BITACORA
  79071            IF ENTRA IS EQUAL "S" THEN                                                                           14/02/20
  79072                 MOVE "N" TO VARERR                                                                              20/02/20
  79073            ELSE                                                                                                 12/02/20
  79074                 MOVE "S" TO VARERR                                                                              20/02/20
  79075            END-IF                                                                                               20/02/20
  79076            EXEC SQL                                                                                             12/02/20
  79077                    UPDATE BITREC                                                                                05/02/20
  79078                    SET                                                                                          06/02/20
  79079                     BITERR = :VARERR                                                                            12/02/20
  79080                    ,BITFIL = :BITFIL-WS                                                                         24/02/20
  79081                    ,BITEFI = :R09ESR-WS                                                                         12/02/20
  79082                    ,BITDUP = :DUPLICADO                                                                         12/02/20
  79083                    ,BITCFI = :R09CON-WS                                                                         12/02/20
  79084                    ,BITPR5 = :TOTALPRO-WS                                                                       20/02/20
  79085                    ,BITGA2 = :TOTALPAJ-WS                                                                       20/02/20
  79086                    ,BITPR6 = :TOTALGAS-WS                                                                       20/02/20
  79087                    ,BITSA1 = :TOTALSAL-WS                                                                       20/02/20
  79088                    ,BITJUD = :SALJUDIC-WS                                                                       14/08/20
  79089                    WHERE BITCA2 = :R09CAS-WS                                                                    05/02/20
  79090                    AND   BITFE9 = :WFHASTACORR                                                                  05/02/20
  79091            END-EXEC.                                                                                            20/02/20
  79092            MOVE ZEROS TO TOTALPRO-WS TOTALPAJ-WS                                                                24/02/20
  79093                          TOTALGAS-WS TOTALSAL-WS                                                                24/02/20
  79099        END-ACTUALIZA-BITREC.                                                                                    20/02/20
  79100                                                                                                                 05/02/20
      *>         Valida la fecha actual
  79101        VALIDA-FEC-ACC.                                                                                          05/02/20
  79102       *---- RUTINAS PARA VERIFICAR LA FECHA DE ACCIDENTE                                                        05/02/20
  79103             MOVE R09FEC-WS TO WFECHA                                                                            20/02/20
  79104       *---- QDESFEC - Desinvierte DDMMAAAA                                                                      05/02/20
  79105             CALL "F009SNAP" USING WFECHA.                                                                       05/02/20
  79106       *---- QVALFEC - Valida la fecha                                                                           05/02/20
  79107             CALL "F007SNAP" USING WFECHA INDICA.                                                                05/02/20
  79108              IF R09FEC-WS >= FDE AND R09FEC-WS <= WFHASTACORR AND                                               05/02/20
  79109                INDICA = "0" THEN                                                                                05/02/20
  79110                  MOVE "S" TO FECHAVALIDA                                                                        20/02/20
  79111              ELSE                                                                                               05/02/20
  79112                  MOVE "N" TO FECHAVALIDA                                                                        20/02/20
  79113              END-IF.                                                                                            20/02/20
  79114        END-VALIDA-FEC-ACC.                                                                                      20/02/20
  79115           
      *>         Validación de duplicados
  79116        VALIDA-DUPLICADOS.                                                                                       05/02/20
  79117       *- REVISAR CASOS DUPLICADOS Y EXCLUIRLOS USAR RY1DUP                                                      05/02/20
  79118            MOVE "S"       TO DUPLICADO                                                                          20/02/20
  79119            MOVE R09CAS-WS TO CAS001.                                                                            20/02/20
  79120            MOVE SPACES TO WCASDUP.                                                                              20/02/20
  79121            EXEC SQL                                                                                             05/02/20
  79122              SELECT CASNU7 INTO:WCASDUP                                                                         20/02/20
  79123              FROM   RY1DUP                                                                                      05/02/20
  79124              WHERE CASNU7 = :CAS001                                                                             05/02/20
  79125            END-EXEC.                                                                                            20/02/20
  79126            IF WCASDUP IS EQUAL SPACES THEN                                                                      20/02/20
  79127                 MOVE "N" TO DUPLICADO                                                                           20/02/20
  79128            END-IF.                                                                                              20/02/20
  79129        END-VALIDA-DUPLICADOS.                                                                                   20/02/20

      *>         Lee siguiente caso para procesar
  79144        LEE-SIGUIENTE-CASO.                                                                                      05/02/20
  79145            MOVE SPACES TO RR09MAE BITFIL-WS                                                                     24/02/20
  79146            MOVE ZEROS  TO SQLSTATE                                                                              20/02/20
  79147            EXEC SQL                                                                                             05/02/20
  79148                 FETCH NEXT FROM CSELCASOS INTO :RR09MAE                                                         20/02/20
  79149            END-EXEC                                                                                             20/02/20
  79150            IF (SQLSTATE(1:2) IS EQUAL "00"                                                                      05/02/20
  79151             OR SQLSTATE(1:2) IS EQUAL "01") THEN                                                                05/02/20
  79152                    PERFORM GRABA-BITREC                                                                         05/02/20
  79153            ELSE                                                                                                 05/02/20
  79154                MOVE "S" TO SALIR-R09                                                                            20/02/20
  79155                DISPLAY "R09RY1RCA - ERROR 1ER READ R09MAE59. FS: "                                              20/02/20
  79156                       FS-R09MAE59 UPON CONSOLE                                                                  05/02/20
  79157                STRING "R09RY1RCA SALIDA SQL " DELIMITED BY SIZE                                                 05/02/20
  79158                SQLSTATE DELIMITED BY SIZE                                                                       05/02/20
  79159                " ULTIMO " DELIMITED BY SIZE                                                                     05/02/20
  79160                CAS001 DELIMITED BY SIZE                                                                         05/02/20
  79161                INTO MENSAJE                                                                                     20/02/20
  79162                PERFORM GRABA-BITACORA                                                                           05/02/20
  79163            END-IF.                                                                                              20/02/20
  79164        END-LEE-SIGUIENTE-CASO.                                                                                  20/02/20
  79165       *                                                                                                         17/01/17
  79200        B020-REVISA-ESTADOS.                                                                                     29/01/20
  79300            MOVE SPACES TO VIVMUE OKCTV ENTRA NUEVO ACERRAR.                                                     24/02/20
  79400       *                           Determina cual fecha usa como TOpe                                            20/02/20
  79401            PERFORM CONSULTA-R08MAE                                                                              05/02/20
  81600       *-- Revisa si no existe en RY1RCA y RY1RCD, sino lo crea                                                  27/02/17
  81700            MOVE SPACES TO WCASO.                                                                                20/02/20
  81800            EXEC SQL                                                                                             27/02/17
  81900               SELECT DISTINCT RY1CAS INTO:WCASO                                                                 20/02/20
  82000               FROM RY1RCA WHERE RY1CAS = :CAS001                                                                27/02/17
  82100            END-EXEC                                                                                             20/02/20
  82200       *-- Cierra o abre el caso                                                                                 18/01/17
  82201            PERFORM CIERRA-O-ABRE-CASO                                                                           05/02/20
  88000            PERFORM CALCULA-MONTO-A-SUMAR                                                                        20/02/20
  91001            PERFORM ACTUALIZA-R09MAE                                                                             05/02/20
  91700       *- Revisar si el caso es nuevo buscando movTOs en RY1RCA                                                  20/02/20
  91800            IF WCASO = SPACES AND R09ESR-WS = 'A' THEN                                                           20/02/20
  91900               MOVE "S" TO NUEVO                                                                                 20/02/20
  92000               MOVE "S" TO ENTRA                                                                                 20/02/20
  92001               CLOSE RY1RCA01 RY1RCD01                                                                           21/01/20
  92100               CALL "RY1GRINI" USING                                                                             27/02/17
  92200                     WFHASTACORR R09AGE-WS R09CAS-WS CONDIC TIPPRO                                               20/01/20
  92201               OPEN I-O RY1RCA01 RY1RCD01                                                                        21/01/20
  92300            END-IF.                                                                                              20/02/20
  93000 RLM        PERFORM BUSCA-SI-ES-RENTA-MENSUAL.                                                                   05/02/20
  93001        END-B020-REVISA-ESTADOS.                                                                                 25/02/20
  95000        B020-EXIT.                                                                                               04/02/17
  95100             EXIT.                                                                                               04/02/17
  95101       *                                                                                                         05/02/20
  95102        CIERRA-O-ABRE-CASO.                                                                                      05/02/20
  95103            MOVE "N"    TO ENTRA.                                                                                25/02/20
  95104            MOVE SPACES TO BITFIL-WS.                                                                            25/02/20
  95105            INITIALIZE ENTRA  WCONDI.                                                                            14/02/20
  95106            IF R09ESS-WS = "A" THEN                                                                              05/02/20
  95107               MOVE "A" TO R09ESR-WS                                                                             20/02/20
  95108               MOVE "S" TO ENTRA                                                                                 20/02/20
  95109            END-IF.                                                                                              20/02/20
  95110       *                           Verifica si Caso está Controvertido                                           05/02/20
  95111            IF R09CTV-WS = "S" THEN                                                                              05/02/20
  95112               MOVE R09CTV-WS TO OKCTV                                                                           20/02/20
  95114               MOVE "S"       TO ENTRA                                                                           20/02/20
  95115               MOVE "LC"      TO R09CON-WS                                                                       20/02/20
  95116               MOVE "A"       TO R09ESR-WS                                                                       28/02/20
  95117            END-IF.                                                                                              20/02/20
  95118            MOVE "AL"         TO TEMRUB.                                                                         20/02/20
  95119            MOVE R09CAS-WS    TO TEMC45.                                                                         20/02/20
  95120            READ TEMFMX1 RECORD.                                                                                 20/02/20
  95121            IF FS-TEMFMX1 = "23" AND R09ESR-WS = 'A' THEN                                                        05/02/20
  95122       *       Es un caso viejo pero está abierTO                                                                20/02/20
  95123       *       Revisa si no tiene movimienTOs en n cantidad de meses                                             20/02/20
  95124               IF R09FEC-WS >= WFTOPE AND                                                                        20/02/20
  95125                  R09FEC-WS <= WFHASTACORR                                                                       05/02/20
  95126                  MOVE "S"    TO ENTRA                                                                           20/02/20
  95127               ELSE                                                                                              05/02/20
  95128       *           IF R09CTV-WS IS NOT EQUAL "S" THEN                                                            29/02/20
  95129                       MOVE "C"    TO R09ESR-WS                                                                  28/02/20
  95130                       MOVE "TOPE" TO BITFIL-WS                                                                  28/02/20
  95131                       MOVE "N"    TO ENTRA                                                                      28/02/20
  95132       *           END-IF                                                                                        29/02/20
  95133               END-IF                                                                                            20/02/20
  95134            ELSE                                                                                                 05/02/20
  95135               IF FS-TEMFMX1 = "00" OR FS-TEMFMX1 = "04" Then                                                    05/02/20
  95136                  IF TEMMAX LESS THAN WFTOPE                                                                     20/02/20
  95137       *- Caso estaba abierTO, pero en esta corrida se va a cerrar...                                            20/02/20
  95138                      IF R09ESR-WS = "A"                                                                         05/02/20
  95139       *                 Si está abierTO en SIMA, permanecerá abierTO                                            20/02/20
  95140                         IF R09ESS-WS = "A" THEN                                                                 05/02/20
  95141                            MOVE "S" TO ENTRA                                                                    20/02/20
  95142                         ELSE                                                                                    11/02/20
  95143       *                    Se cerrará el caso, solo deja último cerrado                                         05/02/20
  95144                            MOVE "C" TO R09ESR-WS                                                                20/02/20
  95145                            MOVE "S" TO ENTRA                                                                    20/02/20
  95146                            MOVE "S" TO ACERRAR                                                                  20/02/20
  95147                         END-IF                                                                                  20/02/20
  95148                      ELSE                                                                                       05/02/20
  95149                         MOVE "C" TO R09ESR-WS                                                                   20/02/20
  95150                         MOVE "N" TO ENTRA                                                                       20/02/20
  95151                         MOVE "TOPE" TO BITFIL-WS                                                                25/02/20
  95152                      END-IF                                                                                     20/02/20
  95153                  ELSE                                                                                           05/02/20
  95154                      MOVE "A" TO R09ESR-WS                                                                      20/02/20
  95155                      MOVE "S" TO ENTRA                                                                          20/02/20
  95156                  END-IF                                                                                         20/02/20
  95157               END-IF                                                                                            20/02/20
  95158            END-IF.                                                                                              20/02/20
  95159        END-CIERRA-O-ABRE-CASO.                                                                                  20/02/20
  95160       *                                                                                                         05/02/20
  95161        CALCULA-MONTO-A-SUMAR.                                                                                   20/02/20
  95162            MOVE ZEROS TO OKMONTO.                                                                               20/02/20
  95163            EXEC SQL                                                                                             05/02/20
  95164               SELECT DISTINCT TEMMNT INTO:OKMONTO                                                               20/02/20
  95165               FROM TEMMTO WHERE TEMC47 = :CAS001                                                                20/02/20
  95166            END-EXEC                                                                                             20/02/20
  95167       *                                                                                                         05/02/20
  95168            IF R09CTV-WS = "S"                                                                                   05/02/20
  95169               MOVE "S" TO ENTRA                                                                                 20/02/20
  95170               MOVE "LC" TO R09CON-WS                                                                            19/03/20
  95171               MOVE "A"  TO R09ESR-WS                                                                            19/03/20
  95172            ELSE                                                                                                 05/02/20
  95173               IF OKMONTO = ZEROS                                                                                20/02/20
  95174                  MOVE "PL" TO R09CON-WS                                                                         20/02/20
  95175               ELSE                                                                                              05/02/20
  95176                  MOVE "LN" TO R09CON-WS                                                                         20/02/20
  95177               END-IF                                                                                            20/02/20
  95178            END-IF.                                                                                              20/02/20
  95179       *                                                                                                         05/02/20
  95180            IF R09ESR-WS IS EQUAL "C"                                                                            11/02/20
  95181            AND OKMONTO IS NOT EQUAL ZEROS THEN                                                                  20/02/20
  95182                 MOVE "A" TO R09ESR-WS                                                                           20/02/20
  95183                 MOVE "S" TO ENTRA                                                                               20/02/20
  95184            END-IF                                                                                               20/02/20
  95185            IF R09ESR-WS IS EQUAL "A" AND OKMONTO IS EQUAL ZEROS                                                 20/02/20
  95186               EXEC SQL                                                                                          05/02/20
  95187                    SELECT DISTINCT SUM(D.RY1GAS) INTO:OKMONTO                                                   20/02/20
  95188                    FROM RY1RCD D                                                                                05/02/20
  95189                    WHERE D.RY1CAD = :CAS001 AND                                                                 05/02/20
  95190                          D.RY1GAS > 0 AND                                                                       05/02/20
  95191                          D.RY1COR <> "T"                                                                        05/02/20
  95192               END-EXEC                                                                                          20/02/20
  95193               IF OKMONTO = ZEROS                                                                                20/02/20
  95194                  MOVE "PL" TO R09CON-WS                                                                         20/02/20
  95195               ELSE                                                                                              05/02/20
  95196                  MOVE "LN" TO R09CON-WS                                                                         20/02/20
  95197               END-IF                                                                                            20/02/20
  95198            END-IF.                                                                                              20/02/20
  95199        END-CALCULA-MONTO-A-SUMAR.                                                                               20/02/20
  95200       *                                                                                                         05/02/20
  95201        BUSCA-SI-ES-RENTA-MENSUAL.                                                                               05/02/20
  95202       * -- Determina SI REVISA SI TIENE RENTRA MENSUAL TRABAJADOR                                               05/02/20
  95203       * -- o Beneficiario ya que esTOs no entran en la reserva ya que                                           20/02/20
  95204       * -- se acumula en la reserva matematica                                                                  05/02/20
  95205            MOVE SPACES TO WR20IN WR54EF.                                                                        20/02/20
  95206            IF ENTRA = "S" THEN                                                                                  14/02/20
  95207               EXEC SQL                                                                                          05/02/20
  95208                    SELECT DISTINCT R20DET.R20INC INTO:WR20IN                                                    20/02/20
  95209                    FROM   R33MAE INNER JOIN R20DET ON R33FIJ = R20FIJ                                           05/02/20
  95210                    WHERE  R20EFI IN("1", "2", "3") AND                                                          05/02/20
  95211                           R33CAS = :CAS001         AND                                                          05/02/20
  95212                           R20INC IN("1", "2", "3", "4")                                                         05/02/20
  95213               END-EXEC                                                                                          20/02/20
  95214       *                                                                                                         05/02/20
  95215               EXEC SQL                                                                                          05/02/20
  95216                  SELECT DISTINCT R54DET.R54EST INTO:WR54EF                                                      20/02/20
  95217                  FROM R33MAE INNER JOIN R54DET ON R33FIJ = R54FIJ                                               05/02/20
  95218                  WHERE  R33CAS = :CAS001 AND R54EST IN("1", "2", "3")                                           05/02/20
  95219               END-EXEC                                                                                          20/02/20
  95220            END-IF.                                                                                              20/02/20
  95221        END-BUSCA-SI-ES-RENTA-MENSUAL.                                                                           20/02/20
  95222       *                                                                                                         13/02/20
  95223        VALIDAR-RENTA.                                                                                           13/02/20
  95224            MOVE "N" TO TIENE-PAGO-TEMP                                                                          20/02/20
  95225                        TIENE-PAGO-PERM                                                                          13/02/20
  95226                        TIENE-PAGO-CTAMED                                                                        13/02/20
  95227            EXEC SQL                                                                                             13/02/20
  95228                 SELECT "S" INTO :TIENE-PAGO-PERM                                                                20/02/20
  95229                 FROM TEMMTO                                                                                     20/02/20
  95230                 WHERE TEMC47 = :R09CAS-WS                                                                       13/02/20
  95231                 AND TEMRBR IN ("IP","IM")                                                                       13/02/20
  95232            END-EXEC                                                                                             20/02/20
  95233            EXEC SQL                                                                                             13/02/20
  95234                 SELECT "S" INTO :TIENE-PAGO-TEMP                                                                20/02/20
  95235                 FROM TEMMTO                                                                                     20/02/20
  95236                 WHERE TEMC47 = :R09CAS-WS                                                                       13/02/20
  95237                 AND TEMRBR = "IT"                                                                               13/02/20
  95238            END-EXEC                                                                                             20/02/20
  95239            EXEC SQL                                                                                             13/02/20
  95240                 SELECT "S" INTO :TIENE-PAGO-CTAMED                                                              20/02/20
  95241                 FROM TEMMTO                                                                                     20/02/20
  95242                 WHERE TEMC47 = :R09CAS-WS                                                                       13/02/20
  95243                 AND TEMRBR = "CM"                                                                               13/02/20
  95244            END-EXEC                                                                                             20/02/20
  95245            IF TIENE-PAGO-TEMP   IS EQUAL "N" AND                                                                13/02/20
  95246               TIENE-PAGO-CTAMED IS EQUAL "N" AND                                                                13/02/20
  95247               TIENE-PAGO-PERM   IS EQUAL "S" THEN                                                               13/02/20
  95248                    MOVE SPACES TO FECHAINICIORENTA                                                              20/02/20
  95249                    EXEC SQL                                                                                     13/02/20
  95250                         SELECT R20FIR INTO :FECHAINICIORENTA                                                    20/02/20
  95251                         FROM R33MAE51, R20DET55, R58DET10                                                       13/02/20
  95252                         WHERE R33CAS = :R09CAS-WS                                                               13/02/20
  95253                         AND R33FIJ = R20FIJ                                                                     13/02/20
  95254                         AND R33FIJ = R58FIJ                                                                     13/02/20
  95255                         AND R58CED = R20PER                                                                     13/02/20
  95256                         AND R20EFI IN ("1","2","3")                                                             13/02/20
  95257                         AND R58EST = "1"                                                                        13/02/20
  95258                         AND R58TIP <> "J"                                                                       13/02/20
  95259                         AND R58FEC BETWEEN :WFDESDECORR AND                                                     13/02/20
  95260                                            :WFHASTACORR                                                         13/02/20
  95261                         FETCH FIRST 1 ROW ONLY                                                                  13/02/20
  95262                    END-EXEC                                                                                     20/02/20
  95263                    IF FECHAINICIORENTA IS EQUAL SPACES THEN                                                     20/02/20
  95264                    EXEC SQL                                                                                     13/02/20
  95265                         SELECT R54INI INTO :FECHAINICIORENTA                                                    20/02/20
  95266                         FROM R33MAE51, R54DET53, R58DET10                                                       13/02/20
  95267                         WHERE R33CAS = :R09CAS-WS                                                               13/02/20
  95268                         AND R33FIJ = R54FIJ                                                                     13/02/20
  95269                         AND R33FIJ = R58FIJ                                                                     13/02/20
  95270                         AND R58CEB = R54CE1                                                                     13/02/20
  95271                         AND R54EST IN ("1","2","3")                                                             13/02/20
  95272                         AND R58EST = "1"                                                                        13/02/20
  95273                         AND R58TIP <> "J"                                                                       13/02/20
  95274                         AND R58FEC BETWEEN :WFDESDECORR AND                                                     13/02/20
  95275                                            :WFHASTACORR                                                         13/02/20
  95276                         FETCH FIRST 1 ROW ONLY                                                                  13/02/20
  95277                    END-EXEC                                                                                     20/02/20
  95278                    END-IF                                                                                       20/02/20
  95279                    IF FECHAINICIORENTA IS NOT EQUAL SPACES THEN                                                 20/02/20
  95280                      MOVE 0 TO MESES-INICIO-RENTA                                                               20/02/20
  95281                      CALL "F010SNAP" USING FECHAHOY                                                             13/02/20
  95282                      CALL "F009SNAP" USING FECHAINICIORENTA                                                     13/02/20
  95283                      CALL "F100SNAP" USING FECHAHOY                                                             13/02/20
  95284                                            FECHAINICIORENTA                                                     13/02/20
  95285                                            MESES-INICIO-RENTA                                                   13/02/20
  95286                      IF MESES-INICIO-RENTA > 3 THEN                                                             13/02/20
  95288                             MOVE "N" TO ENTRA                                                                   05/05/20
  95289                             MOVE "RENTA" TO BITFIL-WS                                                           05/05/20
  95297                      END-IF                                                                                     20/02/20
  95298                    END-IF                                                                                       20/02/20
  95299            END-IF.                                                                                              20/02/20
  95300        END-VALIDAR-RENTA.                                                                                       20/02/20
  95301       *                                                                                                         05/02/20
  95302        CONSULTA-R08MAE.                                                                                         05/02/20
  95303            MOVE R09CED-WS TO R08CED.                                                                            20/02/20
  95304            READ R08MAE RECORD.                                                                                  20/02/20
  95305            IF FS-R08MAE = "23"                                                                                  05/02/20
  95306               MOVE "X" TO VIVMUE                                                                                20/02/20
  95307            ELSE                                                                                                 05/02/20
  95308               IF FS-R08MAE = "00" OR FS-R08MAE = "04"                                                           10/02/20
  95309                  IF R08VIV NOT EQUAL "F" AND R08VIV NOT EQUAL "V"                                               10/02/20
  95310                     IF R08MUE NOT EQUAL SPACES                                                                  20/02/20
  95311                        MOVE "F" TO VIVMUE                                                                       20/02/20
  95312                     ELSE                                                                                        10/02/20
  95313                        MOVE "X" TO VIVMUE                                                                       20/02/20
  95314                     END-IF                                                                                      20/02/20
  95315                  ELSE                                                                                           05/02/20
  95316                     MOVE R08VIV TO VIVMUE                                                                       20/02/20
  95317                  END-IF                                                                                         20/02/20
  95318               END-IF                                                                                            20/02/20
  95319            END-IF.                                                                                              20/02/20
  95320            IF VIVMUE = "F"                                                                                      05/02/20
  95321               MOVE WFTOPEM TO WFTOPE                                                                            20/02/20
  95322            END-IF.                                                                                              20/02/20
  95323            IF VIVMUE = "V" OR VIVMUE = "X"                                                                      05/02/20
  95324               MOVE WFTOPEV TO WFTOPE                                                                            20/02/20
  95325            END-IF.                                                                                              20/02/20
  95326        END-CONSULTA-R08MAE.                                                                                     20/02/20
  95327       *                                                                                                         05/02/20
  95328        ACTUALIZA-R09MAE.                                                                                        05/02/20
  95329            IF R09CTV-WS IS EQUAL "S" THEN                                                                       19/03/20
  95330                  MOVE "A"  TO R09ESR-WS                                                                         19/03/20
  95331                  MOVE "LC" TO R09CON-WS                                                                         19/03/20
  95332            END-IF                                                                                               19/03/20
  95333            OPEN I-O R09MAE59                                                                                    05/02/20
  95334            IF FS-R09MAE59 IS EQUAL "00"                                                                         05/02/20
  95335            OR FS-R09MAE59 IS EQUAL "04" THEN                                                                    05/02/20
  95336                  MOVE R09CAS-WS TO R09CAS OF R09MAE59                                                           20/02/20
  95337                  READ R09MAE59 KEY IS EXTERNALLY-DESCRIBED-KEY                                                  20/02/20
  95338                  NOT INVALID KEY                                                                                05/02/20
  95339                      MOVE R09CON-WS TO R09CON OF R09MAE59                                                       20/02/20
  95340                      MOVE R09ESR-WS TO R09ESR OF R09MAE59                                                       20/02/20
  95341                      REWRITE R09MAE59                                                                           06/02/20
  95342                         IF FS-R09MAE59 IS NOT EQUAL "00"                                                        05/02/20
  95343                         AND FS-R09MAE59 IS NOT EQUAL "04" THEN                                                  05/02/20
  95344                           MOVE SPACES TO MENSAJE                                                                20/02/20
  95345                           STRING "ERROR AL ACTUALIZAR R09MAE "                                                  05/02/20
  95346                           DELIMITED BY SIZE                                                                     05/02/20
  95347                           FS-R09MAE59 DELIMITED BY SIZE                                                         05/02/20
  95348                           " CASO = " DELIMITED BY SIZE                                                          05/02/20
  95349                           R09CAS-WS DELIMITED BY SIZE                                                           05/02/20
  95350                           INTO MENSAJE                                                                          20/02/20
  95351                           PERFORM GRABA-BITACORA                                                                05/02/20
  95352                       END-IF                                                                                    20/02/20
  95353                  END-READ                                                                                       20/02/20
  95354            END-IF                                                                                               20/02/20
  95355            CLOSE R09MAE59.                                                                                      05/02/20
  95356        END-ACTUALIZA-R09MAE.                                                                                    20/02/20
  95357       * 

      *>         Proceso de acumulación de montos.
  95358        B030-ACUMULA-MONTOS.               
  
      *>         1. Inicializa los punteros
      *>         2. Mueve el caso al puntero, asigna valores maximos.
               
  95359            PERFORM D020-INICIALIZA THRU D020-EXIT.                                                              21/01/20
  95360            MOVE R09CAS-WS TO RY1CAS OF RY1RCA01.                                                                20/02/20
  95361            MOVE HIGH-VALUES TO RY1FMR OF RY1RCA01.                                                              20/02/20
  95362            START RY1RCA01 KEY IS NOT LESS EXTERNALLY-DESCRIBED-KEY.                                             21/01/20
  *> 95363       *    READ RY1RCA01 PRIOR  RECORD inTO WRY1RCA01                                                           20/02/20
  95364            READ RY1RCA01 NEXT   RECORD inTO WRY1RCA01                                                           
      *>         Si no es 00 o 04 es un error, se guarda donde se dio el erro, y sale del programa
  95365            IF FS-RY1RCA NOT = "00" AND FS-RY1RCA NOT = "04"                                                     20/02/20
  95366                 MOVE " READ 1 RY1CA " TO LugarErro                                                              20/02/20
  95367                 PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                                 21/01/20
  95368            END-IF.                                                                                              20/02/20
  95369            IF RY1CAS OF WRY1RCA01 = R09CAS-WS                                                                   20/02/20
  *> 95370       *       Actualiza los estados del RY1RCA con los del R09MAE                                               21/01/20
  95371               MOVE R09ESS-WS TO RY1EST OF WRY1RCA01                                                             20/02/20
  95372               MOVE R09ESR-WS TO RY1ES1 OF WRY1RCA01                                                             20/02/20
  95373               IF   R09ESR-WS = "C" THEN                                                                         20/02/20
  95374                    MOVE "T" TO RY1ES3 OF WRY1RCA01                                                              20/02/20
  95375               ELSE                                                                                              20/02/20
  95376                    MOVE "P" TO RY1ES3 OF WRY1RCA01                                                              20/02/20
  95377               END-IF                                                                                            20/02/20
  95378               MOVE R09CON-WS TO RY1CON OF WRY1RCA01                                                             20/02/20
  *> 95379       *--------                                                                                                 21/01/20
  95380               MOVE RY1CAS OF RY1RCA01 TO RY1CAD OF RY1RCD01                                                     20/02/20
  95381               MOVE RY1FMR OF RY1RCA01 TO RY1F03 OF RY1RCD01                                                     20/02/20
  95382               MOVE "A"                TO RY1COR OF RY1RCD01                                                     20/02/20
  95383               START RY1RCD01 KEY IS NOT LESS EXTERNALLY-DESCRIBED-KEY                                           21/01/20
  95384               READ RY1RCD01 NEXT RECORD                                                                         20/02/20
  95385               IF RY1CAS OF WRY1RCA01 = RY1CAD OF RY1RCD01 AND                                                   21/01/20
  95386                  RY1FMR OF WRY1RCA01 = RY1F03 OF RY1RCD01                                                       21/01/20
  95387                  MOVE 0 TO FIN                                                                                  20/02/20
  95388                  MOVE 0 TO INDICE                                                                               20/02/20
  95389                  PERFORM B040-CARGA-DETALLE THRU B040-EXIT UNTIL                                                21/01/20
  95390                          FIN = 1                                                                                21/01/20
  95391               END-IF                                                                                            20/02/20
  95392            END-IF.
      *>         Setea totales a cero                                                                                              20/02/20
  95393            MOVE ZEROS TO TOTALPRO-WS TOTALPAJ-WS                                                                20/02/20
  95394                          TOTALGAS-WS TOTALSAL-WS                                                                20/02/20
  *> 95395       * Suma los rubros IT - Indice 1                                                                                                        21/01/20
  95396            MOVE "IT"      TO TEMRBR.                                                                            20/02/20
  95397            MOVE R09CAS-WS TO TEMC47.                                                                            20/02/20
  95398            READ TEMMTO Record.                                                                                  20/02/20
  95399            IF FS-TEMMTO = "00" OR FS-TEMMTO = "04" AND TEMMNT NOT = 0                                           20/02/20
  95400               MOVE TEMMNT TO TOTALPRO-WS                                                                        20/02/20
  95401               MOVE 1 TO INDICE  
      *>             Llama a proceso de suma de gastos                                                                                20/02/20
  95402               PERFORM B050-SUMA-GASTOS THRU B050-EXIT.                                                          20/02/20
  *> 95403       * Suma los rubros IP - Indice 2                                                                                                       21/01/20
  95404            MOVE "IP"      TO TEMRBR.                                                                            20/02/20
  95405            MOVE R09CAS-WS TO TEMC47.                                                                            20/02/20
  95406            READ TEMMTO RECORD.                                                                                  20/02/20
  95407            IF FS-TEMMTO = "00" OR FS-TEMMTO = "04" AND TEMMNT NOT = 0                                           20/02/20
  95408               MOVE TEMMNT TO TOTALPAJ-WS                                                                        20/02/20
  95409               MOVE 2 TO INDICE                
      *>         llama a proceso de suma de gastos                                                                  20/02/20
  95410               PERFORM B050-SUMA-GASTOS THRU B050-EXIT.                                                          20/02/20
  *> 95411       * Suma los rubros CM - Indice 3                                                                                                        21/01/20
  95412            MOVE "CM"      TO TEMRBR.                                                                            20/02/20
  95413            MOVE R09CAS-WS TO TEMC47.                                                                            20/02/20
  95414            READ TEMMTO RECORD.                                                                                  20/02/20
  95415            IF FS-TEMMTO = "00" OR FS-TEMMTO = "04" AND TEMMNT NOT = 0                                           20/02/20
  95416               MOVE TEMMNT TO TOTALGAS-WS                                                                        20/02/20
  95417               MOVE 3 TO INDICE            
      *>         llama a proceso de suma de gastos                                                                       20/02/20
  95418               PERFORM B050-SUMA-GASTOS THRU B050-EXIT.                                                          20/02/20
  *> 95419       * Suma los rubros IM - Indice 4                                                                                                        21/01/20
  95420            MOVE "IM"      TO TEMRBR.                                                                            20/02/20
  95421            MOVE R09CAS-WS TO TEMC47.                                                                            20/02/20
  95422            READ TEMMTO RECORD.                                                                                  20/02/20
  95423            IF FS-TEMMTO = "00" OR FS-TEMMTO = "04" AND TEMMNT NOT = 0                                           20/02/20
  95424               MOVE TEMMNT TO TOTALSAL-WS                                                                        20/02/20
  95425               MOVE 4 TO INDICE                                                                                  20/02/20
  95426               PERFORM B050-SUMA-GASTOS THRU B050-EXIT.                                                          20/02/20
  *> 95427       * Esto no sé que hace                                                                                                        21/01/20
  95428            IF OKMONTO NOT EQUAL ZEROS                                                                           20/02/20
  95429               PERFORM B060-GASTOS-ACUMULADOS THRU B060-EXIT                                                     20/02/20
  95430            END-IF.                                                                                              20/02/20
  *> 95431       *- AQUI VA SI DEJA LOS SALDOS EN CERO POR ESTAR EN MATEMATICA                                             21/01/20
  *> 95432       *------------------------------------------------------------                                             21/01/20
  95433               If WR20IN >= '1' AND WR20IN <= '4'                                                                21/01/20
  *> 95434       *                     Solo queda Cuenta Médica                                                            21/01/20
  95435                  MOVE ZEROS TO RY1SAL OF WRY1RCD01A                                                             20/02/20
  95436                  MOVE ZEROS TO RY1SAL OF WRY1RCD01B                                                             20/02/20
  95437                  MOVE ZEROS TO RY1SAL OF WRY1RCD01D                                                             20/02/20
  95438                  MOVE ZEROS TO RY1SAL OF WRY1RCD01T                                                             20/02/20
  95439               END-If                                                                                            20/02/20
  *> 95440       *--  Si es Permanente Mensual. No reserva, ya que va a Matemática                                         21/01/20
  95441               If WR54EF NOT = ' '                                                                               21/01/20
  95442                  MOVE ZEROS TO RY1SAL OF WRY1RCD01A                                                             20/02/20
  95443                  MOVE ZEROS TO RY1SAL OF WRY1RCD01B                                                             20/02/20
  95444                  MOVE ZEROS TO RY1SAL OF WRY1RCD01C                                                             20/02/20
  95445                  MOVE ZEROS TO RY1SAL OF WRY1RCD01D                                                             20/02/20
  95446                  MOVE ZEROS TO RY1SAL OF WRY1RCD01T                                                             20/02/20
  95447               END-If                                                                                            20/02/20
  95448               
      *>             Realiza todos los calculos matemáticos                                                                                                  21/01/20
  95449               COMPUTE RY1GAS OF WRY1RCD01T =                                                                    10/02/20
  95450                       RY1GAS OF WRY1RCD01A +                                                                    21/01/20
  95451                       RY1GAS OF WRY1RCD01B +                                                                    21/01/20
  95452                       RY1GAS OF WRY1RCD01C +                                                                    21/01/20
  95453                       RY1GAS OF WRY1RCD01D                                                                      21/01/20
  95454               COMPUTE RY1GA1 OF WRY1RCD01T =                                                                    10/02/20
  95455                       RY1GA1 OF WRY1RCD01A +                                                                    21/01/20
  95456                       RY1GA1 OF WRY1RCD01B +                                                                    21/01/20
  95457                       RY1GA1 OF WRY1RCD01C +                                                                    21/01/20
  95458                       RY1GA1 OF WRY1RCD01D                                                                      21/01/20
  95459               COMPUTE RY1PR1 OF WRY1RCD01T =                                                                    10/02/20
  95460                       RY1PR1 OF WRY1RCD01A +                                                                    21/01/20
  95461                       RY1PR1 OF WRY1RCD01B +                                                                    21/01/20
  95462                       RY1PR1 OF WRY1RCD01C +                                                                    21/01/20
  95463                       RY1PR1 OF WRY1RCD01D                                                                      21/01/20
  95464               COMPUTE RY1SAL OF WRY1RCD01T =                                                                    10/02/20
  95465                       RY1SAL OF WRY1RCD01A +                                                                    21/01/20
  95466                       RY1SAL OF WRY1RCD01B +                                                                    21/01/20
  95467                       RY1SAL OF WRY1RCD01C +                                                                    21/01/20
  95468                       RY1SAL OF WRY1RCD01D                                                                      21/01/20
  *> 95469       *------------------------------------------------------------                                             21/01/20
  *> 95470       *--  Grabar o regraba el registro RY1RCA y los RY1RCD                                                     21/01/20
  95471            IF (WCASO NOT EQUAL SPACES AND R09ESR-WS  = 'A') OR                                                  20/02/20
  95472               (WCASO NOT EQUAL SPACES AND ACERRAR = 'S')                                                        20/02/20
  95473               MOVE WFHASTACORR TO RY1FMR OF WRY1RCA01                                                           20/02/20
  95474                                   RY1F03 OF WRY1RCD01A                                                          21/01/20
  95475                                   RY1F03 OF WRY1RCD01B                                                          21/01/20
  95476                                   RY1F03 OF WRY1RCD01C                                                          21/01/20
  95477                                   RY1F03 OF WRY1RCD01D                                                          21/01/20
  95478                                   RY1F03 OF WRY1RCD01T                                                          21/01/20
  95479               MOVE FHA         TO RY1FE4 OF WRY1RCD01A                                                          20/02/20
  95480                                   RY1FE4 OF WRY1RCD01B                                                          21/01/20
  95481                                   RY1FE4 OF WRY1RCD01C                                                          21/01/20
  95482                                   RY1FE4 OF WRY1RCD01D                                                          21/01/20
  95483                                   RY1FE4 OF WRY1RCD01T                                                          21/01/20
  95484               MOVE R09AGE-WS TO RY1AGE OF WRY1RCA01  
      *>             Escritura de encabezados                                                           20/02/20
  95485               WRITE RRY1RCA01 FROM WRY1RCA01                                                                    20/02/20
  95486                     INVALID KEY                                                                                 21/01/20
  95487                     MOVE " Write Enc " TO LUGARERRO                                                             20/02/20
  95488                     PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                             21/01/20
  95489               END-WRITE            
  
      *>             asigna parámetros a otro puntero                                                             20/02/20
  95490               MOVE R09CAS-WS TO RY1CAD OF WRY1RCD01A                                                            20/02/20
  95491               MOVE "A"       TO RY1COR OF WRY1RCD01A                                                            20/02/20
  95492               MOVE R09AGE-WS TO RY1AGD OF WRY1RCD01A                                                            20/02/20

      *>             Escritura de detalle 
  95493               WRITE RRY1RCD01 FROM WRY1RCD01A                                                                   20/02/20
  95494                     INVALID KEY                                                                                 21/01/20
  95495                     MOVE " Write Det1" TO LUGARERRO                                                             20/02/20
  95496                     PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                             21/01/20
  95497               END-WRITE                                                                                         20/02/20
  95498                  MOVE R09CAS-WS TO RY1CAD OF WRY1RCD01B                                                         20/02/20
  95499                  MOVE "B"       TO RY1COR OF WRY1RCD01B                                                         20/02/20
  95500                  MOVE R09AGE-WS TO RY1AGD OF WRY1RCD01B 
                                                                           20/02/20
  95501                  WRITE RRY1RCD01 FROM WRY1RCD01B                                                                20/02/20
  95502                        INVALID KEY                                                                              21/01/20
  95503                        MOVE " Write Det1" TO LUGARERRO                                                          20/02/20
  95504                        PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                          21/01/20
  95505                  END-WRITE                                                                                      20/02/20
  95506                  MOVE R09CAS-WS TO RY1CAD OF WRY1RCD01C                                                         20/02/20
  95507                  MOVE "C"       TO RY1COR OF WRY1RCD01C                                                         20/02/20
  95508                  MOVE R09AGE-WS TO RY1AGD OF WRY1RCD01C                                                         20/02/20
  95509                  WRITE RRY1RCD01 FROM WRY1RCD01C                                                                20/02/20
  95510                        INVALID KEY                                                                              21/01/20
  95511                        MOVE " Write Det1" TO LUGARERRO                                                          20/02/20
  95512                        PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                          21/01/20
  95513                  END-WRITE                                                                                      20/02/20
  95514                  MOVE R09CAS-WS TO RY1CAD OF WRY1RCD01D                                                         20/02/20
  95515                  MOVE "D"       TO RY1COR OF WRY1RCD01D                                                         20/02/20
  95516                  MOVE R09AGE-WS TO RY1AGD OF WRY1RCD01D                                                         20/02/20
  95517                  WRITE RRY1RCD01 FROM WRY1RCD01D                                                                20/02/20
  95518                        INVALID KEY                                                                              21/01/20
  95519                        MOVE " Write Det1" TO LUGARERRO                                                          20/02/20
  95520                        PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                          21/01/20
  95521                  END-WRITE                                                                                      20/02/20
  95522               MOVE R09CAS-WS TO RY1CAD OF WRY1RCD01T                                                            20/02/20
  95523               MOVE "T"       TO RY1COR OF WRY1RCD01T                                                            20/02/20
  95524               MOVE R09AGE-WS TO RY1AGD OF WRY1RCD01T                                                            20/02/20
  95525               WRITE RRY1RCD01 FROM WRY1RCD01T                                                                   20/02/20
  95526                     INVALID KEY                                                                                 21/01/20
  95527                     MOVE " Write DetT" TO LUGARERRO                                                             20/02/20
  95528                     PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                             21/01/20
  95529               END-WRITE                                                                                         20/02/20
  95530            ELSE                                                                                                 21/01/20
  95531               MOVE CAS001      TO RY1CAS OF RY1RCA01                                                            20/02/20
  95532               MOVE WFHASTACORR TO RY1FMR OF RY1RCA01                                                            20/02/20
  95533               READ RY1RCA01 RECORD                                                                              20/02/20
  95534               IF FS-RY1RCA = "00" OR FS-RY1RCA = "04"                                                           21/01/20
  95535                  REWRITE RRY1RCA01 FROM WRY1RCA01                                                               20/02/20
  95536                     INVALID KEY                                                                                 21/01/20
  95537                     MOVE " ReWrite Enc " TO LUGARERRO                                                           20/02/20
  95538                     PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                             21/01/20
  95539                  END-REWRITE                                                                                    20/02/20
  95540               END-IF                                                                                            25/03/20
  95541               MOVE CAS001      TO RY1CAD OF RY1RCD01                                                            20/02/20
  95542               MOVE WFHASTACORR TO RY1F03 OF RY1RCD01                                                            20/02/20
  95543               IF RY1GAS OF WRY1RCD01A NOT EQUAL ZEROS                                                           25/03/20
  95544                  MOVE "A" TO RY1COR OF RY1RCD01                                                                 20/02/20
  95545                  READ RY1RCD01 RECORD                                                                           20/02/20
  95546                  IF FS-RY1RCD = "00" OR FS-RY1RCD = "04"                                                        21/01/20
  95547                     REWRITE RRY1RCD01 FROM WRY1RCD01A                                                           20/02/20
  95548                     INVALID KEY                                                                                 21/01/20
  95549                       MOVE " ReWrite Det1 " TO LUGARERRO                                                        20/02/20
  95550                       PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                           21/01/20
  95551                     END-REWRITE                                                                                 20/02/20
  95552                  END-IF                                                                                         20/02/20
  95553               END-IF                                                                                            20/02/20
  95554               IF RY1GAS OF WRY1RCD01B NOT EQUAL ZEROS                                                           20/02/20
  95555                  MOVE "B" TO RY1COR OF RY1RCD01                                                                 20/02/20
  95556                  READ RY1RCD01 RECORD                                                                           20/02/20
  95557                  IF FS-RY1RCD = "00" OR FS-RY1RCD = "04"                                                        21/01/20
  95558                     REWRITE RRY1RCD01 FROM WRY1RCD01B                                                           20/02/20
  95559                       INVALID KEY                                                                               21/01/20
  95560                       MOVE " ReWrite Det2 " TO LUGARERRO                                                        20/02/20
  95561                       PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                           21/01/20
  95562                     END-REWRITE                                                                                 20/02/20
  95563                  END-IF                                                                                         20/02/20
  95564               END-IF                                                                                            20/02/20
  95565               IF RY1GAS OF WRY1RCD01C NOT EQUAL ZEROS                                                           20/02/20
  95566                  MOVE "C" TO RY1COR OF RY1RCD01                                                                 20/02/20
  95567                  READ RY1RCD01 RECORD                                                                           20/02/20
  95568                  IF FS-RY1RCD = "00" OR FS-RY1RCD = "04"                                                        21/01/20
  95569                     REWRITE RRY1RCD01 From WRY1RCD01C                                                           21/01/20
  95570                       INVALID KEY                                                                               21/01/20
  95571                       MOVE " ReWrite Det3 " TO LUGARERRO                                                        20/02/20
  95572                       PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                           21/01/20
  95573                     END-REWRITE                                                                                 20/02/20
  95574                  END-IF                                                                                         20/02/20
  95575               END-IF                                                                                            20/02/20
  95576               IF RY1GAS OF WRY1RCD01D NOT EQUAL ZEROS                                                           20/02/20
  95577                  MOVE "D" TO RY1COR OF RY1RCD01                                                                 20/02/20
  95578                  READ RY1RCD01 RECORD                                                                           20/02/20
  95579                  IF FS-RY1RCD = "00" OR FS-RY1RCD = "04"                                                        21/01/20
  95580                     REWRITE RRY1RCD01 From WRY1RCD01D                                                           21/01/20
  95581                       INVALID KEY                                                                               21/01/20
  95582                       MOVE " ReWrite Det4 " TO LUGARERRO                                                        20/02/20
  95583                       PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                           21/01/20
  95584                     END-REWRITE                                                                                 20/02/20
  95585                  END-IF                                                                                         20/02/20
  95586               END-IF                                                                                            20/02/20
  95587                  MOVE "T" TO RY1COR OF RY1RCD01                                                                 20/02/20
  95588                  READ RY1RCD01 RECORD                                                                           20/02/20
  95589              IF FS-RY1RCD = "00" OR FS-RY1RCD = "04"                                                            21/01/20
  95590                 REWRITE RRY1RCD01 From WRY1RCD01T                                                               21/01/20
  95591                   INVALID KEY                                                                                   21/01/20
  95592                     MOVE " ReWrite DetT " TO LUGARERRO                                                          20/02/20
  95593                     PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                             21/01/20
  95594                 END-REWRITE                                                                                     20/02/20
  95595              END-iF                                                                                             20/02/20
  95596            END-IF.                                                                                              20/02/20
  95597        END-B030-ACUMULA-MONTOS.     
      *>     Fin de proceso de acumulación de montos 
                                                                                           25/02/20
  95598        B030-EXIT.                                                                                               21/01/20
  95599             EXIT.                                                                                               21/01/20
 118400       *

      *>     Inicio de proceso de carga de detalles                                                                                                          31/01/17
 118500        B040-CARGA-DETALLE.      
      *>     Switch de rubros                                                                                30/01/17
 118600            IF RY1COR OF RY1RCD01 = "A"                                                                          17/02/17
 118700               MOVE CORRESPONDING RRY1RCD01 TO WRY1RCD01A                                                        20/02/20
 118800               MOVE WFHASTACORR TO RY1F03 OF WRY1RCD01A                                                          20/02/20
 118801            END-IF                                                                                               20/02/20
 118900            IF RY1COR OF RY1RCD01 = "B"                                                                          17/02/17
 119000               MOVE CORRESPONDING RRY1RCD01 TO WRY1RCD01B                                                        20/02/20
 119100               MOVE WFHASTACORR TO RY1F03 OF WRY1RCD01B                                                          20/02/20
 119101            END-IF                                                                                               20/02/20
 119200            IF RY1COR OF RY1RCD01 = "C"                                                                          17/02/17
 119300               MOVE CORRESPONDING RRY1RCD01 TO WRY1RCD01C                                                        20/02/20
 119400               MOVE WFHASTACORR TO RY1F03 OF WRY1RCD01C                                                          20/02/20
 119401            END-IF                                                                                               20/02/20
 119500            IF RY1COR OF RY1RCD01 = "D"                                                                          17/02/17
 119600               MOVE CORRESPONDING RRY1RCD01 TO WRY1RCD01D                                                        20/02/20
 119700               MOVE WFHASTACORR TO RY1F03 OF WRY1RCD01D                                                          20/02/20
 119701            END-IF                                                                                               20/02/20
 119800            IF RY1COR OF RY1RCD01 = "T"                                                                          17/02/17
 119900               MOVE CORRESPONDING RRY1RCD01 TO WRY1RCD01T                                                        20/02/20
 120000               MOVE WFHASTACORR TO RY1F03 OF WRY1RCD01T                                                          20/02/20
 120001            END-IF                                                                                               20/02/20
 120100            READ RY1RCD01 NEXT RECORD  
 
      *>         Evalua códigos de error                                                                          20/02/20
 120200            IF FS-RY1RCD = "00" OR FS-RY1RCD = "04"                                                              27/02/17
 120300               IF RY1CAS OF WRY1RCA01 = RY1CAD OF RY1RCD01 AND                                                   27/02/17
 120400                  RY1FMR OF WRY1RCA01 = RY1F03 OF RY1RCD01                                                       27/02/17
 120500                  NEXT SENTENCE                                                                                  27/02/17
 120600               ELSE                                                                                              27/02/17
 120700                  MOVE 1 TO FIN                                                                                  20/02/20
 120800            ELSE                                                                                                 30/01/17
 120900               IF FS-RY1RCD = "10" THEN                                                                          27/02/17
 121000                  MOVE 1 TO FIN                                                                                  20/02/20
 121100               ELSE                                                                                              27/02/17
 121200                  MOVE " READ RCD Next " TO LugarErro                                                            20/02/20
 121300                  PERFORM D010-REVISA-FILES-STATUS THRU D010-EXIT                                                27/02/17
 121400               END-IF                                                                                            20/02/20
 121500            END-IF.                                                                                              20/02/20
 121501        END-B040-CARGA-DETALLE.
      *>     Fin de carga detalle                                                                                  25/02/20
 121600        B040-EXIT.                                                                                               30/01/17
 121700             EXIT.                                                                                               30/01/17
 *> 121800       *                  
      *>       Suma de gastos 
      *>     Documentación de proceso de suma de gastos
      *>     Switch basado en el índice                                                                                      30/01/17
 121900        B050-SUMA-GASTOS.                                                                                        20/02/20
 122000            IF INDICE = 1                                                                                        10/02/20
 122100                COMPUTE RY1GAS OF WRY1RCD01A =                                                                   10/02/20
 122200                        RY1GAS OF WRY1RCD01A + TEMMNT                                                            17/02/17
 122300            ELSE                                                                                                 20/02/20
 122400                IF INDICE = 2                                                                                    10/02/20
 122500                   COMPUTE RY1GAS OF WRY1RCD01B =                                                                10/02/20
 122600                           RY1GAS OF WRY1RCD01B + TEMMNT                                                         17/02/17
 122700                ELSE                                                                                             20/02/20
 122800                   IF INDICE = 3                                                                                 10/02/20
 122900                      COMPUTE RY1GAS OF WRY1RCD01C =                                                             10/02/20
 123000                              RY1GAS OF WRY1RCD01C + TEMMNT                                                      17/02/17
 123100                   ELSE                                                                                          20/02/20
 123200                      IF INDICE = 4                                                                              10/02/20
 123300                         COMPUTE RY1GAS OF WRY1RCD01D =                                                          10/02/20
 123400                                 RY1GAS OF WRY1RCD01D + TEMMNT                                                   17/02/17
 123500                      END-IF                                                                                     20/02/20
 123600                   END-IF                                                                                        20/02/20
 123700                END-IF                                                                                           20/02/20
 123800            END-IF.                                                                                              20/02/20
 123801        END-B050-SUMA-GASTOS.                   
      *>     Fin suma de gastos                                                                 25/02/20
 124100        B050-EXIT.                                                                                               20/02/20
 124200             EXIT.                                                                                               20/02/20
 124300       *                                                                                                         14/02/17
 124400        B060-GASTOS-ACUMULADOS.                                                                                  20/02/20
 124500            MOVE ZEROS TO TOTAL1 TOTAL2 TOTAL3 TOTAL4 TOTAL5 TOTAL6.                                             20/02/20
 124600            IF RY1GAS OF WRY1RCD01A Not Equal ZEROS                                                              20/02/20
 124700               COMPUTE TOTAL2 = TOTAL2 + RY1GAS OF WRY1RCD01A                                                    20/02/20
 124800               MOVE RY1PAT TO PAUMRU                                                                             20/02/20
 124900               COMPUTE RY1GA1 OF WRY1RCD01A =                                                                    10/02/20
 125000                       RY1GAS OF WRY1RCD01A * RY1CA1 / 100                                                       17/02/17
 125100               COMPUTE TOTAL3 = TOTAL3 + RY1GA1 OF WRY1RCD01A                                                    20/02/20
 125200               COMPUTE GASTOT = RY1GAS OF WRY1RCD01A +                                                           20/02/20
 125300                                RY1GA1 OF WRY1RCD01A                                                             17/02/17
 125400               IF GASTOT >= RY1PR1 OF WRY1RCD01A  THEN                                                           20/02/20
 125500                  COMPUTE RY1PR1 OF WRY1RCD01A =                                                                 10/02/20
 125600                          GASTOT + RY1PR1 OF WRY1RCD01A *                                                        20/02/20
 125700                          PAUMRU / 100                                                                           17/02/17
 125800                  COMPUTE TOTAL5 = TOTAL5 + RY1PR1 OF WRY1RCD01A                                                 20/02/20
 125900               END-IF                                                                                            20/02/20
 126000               COMPUTE RY1SAL OF WRY1RCD01A =                                                                    10/02/20
 126100                          RY1PR1 OF WRY1RCD01A - GASTOT                                                          20/02/20
 126200               COMPUTE TOTAL6 = TOTAL6 + RY1SAL OF WRY1RCD01A                                                    20/02/20
 126300            END-IF.                                                                                              20/02/20
 126400            IF RY1GAS OF WRY1RCD01B Not Equal ZEROS                                                              20/02/20
 126500               COMPUTE TOTAL2 = TOTAL2 + RY1GAS OF WRY1RCD01B                                                    20/02/20
 126600               MOVE RY1PAP TO PAUMRU                                                                             20/02/20
 126700               COMPUTE RY1GA1 OF WRY1RCD01B =                                                                    10/02/20
 126800                       RY1GAS OF WRY1RCD01B * RY1CA1 / 100                                                       17/02/17
 126900               COMPUTE TOTAL3 = TOTAL3 + RY1GA1 OF WRY1RCD01B                                                    20/02/20
 127000               COMPUTE GASTOT = RY1GAS OF WRY1RCD01B +                                                           20/02/20
 127100                                RY1GA1 OF WRY1RCD01B                                                             17/02/17
 127200               IF GASTOT >= RY1PR1 OF WRY1RCD01B  THEN                                                           20/02/20
 127300                  COMPUTE RY1PR1 OF WRY1RCD01B =                                                                 10/02/20
 127400                          GASTOT + RY1PR1 OF WRY1RCD01B *                                                        20/02/20
 127500                          PAUMRU / 100                                                                           17/02/17
 127600                  COMPUTE TOTAL5 = TOTAL5 + RY1PR1 OF WRY1RCD01B                                                 20/02/20
 127700               END-IF                                                                                            20/02/20
 127800               COMPUTE RY1SAL OF WRY1RCD01B =                                                                    10/02/20
 127900                          RY1PR1 OF WRY1RCD01B - GASTOT                                                          20/02/20
 128000               COMPUTE TOTAL6 = TOTAL6 + RY1SAL OF WRY1RCD01B                                                    20/02/20
 128100            END-IF.                                                                                              20/02/20
 128200            IF RY1GAS OF WRY1RCD01C Not Equal ZEROS                                                              20/02/20
 128300               COMPUTE TOTAL2 = TOTAL2 + RY1GAS OF WRY1RCD01C                                                    20/02/20
 128400               MOVE RY1PCM TO PAUMRU                                                                             20/02/20
 128500               COMPUTE RY1GA1 OF WRY1RCD01C =                                                                    10/02/20
 128600                       RY1GAS OF WRY1RCD01C * RY1CA1 / 100                                                       17/02/17
 128700               COMPUTE TOTAL3 = TOTAL3 + RY1GA1 OF WRY1RCD01C                                                    20/02/20
 128800               COMPUTE GASTOT = RY1GAS OF WRY1RCD01C +                                                           20/02/20
 128900                                RY1GA1 OF WRY1RCD01C                                                             17/02/17
 129000               IF GASTOT >= RY1PR1 OF WRY1RCD01C  THEN                                                           20/02/20
 129100                  COMPUTE RY1PR1 OF WRY1RCD01C =                                                                 10/02/20
 129200                          GASTOT + RY1PR1 OF WRY1RCD01C *                                                        20/02/20
 129300                          PAUMRU / 100                                                                           17/02/17
 129400                  COMPUTE TOTAL5 = TOTAL5 + RY1PR1 OF WRY1RCD01C                                                 20/02/20
 129500               END-IF                                                                                            20/02/20
 129600               COMPUTE RY1SAL OF WRY1RCD01C =                                                                    10/02/20
 129700                          RY1PR1 OF WRY1RCD01C - GASTOT                                                          20/02/20
 129800               COMPUTE TOTAL6 = TOTAL6 + RY1SAL OF WRY1RCD01C                                                    20/02/20
 129900            END-IF.                                                                                              20/02/20
 130000            IF RY1GAS OF WRY1RCD01D Not Equal ZEROS                                                              20/02/20
 130100               COMPUTE TOTAL2 = TOTAL2 + RY1GAS OF WRY1RCD01D                                                    20/02/20
 130200               MOVE RY1PIM TO PAUMRU                                                                             20/02/20
 130300               COMPUTE RY1GA1 OF WRY1RCD01D =                                                                    10/02/20
 130400                       RY1GAS OF WRY1RCD01D * RY1CA1 / 100                                                       17/02/17
 130500               COMPUTE TOTAL3 = TOTAL3 + RY1GA1 OF WRY1RCD01D                                                    20/02/20
 130600               COMPUTE GASTOT = RY1GAS OF WRY1RCD01D +                                                           20/02/20
 130700                                RY1GA1 OF WRY1RCD01D                                                             17/02/17
 130800               IF GASTOT >= RY1PR1 OF WRY1RCD01D  THEN                                                           20/02/20
 130900                  COMPUTE RY1PR1 OF WRY1RCD01D =                                                                 10/02/20
 131000                          GASTOT + RY1PR1 OF WRY1RCD01D *                                                        20/02/20
 131100                          PAUMRU / 100                                                                           17/02/17
 131200                  COMPUTE TOTAL5 = TOTAL5 + RY1PR1 OF WRY1RCD01D                                                 20/02/20
 131300               END-IF                                                                                            20/02/20
 131400               COMPUTE RY1SAL OF WRY1RCD01D =                                                                    10/02/20
 131500                          RY1PR1 OF WRY1RCD01D - GASTOT                                                          20/02/20
 131600               COMPUTE TOTAL6 = TOTAL6 + RY1SAL OF WRY1RCD01D                                                    20/02/20
 131700            END-IF.                                                                                              20/02/20
 131701        END-B060-GASTOS-ACUMULADOS.                                                                              25/02/20
 131800                                                                                                                 17/02/17
 132500        B060-EXIT.                                                                                               20/02/20
 132600             EXIT.                                                                                               20/02/20
 139600                                                                                                                 19/12/16
 139700        SQL-REVISASQLSTATE.                                                                                      12/12/16
 139800            IF SQLSTATE(1:2)  NOT EQUAL "00"                                                                     12/12/16
 139900            AND SQLSTATE(1:2) NOT EQUAL "01" THEN                                                                12/12/16
 140000                  DISPLAY "R09_RY1RCS - Error estado SQL: "                                                      12/12/16
 140100               SQLSTATE                                                                                          12/12/16
 140200            END-IF.                                                                                              20/02/20
 140300        END-SQL-REVISASQLSTATE.                                                                                  20/02/20
 140400            EXIT.                                                                                                20/02/17
 140500                                                                                                                 11/01/17
 140600        D010-REVISA-FILES-STATUS.                                                                                11/01/17
 140800            IF (FS-RY1RCA   NOT = "00" AND FS-RY1RCA   NOT = "04") OR                                            20/01/20
 140900               (FS-RY1RCD   NOT = "00" AND FS-RY1RCD   NOT = "04") OR                                            11/01/17
 141000               (FS-TEMFMX1  NOT = "00" AND FS-TEMFMX1  NOT = "04") OR                                            11/01/17
 141100               (FS-TEMMTO   NOT = "00" AND FS-TEMMTO   NOT = "04") OR                                            20/02/20
 141200               (FS-R08MAE   NOT = "00" AND FS-R08MAE   NOT = "04")                                               14/01/17
 141300               DISPLAY "R09RY1RCA - ERROR "  LugarErro " FS-R08MAE:   "                                          14/01/17
 141400                        FS-R08MAE   UPON CONSOLE                                                                 14/01/17
 141700               DISPLAY "R09RY1RCA - ERROR "  LugarErro " FS-TEMFMX1:  "                                          11/01/17
 141800                        FS-TEMFMX1  UPON CONSOLE                                                                 11/01/17
 141900               DISPLAY "R09RY1RCA - ERROR "  LugarErro " FS-TEMMTO:   "                                          20/02/20
 142000                        FS-TEMMTO   UPON CONSOLE                                                                 20/02/20
 142100               DISPLAY "R09RY1RCA - ERROR "  LugarErro " FS-RY1RCA:   "                                          11/01/17
 142200                        FS-RY1RCA   UPON CONSOLE                                                                 11/01/17
 142300               DISPLAY "R09RY1RCA - ERROR "  LugarErro " FS-RY1RCD:   "                                          11/01/17
 142400                        FS-RY1RCD   UPON CONSOLE                                                                 11/01/17
 142512              MOVE SPACES TO MENSAJE                                                                             20/02/20
 142513              STRING "D010-R09RY1RCA ERROR FS R08MAE "                                                           17/01/20
 142514                        DELIMITED BY SIZE                                                                        17/01/20
 142515              FS-R08MAE DELIMITED BY SIZE                                                                        17/01/20
 142516              " " DELIMITED BY SIZE                                                                              17/01/20
 142517              CAS001 DELIMITED BY SIZE                                                                           17/01/20
 142518              INTO MENSAJE                                                                                       20/02/20
 142519              PERFORM GRABA-BITACORA                                                                             17/01/20
 142528              MOVE SPACES TO MENSAJE                                                                             20/02/20
 142529              STRING "D010-R09RY1RCA ERROR FSTEMFMX1 "                                                           17/01/20
 142530                  DELIMITED BY SIZE                                                                              17/01/20
 142531              FS-TEMFMX1 DELIMITED BY SIZE                                                                       17/01/20
 142532              " EN EL CASO QUE SIGUE AL " DELIMITED BY SIZE                                                      17/01/20
 142533              CAS001 DELIMITED BY SIZE                                                                           17/01/20
 142534              INTO MENSAJE                                                                                       20/02/20
 142535              PERFORM GRABA-BITACORA                                                                             17/01/20
 142536              MOVE SPACES TO MENSAJE                                                                             20/02/20
 142537              STRING "D010-R09RY1RCA ERROR FS TEMMTO "                                                           20/02/20
 142538                          DELIMITED BY SIZE                                                                      17/01/20
 142539              FS-TEMMTO DELIMITED BY SIZE                                                                        20/02/20
 142540              " " DELIMITED BY SIZE                                                                              17/01/20
 142541              CAS001 DELIMITED BY SIZE                                                                           17/01/20
 142542              INTO MENSAJE                                                                                       20/02/20
 142543              PERFORM GRABA-BITACORA                                                                             17/01/20
 142544              MOVE SPACES TO MENSAJE                                                                             20/02/20
 142545              STRING "D010-R09RY1RCA ERROR FS RY1RCA "                                                           17/01/20
 142546                                DELIMITED BY SIZE                                                                17/01/20
 142547              FS-RY1RCA DELIMITED BY SIZE                                                                        17/01/20
 142548              " " DELIMITED BY SIZE                                                                              17/01/20
 142549              CAS001 DELIMITED BY SIZE                                                                           17/01/20
 142550              INTO MENSAJE                                                                                       20/02/20
 142551              PERFORM GRABA-BITACORA                                                                             17/01/20
 142552              MOVE SPACES TO MENSAJE                                                                             20/02/20
 142553              STRING "D010-R09RY1RCA ERROR FS RY1RCD "                                                           17/01/20
 142554                             DELIMITED BY SIZE                                                                   17/01/20
 142555              FS-RY1RCD DELIMITED BY SIZE                                                                        17/01/20
 142556              " " DELIMITED BY SIZE                                                                              17/01/20
 142557              CAS001 DELIMITED BY SIZE                                                                           17/01/20
 142558              INTO MENSAJE                                                                                       20/02/20
 142559              EXEC SQL                                                                                           20/01/20
 142560                   UPDATE BITREC                                                                                 20/01/20
 142561                   SET BITERR = "S"                                                                              20/01/20
 142562                   ,BITEFI = BITES2                                                                              27/01/20
 142563                   ,BITCFI = BITCO1                                                                              27/01/20
 142564                   WHERE BITCA2 = :CAS001                                                                        20/01/20
 142565                   AND   BITFE9 = :WFHASTACORR                                                                   20/01/20
 142566              END-EXEC                                                                                           20/02/20
 142700              MOVE SPACES TO LUGARERRO                                                                           20/02/20
 142701            END-IF.                                                                                              20/02/20
 142702        END-D010-REVISA-FILES-STATUS.                                                                            25/02/20
 142800        D010-EXIT.                                                                                               11/01/17
 142900             EXIT.                                                                                               11/01/17
 143000       *                                                                                                         25/02/17

      *>       INICIALIZACION DE VARIABLES
 143100        D020-INICIALIZA.                                                                                         25/02/17
 143200            MOVE ZEROS TO                                                                                        20/02/20
 143300            RY1PRO OF WRY1RCD01A RY1PRO OF WRY1RCD01B                                                            25/02/17
 143400            RY1PRO OF WRY1RCD01C RY1PRO OF WRY1RCD01D                                                            25/02/17
 143500            RY1PRO OF WRY1RCD01T                                                                                 25/02/17
 143600            RY1GAS OF WRY1RCD01A RY1GAS OF WRY1RCD01B                                                            25/02/17
 143700            RY1GAS OF WRY1RCD01C RY1GAS OF WRY1RCD01D                                                            25/02/17
 143800            RY1GAS OF WRY1RCD01T                                                                                 25/02/17
 143900            RY1GA1 OF WRY1RCD01A RY1GA1 OF WRY1RCD01B                                                            25/02/17
 144000            RY1GA1 OF WRY1RCD01C RY1GA1 OF WRY1RCD01D                                                            25/02/17
 144100            RY1GA1 OF WRY1RCD01T                                                                                 25/02/17
 144200            RY1AUM OF WRY1RCD01A RY1AUM OF WRY1RCD01B                                                            25/02/17
 144300            RY1AUM OF WRY1RCD01C RY1AUM OF WRY1RCD01D                                                            25/02/17
 144400            RY1AUM OF WRY1RCD01T                                                                                 25/02/17
 144500            RY1PR1 OF WRY1RCD01A RY1PR1 OF WRY1RCD01B                                                            25/02/17
 144600            RY1PR1 OF WRY1RCD01C RY1PR1 OF WRY1RCD01D                                                            25/02/17
 144700            RY1PR1 OF WRY1RCD01T                                                                                 25/02/17
 144800            RY1SAL OF WRY1RCD01A RY1SAL OF WRY1RCD01B                                                            25/02/17
 144900            RY1SAL OF WRY1RCD01C RY1SAL OF WRY1RCD01D                                                            25/02/17
 145000            RY1SAL OF WRY1RCD01T.                                                                                25/02/17
 145001        END-D020-INICIALIZA.                                                                                     25/02/20
 145100        D020-EXIT.                                                                                               25/02/17
 145200             EXIT.                                                                                               25/02/17
 145300       
      *>         INICIO GRABA BITACORA 
 145301        GRABA-BITACORA.                                                                                          17/01/20
 145302              ACCEPT FECHAMOMENTO    FROM DATE                                                                   20/02/20
 145303              ACCEPT HORACORR        FROM TIME                                                                   17/01/20
 145304              EXEC SQL                                                                                           17/01/20
 145305                INSERT INTO BITCAS VALUES                                                                        20/02/20
 145306                (:FECHAMOMENTO                                                                                   20/02/20
 145307                ,:HORACORR                                                                                       17/01/20
 145308                ," "                                                                                             17/01/20
 145309                ,:MENSAJE)                                                                                       17/01/20
 145310              END-EXEC.                                                                                          20/02/20
 145311        END-GRABA-BITACORA.     
      *>         FIN GRABA BITACORA  
 145312                                                                                                                 17/01/20

      *>        Inicia proceso de recuperación de datos 
 145313        RECUPERACIONDATOS.                                                                                       20/02/20
 145314            MOVE SPACES TO FE3-WS F06-WS F08-WS ANO-WS MES-WS USU-WS                                             20/02/20
 145315            MOVE ZEROS TO SALJUDIC-WS            
 
      *>         Obtiene los parámetros del archivo 
 145316            EXEC SQL                                                                                             17/01/20
 145317                SELECT BITF02                                                                                    20/01/20
 145318                    ,BITF03                                                                                      20/01/20
 145319                    ,BITF04                                                                                      20/01/20
 145320                    ,BITANN                                                                                      20/01/20
 145321                    ,BITMES                                                                                      20/01/20
 145322                    ,BITUS6                                                                                      20/01/20
 145323                    ,BITJUD                                                                                      14/08/20
 145324                INTO :FE3-WS                                                                                     20/02/20
 145325                    ,:F06-WS                                                                                     20/01/20
 145326                    ,:F08-WS                                                                                     20/01/20
 145327                    ,:ANO-WS                                                                                     20/01/20
 145328                    ,:MES-WS                                                                                     20/01/20
 145329                    ,:USU-WS                                                                                     20/01/20
 145330                    ,:SALJUDIC-WS                                                                                14/08/20
 145331                FROM BITREC                                                                                      17/01/20
 145332                WHERE BITFE9 = :WFHASTACORR                                                                      17/01/20
 145333                AND   BITCA2 = "PARAMETROS"                                                                      20/01/20
 145334            END-EXEC.   
      *>          Evalua si el sql retornó registros 
 145335            IF SQLSTATE(1:2) EQUAL "00"                                                                          17/01/20
 145336            OR SQLSTATE(1:2) EQUAL "01" THEN                                                                     17/01/20
 145337                EXEC SQL                                                                                         17/01/20
 145338                  UPDATE RY1PVH                                                                                  17/01/20
 145339                  SET RY1FE3 = :FE3-WS                                                                           17/01/20
 145340                  , RY1F06 = :F06-WS                                                                             17/01/20
 145341                  , RY1F08 = :F08-WS                                                                             17/01/20
 145342                  , RY1ANO = :ANO-WS                                                                             17/01/20
 145343                  , RY1ME1 = :MES-WS                                                                             20/01/20
 145344                  , RY1US4 = :USU-WS                                                                             20/01/20
 145345                END-EXEC                                                                                         20/02/20
 145346                IF SQLSTATE(1:2) EQUAL "00"                                                                      17/01/20
 145347                OR SQLSTATE(1:2) EQUAL "01" THEN                                                                 17/01/20
 145348                    EXEC SQL                                                                                     17/01/20
 145349                      DELETE FROM RY2PRV                                                                         17/01/20
 145350                      WHERE RY1FPV = :WFHASTACORR                                                                17/01/20
 145351                    END-EXEC                                                                                     20/02/20
 145352                    EXEC SQL                                                                                     20/01/20
 145353                      DELETE FROM RY2RCA                                                                         20/01/20
 145354                      WHERE RY1FMR = :WFHASTACORR                                                                20/01/20
 145355                    END-EXEC                                                                                     20/02/20
 145356                    EXEC SQL                                                                                     20/01/20
 145357                      DELETE FROM RY2RCD                                                                         20/01/20
 145358                      WHERE RY1F03 = :WFHASTACORR                                                                20/01/20
 145359                    END-EXEC                                                                                     20/02/20
 145360                    EXEC SQL                                                                                     20/01/20
 145361                      DELETE FROM RY1HVR                                                                         20/01/20
 145362                      WHERE RY1AOD = SUBSTR(:WFHASTACORR , 1 , 4 )                                               20/01/20
 145363                      AND RY1MES = SUBSTR(:WFHASTACORR , 5 , 2 )                                                 20/01/20
 145364                    END-EXEC                                                                                     20/02/20
 145365                    EXEC SQL                                                                                     17/01/20
 145366                      DECLARE CBITREC CURSOR FOR                                                                 17/01/20
 145367                      SELECT BITCA2, BITCO1, BITES1,BITES2                                                       17/01/20
 145368                      FROM BITREC                                                                                17/01/20
 145369                      WHERE BITFE9 = :WFHASTACORR                                                                17/01/20
 145370                    END-EXEC                                                                                     20/02/20
 145371                    EXEC SQL                                                                                     17/01/20
 145372                      OPEN CBITREC                                                                               17/01/20
 145373                    END-EXEC                                                                                     20/02/20
 145374                    MOVE "N" TO SALIR-BITREC                                                                     20/02/20
 145375                    PERFORM RECUPERACADACASO UNTIL SALIR-BITREC = "S"                                            17/01/20
 145376                    EXEC SQL                                                                                     17/01/20
 145377                      CLOSE CBITREC                                                                              17/01/20
 145378                    END-EXEC                                                                                     20/02/20
 145379                    EXEC SQL                                                                                     05/02/20
 145380                      DELETE FROM BITREC                                                                         05/02/20
 145381                    END-EXEC                                                                                     20/02/20
 145382                END-IF                                                                                           20/02/20
 145383            END-IF.                                                                                              20/02/20
 145384        END-RECUPACIONDATOS.
      *>         Fin de recuperación de datos         
  145385                                                                                                                 17/01/20
 145386        RECUPERACADACASO.                                                                                        17/01/20
 145387            EXEC SQL                                                                                             17/01/20
 145388                 FETCH NEXT FROM CBITREC INTO                                                                    20/02/20
 145389                 :BITCA2-WS,:BITCO1-WS,:BITES1-WS,:BITES2-WS                                                     17/01/20
 145390            END-EXEC.                                                                                            20/02/20
 145391            IF SQLSTATE(1:2) EQUAL "00"                                                                          17/01/20
 145392            OR SQLSTATE(1:2) EQUAL "01" THEN                                                                     17/01/20
 145393                EXEC SQL                                                                                         17/01/20
 145394                    UPDATE R09MAE59                                                                              21/01/20
 145395                    SET R09CON = :BITCO1-WS                                                                      17/01/20
 145396                    ,R09ESS = :BITES1-WS                                                                         17/01/20
 145397                    ,R09ESR = :BITES2-WS                                                                         20/01/20
 145398                    WHERE R09CAS = :BITCA2-WS                                                                    17/01/20
 145399                END-EXEC                                                                                         20/02/20
 145400            ELSE                                                                                                 17/01/20
 145401                MOVE "S" TO SALIR-BITREC                                                                         20/02/20
 145402            END-IF.                                                                                              20/02/20
 145403        END-RECUPERACADACASO.                                                                                    20/02/20
 145404                                                                                                                 20/01/20
 145405        SELECCION-CASOS-INTERES.                                                                                 20/01/20
 145428            EXEC SQL                                                                                             20/01/20
 145429                 OPEN CSELCASOS                                                                                  20/01/20
 145430            END-EXEC.                                                                                            20/02/20
 145431            IF SQLSTATE(1:2) IS EQUAL "00" OR                                                                    20/01/20
 145432               SQLSTATE(1:2) IS EQUAL "01" THEN                                                                  20/01/20
 145433                EXEC SQL                                                                                         20/01/20
 145434                    FETCH NEXT FROM CSELCASOS INTO :RR09MAE                                                      20/02/20
 145435                END-EXEC                                                                                         20/02/20
 145436                IF SQLSTATE(1:2) IS EQUAL "00" OR                                                                27/01/20
 145437                   SQLSTATE(1:2) IS EQUAL "01" THEN                                                              27/01/20
 145438                      PERFORM GRABA-BITREC                                                                       27/01/20
 145439                ELSE                                                                                             27/01/20
 145440                    DISPLAY "SQLSTATE OPEN CSELCASOS " SQLSTATE                                                  27/01/20
 145441                    UPON CONSOLE                                                                                 27/01/20
 145442                END-IF                                                                                           20/02/20
 145443            ELSE                                                                                                 27/01/20
 145444                DISPLAY "SQLSTATE OPEN CSELCASOS " SQLSTATE                                                      27/01/20
 145445                UPON CONSOLE                                                                                     27/01/20
 145446            END-IF.                                                                                              20/02/20
 145447        END-SELECCION-CASOS-INTERES.                                                                             20/02/20
 145455                    
      *>     Inicio de grabación en bitácora                                                                                             20/01/20
 145456        GRABA-BITREC.                                                                                            20/01/20
 145465            EXEC SQL                                                                                             20/02/20
 145466                INSERT INTO BITREC VALUES                                                                        20/02/20
 145467                     (:R09CAS-WS                                                                                 20/01/20
 145468                     ,:WFHASTACORR                                                                               20/01/20
 145469                     ,:R09CON-WS                                                                                 20/01/20
 145470                     ,:R09ESS-WS                                                                                 20/01/20
 145471                     ,:R09ESR-WS                                                                                 20/01/20
 145472                     ," "                                                                                        20/01/20
 145473                     ,"N"                                                                                        20/01/20
 145474                     ,:R09ACE-WS                                                                                 20/01/20
 145475                     ,:R09FEC-WS                                                                                 20/01/20
 145476                     ,"N"                                                                                        20/01/20
 145477                     ,:INDICA                                                                                    20/01/20
 145478                     ,:FE3-WS                                                                                    20/01/20
 145479                     ,:F06-WS                                                                                    20/01/20
 145480                     ,:F08-WS                                                                                    20/01/20
 145481                     ,:ANO-WS                                                                                    20/01/20
 145482                     ,:MES-WS                                                                                    20/01/20
 145483                     ,:FECHAMOMENTO                                                                              20/02/20
 145484                     ,:HORAMOMENTO                                                                               20/02/20
 145485                     ,:WUSUARIO                                                                                  27/01/20
 145486                     ,:R09ESR-WS                                                                                 27/01/20
 145487                     ,:R09CON-WS                                                                                 30/01/20
 145488                     ,:TOTALPRO-WS                                                                               20/02/20
 145489                     ,:TOTALPAJ-WS                                                                               20/02/20
 145490                     ,:TOTALGAS-WS                                                                               20/02/20
 145491                     ,:TOTALSAL-WS                                                                               20/02/20
 145492                     ,:SALDORCD                                                                                  23/07/20
 145493                     ,:SALJUDIC-WS)                                                                              14/08/20
 145494            END-EXEC.                                                                                            20/02/20
 145496        END-GRABA-BITREC.                       
      *>     Fin de grabación de bitácora                                                                 20/02/20
 *> 145506       *=================*                                                                                       13/10/15
 *> 145507       *- Fin de fuente -*                                                                                       13/10/15
 *> 145600       *=================*                                                                                       13/10/15
