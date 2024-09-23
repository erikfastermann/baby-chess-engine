pub const DEFAULT_DEPTH: usize = 6;
pub const MAX_DEPTH: usize = DEFAULT_DEPTH;
pub const MAX_DEPTH_QUIESCENCE: usize = MAX_DEPTH + 10;

// See https://www.chessprogramming.org/Simplified_Evaluation_Function

pub const SCORE_PAWN: i32 = 100;
pub const SCORE_KNIGHT: i32 = 320;
pub const SCORE_BISHOP: i32 = 330;
pub const SCORE_ROOK: i32 = 500;
pub const SCORE_QUEEN: i32 = 900;
pub const SCORE_KING: i32 = 20_000;

pub const SCORE_ATTACK_KING_WITH_QUEEN: i32 = -100;
pub const SCORE_ATTACK_KING_WITH_ROOK: i32 = -80;
pub const SCORE_ATTACK_KING_WITH_BISHOP: i32 = -65;
pub const SCORE_ATTACK_KING_WITH_KNIGHT: i32 = -50;
pub const SCORE_KING_PAWN_SHIELD: i32 = 50;

pub const SCORE_ACTIVITY_REACHABLE_FIELDS_QUEEN: i32 = 0;
pub const SCORE_ACTIVITY_REACHABLE_FIELDS_ROOK: i32 = 7;
pub const SCORE_ACTIVITY_REACHABLE_FIELDS_BISHOP: i32 = 7;
pub const SCORE_ACTIVITY_REACHABLE_FIELDS_KNIGHT: i32 = 10;

pub const SCORE_PAWNS_TABLE: [i32; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
     5,  5, 10, 25, 25, 10,  5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5, -5,-10,  0,  0,-10, -5,  5,
     5, 10, 10,-20,-20, 10, 10,  5,
     0,  0,  0,  0,  0,  0,  0,  0,
];

pub const SCORE_KNIGHTS_TABLE: [i32; 64] = [
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50,
];

pub const SCORE_BISHOPS_TABLE: [i32; 64] = [
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20,
];

pub const SCORE_ROOKS_TABLE: [i32; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10, 10, 10, 10, 10,  5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     0,  0,  0,  5,  5,  0,  0,  0,
];

pub const SCORE_QUEENS_TABLE: [i32; 64] = [
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20,
];

pub const SCORE_KING_TABLE_MIDDLE_GAME: [i32; 64] = [
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -10,-20,-20,-20,-20,-20,-20,-10,
     20, 20,  0,  0,  0,  0, 20, 20,
     20, 30, 10,  0,  0, 10, 30, 20,
];

pub const SCORE_KING_TABLE_END_GAME: [i32; 64] = [
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50,
];

// From https://www.chessprogramming.org/Zobrist_Hashing:
// - One number for each piece at each square
// - One number to indicate the side to move is black
// - Four numbers to indicate the castling rights
// - Eight numbers to indicate the file of a valid En passant square, if any
//
// Generated using a cryptographically secure pseudo-random number generator.

pub const HASHES_POSITIONS: [u64; 2 * 7 * 64] = [
    13237566174390517362,
    13665966216082543088,
    10160548395006952891,
    16064370787907262161,
    15939044584758884278,
    15600473714354298852,
    6878126707377514897,
    11039775722108352314,
    16179910448364211772,
    12681679841714161394,
    6722432795422395866,
    16079225358695899800,
    2585787325578058959,
    141150346010948683,
    17719076971469126693,
    7094831755997521613,
    14814249046955664500,
    5799867144268042359,
    7981418586659514930,
    8322387448226196333,
    14491449564115607847,
    2227336101874320262,
    8000342631695452905,
    14139589158896769364,
    15810479624602131781,
    5037244033617277563,
    7407662366957896783,
    15096106692688155035,
    3873865506422656570,
    14114004901265585390,
    14174126051846485545,
    2362545675201173636,
    17328623653178011046,
    18424706742826317155,
    17625757101851572275,
    166757899994181431,
    17961133755373372004,
    3455495687874817326,
    4227329511852832238,
    5610518373191108714,
    16967188925060879362,
    9557087480148196523,
    13003361860775041730,
    590235240999343690,
    4896033578470193020,
    8006539332082059288,
    18258004241073157048,
    11563226373557873030,
    7628721343370437548,
    15150581562427312551,
    17085418476504069614,
    8066354600888905362,
    7889419049113241516,
    4438572257155511873,
    16707266281697917715,
    12710252041334717796,
    5569743119201835840,
    16380871528615920527,
    5570926057773203311,
    6006228725534427884,
    4251792978602843546,
    3913560960369795713,
    2808805760688964867,
    10896338256687371556,
    7286092645814858857,
    4698540642611878394,
    11966403799520681370,
    18442023976478386965,
    15889165233038072884,
    13099273060177769240,
    3429888057808475479,
    10021702766852168130,
    7338632683871381047,
    13504512571828242096,
    1871724307634624168,
    4506294086910733840,
    6514060189829713761,
    530131539481603220,
    1335427415101260742,
    2389423732947465270,
    7436977190045473627,
    14428191297465663860,
    1163900797781611625,
    13709791411982321462,
    16158700863553989001,
    1092295361588694151,
    15716365424598267796,
    18114247899078178665,
    17676028977881448162,
    17796044301500228135,
    6448071101765531803,
    15125201992664068264,
    14631622238553205670,
    466659334466287250,
    13759827920180111914,
    10663348608769206709,
    7180719950227208557,
    14526120876955487473,
    14148798531148065732,
    9364265899527262254,
    9307739219469151129,
    5426658898264676967,
    11937319359560848593,
    7929601361633986653,
    15425142207014035253,
    8819173029240376419,
    21452628438380674,
    15995837625638704778,
    9908958987135070398,
    276081617117334285,
    7501556024352378311,
    11350108347594604501,
    18241230655470222881,
    9029178390615295142,
    1722658851094710611,
    17734871795998191472,
    689103838992613746,
    18082670569447562721,
    18259180614131829836,
    15914963202046304288,
    9209832634280515612,
    249596845518463131,
    179639223261796814,
    9734156521756918068,
    8988254084899115359,
    138764931404374711,
    7249521824959537198,
    16254984461888759594,
    13917961676155810283,
    13490086309113308591,
    1597979334982285229,
    16400077493106466012,
    14856566590639072254,
    15021121821467701235,
    13964163815319341507,
    10607808752002410825,
    1851558597415686010,
    17408157749939474372,
    10750566159363023271,
    5704831001507011024,
    16436582025325954671,
    13647470562799584724,
    6932617060807130652,
    4590722147778548402,
    1505203914730336502,
    14037889447624886019,
    3714598114885765249,
    16618225369028730497,
    14010840795147250280,
    7365987416685937416,
    12648134370661383595,
    17130673604802554861,
    3849171251956799407,
    12741557655018985440,
    12980327175519662583,
    10542253395226485036,
    8766603362856313928,
    6356203400053244137,
    4522610566136806889,
    18287488088069863513,
    9585954235095729488,
    12019087784022752724,
    11884210611474628522,
    5671707545977645718,
    6269728918033030546,
    13918008588977286223,
    8402778815949477710,
    403302990625080741,
    2985748737901635085,
    13217517665327056185,
    9920680074425881377,
    2234399241636164168,
    7813490479291427211,
    11387585045855408592,
    7464463093854858195,
    10630509024182456275,
    834991813865562342,
    8546063622929479620,
    12314152345037346040,
    9349315451674422759,
    15272088432800378595,
    5580255030345244725,
    6842135022676892133,
    5042668166754989981,
    6540068952319645524,
    15472614786418279552,
    4831707122201740344,
    7076032323062633907,
    10091052258755727527,
    15808865845619066340,
    8161750658504813417,
    17164803112003304353,
    2359885215467091698,
    5631749129305608839,
    15628993905245636816,
    6922971465995198568,
    14536365348724450658,
    11351557978232552991,
    14504520634230392285,
    363737429090494169,
    15375345032166576727,
    9389192499261879092,
    16194075369464639805,
    11935922438601870718,
    4597076976841126762,
    16001174405640308080,
    11917418693914670773,
    1337233844266946230,
    14639129881858991360,
    8543576412171007403,
    1358022411242896494,
    5436942548407019168,
    13591373134504726412,
    8070648661725543423,
    10387875076764384261,
    2858493299184838674,
    9587973330470868545,
    1212078182917693575,
    13814857690165206684,
    9734360343998564380,
    3439552968577795177,
    3207883510212119094,
    11882662640771676292,
    13411860708515962820,
    14529602072619010804,
    10630993190412893600,
    15020985302064153792,
    13574159316122296225,
    5156335753808664903,
    10178067706385179033,
    3509219727766675569,
    9578675196457696324,
    16117452760510821147,
    5058621118940615314,
    2377014785420171,
    7359541800925119449,
    9690531128008613591,
    16371885899699434542,
    16167248247623318790,
    794546012580626120,
    168536861412810586,
    10495571402072401722,
    13177466253094200152,
    8791536629866172919,
    7978183257138862570,
    976434216801310497,
    9310867662723652613,
    7435676011247265361,
    1990013259187103058,
    15352587313973946406,
    9226097520076851638,
    6530078310301818387,
    1210725318463687943,
    10907266869767856380,
    16951419247559329013,
    12033516080418911360,
    4355762808909517612,
    12302438693611716322,
    14552694071574783905,
    6053905764084571211,
    15108135694808693057,
    9215249336575799169,
    2298203523057076683,
    3619222580956333001,
    12056091002061545086,
    15922410798465986873,
    14577230696210405573,
    13686673934883886458,
    11259332153792663400,
    1521685719576161018,
    2503670528659842118,
    11042112893500101321,
    6340705158236973111,
    10023953713970507058,
    4053836008829118455,
    3108183785627586165,
    6555468763012844174,
    15053415200806571075,
    5522944572155224331,
    8292872414840910422,
    956013717129276503,
    4273264681262982700,
    15082163485951869111,
    2516914945543541733,
    17399040941525188532,
    2313878950890117378,
    262964725806290890,
    3725875553888767573,
    4897728941826217090,
    9594420431098323190,
    17177800858163972775,
    17420951446546406249,
    2698266312656423637,
    565945710726198279,
    16815822330147128042,
    11371181579390171324,
    2956091067423568943,
    12011221887867762027,
    15513408987147279500,
    5261869735872946910,
    2662281908580429750,
    257606348939388277,
    9135400605744007360,
    16192881952848267740,
    5430964008407781762,
    1695122122477524463,
    3674437419078765029,
    3802826774394048306,
    10536451152624050002,
    6340492805218797792,
    12762027340649252478,
    20832756465813430,
    13291851527151124159,
    13236744838169207319,
    12268429552427898278,
    16766126544936323373,
    8271173795164030860,
    4789349385213969317,
    13127985559249901942,
    16689942050903588575,
    4250607133692126216,
    1800884752617572387,
    12822484594010519907,
    1217769602384498555,
    1399566892493747921,
    275065763133751513,
    15890895053711655651,
    16766054845389805826,
    5318327722344612844,
    3400030310538396405,
    9468576041369401362,
    13633445397213595107,
    6374068163790316704,
    3638591078372725600,
    1980756515959479336,
    10835017776167200038,
    14122754434750161758,
    5755687764973513830,
    4589028647361146716,
    9985421018576483894,
    11928227098125873483,
    12306491779626492931,
    4386505696091991470,
    15074816453285623875,
    12384247697626838102,
    10714264545528507359,
    11024202086940616353,
    16494931445284978862,
    5014521190036447363,
    12541689783688012954,
    14778572412925019897,
    4168231957846765478,
    1191208423692342675,
    16894889406300734994,
    1088820554736352891,
    8080316484182110049,
    6021849084776101506,
    17345712372226638277,
    798600230857764914,
    4439911553534647394,
    13447719442077879332,
    2275277656194459313,
    10451911970843685730,
    3033556382583714863,
    4881610565599572963,
    4442478023549504430,
    17048248141345346154,
    4246990590995346225,
    17437836685454541407,
    6456960622643244526,
    9618797503688304880,
    3915580902853967630,
    8947146538376222054,
    14283908363915799330,
    6213409626612609979,
    16852280572680508633,
    3426151604138068354,
    5509554642398895181,
    12354469753487922853,
    11917182246310200191,
    6913575135135756298,
    17972864962207476426,
    15131447935711580233,
    8993845701885990377,
    17256644641951486316,
    12236295987046345390,
    15329212319687995712,
    8653797018768490456,
    16616964550123841005,
    9186672643135891400,
    1444790849258888605,
    4989318981844984347,
    11226496855845313516,
    6643687983721781839,
    13762260321113762377,
    17355002244822546162,
    15934395373531425617,
    1467468658171018610,
    6281832866795738801,
    2190058202173733732,
    12368268329407598081,
    9812068458960214985,
    9615721832604910206,
    14868662090931394455,
    10638845564954428465,
    5669059342949048847,
    14517204695276311807,
    17172322517477160170,
    8484220878635015450,
    11769749295159301806,
    2646330668323677033,
    453276457017908020,
    3566215195677471215,
    2587688178657962827,
    4090237967566157592,
    12401410423634825787,
    17167036771877986253,
    10654920854845453676,
    162110692407842766,
    14306359870158911328,
    6495658147978604404,
    10026191245585796907,
    10632303266306102690,
    4819153148214506837,
    15370742761902210593,
    6082533605262818396,
    11772100186969836612,
    1526838530763595715,
    13669509174833490339,
    3837429004944484535,
    5733550702969684995,
    2071281304703869522,
    14002760771599676767,
    3073751269457747188,
    13118752222553556891,
    7988052286516876879,
    1892240271798662129,
    11104404102110891130,
    12791824647447864909,
    4985122707327442959,
    10780502090991555047,
    10785094443634667467,
    8993193583201659243,
    14440661191089165742,
    7441948877453111184,
    3755931534664947192,
    6979904730895759475,
    5291877806634157251,
    16083075294756794989,
    16987799474283488023,
    8287817926112293084,
    13929700191263086762,
    5367180578917576324,
    14440383845059031970,
    3943836193965869537,
    2459928119109986351,
    4152598173003185136,
    4421050329263913678,
    1907675788609165388,
    11239029843902481330,
    16305812280587890925,
    3033380727982798027,
    10382206590226348519,
    207629959059054276,
    7331602713343969006,
    8024614580544860765,
    8908587245532399372,
    5231438975100430701,
    8908865108245977622,
    13164009679534465207,
    6231510377111346304,
    8577462288326432174,
    6538540065894489482,
    16929759694064910580,
    2385792263588695545,
    13693039779647301188,
    5178808688312928567,
    11901212266868057294,
    1885801059530036019,
    6505489832183678060,
    10789438512373698968,
    3673374804431525947,
    13291386467376859998,
    4718685782245796527,
    8597431546176742224,
    7674389880704175891,
    7317709812769479681,
    5837790581441553650,
    2624081150066047882,
    429230723650757216,
    8952660186136569311,
    9879950753703096341,
    11233390142662326303,
    10856725556257725144,
    4027132148157912310,
    1449075479259570029,
    12618842237031146917,
    9288151046490694866,
    16170381124505449514,
    17480365560663788818,
    17259599947311963033,
    2668225974329780002,
    3826405265502216581,
    6228322911330111238,
    15118162439231666409,
    1919548459253434978,
    14812048866681813242,
    7202733791457635750,
    10347614208691737160,
    11064792764877817078,
    5406277632637914402,
    10813745220755830253,
    4125453703011121498,
    2263838384658984433,
    11733669734136273469,
    8434313638137987146,
    16680677880866575135,
    10754740640221257485,
    6026249039137191957,
    2415942782464754135,
    10031544814458335164,
    8521880769362471394,
    3720744339024735300,
    15667151605960247856,
    650523469293507759,
    10766869524212551432,
    14730575404055057738,
    158266433658654304,
    6411348168286916356,
    18043156823746099392,
    2156697066090183433,
    16992488050636515948,
    6687772848954587994,
    8081593766277962237,
    6716539999149548116,
    15642317813376904224,
    4789340619625847967,
    7559093892433085995,
    13202070862734901982,
    277803317388194184,
    9782685220669752739,
    14466745358515703348,
    16252319786783701639,
    2628532542112007663,
    13670674034685178326,
    9102843956995337274,
    17920866978187551501,
    7817004057533389909,
    16580740781902355873,
    883186865832059867,
    14118677728223598334,
    16235873696213487871,
    15941855393871152798,
    5745932099248899652,
    18204492550601082345,
    9858379579397456640,
    16984630957939336195,
    12729846857428710961,
    4441987252345742913,
    8536962212938084247,
    14171750034167158157,
    2337113816951222388,
    13458008377062821763,
    10845509916675045056,
    10712235947633021862,
    549523699564937760,
    15219182420958294733,
    15634785449943098100,
    17862528942452353342,
    13378862733116899861,
    3661807481853946038,
    17338676569669254555,
    15097863217509650212,
    8216232158016191000,
    8537509769236689533,
    18392455568370341245,
    11274590993959855384,
    2517836163952232329,
    11431251460492362671,
    7568276541048276344,
    15092436411036478185,
    7656491728864099494,
    6650474847138543052,
    10011192220966134589,
    13476175772219100887,
    1383522032441515410,
    593256362882755546,
    12023703731724694754,
    3185470557124151506,
    7750712796692681252,
    3377321058580806810,
    9736197144927666325,
    8999444847522283578,
    2505877890601846060,
    11608269955029651186,
    2866158118850587822,
    7172441379119422524,
    8464827078428280709,
    3189145346186051818,
    8183386224369174519,
    12791174477676419104,
    621838402046356737,
    7452303033766608620,
    1525730268502238849,
    960908611492796883,
    8824358724136200237,
    5707530227665323114,
    4389093350294325530,
    17706124085285517025,
    9792081800106993513,
    10193755124087704048,
    12530168518861044579,
    4338300492603370527,
    9379118701958340308,
    13732299976684729960,
    14240685070474665909,
    3552621470560778424,
    13204692886132045512,
    13128984529685886979,
    8943025290000069385,
    2880121779140081577,
    7023665261774057946,
    954368405289989014,
    1209364135389586031,
    8650247324899770464,
    16256586318855011272,
    3087217350157691838,
    9606421003600709382,
    9777429122845239133,
    962405729351983201,
    14533565687164346298,
    17944745172396797550,
    2713346133723913318,
    8357342466904641909,
    13215995803505003699,
    2893875437758092192,
    13550254210122503370,
    5570995004751899554,
    8973296960961440291,
    11708927148104101459,
    10618742887547126921,
    10120958130576802608,
    4632820802966331330,
    12654530755087350974,
    7596144326368898558,
    15946684701873830781,
    5019247079020866136,
    1209041960488086955,
    7565876532394919196,
    6324571343458682374,
    4728552745701899145,
    1859247743448893344,
    9899789428801640143,
    16570597581165321220,
    17423307390702869374,
    159256301115423194,
    12959470139768381496,
    7056176068229396870,
    993948398307196727,
    10765302563960834158,
    15883262868551828065,
    9272749260032088534,
    2395857520824111082,
    3777395220684962096,
    17943459263706197422,
    3565894203433810146,
    10850683562491240160,
    6770657632763126878,
    14323558774467264453,
    1735711195481130242,
    16942960116818888354,
    5531579608773711695,
    2527225700842172062,
    14417839856579919676,
    3522737136962611040,
    3983367453267028687,
    12634310001063276868,
    8018591091523710047,
    4416037612223527139,
    15477452156669391928,
    5492622302440110720,
    11569816710762990964,
    8210825986780300981,
    12471435238564468176,
    9882582276250332048,
    11296274223675172985,
    16122215654023033323,
    15256210911505832676,
    8335990602717786575,
    6390072981535673313,
    16051709295270363427,
    16482659433553734026,
    2614227364360165939,
    4141814650760493746,
    12758718901032102804,
    13414345393300020997,
    8763012676419103008,
    17004161955613285241,
    15759555805419566644,
    17978001052374193868,
    10075187276160529314,
    8695832337384195951,
    7308207550201731737,
    5581229321520479696,
    4340366163942324018,
    2247580658141172758,
    5667078061382195577,
    257119662761424116,
    5882676575995326711,
    12252043924474578366,
    18381656251694823937,
    14864494728864033546,
    13294937420405260811,
    9607450712931176602,
    17919865724771083467,
    17781401049346962496,
    10437038609081749149,
    16410941027311006907,
    3016373494298487522,
    15433283746484329583,
    11502377166370272451,
    18359961231447328694,
    483171687441550606,
    6460603876235887991,
    3017502938185318072,
    6091785949358759315,
    9565112214885901261,
    18000077372653777688,
    1542623072911943534,
    12468820584327807801,
    17499262060754913430,
    11279419018338990190,
    5617533328325265736,
    11187096078794719704,
    13323332107558309221,
    12784432405469216661,
    4063783670303455162,
    10413424861379288162,
    6336779711950028565,
    13938703147129907107,
    4261251747460083548,
    3679849208015668885,
    12718928145372511660,
    17157140488125300395,
    8272112204196772377,
    14010583329828158963,
    10190264933048503242,
    12631217800109246317,
    9213828955905409192,
    12479104441466274411,
    8396576199307111067,
    6471741991685363331,
    12753347099186768560,
    1652497227323300804,
    8320542534248836072,
    15499632213735701320,
    14250415673357351417,
    500879024693738728,
    15382262775288040512,
    11998740416873238108,
    9717158869250673372,
    9212213262714414442,
    12263001401861277042,
    13204900175861387193,
    10530565678576772434,
    17649115950631638448,
    10683219122657090040,
    18005377364947907968,
    12990144458980123989,
    6500192374498128093,
    6761186657778088780,
    4858060052873622994,
    13679646575237475340,
    14609091408317080544,
    14779379897749717246,
    6954680820077547482,
    9770097169421176539,
    2708185487051498356,
    12389162448879313753,
    17305190794161457959,
    17482714800695700629,
    8290142263106957258,
    6216313091319055978,
    10422232318037188635,
    536571491854643845,
    10863826746944885720,
    11797311382417733666,
    8393007866656806637,
    3441152076977217,
    2276119865455625503,
    17465405821721352595,
    3362554202136385351,
    15806086277048772497,
    10370804343813276799,
    13315985407558029627,
    9567937083045559309,
    37963774178976479,
    16027245426015038438,
    4323506307955935228,
    4959141881215904803,
    7281049278217034269,
    2510952726357691176,
    1329841786183386023,
    6233074254548660662,
    17335190557958974986,
    8976623962682554553,
    8407445236668652027,
    3783300420476683724,
    16563953755625960312,
    2555344040827801899,
    1521333802168602271,
    1510637165414314094,
    17210741737030976616,
    15035069205323286697,
    6975905541903935668,
    10474988407038806707,
    3567995365189053780,
    13472959299848966004,
    17620339817306839301,
    9914647862943849287,
    7513272488549526079,
    8720367361129550437,
    9437322062195950715,
    1550478726444093437,
    17170201410454084196,
    12561180248644537021,
    5570562610042896299,
    8297437190959391218,
    17982470077297612750,
    8106844699651192313,
    14841993794196608388,
    9542845800482367804,
    8060485829545156731,
    1806913222060799381,
    10775400632372232150,
    9429843253511153654,
    13764051202048271432,
    9326130319057413293,
    17734738190469311173,
    510777301195499239,
    14854147144936075658,
    14025956550701258970,
    16872470415253641477,
    6593342044304388724,
    7274328379038679646,
    18446682380335018650,
    18190528929656510711,
    213281704177628218,
    5241433724153135298,
    13377018555358091098,
    5417503782094409903,
    3358699606002387689,
    10463094821537015915,
    14780879101180843706,
    14995071890376461218,
    6657899103712927653,
    1236648571093489948,
    4299053168738299986,
    1890512274669695890,
    15605597180710664911,
    4237611347279872195,
    9468716452327316962,
    16490185181912310045,
    778020385026907115,
    5821381338939051283,
    14481050703112311821,
    16998086683694361426,
    2217865393140109277,
    12975015384443710467,
    10256769707862389128,
    8570727896624121502,
    15894023196474931608,
    1822585997661914329,
    11189785389784624284,
    13207690739489858432,
    9998463823297374388,
    6396194596507311540,
    1591412754351427959,
    14746163457808199363,
    9191437390240993315,
    12573261121446262289,
    16392188740526865789,
    7549721022317694504,
    18229103015472857819,
    8216834837344820848,
    12850417226341341262,
    11454925092093848064,
    11591310092693728053,
    17784139021655641478,
    14962761257677645062,
    2537314437318119711,
    14266053382219064665,
    13641104659538501716,
    16096051263754453325,
    15793338602260739082,
    10148063377930111205,
];

pub const HASH_BLACK_TO_MOVE: u64 = 7891286395491852702;

pub const HASHES_CASTLE_RIGHTS: [u64; 4] = [
    15637651440526163078,
    16230945548427154178,
    17923647804262039189,
    9194121856959632388,
];

pub const HASHES_EN_PASSANT_FILES: [u64; 8] = [
    18056652242650731501,
    4898486501878801726,
    13127302360491488160,
    11482965074368694031,
    3007357789413413394,
    3943368472309828516,
    12171536940464735720,
    219332632339533906,
];
