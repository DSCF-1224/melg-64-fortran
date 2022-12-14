module melg44497_64

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: type_melg44497_64



    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1          =  95
    integer , parameter :: PARAM_P              =  17
    integer , parameter :: PARAM_SHIFT_1        =   6
    integer , parameter :: PARAM_SHIFT_LUNG_NEG =  37
    integer , parameter :: PARAM_SHIFT_LUNG_POS =  14
    integer , parameter :: PARAM_SHIFT_MM       = 373
    integer , parameter :: PARAM_STATE_SIZE     = 695

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1_OVER = PARAM_STATE_SIZE - PARAM_LAG_1

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_1   = transfer( source= Z'06FBBEE29AAEFD91', mold= 0_INT64 )
    integer(INT64) , parameter :: PARAM_MATRIX_A = transfer( source= Z'4FA9CA36F293C9A9', mold= 0_INT64 )

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_U = shiftl( -1_INT64, PARAM_W - PARAM_P )
    integer(INT64) , parameter :: PARAM_MASK_L = not(PARAM_MASK_U)

    ! constant(s) for this MODULE
    character( len = * ) , parameter :: JUMP_STRING &!
        =  "a31871495c2f64e9655938d42afec1430ebcc71124b13d530790a3dee960d1ee0bb6cc00644378319e36de4ede4065a9f5e8" &!
        // "eae02572ed6f250e22636e39ff9a27fc7090d590afc519f3ea8d3acf0596c3fef37c1076e40125cb16732423af58b3c45b92" &!
        // "3d7ef14d0cab20f1f08f7e6c8d5fdbea98239a80ec69a429bcba74d3268fbe79c67717fdd4d9a854f88ff882d6028cd228a6" &!
        // "6908bf615a2a04c9d3f9c8dae5cf7e9e41f650fd610f45276c45607580b95041d9f8aa66fcf951d3dc7d5728d36eb186d875" &!
        // "5e1cc29818496442eeb2d702f4efeb3a2c8be9dfdf996c0bb45ff97e14f4fa3a9c53a08c452afc3bfcc41c5257957ad1c01f" &!
        // "17cc80fd623d685bb8936f388266c0117f8a5a196488ebc18f1ad252556625a6f710757a1db096b281084a672e093f6c84cf" &!
        // "29e3ba58044eed0c8374f8e09126b9080e10d8c6e33d66339b60ece9dc27df759e51ed311aebec77b83b47ac172919958163" &!
        // "b863203fc60f018b9f36d227489bb2a8c46f3653a8143d08bc9a96a6acd387c1b9e4478b8ffdbcf551982746f2bf021c6c2c" &!
        // "83662de7eccf542d6daf0d997f871e0b6745d31abbf219b036466d9412736c5181639452b9d5d6290b2416317e4125e5739d" &!
        // "d1860d15109f1cc7bca0fcc048a3d0097ba1577138e100c1129692c77d7a09a01c07b9e15513d6679e615e481932fb4a0a17" &!
        // "98b0236610fd0cf9caa571b6e0c180f68532d42384599eca391aa75c9f3e8b9c46012ee53a0c766da2b9fec1703ac90975ae" &!
        // "78f27497f9d01afc7f8de438c485e3dc316acb83999ef522f7dcd99d48a0450d443aa4b305258fb1c8859de56b017eb587ad" &!
        // "f1b0642186179310dbc949c660ddb5560beacad9a28e550907ca4850b0db0b1521c2ea13d4ff184d387d51d19b82210815a5" &!
        // "824a9bedb8b67804ba097d2bfe4012cec291b48260c15c95ec969b83546c0dcc4b2b435f1c7b6fd1b03ad258d34c5b22c30d" &!
        // "0ba88144f844d9928258b784c416f9594acdd17d71f8f07164dc0aa457e751b35813ab6a83abd114c1f938d5b4f839354248" &!
        // "6e665e9946022e781648ea82cc08133ad2e71578dbc1150e52d0338bbf4c7297559450c16c6420995789aca0f16cd2eed150" &!
        // "5a5896f9ed840e9646e36ac47019ccf297697d2d26aa186aa9fa09dbd106aa1796fad677db1360a784d05d13c06b4cfe445a" &!
        // "6c0aa463bccf73be5f6f609e9876d940230938423f7abc6514d51cb17e645e40a4f685680651eab70a8c17af64e43c6486db" &!
        // "9e411dc5f912c44e6bbaeecadc39e4f805f177b642e6a1e029e7a15acaa2f5d71dc2aa1d9775b6c51ef4293353a10145d0d3" &!
        // "f2d8485863484f0a1a48c28f7d56ca311343fb2e18550fd4e67c77da467784acfff8652f959b481a2a4918b8aabf9c0176ee" &!
        // "14e2c6b7279b810a1696050365bbf1f7cd023bb98caad2d0f69cc3ac04d1921c1007eee16d6d9188352580f2f8dee387aecc" &!
        // "1abadd739d932ef5b945ee57532aeff57bdc1e492d261ef2bf7c467c142eee905c8dbe5f452f51839e66fd793575c3b4fe74" &!
        // "8b205662d4e2aaa63f857f8e42e80e79b64bf86dbbf818aad66b3c8fc1bc404a83eb155a32a4a16049795f46433801d59a78" &!
        // "f3a137aa12c698ee2fa24331fd10e990bde99e077fdb9d4e9736ec513ebb73c6c6b67e3c20129b83d07247307c3d58ba43d3" &!
        // "e46d0e6af239f3ee2f0a35b99f8da0a3c6a57d0ea0ce7428bab28fbd061d44c8f38e25291ff9c71f63dfacceae0321e63527" &!
        // "3a22d0888387ebad489088d7efe0463592002545654694b875dd37d73c09037ed729d8b7dc9d228ac76701633fbb09fe7151" &!
        // "de513b1b1b698c05a526dd4112aa81cd372790daaaef1e3abd242d00c0508e4803f669cd59d771fc502e17167369cf408e97" &!
        // "f1df4ac3ea9287170745297e6aae5f2437b07097250d3c2e23800b0fc3ce36ee288502ff035b5247c10a83e82b6fba5da027" &!
        // "a5a67b8958e44546da9271b7c6cc2fadfb2bd979ac9d4859b4d3a4c9209e6166d0b76f501fa3d92fbd750501e29c1df9aa8c" &!
        // "3848282c80fb86a53cf53638093e0ffb29c8b38e84d533300b6845d5f8ef2936e86b654ee2006681ee23333861790bd8c75b" &!
        // "7a4ccdd4f4ec9f671e7fece5c732ec02e80d4a7526b29904b6cf48ad38106d69f18b001fe17f6c976239019116e1d292fd21" &!
        // "b954298e30567b78b322b68d467c6c9372a23ecc62b9b2b75fcdbf47bcbc5e5d9ce305bdc9e06c1cdf01969289003e61e242" &!
        // "3a0e51322ee25110520552f52c17eea231b9c7f01c3eff85c1619ff825c3b7d6b8eab7b4adb82e33a88f7e9e2699d28ec6ce" &!
        // "48de61dd043c02105831cf9e40f1b0f04f5262492d59a37c8f30fa4edd4c668e7db75ac9d9a244b08b5b5ade036b642f940a" &!
        // "47519170d42e8b8ca1a79c8b35105f2b4361578c724807b9384a185cde9e79e5257fd9c5d8e808f4559413b6cc8a130bc0d6" &!
        // "0f7ef58fb4c0c8201ed9e286b89df658c69c732f8cea9f0efa77e3c458d0dbdded1f946602e4b7c73839ac242bf8fca0a195" &!
        // "66e8d077db36b7a899810372f66700b3a655065c2f972b437f7c0c125133fb1d7aafe13a630fbeda24e7ff5691e1534bfdbd" &!
        // "d20903ade7422d0c078d3aea60460ec7a0ba89c5bc9cff2575af6de87f73b3cc77871f3d8c68b8e7d3b94c071cb0e56884ec" &!
        // "bdde7f207ab4a2963dc6e386e26d8f2aa94801911afecdffbc2ca4591c47c9bdbceadc98275ea91ea0a9a3f1985a77c15ebd" &!
        // "fd48050c321dcf4b1fb5296352279dcd83031e8451e83e9c5362711270026af1efec3d123504b3fe2f3ee5f162098e9b43b3" &!
        // "ef408b4f838cc0cda5241400daca6b2001008eb451c85b8bcb29bbd4aecdd3041cb8f8037d559dce46f79b902998a1a1e333" &!
        // "f389ed0d52c2c8d31e84d0b41f26d8785e473ddf758b4cf3acb7d83878107699582771276c122271a71b5220d0cf68f5a941" &!
        // "c768a1d404a84f5a103e0a8da215ccf05ccccc96fba492de5d6b11a62191275decf97723285132f3c19b7809cfc659b338d6" &!
        // "5dc3f2f1950d45b213231c89d811e554174cf2d7aad6be703872c7f1c02faf6267bae046a1293a53d827a1bf0278272be89d" &!
        // "fa1fa85c7f6332865400b6873479d77c0d3c68fd242506c20efdfce8a70a1d0f16d4f1a12b1dd0ab33778deaa3d7a168a371" &!
        // "6ee09a6dc4b1d19c00423732d43a22ad978a9bbd2cf6c5c985a6666f7c3e2881013836d9280ecc3494bd0232629e608920ac" &!
        // "c306ddb61da9b82ae8a3f0a1dd26de008f7f27704451a8d0dce4016689ab6e066f6cbcb58ec4eb1b7d46761f510d3292f16d" &!
        // "e8f07f513d7070c6785b3752a635ce48b81ffae016a145236f8c1d5c2fb94f10e4094bf9ef576e8974dd06d4ec5905f15086" &!
        // "2a40c79ac15f0718c86cf61b3855d39614d940e4954bb61835b63ebd1f378f7ff4fe243800ecd1a98c2dc74438e618207f62" &!
        // "ee0bf82c27b563de1b396c794868406645b13dfffd2eef8e4edcd616427bdb88cf72225ca635b46e75cf7cca446440c8e490" &!
        // "e7e742b7096c9c7177005d3bb8ece97806e21c55d5cf1d23a032c3e86e6e22d2170c34d99b012d8551b6cbf565ac110d99ff" &!
        // "eca2d84404fb855e0df4094fc04998ca30eff10ed73a67d810ef540b659bb08c3307514389c6b3c89d32746c08d24a44f815" &!
        // "28cd6701716cd5c52ff207aacfaca17cbcaa42d079c70cb2446514a94cae89f18d5ed19963d6d4a65fb3490011362c5ee371" &!
        // "14062c0cdf957f67818d8afd34f258d858e0531f2cc0514ead0d9a58e282bedf2722fa0f3ab3253d751e0f3adb5cedf67b1e" &!
        // "865684708989162bad3ea168513df54976e41bd382f989f37865fbfb86dbdfdb04de5466c2b9ad5a54036f8fc0bb7336bfdf" &!
        // "0bac117e40b2a083e68b1d4c6dc062c353e602db6d926437ee88813b554a3ca6f22436bd8f7577fbe99b795ea3ea81fd0852" &!
        // "859052677983a2faedeb66bfbfa46b58fdaa294d6d67beff2c9d11d29c9a9e9d4e97b1c1307735b46bf04dcd2c7590e0df73" &!
        // "77e0b738cc3e83519d1451b361ce805d85cd15a8e075f8b940424bc61dc9d2da15718c1be1e9ae618fc0d954592ebecddce0" &!
        // "21114cfadf8ba8093d282fc9126d993f310d75d3c2430a9c06820a43cd69130e7d0e3d29d5bd3d9f974e765d919db72e99ae" &!
        // "a45c98fe0abd15da1a50b549e5ad2d19f4db2a74079ded2173d66d4c696d9f5d1f72e2f98a0a5f7f9bec9a53e68e5083c154" &!
        // "a5afa1705fbdae6cce3d995ad541cd9135ada17ebc81249091fa24aa760e783bf38e52aee5802f25973dabad3b4162d5acfd" &!
        // "c7d870c04a551f6cff0d6a9541005ebcd07b390312fbd948dea8bc65deba9804758330dc7b149d01fe9032ce1b97b4d6d501" &!
        // "d31fc6e2cb0abfac8b4d457081ffa614d3c149914bb28199b68858d7bce5ffcdacbdc4a142fef87501946c63b1c750fb9d69" &!
        // "93012e7eab85bc3083056c26dcbf8d6c92ddb23d9718378816004e218544f35eb5bb7dee0e396060e49c61897cce64ce419b" &!
        // "e0d6639ace204bbedbcf0835bc713fb17b4cd521ad378417b29074d28c1e75f0b5378b303256f08dad97b2c5584ad5f5217b" &!
        // "eeeedb69e1813ee62891417c166f0ad7326be32925d573087a9f9f593feba6bf08dfbc4d4abd0783c05dee281255e7da959e" &!
        // "a5d3d3c62b8a62ef397490d8da2c35c708ec7136905e52049cc408e1f82c299865406b2da8a72e7759099711518bc80f4d22" &!
        // "27c6447abc91a4fdc6e97a578e4d70d2e48508f18eb03dba323f4b2c02bc24bde1572c9ee8997c91accd1331b3b22147470d" &!
        // "6587a7c343704f7fbb3048b70b7f6e96b4956b69ce4a425be485548a1a99e2fed32d639776a5dc680802c871bef0c88e508c" &!
        // "92390b96c5d7ebeb3d4d40a0c2a4b606ab8ea19d0b6009e59d3e90d731b100747c38250c7df11fe3cf693a46b88c27b93340" &!
        // "0014a842ce5924ba9354343dc64306865fbede5c7650a192f4705eaf59bb80627abdfc70b0d1e6941312fe96eb9eb5bb9704" &!
        // "a268dd4330e35348341001454a96e2a534261d1f4e88d0ba540ff9818c2a26dc847335b0e831963de8ee5342aecccdbea1f9" &!
        // "85b7445c123948835eb75705231fc485c02abbbe592b0c33f5257ee940742e92e43d3d901d6166de063ad9199ddb4da66955" &!
        // "f1f7b4261a653452929c138465b6deb67c189ab8f27271ad3fac1e82ee43394d38a920a1cb102014df9841e16c5a939478fc" &!
        // "cf09dd6639a068c0dcae523c5a25eb316a03b1d6d5872b79387694778f47e7341ccda18384a60866b2302023fc4e18179570" &!
        // "ea41ff437d0b196153c5d82c5dee1b00a21284e90070f80aaaf6db25bea0711902528e1455c5ef810893857c25231dd0bb95" &!
        // "b6d6a32d7501bd185f590f7211d5e70e5051f866ac33c846253cf2449de94db274dde042aa436395bce16340bf94cd7da67b" &!
        // "0bb63f61603c55a4811262936d2d6f338ee4af952beb557fc4ba052484fc95ec5e1d2c8d0fcffb93b65c138835363740234a" &!
        // "641a1464231b23a105d65a16b40e73178b0476fd5998e52f27b6bdd08e971e4495593366a033b69e7094762de5e681980acb" &!
        // "7cb8ec242b37701460ab017d99048b016b70f35579954a74a4cb01a102b5d2e160f8308781408ee92c7dbe2b3ae769f3a178" &!
        // "b89163c96e533a99c5110e8ec37835393a218b2ca23ac7644ed37b747545c61882397ce603e6026ad0bef2fa946cda0d2a59" &!
        // "a3e8afd8ece6dbd974b29c1cd98ec86ce98cf1f6db790bdcda3d5430f32dd00669bed51a5bac10b8ce64c38a385a82f17d6e" &!
        // "496e48cb251942ffb5ae5dd998398157c098c9b04d9af00027ca17b70d46f3390bbf0ab84702c98167ef3750363a336eccc4" &!
        // "00632ced942feab19c7c679d7a28638765ca961a791edf9670d02503a11958f566a231ce8f5cc5b8f7d335f3257e810e4f41" &!
        // "3e1f305bbafa701a392da105d4c315ecdfe9ec45a8ed2fec77ed24be9b2149552ea90803f09cc7fa0a7c798baaadf9b1d7ca" &!
        // "b44f4d1dc5a3cec9a20ce2ed4f67b0dc0d6fd8aaf1608116d037feba862a1986dd3aa9a1307fd6993a43af8db68e170c1326" &!
        // "6c3a40be8c9fe40df9d813ee3eb0e73e027ba7b3e2ad555d5db542f2841b4aba58fd8cae4768225e039d46393b069c8c1a4f" &!
        // "b6b8d109d17059eb790b2e1f88c2419836f372e80faff2d7b669381470b5622fa11f98343d879b1046a972cd2e7334179def" &!
        // "c1922942f1cd3fab0091693af023024bf49f82f74f3b06ff4736062a83c63c51f8c8ffd4ed49cc0611d04c013d3f54eab2a3" &!
        // "5ff997c0214973b1de90e85389d59bc4c8cb1f8c16b5731952f2f5054e92487a138ef6bf60982b3b272984f0da2af7a547b9" &!
        // "2aecb993540c889f02e5e7a5530d8361c507e498ab152b3b7356ae97cddcb67c17817df63c352ceb88c00460c6a4b9bf1bd2" &!
        // "e5c95d10350e2f16bc7141815ae90c75895a951bcf64696f053fc2bca511a9a624c8b67272724ee71a1247a51eb49d3b08ea" &!
        // "9b108b751f99141c5cee292a22a9e5e40d9640bbebdfcece68d563df2a57c8404ae12f579bfa069b648be292581f0ea37a30" &!
        // "34df97734bc9dc9814b18a74149d860531365273a1d60e7fe5d7d4a87e00de305e1d0089c3c9f347cfbb711532cd6e397ad3" &!
        // "5dc60e2bd83145edc01bcc2f06443f51b5039a6315eb9c7f209ae926c842caa8ad18d6a3bd62ac4d6f65153e52460405e492" &!
        // "a1a866c0f5bdfeadda2c00efb3043158d2c6e1be58e939a77e94d1a9b16676141cf4657d8039cb668b939dfe63c67f6c21b1" &!
        // "0424ccf80edee01ce02ccaa95c60edac675e08c86a09ee887cfb725f855ebbfe65c0204a4c15e9de808d047ada80923a17a9" &!
        // "fbb3c69f9ebb7e6dc6efcac9fe2983d6b1c4d5bb82319cb1e65c1a0358f94f12111a0ef891cc7af7b5965bb772951286aac3" &!
        // "8d11b3b454d0f153fcfaa03f8526108eff7cf0cc77b308d44c552777713766f8ceec116e37714eb054f3191b6298be3ac743" &!
        // "edcd071b23a680747a013c93ff92e12396318a6b56bde3c7136073dcee1dd8e7af29e9d6540c714c46305e857e41bd078b8e" &!
        // "5ff3d609ee5a8d36ba9ac2cf58ce62da84d779ae0f699eff6a5a770e92088b2f65ee52347d91f493b79ffe3c6f76b999f32b" &!
        // "122b12dca81a84bcb80cc009806c77c4ceb6b20cefeee1f64bd8ead5fc1591b325683785ac44b6c49aa10b483c69bf998a21" &!
        // "efa40af7aae227a88cb040e4806ad29ea7683318687462fa1952c697ce9373bba2c3f4c6dc683fb52a7c61c9b2ec87246335" &!
        // "d677f7d155843279e46f56cd13bed457b40771f964aa245cfde7f0dcb9c58ed0aec35119f363ee526ca806ed404f3425e0fa" &!
        // "c7da6606958c9f156516893da9df4fb4c7673f96daf9e6c990aaf09ca48c42c91b7d7753f9f2f278d65f578e9ecec1f52b65" &!
        // "e9c5c6977e2a04434742ff8ce6873e225555da21a7902b60b25d4352978050866e6f572eb00519944e68b16d60abf4a2d049" &!
        // "980a2d7d97c103d1ab589d619b90fbd7210a99b9a82b0c32e4e01b8a0099e762c5676e68419187468fceb8aedf7e1fc0cfc7" &!
        // "1a1212ee54d1177e8d61e9ea391421371b57fc522c256e7f5704909f7a677ad8c349f19198a0d1e343f8fddf40e0853e71f2" &!
        // "07d5a9ca9f6a882ea8a67d39b7c4237fb1f7f3f9c644d5e468df85ed5de9373abcc8a7fcb4b91208b5d80c5e3305351e120e" &!
        // "0166f18cc179bab703e1ef0d9a3f1edd3e8a4fa1f8336abcc1899a4d2f67d9a3b3d8507d2d5dd9845651d528a5292936b62c" &!
        // "49c60396454bd2019419a1f3693c781a7628038f05735ae85998ec14f2b72d89c94ed01fbf5fca09afa7788be289c32cae4e" &!
        // "217511d3067b7d9e0c27e0110"


    ! declaration of a user-defined TYPE
    type , extends(type_melg64_abstract) :: type_melg44497_64

        contains

        ! kind: FUNCTION
        procedure , nopass , private :: lag_1          => lag_1_melg44497_64
        procedure , nopass , private :: lag_1_over     => lag_1_over_melg44497_64
        procedure , nopass , private :: mask_1         => mask_1_melg44497_64
        procedure , nopass , private :: mask_l         => mask_l_melg44497_64
        procedure , nopass , private :: mask_u         => mask_u_melg44497_64
        procedure , nopass , private :: matrix_a       => matrix_a_melg44497_64
        procedure , nopass , private :: shift_1        => shift_1_melg44497_64
        procedure , nopass , private :: shift_lung_neg => shift_lung_neg_melg44497_64
        procedure , nopass , private :: shift_lung_pos => shift_lung_pos_melg44497_64
        procedure , nopass , private :: shift_mm       => shift_mm_melg44497_64
        procedure , nopass , private :: state_size     => state_size_melg44497_64

        ! kind: SUBROUTINE
        procedure , pass , public :: jump => jump_melg44497_64

    end type type_melg44497_64



    contains



    integer pure function lag_1_melg44497_64 () result(lag_1)
        lag_1 = PARAM_LAG_1
    end function



    integer pure function lag_1_over_melg44497_64 () result(lag_1_over)
        lag_1_over = PARAM_LAG_1_OVER
    end function



    integer(INT64) pure function mask_1_melg44497_64 () result(mask_1)
        mask_1 = PARAM_MASK_1
    end function



    integer(INT64) pure function mask_l_melg44497_64 () result(mask_l)
        mask_l = PARAM_MASK_L
    end function



    integer(INT64) pure function mask_u_melg44497_64 () result(mask_u)
        mask_u = PARAM_MASK_U
    end function



    integer(INT64) pure function matrix_a_melg44497_64 () result(matrix_a)
        matrix_a = PARAM_MATRIX_A
    end function



    integer pure function shift_1_melg44497_64 () result(shift_1)
        shift_1 = PARAM_SHIFT_1
    end function



    integer pure function shift_lung_neg_melg44497_64 () result(shift_lung_neg)
        shift_lung_neg = PARAM_SHIFT_LUNG_NEG
    end function



    integer pure function shift_lung_pos_melg44497_64 () result(shift_lung_pos)
        shift_lung_pos = PARAM_SHIFT_LUNG_POS
    end function



    integer pure function shift_mm_melg44497_64 () result(shift_mm)
        shift_mm = PARAM_SHIFT_MM
    end function



    integer pure function state_size_melg44497_64 () result(state_size)
        state_size = PARAM_STATE_SIZE
    end function



    subroutine jump_melg44497_64 ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg44497_64) , intent(inout) :: melg64

        ! variable(s) for this SUBROUTINE
        integer :: itr

        ! variable(s) for this SUBROUTINE
        type(type_melg44497_64) :: melg64_init

        ! STEP.01
        ! initialize `melg64_init`
        call melg64_init%initialize_for_jump( melg64 )

        ! STEP.02
        do itr = 1, len(jump_string)
            call melg64%jump_unit( melg64_init, JUMP_STRING(itr:itr) )
        end do

        ! STEP.03
        ! update the new initial state
        call melg64%copy_from(melg64_init)

    end subroutine

end module
