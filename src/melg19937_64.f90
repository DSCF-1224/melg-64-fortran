module melg19937_64

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: type_melg19937_64



    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1          =  19
    integer , parameter :: PARAM_P              =  33
    integer , parameter :: PARAM_SHIFT_1        =  16
    integer , parameter :: PARAM_SHIFT_LUNG_NEG =  23
    integer , parameter :: PARAM_SHIFT_LUNG_POS =  33
    integer , parameter :: PARAM_SHIFT_MM       =  81
    integer , parameter :: PARAM_STATE_SIZE     = 311

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1_OVER = PARAM_STATE_SIZE - PARAM_LAG_1

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_1   = transfer( source= Z'6AEDE6FD97B338EC', mold= 0_INT64 )
    integer(INT64) , parameter :: PARAM_MATRIX_A = transfer( source= Z'5C32E06DF730FC42', mold= 0_INT64 )

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_U = shiftl( -1_INT64, PARAM_W - PARAM_P )
    integer(INT64) , parameter :: PARAM_MASK_L = not(PARAM_MASK_U)

    ! constant(s) for this MODULE
    character( len = * ) , parameter :: JUMP_STRING &!
        =  "1510de5f1aeb1b349b7d2f3dc278bf1e6358d09c083c53b2b52b0b37aa42ec96ae92d9199e5ddb4f8f19419a1ae8d41d208c" &!
        // "c209439db14c17bc032c1aa482b589174bb3ac3964a128c742017ff511a9ddd720f397969f0c4dc862608725d5465dd0d257" &!
        // "99d29ff579515657f3b7f58f5f6090d3c2c283b9e1cc517b48d4df4f03db955624557939ba23ff0b68b195a7a7413dcb3029" &!
        // "25711acc4fbc5554193ddcf43bfd9deeda0e3a684770ef6b11b8129f937e0c41e8c7c435bb76c6ca0518d6cd8809410c33a5" &!
        // "f5f39573f7ed9479abe9a5ee7bf09e189b1737f6fe53897026d792327de7e2c9ca050fa66f23eab9a0a83b67a9e6d54d70ce" &!
        // "46664dbc4af7cee88756fc50f16b841b76167c66613ef43b00b775aeed0e260fde67da03f6051ba11dbfa2070447f3aba151" &!
        // "e001404a11d3049e53f177ee4c275cffcf4c6e5c7b8a1e8db086731abb01ea50ec8440bc45fdd3c23679a68b29b2457d0013" &!
        // "878d8a7f1dccc595f99e656b64da2715a392eb68a517989be24c663dcbfb663ff38c567fa6b5fe8bdccbd30163524a9a1d63" &!
        // "cf609eb93a1fe3cca5e1220bd05e4dcb611a459d6ee70bbf5786d6fb887aea96e70e78af7f50dcbc638664ac28efcab6356d" &!
        // "ed959bb79355c5bc5e189a20bb8f64e5fcb444c2f29c57fce7a70208115da1b8a663c8062cbc98e353526b1d72371c07fb0c" &!
        // "ad50a923eef2c5c865d733be91978e1279cc45ea20f534e428422f72c30957e7fab79da909526d097b4a3a790c2b3cae28ef" &!
        // "52e5eb4302858110e1bcc31187bdbf79012e770ff95126a7a04b4059e2a9f9f885a6af3d5d067148e05bdd01bdc8f7a33b47" &!
        // "5631f89a08e92e61a25618846b55a2f42ab42c56ce3d3948fdf515b90b344f726bfe8543a93367cd5d95b08d4da0bcc7b2fc" &!
        // "65384a51eb16766ee2ee3bdf82b6cf24c7a81e826d2e9f81e81917ead9c3ca2b0ea0a2395cf4804080dd0cbf4698e412b7a2" &!
        // "49ddc89bc939e34857437be5fc1586f932a0a10c48121eb5e83a1d4e4bd682d9674d6d42f8ec190dada2ba9c4c0c25392b1c" &!
        // "fc32916c9f7dd5978badc53796d2c2843880adfaff7d83b73c5959b9a7424715d2f7a47e1c0363c7d3f60c332c8bb39b8656" &!
        // "08c1035c2773f53a0edc2582182a5cffaa5acd15820daeff1658c64ac4b579f8134fd1db297c1d4d4dd03b4f063a293a2cbd" &!
        // "a3aaf381e6cf54a0cd949e5ed2473852484566db89de18654d8efa020ed963c9d26dbba50a3de5f0c3b6e72b477c8f26284d" &!
        // "cf561c3df5780cef6197039cc076391022a0d57845e992e3b52189c95e92172461838b14f014f452ab24460be82113d41f31" &!
        // "47e210c03f8430b223836d1efe5ef96bf56708dbad033d57fa74beb1314c1abf1b328b4145c359bc4b6befc94c6bec8762f5" &!
        // "feaa4f14f309e5e51415479d1f16821528b707599eb530a8986b751ccce0d17055894116cd032af55860af016dff76fa14ce" &!
        // "b606c4b277f5968f897d91b544db7cf0de9fb237d5990007517e0aab7a73866d498e76f772006d3bf2387c552ba3d72e3a6a" &!
        // "a324edeea5989a45b0468ec514127156141de06e22c78347d6dc48c07dd42b1a9c543deed9006daa8ae676dc328f7dbc5d90" &!
        // "02d2f481f9cc4c7b9a433377bf61d0d75eae143ff8c7e7e0f09a805ee12e187c02724a9c5e6789dd2a5300753bdfcc1c964c" &!
        // "818d2a45e13e4ba89ea90fdd45b40a1b76079cbcbfc717162eb27d7a902f213646ed65e7f00e5fbc0cd74bb099e00ed350b4" &!
        // "93225e88e5693d999244b8d0f1f9bbfad03e5223416fd790bcc6e047abd1523245c6a46d397f63b38ecebaf79234b53b9b02" &!
        // "374cdf7bcaa9558043e1018eb14ec31b1fb56a7e6aa673010812cf5abc0ed2ec1df75a615632f59968a92de6cc183c4c1555" &!
        // "3fe5ca263cf3cffd1342e60975ac2de843f5b5a6314e382dd6a6887b87e29f9b31b0d7a2dc31e9f07212fa0c2e69db50d30b" &!
        // "d676460a94a9822f5aaf5af01bc566136da7138ba69554577a2ef2f5d91051ec7ee3645a0df47bbea49e2a47c1279e3510e0" &!
        // "8c89c9d5b20966125b582469b13d99308119423dab451f29b8b4f6ebeff94a06c74d9f6e040c269c39b1c5942cd96f812b35" &!
        // "b047357ddb08863649a13cb38a4e10d047b8aa84a81870de3cd774a4b6174291bc3731437aefa7dbbf2af9c497dec0a90a36" &!
        // "55395944fc6a0c3e46326a10d905fbd5cd90ccd46baac32cff4f6e48936de047e3eb24cf7e7e64ac7616ed8fe0ad751daee7" &!
        // "bc8e09ab4447718355e92fbd583a3165466d722c4fb0f904d865b77b99053db2709ae3c721b714ae8bbdac87fc0b81a5c5dd" &!
        // "c2e042e3155801276efc14e508e5fff27ad21ff1c97565737320b1344df216188bb3872a28c11ecc1aabce8cdf9749b6bc67" &!
        // "39628e3f35b531a32dac218196becb2945904b35079ce2bbd97f811fb71c2fa1d9cc5ea65a9d88ee77ab2a52e48e8aaf4e4d" &!
        // "91679618ffe441b8c319bf6c6589e118f3abd0f8c22fc930af64e1b0e4616c1f5f94c50ea240ea8cdd7d57f9b7ee11c3516f" &!
        // "16115bc995e586f3483ca5be4bbf1c1fe4578934f77c03e307f6096854e9a93d28cd7331ce91371a2f50ae608d1f0348f8ce" &!
        // "3ce48eaaf83f7195ea7b3fbcf4b331d4a2c7f21843b745164e4b71678b8ea41580feef7db43f090915ec7edae77eb058d37f" &!
        // "a04571f4bad32d08d364301a7f0fc633fdfe3f9695f0edf8de2187dee171988c47da64da030fcbcfd8fc3b77a59943d46927" &!
        // "c869e6065b237a0d9e32a72cf0e15ae969b0672a5f5835cdba88ce9173abe094d95ae7acee85e176fb826b9ffe01ca860f95" &!
        // "06540e6f415a9c5ba8ad9a8dd306188fc1973dcd33f75c4b58f5d6a6df6a5ed88f4514690dee844b77c5fc6bb2090d5b6364" &!
        // "fc31b0ec50e29cca44752024bc3270f553570ac196066eb1f04e09be04b7301a915080ebeaea4c749c04f2d4cf79c5805d08" &!
        // "beb34b966fbc5e153f80a00101883c93861bbee60c52470053546aeb57e487092b60884ab20f738f87c9ab6bca2a3370ffaf" &!
        // "745ccbc44bae13befd29deacddb38d0124e02ef8aa656a87f747e0deac35e7fe2f191ed119a6908a909222deffb028e5e12f" &!
        // "ea7c3be122fb684ebf83f8adcba142affa7753e27370b493fed258a4db5068042a9e4db38d160f388f4064dfd13b3bbfe95b" &!
        // "cd6176ce99fef56573fc8141bc4a290202b2437df2886f2dcfb693d3110b78220a7007b695bfda744a356cbce15814d2eaf7" &!
        // "1e322e9542d4933c7051e83f5a1636c72bda12822d803ca4daa66e5baa793271a6b301d1ec7a818a4b5ddca7d1141d830883" &!
        // "cd1586b50b0cdee0f4d445752b2716b5cc44d8b2e1149b4ec4ca06f87fa7be9b4aad509804b64f3edebba10fc687f20d238a" &!
        // "39f3b219c2e8f8f6f3533671843a521a457df1dbccc54b624ba0609fed10acfb9b3442bbf93f5689415d4243a06f53958e06" &!
        // "f28b7b4e5d08ea178bc92eee27adb94f002b7d0bbc0da400752421ab4edcce592d9996d2472b967043d20"



    ! declaration of a user-defined TYPE
    type , extends(type_melg64_abstract) :: type_melg19937_64

        contains

        ! kind: FUNCTION
        procedure , nopass , private :: lag_1          => lag_1_melg19937_64
        procedure , nopass , private :: lag_1_over     => lag_1_over_melg19937_64
        procedure , nopass , private :: mask_1         => mask_1_melg19937_64
        procedure , nopass , private :: mask_l         => mask_l_melg19937_64
        procedure , nopass , private :: mask_u         => mask_u_melg19937_64
        procedure , nopass , private :: matrix_a       => matrix_a_melg19937_64
        procedure , nopass , private :: shift_1        => shift_1_melg19937_64
        procedure , nopass , private :: shift_lung_neg => shift_lung_neg_melg19937_64
        procedure , nopass , private :: shift_lung_pos => shift_lung_pos_melg19937_64
        procedure , nopass , private :: shift_mm       => shift_mm_melg19937_64
        procedure , nopass , private :: state_size     => state_size_melg19937_64

        ! kind: SUBROUTINE
        procedure , pass , public :: jump => jump_melg19937_64

    end type type_melg19937_64



    contains



    integer pure function lag_1_melg19937_64 () result(lag_1)
        lag_1 = PARAM_LAG_1
    end function



    integer pure function lag_1_over_melg19937_64 () result(lag_1_over)
        lag_1_over = PARAM_LAG_1_OVER
    end function



    integer(INT64) pure function mask_1_melg19937_64 () result(mask_1)
        mask_1 = PARAM_MASK_1
    end function



    integer(INT64) pure function mask_l_melg19937_64 () result(mask_l)
        mask_l = PARAM_MASK_L
    end function



    integer(INT64) pure function mask_u_melg19937_64 () result(mask_u)
        mask_u = PARAM_MASK_U
    end function



    integer(INT64) pure function matrix_a_melg19937_64 () result(matrix_a)
        matrix_a = PARAM_MATRIX_A
    end function



    integer pure function shift_1_melg19937_64 () result(shift_1)
        shift_1 = PARAM_SHIFT_1
    end function



    integer pure function shift_lung_neg_melg19937_64 () result(shift_lung_neg)
        shift_lung_neg = PARAM_SHIFT_LUNG_NEG
    end function



    integer pure function shift_lung_pos_melg19937_64 () result(shift_lung_pos)
        shift_lung_pos = PARAM_SHIFT_LUNG_POS
    end function



    integer pure function shift_mm_melg19937_64 () result(shift_mm)
        shift_mm = PARAM_SHIFT_MM
    end function



    integer pure function state_size_melg19937_64 () result(state_size)
        state_size = PARAM_STATE_SIZE
    end function



    subroutine jump_melg19937_64 ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg19937_64) , intent(inout) :: melg64

        ! variable(s) for this SUBROUTINE
        integer :: itr

        ! variable(s) for this SUBROUTINE
        type(type_melg19937_64) :: melg64_init

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
