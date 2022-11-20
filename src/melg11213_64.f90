module melg11213_64

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: type_melg11213_64



    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1          =   4
    integer , parameter :: PARAM_P              =  13
    integer , parameter :: PARAM_SHIFT_1        =   5
    integer , parameter :: PARAM_SHIFT_LUNG_NEG =  33
    integer , parameter :: PARAM_SHIFT_LUNG_POS =  13
    integer , parameter :: PARAM_SHIFT_MM       =  45
    integer , parameter :: PARAM_STATE_SIZE     = 175

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1_OVER = PARAM_STATE_SIZE - PARAM_LAG_1

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_1   = transfer( source= Z'BD2D1251E589593F', mold= 0_INT64 )
    integer(INT64) , parameter :: PARAM_MATRIX_A = transfer( source= Z'DDBCD6E525E1C757', mold= 0_INT64 )

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_U = shiftl( -1_INT64, PARAM_W - PARAM_P )
    integer(INT64) , parameter :: PARAM_MASK_L = not(PARAM_MASK_U)

    ! constant(s) for this MODULE
    character( len = * ) , parameter :: JUMP_STRING &!
        =  "f455de2f54520b2dd8ce35d97fd8b50e4ef4f8776263cf2a26f392b0e71ad26509d8d20ada6ff2b026ca14e21cc91597f6af" &!
        // "280a5e22ecde9f9e6653fedcba04c3ece760f941994129db87c28701d351c7d54c951a38c11490cea33913acd7e0d5960300" &!
        // "69e553711ebd8ac376a32a080863759449d00326bfc085951a2014ed68db92fb6a0db234c6175dd7e38fc03352e91d9988a5" &!
        // "6734941d00e65311c51f6358653537be02609619a45c1c3d12c257a1d559d4b485df547b60aff026ac32dcfa01f1e2d2105a" &!
        // "d68e3a268c67594f8b90bee09fe3df899ce7c2f02eb05070d3707f50d2f6f8bd2864833733fd6505bd2d08deefccea8725c4" &!
        // "7fd4febd22882362955bf5bd8a604118bc1ce86a8f9d0a76bc6f709cfb024f4d2c68a01777d344a10316999c02093f73e14f" &!
        // "5cf573e4242e09ff6c154a77dc7796f812ee713886219caae3f3db710d37b215b79a49fbc6339eedcc63785fa5f96fbf0352" &!
        // "13d4455545447e708d555f6b060c0fd27d713a0285bb7b15656cc5bde689b6a61f5ab0140b4a7d2ed3b44ee302f9e21e747c" &!
        // "9ed7d1d5005e72f00dc6a5e6264d44deff7fd1ed39fa33dde611adf01d41e1241b9eed738b88c3280523126b6c5c16679cbf" &!
        // "9aaf27e035bbc52449d4d7e2f129b18924549b731c7efd1c4120898ed1fe1f005db1160a6918c8b22823385aea6961f18487" &!
        // "e3bfb3ec478ac6c9c5fe8b89f590cd59ddb4dec62ee27bc32d534566c4d4e9c8eb6989a74723b11baf1524db1ed389e8f245" &!
        // "e97fd9322674e177e5e34eef5254aafac857351aa2a352f0d2128d4f97b6803e02031e25b5f51ba3f7cb1d789614d01d3a82" &!
        // "7067c24deb627f8a84eee83dcff9ce5e5886981388ea4a81452e6b68e05227585898f16ce6a163cfca9db2964ade5ffc7754" &!
        // "88e48e9f1ea25afcdaf40745087fa358bacb25070a1a23b0ff7f67670327fd511a897ff2aa995c51f4218217d604495aed21" &!
        // "3a99b36560444269159c1510083af95f2660abf69e311bed3dfd37177aac8784d12f1654d49197ba58b598f8bd0aea9711c6" &!
        // "8bffded44acb765090baf2744e43fba03f80ed667e3880b2a4fe0a56b4c84b2ecdc76f2150615cf54ad6469e4fcf10ab3f20" &!
        // "be5080409cc4325d67a91f535b2ed5c980f27142904ef64f5de5bba1e5ef812c6802dc8671da7a60cd74c74277a19c01c917" &!
        // "c275594677bee39ba5afbbf5ebfd7ac0c5cde0cf1c4df295d6ae684dc704937f930d3200f163d54760bdd961e96ffc64aae2" &!
        // "d05f357362b29e0dda3e808366808aca5ff9c0391b7b00b9fcf923d7b5ebf3d876fb1ea6058febf4c65c5f925c6a8b7a2af3" &!
        // "f32988838dbf961486f1805b68f7b53c74dabb57c89f0d44606897da58f9967d79270678b7e0bebd9bbaa058152f17c7c0ee" &!
        // "5fbf16ad091f19b744f11a980084b3a585682a413fe1b30aa50f836e60c647fa9d27628e7a5abe87dbda4cd4c4ecc165cafb" &!
        // "307ad89d35655188d77f1d5ef339a1b84cef2c5991a3078900dbeeb76b0516d27f22e98a4c0fb9282535d2a8afa7bd0b7ddc" &!
        // "9c14b007eb5dc9bed6ef7fe875758977750d9f1420110c3804645c4524abe712a40c426d466096ca8ad26232c4441f1d2091" &!
        // "3e1be38e0fbad2c67304d39ce9a5e130b06896c40348a52b32be85dcae102dd2b631301decf98b436a52891fac17bdca7e7a" &!
        // "0f9e7a685fb8c1c58f2095c7e44c67cbe61ad1015947043d97bb9d9d40d5d54eba0f14383f6a16fce87e1d0719528c25c71b" &!
        // "441672c77b0a4dfb081176ed7add6b804052de243b08b9ab30517c1ee135c7b3690b7805b0d2664cbba443d0bf61ca0b276b" &!
        // "332b641579ca19de873a074e642eeb5cdb4a202bed3e0cb8fd1bfc313f5013fdb325e217790e418acb9da12088efb02ee740" &!
        // "708c49f4136c8f31c60998c756c8af8537426dd25ae152e65527c66b475477fbabe007c9e65b91acceb9227d3baf24dccd8c" &!
        // "18d0"



    ! declaration of a user-defined TYPE
    type , extends(type_melg64_abstract) :: type_melg11213_64

        contains

        ! kind: FUNCTION
        procedure , nopass , private :: lag_1          => lag_1_melg11213_64
        procedure , nopass , private :: lag_1_over     => lag_1_over_melg11213_64
        procedure , nopass , private :: mask_1         => mask_1_melg11213_64
        procedure , nopass , private :: mask_l         => mask_l_melg11213_64
        procedure , nopass , private :: mask_u         => mask_u_melg11213_64
        procedure , nopass , private :: matrix_a       => matrix_a_melg11213_64
        procedure , nopass , private :: shift_1        => shift_1_melg11213_64
        procedure , nopass , private :: shift_lung_neg => shift_lung_neg_melg11213_64
        procedure , nopass , private :: shift_lung_pos => shift_lung_pos_melg11213_64
        procedure , nopass , private :: shift_mm       => shift_mm_melg11213_64
        procedure , nopass , private :: state_size     => state_size_melg11213_64

        ! kind: SUBROUTINE
        procedure , pass , public :: jump => jump_melg11213_64

    end type type_melg11213_64



    contains



    integer pure function lag_1_melg11213_64 () result(lag_1)
        lag_1 = PARAM_LAG_1
    end function



    integer pure function lag_1_over_melg11213_64 () result(lag_1_over)
        lag_1_over = PARAM_LAG_1_OVER
    end function



    integer(INT64) pure function mask_1_melg11213_64 () result(mask_1)
        mask_1 = PARAM_MASK_1
    end function



    integer(INT64) pure function mask_l_melg11213_64 () result(mask_l)
        mask_l = PARAM_MASK_L
    end function



    integer(INT64) pure function mask_u_melg11213_64 () result(mask_u)
        mask_u = PARAM_MASK_U
    end function



    integer(INT64) pure function matrix_a_melg11213_64 () result(matrix_a)
        matrix_a = PARAM_MATRIX_A
    end function



    integer pure function shift_1_melg11213_64 () result(shift_1)
        shift_1 = PARAM_SHIFT_1
    end function



    integer pure function shift_lung_neg_melg11213_64 () result(shift_lung_neg)
        shift_lung_neg = PARAM_SHIFT_LUNG_NEG
    end function



    integer pure function shift_lung_pos_melg11213_64 () result(shift_lung_pos)
        shift_lung_pos = PARAM_SHIFT_LUNG_POS
    end function



    integer pure function shift_mm_melg11213_64 () result(shift_mm)
        shift_mm = PARAM_SHIFT_MM
    end function



    integer pure function state_size_melg11213_64 () result(state_size)
        state_size = PARAM_STATE_SIZE
    end function



    subroutine jump_melg11213_64 ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg11213_64) , intent(inout) :: melg64

        ! variable(s) for this SUBROUTINE
        integer :: itr

        ! variable(s) for this SUBROUTINE
        type(type_melg11213_64) :: melg64_init

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
