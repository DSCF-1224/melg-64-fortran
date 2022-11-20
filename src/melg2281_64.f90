module melg2281_64

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: type_melg2281_64



    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1          =  6
    integer , parameter :: PARAM_P              = 41
    integer , parameter :: PARAM_SHIFT_1        =  6
    integer , parameter :: PARAM_SHIFT_LUNG_NEG = 36
    integer , parameter :: PARAM_SHIFT_LUNG_POS = 21
    integer , parameter :: PARAM_SHIFT_MM       = 17
    integer , parameter :: PARAM_STATE_SIZE     = 35

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1_OVER = PARAM_STATE_SIZE - PARAM_LAG_1

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_1   = transfer( source= Z'E4E2242B6E15AEBE', mold= 0_INT64 )
    integer(INT64) , parameter :: PARAM_MATRIX_A = transfer( source= Z'7CBE23EBCA8A6D36', mold= 0_INT64 )

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_U = shiftl( -1_INT64, PARAM_W - PARAM_P )
    integer(INT64) , parameter :: PARAM_MASK_L = not(PARAM_MASK_U)

    ! constant(s) for this MODULE
    character( len = * ) , parameter :: JUMP_STRING &!
        =  "153f3f5f58ab21e2b7e825fdc3cf74144f37d5320d6d4a08d4" &!
        // "5b84ceb30294b6f66be04d2b9a7bd2fe0ffe28dfc60c814e82" &!
        // "c4f85543a992fb7abf20f2f45c4b9e10729797ee8c34624102" &!
        // "b21adc05b2abaf1e08bd353b30d2ee3b889f4df1209245d8f5" &!
        // "4c836ee63466f0ed7bbf5816c6d3b36c9676b8a9d48f82a60a" &!
        // "87d7d40a5da53a7fcf46ee5f3052bb8010509c9a550d29867c" &!
        // "0f8d0b65ac69c69889d72ef9f7d782dacdb6d849a54d67c5d1" &!
        // "98468a02b28eabac4fa905fb06a1c2cd8def5e9ee05da25d92" &!
        // "be43269cddcd54a96543292fb854cd62a1d45c417f8666ef7c" &!
        // "fa5404456991aec230fe92c6eb513151d9810de985906e49f6" &!
        // "245bbcdcf257700469db91830d7e08dab027f5bb294962cf6b" &!
        // "bb3b53f1c22932113a870"



    ! declaration of a user-defined TYPE
    type , extends(type_melg64_abstract) :: type_melg2281_64

        contains

        ! kind: FUNCTION
        procedure , nopass , private :: lag_1          => lag_1_melg2281_64
        procedure , nopass , private :: lag_1_over     => lag_1_over_melg2281_64
        procedure , nopass , private :: mask_1         => mask_1_melg2281_64
        procedure , nopass , private :: mask_l         => mask_l_melg2281_64
        procedure , nopass , private :: mask_u         => mask_u_melg2281_64
        procedure , nopass , private :: matrix_a       => matrix_a_melg2281_64
        procedure , nopass , private :: shift_1        => shift_1_melg2281_64
        procedure , nopass , private :: shift_lung_neg => shift_lung_neg_melg2281_64
        procedure , nopass , private :: shift_lung_pos => shift_lung_pos_melg2281_64
        procedure , nopass , private :: shift_mm       => shift_mm_melg2281_64
        procedure , nopass , private :: state_size     => state_size_melg2281_64

        ! kind: SUBROUTINE
        procedure , pass , public :: jump => jump_melg2281_64

    end type type_melg2281_64



    contains



    integer pure function lag_1_melg2281_64 () result(lag_1)
        lag_1 = PARAM_LAG_1
    end function



    integer pure function lag_1_over_melg2281_64 () result(lag_1_over)
        lag_1_over = PARAM_LAG_1_OVER
    end function



    integer(INT64) pure function mask_1_melg2281_64 () result(mask_1)
        mask_1 = PARAM_MASK_1
    end function



    integer(INT64) pure function mask_l_melg2281_64 () result(mask_l)
        mask_l = PARAM_MASK_L
    end function



    integer(INT64) pure function mask_u_melg2281_64 () result(mask_u)
        mask_u = PARAM_MASK_U
    end function



    integer(INT64) pure function matrix_a_melg2281_64 () result(matrix_a)
        matrix_a = PARAM_MATRIX_A
    end function



    integer pure function shift_1_melg2281_64 () result(shift_1)
        shift_1 = PARAM_SHIFT_1
    end function



    integer pure function shift_lung_neg_melg2281_64 () result(shift_lung_neg)
        shift_lung_neg = PARAM_SHIFT_LUNG_NEG
    end function



    integer pure function shift_lung_pos_melg2281_64 () result(shift_lung_pos)
        shift_lung_pos = PARAM_SHIFT_LUNG_POS
    end function



    integer pure function shift_mm_melg2281_64 () result(shift_mm)
        shift_mm = PARAM_SHIFT_MM
    end function



    integer pure function state_size_melg2281_64 () result(state_size)
        state_size = PARAM_STATE_SIZE
    end function



    subroutine jump_melg2281_64 ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg2281_64) , intent(inout) :: melg64

        ! variable(s) for this SUBROUTINE
        integer :: itr

        ! variable(s) for this SUBROUTINE
        type(type_melg2281_64) :: melg64_init

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
