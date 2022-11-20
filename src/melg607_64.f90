module melg607_64

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: type_melg607_64



    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1          =  3
    integer , parameter :: PARAM_P              = 31
    integer , parameter :: PARAM_SHIFT_1        = 30
    integer , parameter :: PARAM_SHIFT_LUNG_NEG = 13
    integer , parameter :: PARAM_SHIFT_LUNG_POS = 35
    integer , parameter :: PARAM_SHIFT_MM       =  5
    integer , parameter :: PARAM_STATE_SIZE     =  9

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1_OVER = PARAM_STATE_SIZE - PARAM_LAG_1

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_1   = transfer( source= Z'66EDC62A6BF8C826', mold= 0_INT64 )
    integer(INT64) , parameter :: PARAM_MATRIX_A = transfer( source= Z'81F1FD68012348BC', mold= 0_INT64 )

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_U   = shiftl( -1_INT64, PARAM_W - PARAM_P )
    integer(INT64) , parameter :: PARAM_MASK_L   = not(PARAM_MASK_U)

    ! constant(s) for this MODULE
    character( len = * ) , parameter :: JUMP_STRING &!
        =  "f3d27aef5c025caca71e8dfb38d8e7ce5fe0d46c04317c6f50" &!
        // "ef41c5edce6ebf48fe2929dd0ca41af901d536b52ae616662b" &!
        // "620bad0a18060e54c127d729bdcb439f7ee398bec8e7195562" &!
        // "9c"



    ! declaration of a user-defined TYPE
    type , extends(type_melg64_abstract) :: type_melg607_64

        contains

        ! kind: FUNCTION
        procedure , nopass , private :: lag_1          => lag_1_melg607_64
        procedure , nopass , private :: lag_1_over     => lag_1_over_melg607_64
        procedure , nopass , private :: mask_1         => mask_1_melg607_64
        procedure , nopass , private :: mask_l         => mask_l_melg607_64
        procedure , nopass , private :: mask_u         => mask_u_melg607_64
        procedure , nopass , private :: matrix_a       => matrix_a_melg607_64
        procedure , nopass , private :: shift_1        => shift_1_melg607_64
        procedure , nopass , private :: shift_lung_neg => shift_lung_neg_melg607_64
        procedure , nopass , private :: shift_lung_pos => shift_lung_pos_melg607_64
        procedure , nopass , private :: shift_mm       => shift_mm_melg607_64
        procedure , nopass , private :: state_size     => state_size_melg607_64

        ! kind: SUBROUTINE
        procedure , pass , public :: jump => jump_melg607_64

    end type type_melg607_64



    contains



    integer pure function lag_1_melg607_64 () result(lag_1)
        lag_1 = PARAM_LAG_1
    end function



    integer pure function lag_1_over_melg607_64 () result(lag_1_over)
        lag_1_over = PARAM_LAG_1_OVER
    end function



    integer(INT64) pure function mask_1_melg607_64 () result(mask_1)
        mask_1 = PARAM_MASK_1
    end function



    integer(INT64) pure function mask_l_melg607_64 () result(mask_l)
        mask_l = PARAM_MASK_L
    end function



    integer(INT64) pure function mask_u_melg607_64 () result(mask_u)
        mask_u = PARAM_MASK_U
    end function



    integer(INT64) pure function matrix_a_melg607_64 () result(matrix_a)
        matrix_a = PARAM_MATRIX_A
    end function



    integer pure function shift_1_melg607_64 () result(shift_1)
        shift_1 = PARAM_SHIFT_1
    end function



    integer pure function shift_lung_neg_melg607_64 () result(shift_lung_neg)
        shift_lung_neg = PARAM_SHIFT_LUNG_NEG
    end function



    integer pure function shift_lung_pos_melg607_64 () result(shift_lung_pos)
        shift_lung_pos = PARAM_SHIFT_LUNG_POS
    end function



    integer pure function shift_mm_melg607_64 () result(shift_mm)
        shift_mm = PARAM_SHIFT_MM
    end function



    integer pure function state_size_melg607_64 () result(state_size)
        state_size = PARAM_STATE_SIZE
    end function



    subroutine jump_melg607_64 ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg607_64) , intent(inout) :: melg64

        ! variable(s) for this SUBROUTINE
        integer :: itr

        ! variable(s) for this SUBROUTINE
        type(type_melg607_64) :: melg64_init

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
