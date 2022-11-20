module melg4253_64

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: type_melg4253_64



    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1          =  9
    integer , parameter :: PARAM_P              = 29
    integer , parameter :: PARAM_SHIFT_1        =  5
    integer , parameter :: PARAM_SHIFT_LUNG_NEG = 30
    integer , parameter :: PARAM_SHIFT_LUNG_POS = 20
    integer , parameter :: PARAM_SHIFT_MM       = 29
    integer , parameter :: PARAM_STATE_SIZE     = 66

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_LAG_1_OVER = PARAM_STATE_SIZE - PARAM_LAG_1

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_1   = transfer( source= Z'CB67B0C18FE14F4D', mold= 0_INT64 )
    integer(INT64) , parameter :: PARAM_MATRIX_A = transfer( source= Z'FAC1E8C56471D722', mold= 0_INT64 )

    ! constant(s) for this MODULE
    integer(INT64) , parameter :: PARAM_MASK_U = shiftl( -1_INT64, PARAM_W - PARAM_P )
    integer(INT64) , parameter :: PARAM_MASK_L = not(PARAM_MASK_U)

    ! constant(s) for this MODULE
    character( len = * ) , parameter :: JUMP_STRING &!
        =  "514609396aa32e1815afd614eabdd3ebacb4868f08cfed4ca1" &!
        // "e27c40ed5a24db338fe372795db756f0f632ce67327a5e61e5" &!
        // "53e9248920f860bf759719e5db8ace1d5334763fa5df0e92dc" &!
        // "9e78719aae25aa0e8125b0a63fc035b9605c185a4fe35cc18f" &!
        // "98210fb398dc6cd68932a6c4dead9efd6f410086ccbe8d2518" &!
        // "9be700ed70bd07af780e7cbda0172647d929221aec90cd5bc7" &!
        // "1d52b673c34edf12ab6fa5b72cb466b514dec1695e3aafbc15" &!
        // "6e1a4c7d289d7644359e108ccad0247e120f3ca0d7d5007776" &!
        // "f2df463f383eaa3abe97e4248764e79e8219ac22b00c622376" &!
        // "d0d17dcb3d280de5e87c3b0b826a65c36c84704026ef8351df" &!
        // "3e7f428d113311ae397d20fe518709867ae8f076ae58cf2498" &!
        // "945fa9fc5dfa12d6db79078d3ad42c07655feb5a7846af5d6d" &!
        // "1422db5ae9dcc999418ac24a1f4e4c2145fa7a7c74631de210" &!
        // "6b284f0f26377cca29a1740104cdb723b8c907d50204da74d2" &!
        // "d3ebe9fa9eed13e21ed507151567b864798ac67aa55dec472f" &!
        // "2907042795f242448c0b9772d51b18fd7ce9b2eed2effbd069" &!
        // "417d2d1ea1b14d2b5753d3a11295aa1de6b0d9e7172646c86f" &!
        // "610133672dd7a1014e2916c5dbdff1040034e07d52e90edb36" &!
        // "59e7b612a1c45d49280072c8da8a0f5e5497d8eebde67d6a9a" &!
        // "1b29375720036d84245ff9b96c670c555610f35ed15889e97a" &!
        // "cc8a7812be1fd09440714e353d1c197a9addf30acc7a0bc90f" &!
        // "b1e354125eb388"



    ! declaration of a user-defined TYPE
    type , extends(type_melg64_abstract) :: type_melg4253_64

        contains

        ! kind: FUNCTION
        procedure , nopass , private :: lag_1          => lag_1_melg4253_64
        procedure , nopass , private :: lag_1_over     => lag_1_over_melg4253_64
        procedure , nopass , private :: mask_1         => mask_1_melg4253_64
        procedure , nopass , private :: mask_l         => mask_l_melg4253_64
        procedure , nopass , private :: mask_u         => mask_u_melg4253_64
        procedure , nopass , private :: matrix_a       => matrix_a_melg4253_64
        procedure , nopass , private :: shift_1        => shift_1_melg4253_64
        procedure , nopass , private :: shift_lung_neg => shift_lung_neg_melg4253_64
        procedure , nopass , private :: shift_lung_pos => shift_lung_pos_melg4253_64
        procedure , nopass , private :: shift_mm       => shift_mm_melg4253_64
        procedure , nopass , private :: state_size     => state_size_melg4253_64

        ! kind: SUBROUTINE
        procedure , pass , public :: jump => jump_melg4253_64

    end type type_melg4253_64



    contains



    integer pure function lag_1_melg4253_64 () result(lag_1)
        lag_1 = PARAM_LAG_1
    end function



    integer pure function lag_1_over_melg4253_64 () result(lag_1_over)
        lag_1_over = PARAM_LAG_1_OVER
    end function



    integer(INT64) pure function mask_1_melg4253_64 () result(mask_1)
        mask_1 = PARAM_MASK_1
    end function



    integer(INT64) pure function mask_l_melg4253_64 () result(mask_l)
        mask_l = PARAM_MASK_L
    end function



    integer(INT64) pure function mask_u_melg4253_64 () result(mask_u)
        mask_u = PARAM_MASK_U
    end function



    integer(INT64) pure function matrix_a_melg4253_64 () result(matrix_a)
        matrix_a = PARAM_MATRIX_A
    end function



    integer pure function shift_1_melg4253_64 () result(shift_1)
        shift_1 = PARAM_SHIFT_1
    end function



    integer pure function shift_lung_neg_melg4253_64 () result(shift_lung_neg)
        shift_lung_neg = PARAM_SHIFT_LUNG_NEG
    end function



    integer pure function shift_lung_pos_melg4253_64 () result(shift_lung_pos)
        shift_lung_pos = PARAM_SHIFT_LUNG_POS
    end function



    integer pure function shift_mm_melg4253_64 () result(shift_mm)
        shift_mm = PARAM_SHIFT_MM
    end function



    integer pure function state_size_melg4253_64 () result(state_size)
        state_size = PARAM_STATE_SIZE
    end function



    subroutine jump_melg4253_64 ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg4253_64) , intent(inout) :: melg64

        ! variable(s) for this SUBROUTINE
        integer :: itr

        ! variable(s) for this SUBROUTINE
        type(type_melg4253_64) :: melg64_init

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
