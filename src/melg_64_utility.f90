module melg_64_utility

    ! required MODULE
    use , intrinsic :: iso_c_binding
    use , intrinsic :: iso_fortran_env

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: ASCII_CODE_DIGIT_0
    public  :: ASCII_CODE_SMALL_A
    public  :: ieor
    public  :: mat3neg
    public  :: mat3pos
    public  :: MINVAL_STATE_INDEX
    public  :: PARAM_W
    public  :: SELECTOR_CASE_1
    public  :: SELECTOR_CASE_2
    public  :: SELECTOR_CASE_3
    public  :: SELECTOR_CASE_4
    public  :: type_melg64_abstract



    ! constant(s) for this MODULE
    integer , parameter :: MINVAL_STATE_INDEX = 1

    ! constant(s) for this MODULE
    integer , parameter :: PARAM_W = 64

    integer , parameter :: ASCII_CODE_DIGIT_0 = iachar('0')
    integer , parameter :: ASCII_CODE_SMALL_A = iachar('a')

    ! constant(s) for this MODULE
    real(REAL64) , parameter :: COEF_RES53 = transfer( source= Z'3CA0000000000000', mold= 0.0_REAL64 )
    real(REAL64) , parameter :: PI253_1    = 1.0_REAL64 / ( 2.0_REAL64 ** 53 - 1.0_REAL64 )
    real(REAL64) , parameter :: PI253      = 1.0_REAL64 / ( 2.0_REAL64 ** 53 )
    real(REAL64) , parameter :: PI252      = 1.0_REAL64 / ( 2.0_REAL64 ** 52 )

    ! constant(s) for this MODULE
    enum, bind(c)
        enumerator :: SELECTOR_CASE_1 = 1
        enumerator :: SELECTOR_CASE_2
        enumerator :: SELECTOR_CASE_3
        enumerator :: SELECTOR_CASE_4
    end enum



    ! declaration of a user-defined TYPE
    type , abstract :: type_melg64_abstract

        ! field(s) of this TYPE
        integer , public :: index
        integer , public :: selector

        ! field(s) of this TYPE
        integer(INT64) , public :: lung

        ! field(s) of this TYPE
        integer(INT64) , allocatable , public :: state(:)

        contains

        ! kind: FUNCTION
        procedure , nopass , public  :: default_seed
        procedure ,   pass , private :: mag01

        ! kind: FUNCTION
        procedure( lag_1_abstract          ) , nopass , deferred , private :: lag_1
        procedure( lag_1_over_abstract     ) , nopass , deferred , private :: lag_1_over
        procedure( mask_1_abstract         ) , nopass , deferred , private :: mask_1
        procedure( mask_l_abstract         ) , nopass , deferred , private :: mask_l
        procedure( mask_u_abstract         ) , nopass , deferred , private :: mask_u
        procedure( matrix_a_abstract       ) , nopass , deferred , private :: matrix_a
        procedure( shift_1_abstract        ) , nopass , deferred , private :: shift_1
        procedure( shift_lung_neg_abstract ) , nopass , deferred , private :: shift_lung_neg
        procedure( shift_lung_pos_abstract ) , nopass , deferred , private :: shift_lung_pos
        procedure( shift_mm_abstract       ) , nopass , deferred , private :: shift_mm
        procedure( state_size_abstract     ) , nopass , deferred , private :: state_size

        ! kind: SUBROUTINE
        procedure , pass , public :: copy_from
        procedure , pass , public :: genrand64_int64
        procedure , pass , public :: genrand64_int64_non_negative
        procedure , pass , public :: genrand64_real1
        procedure , pass , public :: genrand64_real2
        procedure , pass , public :: genrand64_real3
        procedure , pass , public :: genrand64_res53
        procedure , pass , public :: initialize_for_jump
        procedure , pass , public :: jump_unit

        ! kind: SUBROUTINE
        procedure , pass , private :: add
        procedure , pass , private :: allocate_state
        procedure , pass , private :: deallocate_state
        procedure , pass , private :: initialize_by_array_with_size
        procedure , pass , private :: initialize_by_array_without_size
        procedure , pass , private :: initialize_by_scalar
        procedure , pass , private :: reallocate_state

        ! kind: SUBROUTINE
        procedure( jump_abstract ) , pass , deferred , public :: jump

        ! kind: INTERFACE
        generic , public :: initialize => initialize_by_array_without_size
        generic , public :: initialize => initialize_by_scalar

    end type type_melg64_abstract



    ! declaration of a user-defined INTERFACE
    abstract interface

        integer         module pure function lag_1_abstract           (); end function
        integer         module pure function lag_1_over_abstract      (); end function
        integer         module pure function len_jump_string_abstract (); end function
        integer (INT64) module pure function mask_1_abstract          (); end function
        integer (INT64) module pure function mask_l_abstract          (); end function
        integer (INT64) module pure function mask_u_abstract          (); end function
        integer (INT64) module pure function matrix_a_abstract        (); end function
        integer         module pure function shift_1_abstract         (); end function
        integer         module pure function shift_lung_neg_abstract  (); end function
        integer         module pure function shift_lung_pos_abstract  (); end function
        integer         module pure function shift_mm_abstract        (); end function
        integer         module pure function state_size_abstract      (); end function

    end interface

    abstract interface

        module subroutine jump_abstract ( melg64 )

            ! dummy bounded argument for this FUNCTION
            class(type_melg64_abstract) , intent(inout) :: melg64

        end subroutine

    end interface

    interface ieor
        module procedure :: ieor_4_int64
    end interface ieor




    contains



    integer(INT64) pure elemental function default_seed ()
        default_seed = 19650218_INT64
    end function



    integer(INT64) pure elemental function ieor_4_int64 ( i, j, k, l ) result(ieor_4)

        ! dummy argument(s) for this FUNCTION
        integer(INT64) , intent(in) :: i, j, k, l

        ieor_4 = ieor( ieor( ieor( i, j ), k ), l )

    end function



    integer(INT64) pure function mag01 ( melg64, i )

        ! dummy bounded argument for this FUNCTION
        class(type_melg64_abstract) , intent(in) :: melg64

        ! dummy unbounded argument(s) for this FUNCTION
        integer(INT64) , intent(in) :: i

        select case ( iand( i, 1_INT64 ) )
            case(0_INT64) ; mag01 = 0_INT64
            case(1_INT64) ; mag01 = melg64%matrix_a()
        end select

    end function



    integer(INT64) pure elemental function mat3neg ( i, shift )

        ! dummy argument(s) for this FUNCTION
        integer (INT64) , intent(in) :: i
        integer         , intent(in) :: shift

        mat3neg = ieor( i, shiftl( i, shift ) )

    end function



    integer(INT64) pure elemental function mat3pos ( i, shift )

        ! dummy argument(s) for this FUNCTION
        integer (INT64) , intent(in) :: i
        integer         , intent(in) :: shift

        mat3pos = ieor( i, shiftr( i, shift ) )

    end function



    subroutine add ( melg64_self, melg64_init )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(in) :: melg64_self

        ! unbounded dummy argument(s) for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64_init

        ! variable(s) for this SUBROUTINE
        integer :: itr
        integer :: diff1, diff2

        ! STEP.01
        ! add `lung`
        melg64_init%lung = ieor( melg64_init%lung, melg64_self%lung )

        ! STEP.02
        ! add the states
        associate(&!
            index_init => melg64_init%index , &!
            index_self => melg64_self%index , &!
            state_init => melg64_init%state , &!
            state_self => melg64_self%state   &!
        )

            if (index_init .le. index_self) then

                diff2 = index_self               - index_init
                diff1 = melg64_self%state_size() - diff2

                do concurrent ( itr = (index_init + 1) : diff1 )
                    state_init(itr) = ieor( state_init(itr) , state_self(itr + diff2) )
                end do

                do concurrent ( itr = (diff1 + 1) : melg64_self%state_size() )
                    state_init(itr) = ieor( state_init(itr) , state_self(itr - diff1) )
                end do

                do concurrent ( itr = MINVAL_STATE_INDEX : index_init )
                    state_init(itr) = ieor( state_init(itr) , state_self(itr + diff2) )
                end do

            else

                diff2 = index_init               - index_self
                diff1 = melg64_self%state_size() - diff2

                do concurrent ( itr = (index_init + 1) : melg64_self%state_size() )
                    state_init(itr) = ieor( state_init(itr) , state_self(itr - diff2) )
                end do

                do concurrent ( itr = MINVAL_STATE_INDEX : diff2 )
                    state_init(itr) = ieor( state_init(itr) , state_self(itr + diff1) )
                end do

                do concurrent ( itr = (diff2 + 1) : index_init )
                    state_init(itr) = ieor( state_init(itr) , state_self(itr - diff2) )
                end do

            end if

        end associate

    end subroutine



    subroutine allocate_state ( melg64, state_size )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded dummy argument(s) for this SUBROUTINE
        integer , intent(in) :: state_size

        allocate( melg64%state( MINVAL_STATE_INDEX : state_size ) )

    end subroutine



    subroutine copy_from ( melg64, source )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(in) :: source

        melg64%index    = source%index
        melg64%lung     = source%lung
        melg64%state(:) = source%state(:)
        melg64%selector = source%selector

    end subroutine



    subroutine deallocate_state ( melg64 )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        if ( allocated(melg64%state) ) then
            deallocate( melg64%state )
        end if

    end subroutine



    subroutine initialize_by_array_with_size ( melg64, init_key_size, init_key_val )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        integer , intent(in) :: init_key_size

        ! unbounded(s) dummy argument for this SUBROUTINE
        integer(INT64) , intent(in) :: init_key_val (init_key_size)

        ! constant(s) for this SUBROUTINE
        integer(INT64) , parameter :: COEFFICIENT_1 = 3935559000370003845_INT64
        integer(INT64) , parameter :: COEFFICIENT_2 = 2862933555777941757_INT64

        ! support variable(s) for this SUBROUTINE
        integer(INT64) :: itr
        integer(INT64) :: itr_key
        integer(INT64) :: itr_state

        call melg64%initialize( melg64%default_seed() )

        associate( &!
            index => melg64%index , &!
            state => melg64%state   &!
        )

            itr_state = MINVAL_STATE_INDEX + 1
            itr_key   = MINVAL_STATE_INDEX

            do itr = max( melg64%state_size(), init_key_size ), 1, -1

                state(itr_state) &!
                    = ieor( state(itr_state), mat3pos( state(itr_state - 1), 62 ) * COEFFICIENT_1 ) &!
                    + init_key_val(itr_key) &!
                    + (itr_key - 1)

                itr_key   = itr_key   + 1
                itr_state = itr_state + 1

                call cycle_state

                if ( itr_key .gt. init_key_size ) then
                    itr_key = 1
                end if

            end do

            do itr = ( melg64%state_size() - 1 ), MINVAL_STATE_INDEX, -1

                state(itr_state) &!
                    = ieor( state(itr_state), mat3pos( state(itr_state - 1), 62 ) * COEFFICIENT_2 ) &!
                    - (itr_state - 1)

                itr_state = itr_state + 1

                call cycle_state

            end do

            melg64%lung &!
                = ieor( melg64%lung, mat3pos( state( melg64%state_size() ), 62 ) * COEFFICIENT_2 ) &!
                - melg64%state_size()

            state(MINVAL_STATE_INDEX) = ior( state(MINVAL_STATE_INDEX), shiftl( 1_INT64, 63 ) )

            index = MINVAL_STATE_INDEX

        end associate



        contains



        subroutine cycle_state

            if ( itr_state .gt. melg64%state_size() ) then

                melg64%state( MINVAL_STATE_INDEX  ) = &!
                melg64%state( melg64%state_size() )

                itr_state = MINVAL_STATE_INDEX + 1

            end if

        end subroutine

    end subroutine initialize_by_array_with_size



    subroutine initialize_by_array_without_size ( melg64, init_key )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        integer(INT64) , intent(in) :: init_key(:)

        call melg64%initialize_by_array_with_size( &!
            init_key_size = size( init_key(:) ) , &!
            init_key_val  =       init_key(:)     &!
        )

    end subroutine



    subroutine initialize_by_scalar ( melg64, seed )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        integer(INT64) , intent(in) :: seed

        ! constant(s) for this SUBROUTINE
        integer(INT64) , parameter :: COEFFICIENT = 6364136223846793005_INT64

        ! support variable(s) for this SUBROUTINE
        integer :: itr

        call melg64%reallocate_state( melg64%state_size() )

        associate( &!
            index    => melg64%index    , &!
            state    => melg64%state    , &!
            selector => melg64%selector   &!
        )

            state(MINVAL_STATE_INDEX) = seed

            do itr = (MINVAL_STATE_INDEX + 1), melg64%state_size()
                state(itr) = COEFFICIENT * mat3pos( state( itr - 1 ), 62 ) + ( itr - 1 )
            end do

            melg64%lung = COEFFICIENT * mat3pos( state( itr - 1 ), 62 ) + ( itr - 1 )

            index    = MINVAL_STATE_INDEX
            selector = SELECTOR_CASE_1

        end associate

    end subroutine



    subroutine initialize_for_jump ( melg64_self, melg64_source )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64_self

        ! unbounded dummy argument(s) for this SUBROUTINE
        class(type_melg64_abstract) , intent(in) :: melg64_source

        call melg64_self%reallocate_state( melg64_source%state_size() )

        melg64_self%lung     = 0_INT64
        melg64_self%state(:) = 0_INT64
        melg64_self%index    = melg64_source%index
        melg64_self%selector = melg64_source%selector

    end subroutine



    subroutine jump_unit ( melg64_self, melg64_init, jump_char )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64_self

        ! unbounded dummy argument(s) for this SUBROUTINE
        character( len= 1 ) , intent(in) :: jump_char

        ! unbounded dummy argument(s) for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64_init

        ! variable(s) for this SUBROUTINE
        integer :: itr_add
        integer :: bits
        integer :: mask

        ! variable(s) for this SUBROUTINE
        integer(INT64) :: harvest

        if ( ( 'a' .le. jump_char ) .and. ( jump_char .le. 'f' ) ) then
            bits = iachar( jump_char ) - ASCII_CODE_SMALL_A + 10
        else
            bits = iachar( jump_char ) - ASCII_CODE_DIGIT_0
        end if

        bits = iand( bits, 15 )
        mask = 8

        do itr_add = 1, 4

            if ( iand( bits, mask ) .ne. 0 ) then
                call melg64_self%add(melg64_init)
            end if

            call melg64_self%genrand64_int64(harvest)
            mask = shiftr( mask, 1 )

        end do

    end subroutine



    subroutine genrand64_int64 ( melg64, harvest )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        integer(INT64) , intent(out) :: harvest

        associate( &!
            index    => melg64%index    , &!
            lung     => melg64%lung     , &!
            state    => melg64%state    , &!
            selector => melg64%selector   &!
        )

            select case (selector)

                case(SELECTOR_CASE_1)

                    ! STEP.01
                    harvest      = initialize_harvest_123()
                    lung         = update_lung( index + melg64%shift_mm() )
                    state(index) = ieor( harvest, mat3pos( lung, melg64%shift_lung_pos() ) )
                    harvest      = finalize_harvest( melg64%lag_1() )

                    ! STEP.02
                    index = index + 1

                    ! STEP.03
                    if ( index .gt. ( melg64%state_size() - melg64%shift_mm() ) ) then
                        selector = SELECTOR_CASE_2
                    end if

                case(SELECTOR_CASE_2)

                    ! STEP.01
                    harvest      = initialize_harvest_123()
                    lung         = update_lung( index + ( melg64%shift_mm() - melg64%state_size() ) )
                    state(index) = ieor( harvest, mat3pos( lung, melg64%shift_lung_pos() ) )
                    harvest      = finalize_harvest( melg64%lag_1() )

                    ! STEP.02
                    index = index + 1

                    ! STEP.03
                    if ( index .gt. melg64%lag_1_over() ) then
                        selector = SELECTOR_CASE_3
                    end if

                case(SELECTOR_CASE_3)

                    ! STEP.01
                    harvest      = initialize_harvest_123()
                    lung         = update_lung( index + melg64%shift_mm() - melg64%state_size() )
                    state(index) = ieor( harvest, mat3pos( lung, melg64%shift_lung_pos() ) )
                    harvest      = finalize_harvest( - melg64%lag_1_over() )

                    ! STEP.02
                    index = index + 1

                    ! STEP.03
                    if ( index .eq. melg64%state_size() ) then
                        selector = SELECTOR_CASE_4
                    end if

                case(SELECTOR_CASE_4)

                    ! STEP.01
                    harvest                      = initialize_harvest_4()
                    lung                         = update_lung( melg64%shift_mm() )
                    state( melg64%state_size() ) = ieor( harvest, mat3pos( lung, melg64%shift_lung_pos() ) )
                    harvest                      = finalize_harvest( - melg64%lag_1_over() )

                    ! STEP.02
                    index = MINVAL_STATE_INDEX

                    ! STEP.03
                    selector = SELECTOR_CASE_1

            end select

        end associate



        contains



        integer(INT64) pure elemental function initialize_harvest ( state_index_u, state_index_l ) result(initial_harvest)

            ! dummy argument(s) for this FUNCTION
            integer, intent(in) :: state_index_u, state_index_l

            initial_harvest = &!
                ior( &!
                    iand( melg64%state( state_index_u ) , melg64%mask_u() ) , &!
                    iand( melg64%state( state_index_l ) , melg64%mask_l() )   &!
                )

        end function



        integer(INT64) pure elemental function initialize_harvest_123 () result(initial_harvest)

            associate( index => melg64%index )

                initial_harvest = &!
                    initialize_harvest( &!
                        state_index_u = index     , &!
                        state_index_l = index + 1   &!
                    )

            end associate

        end function



        integer(INT64) pure elemental function initialize_harvest_4 () result(initial_harvest)

            initial_harvest = &!
                initialize_harvest( &!
                    state_index_u = melg64%state_size() , &!
                    state_index_l = MINVAL_STATE_INDEX    &!
                )

        end function



        integer(INT64) pure elemental function finalize_harvest ( offset_index ) result(finalized_harvest)

            ! unbounded dummy argument(s) for this FUNCTION
            integer , intent(in) :: offset_index

            associate( &!
                index => melg64%index , &!
                state => melg64%state   &!
            )

                finalized_harvest = &!
                    ieor( &!
                        mat3neg( state(index), melg64%shift_1() ) , &!
                        iand( state( index + offset_index ), melg64%mask_1() ) &!
                    )

            end associate

        end function



        integer(INT64) pure elemental function update_lung ( state_index ) result(updated_lung) 

            ! dummy argument(s) for this FUNCTION
            integer, intent(in) :: state_index

            updated_lung = &!
                ieor( &!
                    shiftr( harvest, 1 ) , &!
                    melg64%mag01(harvest) , &!
                    melg64%state( state_index ) , &!
                    mat3neg( melg64%lung, melg64%shift_lung_neg() ) &!
                )

        end function



    end subroutine



    subroutine genrand64_int64_non_negative ( melg64, harvest )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        integer(INT64) , intent(out) :: harvest

        call melg64%genrand64_int64(harvest)
        harvest = shiftr( harvest, 1 )

    end subroutine



    ! Generates a random number on [0,1]-real-interval
    subroutine genrand64_real1 ( melg64, harvest )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        real(REAL64) , intent(out) :: harvest

        ! variable(s) for this SUBROUTINE
        integer(INT64) :: harvest_int64

        call melg64%genrand64_int64(harvest_int64)
        harvest = real( shiftr(harvest_int64, 11) , REAL64 ) * PI253_1

    end subroutine



    ! Generates a random number on [0,1)-real-interval
    subroutine genrand64_real2 ( melg64, harvest )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        real(REAL64) , intent(out) :: harvest

        ! variable(s) for this SUBROUTINE
        integer(INT64) :: harvest_int64

        call melg64%genrand64_int64(harvest_int64)
        harvest = real( shiftr(harvest_int64, 11) , REAL64 ) * PI253

    end subroutine



    ! Generates a random number on (0,1)-real-interval
    subroutine genrand64_real3 ( melg64, harvest )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        real(REAL64) , intent(out) :: harvest

        ! variable(s) for this SUBROUTINE
        integer(INT64) :: harvest_int64

        call melg64%genrand64_int64(harvest_int64)
        harvest = ( real( shiftr(harvest_int64, 12) , REAL64 ) + 0.5_REAL64 ) * PI252

    end subroutine



    ! Generates a random number on [0,1)-real-interval with 53-bit significant bits
    subroutine genrand64_res53 ( melg64, harvest )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded(s) dummy argument for this SUBROUTINE
        real(REAL64) , intent(out) :: harvest

        ! variable(s) for this SUBROUTINE
        integer(INT64) :: harvest_int64

        call melg64%genrand64_int64(harvest_int64)
        harvest = real( shiftr(harvest_int64, 11) , REAL64 ) * COEF_RES53

    end subroutine



    subroutine reallocate_state ( melg64, state_size )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded dummy argument(s) for this SUBROUTINE
        integer , intent(in) :: state_size

        call melg64 % deallocate_state
        call melg64 %   allocate_state( state_size )

    end subroutine

end module
