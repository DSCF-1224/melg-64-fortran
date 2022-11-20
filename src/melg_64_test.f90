module melg_64_test

    ! required MODULE
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg_64_utility , only: type_melg64_abstract

    ! require all variables to be explicitly declared
    implicit none

    ! accessibility setting
    private
    public  :: reproduce_original_test



    ! definition of user-defined INTERFACE
    interface reproduce_original_test
        module procedure :: reproduce_original_test_melg64
    end interface reproduce_original_test



    contains



    subroutine reproduce_original_test_melg64 ( melg64, export_unit, sample_size )

        ! bounded dummy argument for this SUBROUTINE
        class(type_melg64_abstract) , intent(inout) :: melg64

        ! unbounded dummy argument(s) for this SUBROUTINE
        integer , intent(in) :: export_unit
        integer , intent(in) :: sample_size

        ! constant(s) for this SUBROUTINE
        integer(INT64) , parameter :: INIT_KEY(4) = [74565_INT64, 144470_INT64, 214375_INT64, 284280_INT64]

        ! variable(s) for this SUBROUTINE
        integer(INT64) :: harvest_int64

        ! variable(s) for this SUBROUTINE
        real(REAL64) :: harvest_real64

        ! support variable(s) for this SUBROUTINE
        integer :: itr

        call melg64%initialize( INIT_KEY(:) )

        write( unit= export_unit, fmt= '(I0,1X,A)' ) sample_size, "outputs of genrand64_int64()"

        do itr = 1, sample_size

            call melg64%genrand64_int64(harvest_int64)
            if ( mod( itr, 5 ) .ne. 1 ) write( unit= export_unit,  fmt= '(1X)', advance= 'NO' )!
            write( unit= export_unit,  fmt= '(Z20.16)', advance= 'NO' ) harvest_int64
            if ( mod( itr, 5 ) .eq. 0 ) write( unit= export_unit,  fmt= '(A)' ) ' '

            flush(export_unit)

        end do

        write( unit= export_unit, fmt= '(/,I0,1X,A)' ) sample_size, "outputs of genrand64_res53()"

        do itr = 1, sample_size

            call melg64%genrand64_res53(harvest_real64)
            if ( mod( itr, 5 ) .ne. 1 ) write( unit= export_unit,  fmt= '(1X)', advance= 'NO' )!
            write( unit= export_unit,  fmt= '(F17.15)',advance= 'NO' ) harvest_real64
            if ( mod( itr, 5 ) .eq. 0 ) write( unit= export_unit,  fmt= '(A)' ) ' '

            flush(export_unit)

        end do

        write( unit= export_unit, fmt= '(/,A)' ) "jump ahead by 2^256 steps"

        call melg64%jump

        write( unit= export_unit, fmt= '(I0,1X,A)' ) sample_size, "outputs of genrand64_int64()"

        do itr = 1, sample_size

            call melg64%genrand64_int64(harvest_int64)
            if ( mod( itr, 5 ) .ne. 1 ) write( unit= export_unit,  fmt= '(1X)', advance= 'NO' )!
            write( unit= export_unit,  fmt= '(Z20.16)', advance= 'NO' ) harvest_int64
            if ( mod( itr, 5 ) .eq. 0 ) write( unit= export_unit,  fmt= '(A)' ) ' '

            flush(export_unit)

        end do

    end subroutine

end module
