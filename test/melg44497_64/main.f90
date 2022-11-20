program test_melg44497_64

    ! required MODULE(s)
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: melg44497_64
    use , non_intrinsic :: melg_64_test , only: reproduce_original_test

    ! require all variables to be explicitly declared
    implicit none

    ! variable(s) for this PROGRAM
    type(type_melg44497_64) :: melg64

    call reproduce_original_test( melg64, OUTPUT_UNIT, 1000 )

end program test_melg44497_64
