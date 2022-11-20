# Fortran-2008 Implementation of MELG-64

## Original Implementation

The files in this directory are based on [S. Harase and T. Kimoto's implementation](https://github.com/sharase/melg-64).

## Used environment

- WSL Ubuntu-20.04
- GNU Fortran (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0

## Implemented `SUBROUTINE`

|`SUBROUTINE`                  |MELG607    |MELG1279   |MELG2281   |MELG4253   |MELG11213  |MELG19937  |MELG44497  |Description|
|:-----------------------------|:---------:|:---------:|:---------:|:---------:|:---------:|:---------:|:---------:|:-|
|`genrand64_int64`             |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|
|`genrand64_int64_non_negative`|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|
|`initialize`                  |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|using a scalar|
|`initialize`                  |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|using a 1D-array|
|`jump`                        |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|
|`genrand64_real1`             |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|
|`genrand64_real2`             |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|
|`genrand64_real3`             |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|
|`genrand64_fast_res52`        |&cross;    |&cross;    |&cross;    |&cross;    |&cross;    |&cross;    |&cross;    |
|`genrand64_fast_res52_open`   |&cross;    |&cross;    |&cross;    |&cross;    |&cross;    |&cross;    |&cross;    |
|`genrand64_res53`             |&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|&checkmark;|

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%genrand64_int64(harvest)`

- Objective
  - Generate a random number on [-2<sup>63</sup>, 2<sup>63</sup>-1]-interval
- Argument
  - `harvest`
    - Type&colon;&ensp;`integer(kind=INT64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%genrand64_int64_non_negative(harvest)`

- Objective
  - Generate a random number on [0, 2<sup>63</sup>-1]-interval
- Argument
  - `harvest`
    - Type&colon;&ensp;`integer(kind=INT64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%initialize(harvest)`

- Objective
  - Initialize the MELG-64 random number generator
- Argument
  - `harvest`
    - Type&colon;&ensp;scalar or 1D-array of `integer(kind=INT64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential
- Warning
  - You must call this `SUBROUTINE` before generating random numbers.

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%jump()`

- Objective
  - This is a jump function for the generator.
  - It is equivalent to 2^256 calls to `genrand64_int64()`.
- Argument
  - None

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%genrand64_real1(harvest)`

- Objective
  - Generate a random number on [0,1]-real-interval
- Argument
  - `harvest`
    - Type&colon;&ensp; `real(kind=REAL64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%genrand64_real2(harvest)`

- Objective
  - Generate a random number on [0,1)-real-interval
- Argument
  - `harvest`
    - Type&colon;&ensp;`real(kind=REAL64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%genrand64_real3(harvest)`

- Objective
  - Generate a random number on (0,1)-real-interval
- Argument
  - `harvest`
    - Type&colon;&ensp;`real(kind=REAL64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential

### `SUBROUTINE`&ensp;&colon;&ensp;`melg64%genrand64_res53(harvest)`

- Objective
  - Generate a random number on [0,1)-real-interval with 53-bit significant bits
- Argument
  - `harvest`
    - Type&colon;&ensp;`real(kind=REAL64)`
    - Intent&colon;&ensp;`intent(out)`
    - Optional&colon;&ensp;essential

## Files in this directory

- README.md
  - the present file
- [melg_64_utility.f90](melg_64_utility.f90)
  - `MODULE`&colon;&ensp;`melg_64_utility` is defined in this source file.
    - This `MODULE` depends on the intrinsic ones&colon;&ensp;`ISO_FORTRAN_ENV` and `ISO_C_BINDING`.
- [melg607_64.f90](melg607_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg607-64/melg607-64.c).
  - `MODULE`&colon;&ensp;`melg607_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg607_64` is defined in this `MODULE`.
- [melg1279_64.f90](melg1279_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg1279-64/melg1279-64.c).
  - `MODULE`&colon;&ensp;`melg1279_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg1279_64` is defined in this `MODULE`.
- [melg2281_64.f90](melg2281_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg2281-64/melg2281-64.c).
  - `MODULE`&colon;&ensp;`melg2281_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg2281_64` is defined in this `MODULE`.
- [melg4253_64.f90](melg4253_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg4253-64/melg4253-64.c).
  - `MODULE`&colon;&ensp;`melg4253_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg4253_64` is defined in this `MODULE`.
- [melg11213_64.f90](melg11213_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg11213-64/melg11213-64.c).
  - `MODULE`&colon;&ensp;`melg11213_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg11213_64` is defined in this `MODULE`.
- [melg19937_64.f90](melg19937_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg19937-64/melg19937-64.c).
  - `MODULE`&colon;&ensp;`melg19937_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg19937_64` is defined in this `MODULE`.
- [melg44497_64.f90](melg44497_64.f90)
  - The original implementation is [here](https://github.com/sharase/melg-64/blob/master/melg44497-64/melg44497-64.c).
  - `MODULE`&colon;&ensp;`melg44497_64` is defined in this source file.
    - This `MODULE` depends on the intrinsic one&colon;&ensp;`ISO_FORTRAN_ENV`.
    - This `MODULE` depends on the non-intrinsic one&colon;&ensp;[melg_64_utility](melg_64_utility.f90).
    - A user-defined `TYPE`&colon;&ensp;`type_melg44497_64` is defined in this `MODULE`.

<!-- EOF -->
