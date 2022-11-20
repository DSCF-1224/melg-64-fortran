# Reference Data for MELG-64-fortran

## Original Implementation

The files in this directory are based on [S. Harase and T. Kimoto's implementation](https://github.com/sharase/melg-64).

## Files in this directory

- README.md
  - the present file

- [include](include)
  - [main.c](include/main.c)
    - The `main()` is defined in this source file.
    - Export the generated samples by MELG-64.
    - This source file is used for all following tests.
    - The format of `printf` is changed for Fortran from the original implementation.</br>(It is because, Fortran does not support the unsigned integer...)

- [melg607-64](melg607-64)
  - [melg607-64.c](melg607-64/melg607-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg607-64/melg607-64.c), except for `main()`.
  - [test.sh](melg607-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg607-64.txt](melg607-64/melg607-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

- [melg1279-64](melg1279-64)
  - [melg1279-64.c](melg1279-64/melg1279-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg1279-64/melg1279-64.c), except for `main()`.
  - [test.sh](melg1279-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg1279-64.txt](melg1279-64/melg1279-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

- [melg2281-64](melg2281-64)
  - [melg2281-64.c](melg2281-64/melg2281-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg2281-64/melg2281-64.c), except for `main()`.
  - [test.sh](melg2281-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg2281-64.txt](melg2281-64/melg2281-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

- [melg4253-64](melg4253-64)
  - [melg4253-64.c](melg4253-64/melg4253-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg4253-64/melg4253-64.c), except for `main()`.
  - [test.sh](melg4253-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg4253-64.txt](melg4253-64/melg4253-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

- [melg11213-64](melg11213-64)
  - [melg11213-64.c](melg11213-64/melg11213-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg11213-64/melg11213-64.c), except for `main()`.
  - [test.sh](melg11213-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg11213-64.txt](melg11213-64/melg11213-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

- [melg19937-64](melg19937-64)
  - [melg19937-64.c](melg19937-64/melg19937-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg19937-64/melg19937-64.c), except for `main()`.
  - [test.sh](melg19937-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg19937-64.txt](melg19937-64/melg19937-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

- [melg44497-64](melg44497-64)
  - [melg44497-64.c](melg44497-64/melg44497-64.c)
    - This source file is identical to the [original implementation](https://github.com/sharase/melg-64/blob/master/melg44497-64/melg44497-64.c), except for `main()`.
  - [test.sh](melg44497-64/test.sh)
    - For compile and execute the reference implementation.
  - [melg44497-64.txt](melg44497-64/melg44497-64.txt)
    - The result of the reference implementation.</br>This file is required for testing Fortran implementations.

## Used environment

- WSL Ubuntu-20.04
- gcc (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0

<!-- EOF -->
