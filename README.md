# NASTRAN-95 - a cross-platform and modernized fork

This fork aims is to provide an easy compilable modification of the original NASTRAN-95 release. The goal is to provide clear instructions and resources for users interested in exploring and utilizing the NASTRAN-95 source code.

## Introduction

NASTRAN (NASA Structural Analysis System) is a pioneering finite element analysis (FEA) program developed in the early 1970s. It revolutionized the field of computer-aided engineering, allowing engineers to model and analyze complex structures with remarkable accuracy and efficiency. NASTRAN's capabilities encompass a wide range of structural analysis functionalities, including:

* Static Analysis: Determining the response of structures to static loads, thermal expansion, and enforced deformations.
* Dynamic Analysis: Studying structural behavior under dynamic loads, including vibration analysis, transient response, and frequency response.
* Elastic Stability Analysis: Evaluating the buckling strength of structures under compressive loads.
Complex Eigenvalue Analysis: Analyzing vibration and dynamic stability issues involving complex eigenvalues.
* Random Response Analysis: Examining structural behavior under random excitations.

## Getting Started
### Accessing the Source Code

The NASTRAN-95 source code is available on GitHub under the nasa/NASTRAN-95 repository. You can access the code by either cloning the repository or downloading the source files directly.

### Licensing

NASTRAN-95 is released under the NASA Open Source Agreement version 1.3. Please review the license agreement carefully before using the software.

### Building and Running NASTRAN

Building and running NASTRAN-95 can be a complex process, depending on your operating system and development environment. The original documentation provides guidance for specific systems available in the 1970s, but it may require adaptation to modern systems.

To assist you in building and running NASTRAN-95, we recommend exploring the following resources:

* Programmer's Manual: This manual provides detailed information on the program's structure, subroutines, and modules.
* User's Manual: This manual offers instructions on how to use NASTRAN-95 to solve various structural analysis problems.

* NASTRAN Primer Static and Normal Modes Analysis: This book by Harry G. Schaeffer offers a practical introduction to using NASTRAN for static and normal modes analysis.

While official technical support is not available for NASTRAN-95, there are online communities and forums where you can connect with other users and seek assistance.

### Disclaimer

Please note that NASTRAN-95 is a historical software program and may require significant effort to adapt to modern computing environments. We encourage users to explore the source code and documentation with a spirit of exploration and historical interest.

Important: As with any historical software, it is crucial to thoroughly validate and verify the results obtained from NASTRAN-95 before relying on them for any critical applications.


### Aim of this Fork
The primary objective of this fork is to simplify the building and compilation of the NASTRAN-95 codebase on modern platforms. We are particularly focusing on supporting various operating systems such as:

* Windows
* macOS
* GNU/Linux (with a focus on Ubuntu)

Additionally, we aim to ensure compatibility with different compilers, including:

* Intel's ifort
* GNU's GFortran

## Compile:

* clone the repository `git clone https://github.com/Foadsf/NASTRAN-95.git`
* go to the directory, eg., `cd NASTRAN-95`

### Windows:

1. install CMake, you may use Chocolatey `choco install cmake` in an elevated PowerShell terminal
2. install Intel's Fortran compiler, assuming you already have `nmake.exe` and `ninja.exe` installed via Microsoft Visual Studio
3. compile with `build.bat` and the executables should be in the `build\bin` folder

### Linux

1. assuming you have build essentials including GNU's GFortran, CMake, and Ninja installed
2. compile with `./build.sh` and the executables should be in the `build/bin` folder