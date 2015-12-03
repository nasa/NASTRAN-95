=PAGE=
2.3  CASE CONTROL DECK

2.3.1  Data Selection

  The Case Control cards that are used for selecting items from the Bulk Data
Deck are listed below in functional groups. A detailed description of each
card is given in Section 2.3.4. The first four characters of the mnemonic are
sufficient if unique.

  The following Case Control cards are associated with the selection of
applied loads for both static and dynamic analysis:

  1. DEFORM - selects element deformation set.

  2. DLOAD - selects dynamic loading condition.

  3. DSCOEFFICIENT - selects loading factor for normal modes with
     differential stiffness.

  4. LOAD - selects static structural loading condition or heat power and/or
     flux.

  5. NONLINEAR - selects nonlinear loading condition for transient response.

  6. PLCOEFFICIENT - selects loading increments for piecewise linear
     analysis.

  The following case control cards are used for the selection of constraints:

  1. AXISYMMETRIC - selects boundary conditions for conical shell and
     axisymmetric solid elements; specifies the existence of fluid harmonics
     for a hydroelastic problem; or the applied source magnetic field in
     magnetostatic problem.

  2. MPC - selects set of multipoint constraints for structural displacement
     or heat transfer boundary temperature relationships.

  3. SPC - selects set of single-point constraints for structural
     displacements or heat transfer boundary temperatures.

  The following case control cards are used for the selection of direct input
matrices:

  1. B2PP - selects direct input structural damping or thermal capacitance
     matrices.

  2. K2PP - selects direct input structural stiffness or thermal conductance
     matrices.

  3. M2PP - selects direct input mass matrices.

  4. TFL - selects transfer functions.

  The following case control cards specify the conditions for dynamic
analyses:

  1. CMETHOD - selects the conditions for complex eigenvalue extraction.

  2. FREQUENCY - selects the frequencies to be used for frequency and random
     response calculations.

  3. IC - selects the initial conditions for direct transient response.

  4. METHOD - selects the conditions for real eigenvalue analysis.

  5. RANDOM - selects the power spectral density functions to be used in
     random analysis.

  6. SDAMPING - selects table to be used for determination of modal damping.

  7. TSTEP - selects time steps to be used for integration in transient
     response problems.

  8. FMETHOD - selects method to be used in aeroelastic flutter analysis.

  9. GUST - selects aerodynamic gust loading in aeroelastic response
     analysis.

  The following case control cards are associated with the use of thermal
fields:

  1. TEMPERATURE(LOAD) - selects thermal field to be used for determining
     equivalent static loads.

  2. TEMPERATURE(MATERIAL) - selects thermal field to be used for determining
     structural material properties or an estimate of the temperature
     distribution for heat transfer iterations.

  3. TEMPERATURE - selects thermal field for determining both equivalent
     static loads and material properties.

2.3.2  Output Selection

  Printer output requests may be grouped in packets following OUTPUT cards or
the individual requests may be placed anywhere in the Case Control Deck ahead
of any structure plotter or curve plotter requests. Plotter requests are
described in Section 4. The Case Control cards that are used for output
selection are listed below in functional groups. A detailed description of
each card is given in Section 2.3.4.

  The following cards are associated with output control, titling and bulk
data echoes:

  1. TITLE - defines a text to be printed on first line of each page of
     output.

  2. SUBTITLE - defines a text to be printed on second line of each page of
     output.

  3. LABEL - defines a text to be printed on third line of each page of
     output.

  4. LINE - sets the number of data lines per printed page, default is 50 for
     11-Inch paper.

  5. MAXLINES - sets the maximum number of output lines, default is 20000.

  6. ECHO - selects echo options for Bulk Data Deck, default is a sorted bulk
     data echo. Note: Echoes of the Executive Control and the Case Control
     decks are automatically printed and cannot be suppressed.

  The following cards are used in connection with some of the specific output
requests for calculated quantities:

  1. SET - defines lists of point numbers, elements numbers, or frequencies
     for use in output requests.

  2. OFREQUENCY - selects a set of frequencies to be used for output requests
     in frequency and aeroelastic response problems (default is all
     frequencies) or flutter velocities.

  3. TSTEP - selects a set of time steps to be used for output requests in
     transient response problems.

  4. OTIME - selects a set of times to be used for output requests in
     transient analysis problems (default is all times).

  The following cards are used to make output requests for the calculated
response of components in the SOLUTION set (components in the direct or modal
formulation of the general K system) for dynamics problems:

  1. SACCELERATION - requests the acceleration of the independent components
     for a selected set of points or modal coordinates.

  2. SDISPLACEMENT - requests the displacements of the independent components
     for a selected set of points or modal coordinates or the temperatures of
     the independent components for a selected set of points in heat
     transfer.

  3. SVELOCITY - requests the velocities of the independent components for a
     selected set of points or modal coordinates or the change in temperature
     with respect to time of the independent components for a selected set of
     points in heat transfer.

  4. NLLOAD - requests the nonlinear loads for a selected set of physical
     points (grid points and extra points introduced for dynamic analysis) in
     transient response problems.

  The following cards are used to make output requests for stresses and
forces, as well as calculated response of degrees of freedom used in the
model:

  1. FORCE or ELFORCE - requests the forces in a set of structural elements
     or the temperature gradients and fluxes in a set of structural or heat
     elements in heat transfer.

  2. STRESS or ELSTRESS - requests the stresses in a set of structural
     elements or the velocity components in a fluid element in acoustic
     cavity analysis.

  3. SPCFORCES - requests the single-point forces of constraint at a set of
     points or the thermal power transmitted to a selected set of points in
     heat transfer.

  4. OLOAD - selects a set of applied loads for output.

  5. ACCELERATION - requests the accelerations for a selected set of PHYSICAL
     points (grid, scalar and fluid points plus extra points introduced for
     dynamic analysis).

  6. DISPLACEMENT - requests the displacements for a selected set of PHYSICAL
     points or the temperatures for a selected set of PHYSICAL points in heat
     transfer or the pressures for a selected set of PHYSICAL points in
     hydroelasticity.

  7. VELOCITY - requests the velocities for a selected set of PHYSICAL points
     or the change in temperatures with respect to time for a selected set of
     PHYSICAL points in heat transfer.

  8. HARMONICS - controls the number of harmonics that will be output for
     requests associated with the conical shell, axisymmetric solids and
     hydroelastic problems.

  9. ESE - requests structural element strain energies in Rigid Format 1.

  10.  GPFORCE - requests grid point force balance due to element forces,
       forces of single point constraint, and applied loads in Rigid Format
       1.

  11.  THERMAL - requests temperatures for a set of PHYSICAL points in heat
       transfer.

  12.  PRESSURE - requests pressures for a set of PHYSICAL points in
       hydroelasticity.

  13.  VECTOR - requests displacements for a selected set of PHYSICAL
       points.

  14.  MPCFORCE - requests multipoint forces of constraint at a set of
       points in Rigid Formats 1, 2, 3, 14, and 15.

  15.  NCHECK - requests significant digits to indicate numerical accuracy
       of element stress and force computations.

  16.  AEROF - requests frequency-dependent aerodynamic loads on
       interconnection points in aeroelastic response analysis.

  17.  STRAIN - requests the strains/curvatures in a set of structural
       elements (applicable to TRIA1, TRIA2, QUAD1, and QUAD2 only).

  18.  SCAN - SCANs output data and eliminates values that do not meet the
       specification set by this SCAN card.

2.3.3  Subcase Definition

  In general, a separate subcase is defined for each loading condition. In
statics problems separate subcases are also defined for each set of
constraints. In complex eigenvalue analysis and frequency response separate
subcases are defined for each unique set of direct input matrices. Subcases
may be used in connection with output requests, such as in requesting
different output for each mode in a real eigenvalue problem.

  The Case Control Deck is structured so that a minimum amount of repetition
is required. Only one level of subcase definition is necessary. All items
placed above the subcase level (ahead of the first subcase) will be used for
all following subcases, unless overridden within the individual subcase.

  In statics problems, subcases may be combined through the use of the SUBCOM
feature. Individual loads may be defined in separate subcases and then
combined by the SUBCOM. If the loads are mechanical, the responses are
combined as shown in example 2, which follows. If a thermal load is involved,
the responses due to mechanical and thermal loads may be recovered as shown in
example 1. By redefining the thermal load(s) at the SUBCOM level, stresses and
forces may be recovered.

  In statics problems, provision has been made for the combination of the
results of several subcases. This is convenient for studying various
combinations of individual loading conditions and for the superposition of
solutions for symmetrical and antisymmetrical boundaries.

  Typical examples of subcase definition are given following a brief
description of the cards used in subcase definitions.

  The following case control cards are associated with subcase definition:

  1. SUBCASE - defines the beginning of a subcase that is terminated by the
     next subcase delimiters encountered.

  2. SUBCOM - defines a combination of two or more immediately preceding
     subcases in statics problems. Output requests above the subcase level
     are used.

  3. SUBSEQ  - must appear in a subcase defined by SUBCOM to give the
     coefficients for making the linear combination of the preceding
     subcases.

  4. SYM - defines a subcase in statics problems for which only output
     requests within the subcase will be honored. Primarily for use with
     symmetry problems where the individual parts of the solution may not be
     of interest.

  5. SYMCOM - defines a combination of two or more immediately preceding SYM
     subcases in static problems. Output requests above the subcase level are
     used.

  6. SYMSEQ - may appear in a subcase defined by SYMCOM to give the
     coefficient for making the linear combination of the preceding SYM
     subcases. A default value of 1.0 is used if no SYMSEQ card appears.

  7. REPCASE - defines a subcase in statics problems that is used to make
     additional output requests for the previous real subcase. This card is
     required because multiple output requests for the same item are not
     permitted in the same subcase. Output requests above the subcase level
     are still used. .

  8. MODES - controls the output for a given subcase as specified by the
     number of modes, otherwise all modes will be used.

  The following examples of Case Control Decks indicate typical ways of
defining subcases:

1. Static analysis with multiple loads

  OUTPUT
      DISPLACEMENT = ALL
  MPC = 3
      SUBCASE 1
          SPC = 2
          TEMPERATURE(LOAD) = 101
          LOAD = 11
      SUBCASE 2
          SPC = 2
          DEFORM = 52
          LOAD = 12
      SUBCASE 3
          SPC = 4
          LOAD = 12
      SUBCASE 4
          MPC = 4
          SPC = 4

Four subcases are defined in this example. The displacements at all grid
points will be printed for all four subcases. MPC = 3 will be used for the
first three subcases and will be overridden by MPC = 4 in the last subcase.
Since the constraints are the same for subcases 1 and 2 and the subcases are
contiguous, the static solutions will be performed simultaneously. In subcase
1, thermal load 101 and external load 11 are internally superimposed, as are
the external and deformation loads in subcase 2. In subcase 4 the static
loading will result entirely from enforced displacements of grid points.

2. Linear combination of subcases

  SPC = 2
  OUTPUT
      SET 1 = 1 THRU 10,20,30
      DISPLACEMENT = ALL
      STRESS = 1
  SUBCASE 1
      LOAD = 101
      OLOAD = ALL
  SUBCASE 2
      LOAD = 201
      OLOAD = ALL
  SUBCOM 51
      SUBSEQ = 1.0,1.0
  SUBCOM 52
      SUBSEQ = 2.5,1.5

Two static loading conditions are defined in subcases 1 and 2. SUBCOM 51
defines the sum of subcases 1 and 2. SUBCOM 52 defines a linear combination
consisting of 2.5 times subcase 1 plus 1.5 times subcase 2. The displacements
at all grid points and the stresses for the element numbers in SET will be
printed for all four subcases. In addition, the nonzero components of the
static load vectors will be printed for subcases 1 and 2.

3. Statics problem with one plane of symmetry

  OUTPUT
      SET 1 = 1,11,21,31,51
      SET 2 = 1 THRU 10, 101 THRU 110
      DISPLACEMENT = 1
      ELFORCE = 2
  SYM 1
      SPC = 11
      LOAD = 21
      OLOAD = ALL
  SYM 2
      SPC = 12
      LOAD = 22
  SYMCOM 3
  SYMCOM 4
      SYMSEQ 1.0,-1 .0

Two SYM subcases are defined in subcases 1 and 2. SYMCOM 3 defines the sum and
SYMCOM 4 the difference of the two SYM subcases. The nonzero components of the
static load will be printed for subcase 1 and no output is requested for
subcase 2. The displacements for the grid point numbers in set 1 and the
forces for elements in set 2 will be printed for subcases 3 and 4.

4. Use of REPCASE in statics problems

  SET 1 = 1 THRU 10, 101 THRU 110, 201 THRU 210
  SET 2 = 21 THRU 30, 121 THRU 130, 221 THRU 230
  SET 3 = 31 THRU 40, 131 THRU 140, 231 THRU 240
      SUBCASE 1
          LOAD =10
          SPC = 11
          DISPLACEMENT = ALL
          SPCFORCE = 1
          ELFORCE = 1
      REPCASE 2
          ELFORCE = 2
      REPCASE 3
          ELFORCE = 3

This example defines one subcase for solution and two subcases for output
control. The displacements at all grid points and the nonzero components of
the single-point forces of constraint along with forces for the elements in
SET 1 will be printed for SUBCASE 1. The forces for elements in SET 2 will be
printed for REPCASE 2 and the forces for elements in SET 3 will be printed for
REPCASE 3.

5. Use of MODES in eigenvalue problems

      METHOD = 2
      SPC = 10
  SUBCASE 1
      DISPLACEMENT = ALL
      STRESS = ALL
      MODES = 2
  SUBCASE 3
      DISPLACEMENT = ALL

In this example the displacements at all grid points will be printed for all
modes. The stresses in all elements will be printed for the first two modes.

2.3.4  Case Control Card Descriptions

The format of the Case Control cards is free-field. In presenting general
formats for each card embodying all options, the following conventions are
used:

1. Upper-case letters and parentheses must be punched as shown.

2. Lower-case letters indicate that a substitution must be made.

                   É »
3. Double brackets º º indicate that a choice of contents is mandatory.
                   È ¼

            Ú ¿
4. Brackets ³ ³ contain an option that may be omitted or included by you.
            À Ù

5. First listed options or values are the default values.

6. Physical card consists of information punched in columns 1 through 72 of a
   card. Most case control cards are limited to a single physical card.

7. Logical card may have more than 72 columns with the use of continuation
   cards. A continuation card is honored by ending the preceding card with
   a comma.

  The structure plotter output request packet and the x-y output request
packet, while part of the Case Control Deck, are treated separately in
Sections 4.2 and 4.3, respectively.
=PAGE=
ACCELERATION - Acceleration Output Request

Description

Requests form and type of acceleration vector output.

Format and Example(s)

             Ú                           ¿      É       »
ACCELERATION ³ ( SORT1 , PRINT , REAL  ) ³   =  º  ALL  º
             ³   SORT2   PUNCH   IMAG    ³      º  n    º
             ³                   PHASE   ³      º  NONE º
             À                           Ù      È       ¼

ACCELERATION = 5
ACCELERATION(SORT2, PHASE) = ALL
ACCELERATION(SORT1, PRINT, PUNCH, PHASE) = 17

Option    Meaning

SORT1     Output will be presented as a tabular listing of grid points for
          each load, frequency, eigenvalue, or time, depending on the rigid
          format. SORT1 is not available in transient problems (where the
          default is SORT2).

SORT2     Output will be presented as a tabular listing of frequency or time
          for each grid point. SORT2 is available only in transient and
          frequency response problems.

PRINT     The printer will be the output device.

PUNCH     The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on frequency response
          problems.

PHASE     Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
          frequency response problems.

ALL       Accelerations for all points will be output.

n         Set identification of a previously appearing SET card. Only
          accelerations of points whose identification numbers appear on
          this SET card will be output (Integer > 0).

NONE      Accelerations for no points will be output.

Remarks

1.Both PRINT and PUNCH may be requested.

2.An output request for ALL in transient and frequency response problems
  generally produces large amounts of printout. An alternative to this would
  be to define a SET of interest.

3.Acceleration output is only available for transient and frequency response
  problems.

4.In a frequency response problem any request for SORT2 output causes all
  output to be SORT2.

5.ACCELERATION = NONE allows overriding an overall output request.
=PAGE=
AEROF - Aerodynamic Force Output Request

Description

Requests the aerodynamic loads on the interconnection points.

Format and Example(s)

AEROF = n

AEROF = ALL

AEROF = 5

Option    Meaning

n         Set identification of a previously appearing SET card. Only
          aerodynamic forces on points referenced will be output.

ALL       Aerodynamic forces on all points will be output.

Remarks

1.Only frequency-dependent forces may be requested (frequency response or
  random analysis).

2.The point identification numbers are the box or body element IDs.

3.The dimensions of the output are force (or moment) per unit dynamic
  pressure.
=PAGE=
AXISYMMETRIC - Boundary Conditions, Hydroelastic Harmonics, or Magnetic Field

Description

Selects boundary conditions for problems containing CCONEAX, CTRAPAX, or
CTRIAAX elements; specifies the existence of fluid harmonics for hydroelastic
problems; or specifies the applied source magnetic field in the magnetostatics
problem.

Format and Example(s)

               É          »
               º SINE     º
               º COSINE   º
               º FLUID    º
AXISYMMETRIC = º ANOM     º
               º ANTIANOM º
               º SYMM     º
               º ANTISYMM º
               º SYMMANOM º
               È          ¼

AXISYMMETRIC = COSINE

Option    Meaning

SINE      Sine boundary conditions will be used.

COSINE    Cosine boundary conditions will be used.

FLUID     Existence of fluid harmonics.

SYMM, ANTISYMM, ANOM, ANTIANOM, SYMMANON  Used in magnetostatics problems.

Remarks

1.This card is required for problems containing the elements named above.

2.If this card is used for hydroelastic problems, at least one harmonic must
  be specified on the AXIF card.

3.See Section 1.3.6 of User's Manual for a discussion of the conical shell
  problem.

4.See Section 1.3.7 of User's Manual for a discussion of the axisymmetric
  solid problem.

5.See Section 1.7.1 of User's Manual for a discussion of the hydroelastic
  formulation.

6.The sine boundary condition will constrain components 1, 3, and 5 at every
  ring for the zero harmonic.

7.The cosine boundary condition will constrain components 2, 4, and 6 at
  every ring for the zero harmonic.

8.SPC and MPC case control cards may also be used to apply additional
  constraints.

9.See PROLATE bulk data card for magnetostatic problem involving the prolate
  spheroidal surface harmonic expansion.
=PAGE=
BEGIN BULK - End of Case Control Deck

Description

Indicates the end of the Case Control Deck directives and controls. Cards
appearing after this card are assumed to be Bulk Data Deck cards.

Format and Example(s)

BEGIN BULK
=PAGE=
B2PP - Direct Input Damping Matrix Selection

Description

Selects a direct input damping matrix.

Format and Example(s)

B2PP  =  name

B2PP  =  BDMIG

B2PP  =  B2PP

Option    Meaning

name      BCD name of [B2pp] matrix that is input on the DMIG or DMIAX bulk
          data card.

Remarks

1.B2PP is used only in dynamics problems.

2.DMIG and DMIAX matrices will not be used unless selected.
=PAGE=
CMETHOD - Complex Eigenvalue Extraction Method Selection

Description

Selects complex eigenvalue extraction data to be used by module CEAD.

Format and Example(s)

CMETHOD  =  n

CMETHOD  =  77

Option    Meaning

n         Set identification of EIGC (and EIGP) card (Integer > 0).

Remarks

1.Eigenvalue extraction data must be selected when extracting complex
  eigenvalues using functional module CEAD.
=PAGE=
DEFORM - Element Deformation Static Load

Description

Selects the element deformation set to be applied to the structural model.

Format and Example(s)

DEFORM = n

DEFORM = 27

Option    Meaning

n         Set identification of DEFORM cards (Integer > 0).

Remarks

1.DEFORM bulk data cards will not be used unless selected in the Case Control
  Deck.

2.DEFORM is only applicable in statics, inertia relief, differential
  stiffness, and buckling problems.

3.The total load applied will be the sum of external, (LOAD), thermal
  (TEMP(LOAD)), element deformation (DEFORM), and constrained displacement
  loads (SPC).

4.Static, thermal, and element deformation loads should have unique
  identification numbers.
=PAGE=
DISPLACEMENT - Displacement Output Request

Description

Requests form and type of displacement vector output.

Format and Example(s)

               Ú                         ¿       É      »
DISPLACEMENT   ³ ( SORT1, PRINT, REAL  ) ³       º ALL  º
               ³   SORT2  PUNCH  IMAG    ³  =    º  n   º
               ³         NOPRINT PHASE   ³       º NONE º
               À                         Ù       È      ¼

DISPLACEMENT  =  5

DISPLACEMENT(REAL)  =  ALL

DISPLACEMENT(SORT2, PUNCH, REAL)  =  ALL

Option    Meaning

SORT1     Output will be presented as a tabular listing of grid points for
          each load, frequency. eigenvalue, or time, depending on the rigid
          format. SORT1 is not available in transient problems (where the
          default is SORT2).

SORT2     Output will be presented as a tabular listing of load, frequency,
          or time for each grid point. SORT2 is available only in static
          analysis, transient, and frequency response problems.

PRINT     The printer will be the output device.

PUNCH     The card punch will be the output device.

NOPRINT   Displacement is calculated and saved on output file. The output
          file will not be sent to the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
          frequency response problems.

PHASE     Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
          complex eigenvalue or frequency response problems.

ALL       Displacements for all points will be output.

NONE      Displacements for no points will be output.

n         Set identification of previously appearing SET card. Only
          displacements of points whose identification numbers appear on
          this SET card will be output (Integer > 0).

Remarks

1.Both PRINT and PUNCH may be requested.

2.An output request for ALL in transient and frequency response problems
  generally produces large amounts of printout. An alternative to this would
  be to define a SET of interest.

3.In static analysis or frequency response problems, any request for SORT2
  causes all output to be SORT2.

4.VECTOR, PRESSURE, and THERMAL are alternate forms and are entirely
  equivalent to DISPLACEMENT.

5.DISPLACEMENT = NONE allows overriding an overall output request.
=PAGE=
DLOAD - Dynamic Load Set Selection

Description

Selects the dynamic load to be applied in a transient or frequency response
problem.

Format and Example(s)

DLOAD  =  n

DLOAD  =  73

Option    Meaning

n         Set identification of a DLOAD, RLOAD1, RLOAD2, TLOAD1, or TLOAD2
          card (Integer > 0).

Remarks

1.The above loads will not be used by NASTRAN unless selected in Case
  Control.

2.RLOAD1 and RLOAD2 may only be selected in a frequency response problem.

3.TLOAD1 and TLOAD2 may only be selected in a transient response problem.

4.Either RLOAD or TLOAD (but not both) may be selected in an aeroelastic
  response problem. If RLOAD is selected, a frequency response is calculated.
  If TLOAD is selected, then transient response is computed by Fourier
  transform.
=PAGE=
DSCOEFFICIENT - Differential Stiffness Coefficient Set

Description

Selects the coefficient set for a normal modes with differential stiffness
problem.

Format and Example(s)

                É         »
DSCOEFF1C1ENT = º DEFAULT º
                º   n     º
                È         ¼

DSCOEF = 15

DSCOEF = DEFAULT

Option    Meaning

DEFAULT   A single default coefficient of value 1.0.

n         Set identification of DSFACT card (Integer > 0).

Remarks

1.DSFACT cards will not be used unless selected.

2.DSCOEFFICIENT must appear in the second subcase of a normal modes with
  differential stiffness problem.
=PAGE=
ECHO - Bulk Data Echo Request

Description

Requests echo of Bulk Data Deck.

Format and Example(s)

        Ú        ¿
        ³ SORT   ³
        ³ UNSORT ³
ECHO  = ³ BOTH   ³   (see Remark 1 for default values)
        ³ NONE   ³
        ³ NONO   ³
        ³ PUNCH  ³
        À        Ù
ECHO = BOTH
ECHO = PUNCH, SORT

Option    Meaning

SORT      Sorted echo will be printed.

UNSORT    Unsorted echo will be printed.

BOTH      Both sorted and unsorted echo will be printed.

NONE or NONO  No echo will be printed.

PUNCH     The sorted Bulk Data Deck will be punched onto cards.

Remarks

1.If no ECHO card appears, ECHO = BOTH is assumed for restart runs. For all
  other runs, ECHO = SORT is assumed.

2.You are cautioned against suppressing the sorted echo in a checkpoint run
  as it will be difficult to change the data in a subsequent restart run.

3.In a restart run, the unsorted echo lists only the new bulk data submitted
  with the run, while the sorted echo lists the resequenced and renumbered
  revised bulk data.

4.If CHKPNT YES is specified, a sorted echo will be printed unless ECHO =
  NONE.

5.Unrecognizable options will be treated as SORT.

6.Any option overrides the default. Thus, for example, if both print and
  punch are desired, both SORT and PUNCH must be requested on the same card.

7.The NONE option cannot be combined with the PUNCH option. If punch output
  only is desired, ECHO = PUNCH will suffice.

8.In a restart run, ECHO = NONO suppresses also the printing of the NASTRAN
  DMAP compiler source listing. Do not use ECHO = NONO and CHKPNT YES
  together.

9.If ECHO = NONE or NONO, the resequencing cards SEQGP as generated by BANDIT
  are not printed.
=PAGE=
ELFORCE - Element Force Output Request

Description

Requests form and type of element force output.

Format and Example(s)

           Ú                                ¿    É      »
           ³ ( SORT1  , PRINT   ,   REAL  ) ³    º ALL  º
ELFORCE    ³   SORT2    PUNCH       IMAG    ³ =  º  n   º
           ³           NOPRINT      PHASE   ³    º NONE º
           À                                Ù    È      ¼

ELFORCE  =  ALL

ELFORCE(REAL, PUNCH, PRINT)  =  17

ELFORCE  =  25

ELFORCE(SORT2,NOPRINT)  =   ALL

Option    Meaning

SORT1     Output will be presented as a tabular listing of elements for each
          load, frequency, eigenvalue, or time, depending on the rigid
          format. SORT1 is not available in transient problems (where the
          default is SORT2).

SORT2     Output will be presented as a tabular listing of load, frequency,
          or time for each element type. SORT2 is available only in static
          analysis, transient and frequency response problems.

PRINT     The printer will be the output device.

PUNCH     The card punch will be the output device.

NOPRINT   Force is calculated and saved on output file. The output file will
          not be sent to the output device

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
          frequency response problems.

PHASE     Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
          complex eigenvalue or frequency response problems.

ALL       Forces for all elements will be output.

NONE      Forces for no elements will be output.

n         Set identification of a previously appearing SET card. Only forces
          of elements whose identification numbers appear on this SET card
          will be output (Integer > 0).

Remarks

1.Both PRINT and PUNCH may be requested.

2.An output request for ALL in transient and frequency response problems
  generally produces large amounts of printout. An alternative to this would
  be to define a SET of interest.

3.In static analysis or frequency response problems, any request for SORT2
  output causes all output to be SORT2.

4.FORCE is an alternate form and is entirely equivalent to ELFORCE.

5.ELFORCE = NONE allows overriding an overall request.

6.In heat transfer analysis, ELFORCE output consists of heat flow through and
  out of the elements.
=PAGE=
ELSTRESS - Element Stress Output Request

Description

Requests form and type of element stress output.

Format and Example(s)

          Ú                                                ¿     É      »
          ³ ( SORT1   ,   PRINT    ,   EXTREME  ,   REAL ) ³     º ALL  º
ELSTRESS  ³   SORT2       PUNCH        LAYER        IMAG   ³ =   º  n   º
          ³              NOPRINT                    PHASE  ³     º NONE º
          À                                                Ù     È      ¼

ELSTRESS  =  5

ELSTRESS  =  ALL

ELSTRESS(SORT1, PRINT, PUNCH, PHASE) = 15

Option    Meaning

SORT1     Output will be presented as a tabular listing of elements for each
          load, frequency, eigenvalue, or time, depending on the rigid
          format. SORT1 is not available in transient problems (where the
          default is SORT2).

SORT2     Output will be presented as a tabular listing of load, frequency,
          or time for each element type. SORT2 is available only in static
          analysis, transient, and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

NOPRINT     Stresses are calculated and saved on output file. The output file
            will not be sent to the output device

EXTREME or LAYER  Requests stresses to be calculated at the extreme (top and 
            bottom) fibers of a plate element or, for composites, the stresses
            for each layer. (See Remarks 7 and 8)

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Stresses for all elements will be output.

n           Set identification of a previously appearing SET card (Integer >
            0). Only stresses for elements whose identification numbers appear
            on this SET card will be output.

NONE        Stresses for no elements will be output.

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative to this would
   be to define a SET of interest.

3. In static analysis or frequency response problems, any request for SORT2
   output causes all output to be SORT2.

4. ELSTRESS is an alternate form and is entirely equivalent to STRESS.

5. ELSTRESS = NONE allows overriding an overall request.

6. If element stresses in the material coordinate system are desired (only for
   TRIA1, TRIA2, QUAD1, and QUAD2 elements and only in Rigid Format 1), the
   parameter STRESS (see the description of the PARAM bulk data card in
   Section 2.4.2) should be set to be a positive integer. If, in addition to
   element stresses in the material coordinate system, stresses at the
   connected grid points are also desired, the parameter STRESS should be set
   to 0.

7. When LAYER is selected, individual layer stresses and/or failure indices
   will be output.

8. The option EXTREME and LAYER is only applicable for the QUAD4 and TRIA3
   elements.
=PAGE=
ESE - Element Strain Energy Output Request

Description

Requests strain energy output and per cent of total strain energy with respect
to all elements.

Format and Example(s)

    Ú         ¿   É      »
    ³( PRINT )³   º ALL  º
ESE ³  PUNCH  ³ = º  n   º
    ³         ³   º NONE º
    À         Ù   È      ¼

ESE (PUNCH) = 5

ESE (PRINT,PUNCH) = ALL

Option      Meaning

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

ALL         Strain energies will be output for all elements for which
            stiffness matrices exist.

NONE        Strain energies for no elements will be output.

n           Set identification of previously appearing SET card (Integer > 0).
            Only strain energies for elements whose identification numbers
            appear on this SET card will be output.

Remarks

1. Element strain energies are output from static analysis (Rigid Format 1)
   only.

2. The output will be in SORT1 format.

3. Both PRINT and PUNCH may be requested.

4. ESE = NONE allows overriding an overall output request.
=PAGE=
FMETHOD - Flutter Analysis Method

Description

Selects the FLUTTER parameters to be used by the flutter module (FA1).

Format and Example(s)

FMETHOD = n

FMETHOD = 72

Option      Meaning

n           Set identification number of a FLUTTER card (integer > 0).

Remarks

1. An FMETHOD card is required for flutter analysis.
=PAGE=
FORCE - Element Force Output Request

Description

Requests form and type of element force output.

Format and Example(s)

        Ú                        ¿   É      »
        ³ ( SORT1, PRINT, REAL ) ³   º  ALL º
FORCE   ³   SORT2  PUNCH  IMAG   ³ = º   n  º
        ³                PHASE   ³   º NONE º
        À                        Ù   È      ¼

FORCE = ALL

FORCE(REAL, PUNCH, PRINT) = 17

FORCE = 25

Option      Meaning

SORT1       Output will be presented as a tabular listing of elements for each
            load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of load, frequency,
            or time for each element type. SORT2 is available only in static
            analysis, transient, and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary printout on complex eigenvalue or 
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Forces for all elements will be output.

n           Set identification of a previously appearing SET card. Only forces
            whose element identification numbers appear on this SET card will
            be output (Integer > 0).

NONE        Forces for no elements will be output.

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative to this would
   be to define a SET of interest.

3. In static analysis or frequency response problems, any request for SORT2
   output causes all output to be SORT2.

4. ELFORCE is an alternate form and is entirely equivalent to FORCE.

5. FORCE = NONE allows overriding an overall request.

6. In heat transfer analysis, ELFORCE output consists of heat flow through and
   out of the elements.
=PAGE=
FREQUENCY - Frequency Set Selection

Description

Selects the set of frequencies to be solved in frequency response problems.

Format and Example(s)

FREQUENCY = n

FREQUENCY = 17

Option      Meaning

n           Set identification of a FREQ, FREQ1, or FREQ2 type card (Integer >
            0).

Remarks

1. The FREQ, FREQ1, or FREQ2 cards will not be used unless selected in Case
   Control.

2. A frequency set selection is required for a frequency response problem.

3. A frequency set selection is required for transient response by Fourier
   methods.
=PAGE=
GPFORCE - Grid Point Force Balance Output Request

Description

Requests grid point force balance output from applied loads, single-point
constraints, and element constraints.

Format and Example(s)

         Ú         ¿   É      »
         ³( PRINT )³   º ALL  º
GPFORCE  ³  PUNCH  ³ = º  n   º
         ³         ³   º NONE º
         À         Ù   È      ¼

Option      Meaning

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

ALL         Force balance will be output for all elements connected to grid
            points or scalar points.

NONE        Force balance for no grid points will be output.

n           Set identification of previously appearing SET card (Integer > 0).
            Only force balance for points whose identification numbers appear
            on this SET card will be output.

Remarks

1. Grid point force balance is output from Statics Analysis (Rigid Format 1)
   only.

2. The output will be in SORT1 format.

3. Both PRINT and PUNCH may be requested.

4. GPFORCE = NONE allows overriding an overall output request.
=PAGE=
GUST - Aerodynamic Gust Load Request

Description

Selects the gust field in an aeroelastic response problem.

Format and Example(s)

GUST = n

GUST = 73

Option      Meaning

n           Set identification of a GUST bulk data card (Integer > 0).

Remarks

1. The above GUST will not be used by NASTRAN unless selected in Case Control.

2. The choice of transient or frequency response gust depends upon the type of
   TLOAD or RLOAD referenced on the selected GUST card.
=PAGE=
HARMONICS - Harmonic Printout Control

Description

Controls number of harmonics output for problems containing CCONEAX, CTRAPAX,
or CTRIAAX elements.

Format and Example(s)

             É      »
             º  0   º
HARMONICS  = º ALL  º
             º NONE º
             º  n   º
             È      ¼

Option      Meaning

ALL         All harmonics will be output.

NONE        No harmonics will be output.

n           Available harmonics up to and including n will be output (Integer
            >= 0).

Remarks

1. If no HARMONICS card appears in Case Control, only 0 harmonic output will
   be printed.
=PAGE=
IC - Transient Initial Condition Set Selection

Description

To select the initial conditions for direct transient problems.

Format and Example(s)

IC = n

IC = 17

Option      Meaning

n           Set identification of TIC card (Integer > 0) for structural
            analysis. Set identification of TEMP and/or TEMPD card (Integer >
            0) for heat transfer analysis.

Remarks

1. TIC cards will not be used (hence no initial conditions) unless selected in
   Case Control.

2. Initial conditions are not allowed in a modal transient problem.
=PAGE=
K2PP - Direct Input Stiffness Matrix Selection

Description

Selects a direct input stiffness matrix.

Format and Example(s)

K2PP  =  name

K2PP  =  KDMIG

K2PP  =  K2PP

Option      Meaning

name        BCD name of a [K22dpp] matrix that is input on the DMIG or DMIAX
            bulk data card.

Remarks

1. K2PP is used only in dynamics problems.

2. DMIG and DMIAX matrices will not be used unless selected.
=PAGE=
LABEL - Output Label

Description

Defines a BCD (alphanumeric) label which will appear on the third heading line
of each page of NASTRAN printer output.

Format and Example(s)

        É              »
LABEL = º Any BCD data º
        È              ¼

LABEL = SAMPLE OF A LABEL CARD

Remarks

1. LABEL appearing at the subcase level will label output for that subcase
   only.

2. LABEL appearing before all subcases will label any outputs which are not
   subcase dependent.

3. If no LABEL card is supplied, the label line will be blank.

4. LABEL information is also placed on NASTRAN plotter output as applicable.
=PAGE=
LINE - Data Lines Per Page

Description

Defines the number of data lines per printed page.

Format and Example(s)

       É  »
       º42º
LINE = ºn º  CDC
       È  ¼
       É  »
       º55º
LINE = ºn º  DEC VAX, IBM, and UNIVAC
       È  ¼

Option      Meaning

n           Number of data lines per page (Integer >= 10).

Remarks

1. If no LINE card appears, the appropriate default is used.

2. For 11 inch paper, 50 is the recommended number; for 8-1/2 inch paper, 35
   is the recommended number.

3. Alternatively, the number of data lines per printed page can also be
   defined by means of the NLINES keyword on the NASTRAN card (see Section
   2.1).
=PAGE=
LOAD - External Static Load Set Selection

Description

Selects the external static load set to be applied to the structural model.

Format and Example(s)

LOAD = n

LOAD = 15

Option      Meaning

n           Set identification of at least one external load card and hence
            must appear on at least one FORCE, FORCE1, FORCE2, MOMENT,
            MOMENT1, MOMENT2, GRAV, PLOAD, PLOAD2, PLOAD3, RFORCE, PRESAX,
            FORCEAX, MOMAX, SLOAD, or LOAD card (Integer > 0).

Remarks

1. The above static load cards will not be used by NASTRAN unless selected in
   Case Control.

2. A GRAV card cannot have the same set identification number as any of the
   other loading card types. If it is desired to apply a gravity load along
   with other static loads, a LOAD bulk data card must be used.

3. If n is to be the set identification number of a bulk data LOAD card (see
   description in Section 2.4), then it must be different from the load set
   identification numbers of all external static load sets in the Bulk Data
   Deck.

4. LOAD is only applicable in statics, inertia relief, differential stiffness,
   buckling, and piecewise linear problems.

5. The total load applied will be the sum of external (LOAD), thermal
   (TEMP(LOAD)), element deformation (DEFORM), and constrained displacement
   (SPC) Loads.

6. Static, thermal, and element deformation loads must have unique set
   identification numbers.

7. The rigid formats that accept a static load card expect it to appear in the
   Case Control deck in a certain place with respect to subcase definitions.
   See Section 3 for specific instructions.
=PAGE=
M2PP - Direct input Mass Matrix Selection

Description

Selects a direct input mass matrix.

Format and Example(s)

M2PP  =  name

M2PP  =  MDMIG

M2PP  =  M2PP

Option      Meaning

name        BCD name of a [M22dpp] matrix that is input on the DMIG or DMIAX
            bulk data card.

Remarks

1. M2PP is supported only in dynamics problems.

2. DMIG and DMIAX matrices will not be used unless selected.
=PAGE=
MAXLINES - Maximum Number of Output Lines

Description

Sets the maximum number of output lines to a given value.

Format and Example(s)

            É       »
MAXLINES =  º 20000 º
            º   n   º
            È       ¼

MAXLINES = 50000

Option      Meaning

n           Maximum number of output lines (Integer > 0).

Remarks

1. Any time this number is exceeded, NASTRAN will terminate through PEXIT.

2. This card may or may not override system operating control cards. You
   should check with the local operations staff.

3. Default is MAXLINES = 20000.
=PAGE=
METHOD - Real Eigenvalue Extraction Method Selection

Description

Selects the real eigenvalue parameters to be used by the READ module.

Format and Example(s)

METHOD = n

METHOD = 33

Option      Meaning

n           Set identification number of an EIGR card (normal modes or modal
            formulation) or an EIGB card (buckling). (Integer > 0).

Remarks

1. An eigenvalue extraction method must be selected when extracting real
   eigenvalues using functional module READ.

2. Each of the rigid formats that accepts an eigenvalue method card expects it
   to appear in the Case Control Deck in a certain place with respect to
   subcase definitions. See Section 3 for specific instructions.
=PAGE=
MODES - Duplicate Case Control

Description

Repeats case control MODES times, to allow control of output in eigenvalue
problems.

Format and Example(s)

MODES = n

MODES = 1

Option      Meaning

n           Number of modes, starting with the first and proceeding
            sequentially upward, for which the case control or subcase control
            is to apply. (Integer > 0).

Remarks

1. This card can be illustrated by an example. Suppose stress output is
   desired for the first five modes only and displacements only thereafter.
   The following example would accomplish this:

   SUBCASE 1
   MODES = 5
   OUTPUT
   STRESS = ALL
   SUBCASE 6
   OUTPUT
   DISPLACEMENTS = ALL
   BEGIN BULK

2. The MODES card causes the results for each eigenvalue to be considered as a
   separate, successively numbered subcase, beginning with the subcase number
   containing the MODES card.

3. If the MODES card is not used, eigenvalue results are considered to be a
   part of a single subcase. Hence, any output requests for the single subcase
   will apply for all eigenvalues.

4. All eigenvectors with mode numbers greater than the number of records in
   Case Control are printed with the descriptors of the last Case Control
   record. For example, to suppress all printout for modes beyond the first
   three, the following Case Control deck could be used:

   SUBCASE 1
   MODES = 3
   DISPLACEMENTS = ALL
   SUBCASE 4
   DISPLACEMENTS = NONE
   BEGIN BULK
=PAGE=
MPC - Multipoint Constraint Set Selection

Description

Selects the multipoint constraint set to be applied to the structural model.

Format and Example(s)

MPC = n

MPC = 17

Option      Meaning

n           Set identification of a multipoint constraint set and hence must
            appear on at least one MPC, MPCADD, MPCAX, or MPCS card. (Integer
            > 0).

Remarks

1. MPC, MPCADD, MPCAX, or MPCS cards will not be used by NASTRAN unless
   selected in Case Control.
=PAGE=
MPCFORCE - Multipoint Forces of Constraint Output Request

Description

Requests multipoint force of constraint vector output.

Format and Example(s)

           Ú                  ¿   É     »
           ³                  ³   º ALL º
MPCFORCE   ³ ( SORT1, PRINT ) ³ = º  n  º
           ³          PUNCH   ³   º NONEº
           À                  Ù   È     ¼

MPCFORCE = 10

MPCFORCE(PRINT,PUNCH) = ALL

MPCFORCE = NONE

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each subcase or frequency, depending on the rigid format. SORT2 is
            not available.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

ALL         Multipoint forces of constraint for all points will be output
            (only nonzero entries).

NONE        Multipoint forces of constraint for no points will be output.

n           Set identification of previously appearing SET card. Only
            multipoint constraint forces for points whose identification
            numbers appear on this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. MPCFORCE = NONE allows overriding an overall output request.

3. MPCFORCE is only valid for statics and real eigenvalue analyses.

4. A request for MPCFORCE is not allowed for axisymmetric elements.

5. See the PARAM bulk data card for use of related parameters OPT and GRDEQ.
=PAGE=
NCHECK - Stress and Element Forces Numerical Accuracy Check

Description

Requests stress and element force numerical accuracy check.

Format and Example(s)

NCHECK [= n]

NCHECK

NCHECK = 6

Option      Meaning

n           A printout of the number of significant digits accuracy is issued
            for each element having an entry with less than n significant
            digits in the stress or force calculation.

Remarks

1. All the elements requested on the STRESS and/or FORCE card (or their
   equivalent ELSTRESS and/or ELFORCE card) are checked.

2. The default for n is five (5) when n is not specified.

3. These checks measure the quality of the computations to obtain element
   stresses and element forces. They do not measure the quality of the model
   being analyzed.

4. See Theoretical Manual Section 3.7.2 for a description of the accuracy
   check.

5. The printout identifies the element types, identification number and the
   subcase. The entries checked are as follows.

   ELEMENT TYPE                            ENTRIES

   ROD,CONROD,TUBE                         FA,T,åA,åT

   BAR                                     FA,T,M1a,M1b,M2a,M2b,V1,V2,åa
   
   TRMEM,QDMEM,QDMEMl                      åx,åy,çxy

   TRPLT,QDPLT,TRIA1,TRIA2,QUAD1,QUAD2     åx1,åy1,åxy1,åx2,åy2,çxy2,Mx,My,Mxy,
   TRBSC                                   Vx,Vy

   HEXA1,HEXA2,WEDGE,TETRA                 åx,åy,åz,çyz,çxz,çxy

   SHEAR                                   åMAX,åAVE, corner forces, kick
                                           forces, and shears.

   TWIST                                   åMAX,åAVE,M1-3,M2-4

   QDMEM2                                  åx,åy,çxy, corner forces, kick
                                           forces, and shears.

   IHEX1, IHEX2, IHEX3                     åNORMAL, åSHEAR, and åPRINCIPAL for
                                           each direction, grid point, and
                                           centroid.
=PAGE=
NLLOAD - Nonlinear Load Output Request

Description

Requests form and type of nonlinear load output for transient problems.

Format and Example(s)

       Ú       ¿   É     »
       ³       ³   º ALL º
NLLOAD ³(PRINT)³ = º  n  º
       ³ PUNCH ³   º NONEº
       À       Ù   È     ¼

NLLOAD = ALL

Option      Meaning

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

ALL         Nonlinear loads for all solution points will be output.

NONE        Nonlinear loads will not be output.

n           Set identification of previously appearing SET card.(Integer > 0).
            Only non-linear loads for points whose identification numbers
            appear on this SET card will be output.

Remarks

1. Both PRINT and PUNCH may be used.

2. Nonlinear loads are output only in the solution (D or H) set.

3. The output format will be SORT2.

4. An output request for ALL in transient response problems generally produces
   large amounts of printout. An alternative to this would be to define a SET
   of interest.

5. THERMAL = NONE allows overriding an overall output request.
=PAGE=
NONLINEAR - Nonlinear Load Set Selection

Description

Selects nonlinear load for transient problems.

Format and Example(s)

NONLINEAR  =  n

NONLINEAR LOAD SET  =  75

Option      Meaning

n           Set identification of NOLINi cards (Integer > 0).

Remarks

1. NOLINi cards will not be used unless selected in Case Control.
=PAGE=
OFREQUENCY - Output Frequency Set

Description

Selects from the solution set of frequencies a subset for output requests in
direct or modal frequency analysis. In flutter analysis, it selects a subset
of velocities.

Format and Example(s)

             É     »
OFREQUENCY = º ALL º
             º  n  º
             È     ¼

OFREQUENCY = ALL

OFREQUENCY SET = 15

Option      Meaning

ALL         Output for all frequencies will be printed out.

n           Set identification of previously appearing SET card (Integer > 0).
            Output for frequencies closest to those given on this SET card
            will be produced.

Remarks

1. OFREQUENCY is defaulted to ALL if it is not supplied.

2. In flutter analysis, the selected set lists velocities in input units. If
   there are n velocities in the list, the n points with velocities closest to
   those in the list will be selected for output.

3. This card is used in conjunction with the MODACC module to limit the
   frequencies for which mode acceleration computations are performed.

4. In flutter analysis, the selected set refers to the imaginary part of the
   complex eigenvalues.

   K or KE method:   Velocity (input units)
   PK method:        Frequency

5. In aeroelastic response (with RLOAD selection), the selected set refers to
   the frequency (cycles per unit time).
=PAGE=
OLOAD - Applied Load Output Request

Description

Requests form and type of applied load vector output.

Format and Example(s)

       Ú                        ¿      É      »
OLOAD  ³ ( SORT1, PRINT, REAL ) ³      º ALL  º
       ³   SORT2  PUNCH  IMAG   ³  =   º  n   º
       ³                 PHASE  ³      º NONE º
       À                        Ù      È      ¼

OLOAD   =  ALL
SLOAD(SORT1, PHASE)  =  5

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of load, frequency,
            or time for each grid point. SORT2 is available only in static
            analysis, transient and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Applied loads for all points will be output. (SORT1 will only
            output nonzero values.)

NONE        Applied loads for no points will be output.

n           Set identification of previously appearing SET card. Only loads on
            points whose identification numbers appear on this SET card will
            be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. In static analysis or frequency response problems, any request for SORT2
   output causes all output to be SORT2.

4. A request for SORT2 causes loads (zero and nonzero) to be output.

5. OLOAD = NONE allows overriding an overall output request.
=PAGE=
OTIME - Output Time Set

Description

Selects from the solution set of times a subset for output requests.

Format and Example(s)

        É   »
OTIME = ºALLº
        º n º
        È   ¼

OTIME = ALL

OTIME = 15

Option      Meaning

ALL         Output for all times will be printed out.

n           Set identification of previously appearing SET card. (Integer >
            0). Output for times closest to those given on this SET card will
            be output.

Remarks

1. OTIME is defaulted to ALL if it is not supplied.

2. The OTIME card is particularly useful for restarts to request a subset of
   the output (that is, stresses at only peak times, etc.).

3. This card can be used in conjunction with the MODACC module to limit the
   times for which mode acceleration computations are performed.
=PAGE=
OUTPUT - Output Packet Delimiter

Description

Delimits the various output packets, structure plotter, curve plotter, and
printer/punch.

Format and Example(s)

        Ú         ¿
        ³( PLOT  )³
OUTPUT  ³  XYOUT  ³
        ³  XYPLOT ³
        À         Ù
OUTPUT

OUTPUT(PLOT)

OUTPUT(XYOUT)

Option      Meaning

No qualifier  Beginning of printer output packet. This is not a required card.

PLOT        Beginning of structure plotter packet. This card must precede all
            structure plotter control cards.

XYOUT or XYPLOT  Beginning of curve plotter packet. This card must precede all
            curve plotter control cards. XYPLOT and XYOUT are entirely
            equivalent.

Remarks

1. The structure plotter packet and the curve plotter packet must be at the
   end of the Case Control Deck. Either may come first.

2. The delimiting of a printer packet is completely optional.
=PAGE=
PLCOEFFICIENT - Piecewise Linear Coefficient Set

Description

Selects the coefficient set for piecewise linear problems.

Format and Example(s)

PLCOEFFICIENT  =  n

PLCOEFFICIENT  =  25

Option      Meaning

n           Set identification of PLFACT card (Integer > 0).

Remarks

1. PLFACT cards will not be used unless selected.
=PAGE=
PLOTID - Plotter Identification

Description

Defines BCD (alphanumeric) identification which will appear on the first frame
of any NASTRAN plotter output.

Format and Example(s)

           É              »
PLOTID  =  º Any BCD data º
           È              ¼

PLOTID  =  RETURN TO B.J. SMITH, ROOM.201, BLDG 85, ABC COMPANY

Remarks

1. PLOTID must appear before any OUTPUT(PLOT), OUTPUT(XYOUT), or
   OUTPUT(XYPLOT) cards.

2. The presence of PLOTID causes a special header frame to be plotted with the
   supplied identification plotted several times. This allows for easy
   identification of the NASTRAN plotter output.

3. If no PLOTID card appears, no ID frame will be plotted.

4. The PLOTID header frame will not be generated for table plotters.
=PAGE=
PRESSURE - Hydroelastic Pressure Output Request

Description

Requests form and type of displacement and hydroelastic pressure vector
output.

Format and Example(s)

          Ú                      ¿   É      »
          ³( SORT1, PRINT, REAL )³   º ALL  º
PRESSURE  ³  SORT2  PUNCH  IMAG  ³ = º  n   º
          ³                PHASE ³   º NONE º
          À                      Ù   È      ¼

PRESSURE = 5
PRESSURE(IMAG) = ALL
PRESSURE(SORT2, PUNCH, REAL) = ALL

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each grid point. SORT2 is available only in transient and
            frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Displacements and pressures for all points will be output.

NONE        Displacements and pressures for no points will be output.

n           Set identification of previously appearing SET card. Only
            displacements and pressures of points whose identification numbers
            appear on this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. In a frequency response problem any request for SORT2 causes all output to
   be SORT2.

4. DISPLACEMENT and VECTOR are alternate forms and are entirely equivalent to
   PRESSURE.

5. PRESSURE = NONE allows overriding an overall output request.
=PAGE=
RANDOM - Random Analysis Set Selection

Description

Selects the RANDPS and RANDTi cards to be used in random analysis.

Format and Example(s)

RANDOM  =  n

RANDOM  =  177

Option      Meaning

n           Set identification of RANDPS and RANDTi cards to be used in random
            analysis (Integer > 0).

Remarks

1. RANDPS cards must be selected to do random analysis.

2. RANDPS must be selected in the first subcase of the current loop. RANDPS
   may not reference subcases in a different loop.
=PAGE=
READFILE - Directive to Read Input Cards

Description

Defines a file that contains the input cards.

Format and Example(s)

          ÚÄ         Ä¿
          ³           ³
          ³ ,NOPRINT, ³
READFILE  ³ ,NOPRINT  ³  [ = ]  filename
          ³ (NOPRINT) ³
          ÀÄ         ÄÙ

READFILE  ABC
READFILE  NOPRINT  ABC
READFILE, NOPRINT  ABC
READFILE, NOPRINT, ABC
READFILE (NOPRINT) ABC
READFILE  = ABC
READFILE  NOPRINT  = ABC
READFILE, NOPRINT  = ABC
READFILE (NOPRINT) = ABC

Remarks

1. This card can be used in Executive, Case Control, and Bulk Data Decks.

2. Input cards are saved in the file named filename.

3. Comma, equal sign, and parentheses are not allowed in filename.

4. NOPRINT allows reading in the input cards, such as the DMAP alters or
   restart dictionary, without printing them out. The default is to print
   them.

5. Since this card can also be used in the Case Control Deck, an equal sign is
   also allowed.

6. Nested READFILE is allowed.

7. See Sections 2.0.2.1 and 2.0.2.2 for more information.
=PAGE=
REPCASE - Repeat Case Subcase Delimiter

Description

Delimits and identifies a repeated subcase.

Format and Example(s)

REPCASE    n

REPCASE    137

Option      Meaning

n           Subcase identification number (integer > 1).

Remarks

1. The subcase identification number, n, must be strictly increasing (that is,
   greater than all previous subcase identification numbers).

2. This case will only re-output the previous real case. This allows
   additional set specification.

3. REPCASE may only be used in statics or inertia relief.

4. One or more repeated subcases (REPCASEs) must immediately follow the
   subcase (SUBCASE) to which they refer. (See example 4 in Section 2.3.3.)
=PAGE=
SACCELERATION - Solution Set Acceleration Output Request

Description

Requests form and type of solution set acceleration output.

Format and Example(s)

              Ú                      ¿   É      »
SACCELERATION ³( SORT1, PRINT, REAL )³ = º ALL  º
              ³  SORT2  PUNCH  IMAG  ³   º  n   º
              ³                PHASE ³   º NONE º
              À                      Ù   È      ¼

SACCELERATION = ALL

SACCELERATION(PUNCH, IMAG) = 142

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each grid point (or mode number). SORT2 is available only in
            transient and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on frequency response
            problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            frequency response problems.

ALL         Acceleration for all solution points (modes) will be output.

NONE        Acceleration for no solution points (modes) will be output.

n           Set identification of a previously appearing SET card. Only
            accelerations of points whose identification numbers appear on
            this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. Acceleration output is only available for transient and frequency response
   problems.

4. In a frequency response problem any request for SORT2 output causes all
   output to be SORT2.

5. SACCELERATION = NONE allows overriding an overall output request.
=PAGE=
SCAN - Output Scan Request

Description

Scan output data and eliminate values that do not meet the specification set
by this SCAN card.

Format and Example(s)

         É       »                           É       »
         ºSTRESS º                           º topn  º Ú        ¿
SCAN   ( ºFORCE  º , element, component )  = º       º ³, SET  i³
         ºHELP   º                           ºmax,minº À        Ù
         ºON-LINEº                           È       ¼
         È       ¼

SCAN (STRESS, CBAR, AXIAL) = 10

SCAN (STRESS, BAR, AXIAL, SA-MAX) = 15, SET 102

SCAN (FORCE, ROD, 2, 3) = 17

SCAN (FORCE, 3, CROD, 2) = +2000., -1500., SET 102

SCAN (ROD, AXIAL, FORCE, TORQUE) = 5000., 400.

SCAN (HELP)

Option      Meaning

STRESS      Request scan on stress file, of SORT1 or SORT2 format.

FORCE       Request scan on force file, of SORT1 or SORT2 format.

element     Any NASTRAN element name, with or without the leading letter "C"
            (BCD).

component   One or more components specified by keywords (BCD), or by numeric
            codes (Integer > 0). The numeric codes are the field numbers on
            the heading of the output page, whose values are to be scanned.
            (Each element has its own page heading.) See Remark 11 for the
            keywords and their corresponding field numbers.

topn        The highest n values, and the lowest n values, found by SCAN in
            the field(s) specified by component are printed out; for example,
            top n tension and top n compression stresses (Integer > 0).

max,min     Values greater than max and less than min, in the field(s)
            specified by component, are printed out (Real).

i           Element set identification of a previously appearing SET card
            (Integer > 0). Only forces or stresses of elements whose
            identification numbers appear on this SET card will be scanned for
            output. (Default is all.)

HELP        A table of the component keywords and their corresponding field
            numbers will be printed immediately before the Bulk Data Deck, and
            the job will continue.

ON-LINE     Request SCAN operation to be run on-line under real-time
            environment.

Remarks

1. Multiple SCAN cards can be requested in a NASTRAN run. They do not override
   one another.

2. A SCAN card specifies only one element type; an element type can have more
   than one SCAN card.

3. More than one component field can be requested in a SCAN card. However,
   these fields will be scanned together as a group.

4. SCAN sorts and prints the scanned values in descending order. All fields of
   the same output line are printed.

5. If the component keyword is misspelled, a list of the valid names and their
   corresponding fields will be printed automatically and the job will be
   flagged for fatal error termination.

6. Some component keywords imply multi-field scan; for example, "AXIAL" may
   imply axial forces for grid points 1, 2, 3, etc.

7. Component numeric code specifies field numbers 1 through 62 only.

8. Normally, SCAN will scan only data already generated for the Output File
   Processor (OFP). That is, SCAN cannot scan data that has not been created.
   However, if no ELSTRESS (or STRESS) card is specified before a stress SCAN
   card, a STRESS card is generated internally in the following form:

   STRESS (SORT1, NOPRINT, REAL) = ALL

   Forces are handled similarly.

9. The LABEL line (after TITLE and SUBTITLE) is limited to 36 characters. The
   rest of the line is replaced by the SCAN header.

10.   When the ON-LINE option is requested, the other input parameters are not
      needed on the SCAN card. These parameters will be prompted for on the
      CRT screen by the computer system when the SCAN module is executed.

11.   The component keywords for stress and force, and their corresponding
      output field numbers, are listed below. This table is printed by SCAN
      (HELP).

FORCE/STRESS       KEYWORD        COMPONENT (OUTPUT FIELD NO.)

ROD, TUBE, CONROD
     STRESS        AXIAL             2
     STRESS        TORSIONAL         4
     STRESS        MARGIN            3, 5
     FORCE         AXIAL             2
     FORCE         TORQUE            3

SHEAR, TWIST
     STRESS        MAX-SHR           2
     STRESS        MARGIN            4
     STRESS        AVG               3
     STRESS        MAX               2
     FORCE         FORCE-1           2
     FORCE         FORCE-2           3
     FORCE         MOMENT-1          2
     FORCE         MOMENT-2          3

TRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT
     STRESS        NORM-X            3, 11
     STRESS        NORM-Y            4, 12
     STRESS        SHEAR-XY          5, 13
     STRESS        MAJOR             7, 15
     STRESS        MINOR             8, 16
     STRESS        MAX-SHR           9, 17
     FORCE         MOMENT-X          2
     FORCE         MOMENT-Y          3
     FORCE         SHEAR-X           5
     FORCE         SHEAR-Y           6
     FORCE         TWIST             4

TRMEM, QDMEM, QDMEM1, QDMEM2
     STRESS         NORM-X           2
     STRESS         NORM-Y           3
     STRESS         SHEAR-XY         4
     STRESS         MAJOR            6
     STRESS         MINOR            7
     STRESS         MAX-SHR          8
     FORCE          FORCE-12         3, 4
     FORCE          FORCE-23         5, 6
     FORCE          FORCE-34         7, 8
     FORCE          FORCE-41         2, 9
     FORCE          KICK ON1        10
     FORCE          KICK ON2        12
     FORCE          KICK ON3        14
     FORCE          KICK ON4        16
     FORCE          SHEAR-XY        11
     FORCE          SHEAR-YZ        13
     FORCE          SHEAR-ZX        15
     FORCE          SHEAR           17

ELAS1, ELAS2, ELAS3, IS2D8
     STRESS         OCT-SHR          2
     FORCE          CIRCUM           2
     FORCE          FORCE-1          4, 9
     FORCE          FORCE-2          3, 6
     FORCE          FORCE-3          5, 8
     FORCE          FORCE-4          2, 7

BAR, ELBOW
     STRESS         SA-MAX           7, 8
     STRESS         SB-MAX          14, 15
     STRESS         MARGIN           9, 16
     STRESS         AXIAL            6
     FORCE          AXIAL            8
     FORCE          TORQUE           9
     FORCE          SHEAR            5, 6
     FORCE          MOMENT-A         2, 3
     FORCE          MOMENT-B         4, 5

CONEAX
     STRESS         NORM-U           4, 22
     STRESS         NORM-V           5, 23
     STRESS         SHEAR-UV         6, 24
     STRESS         MAJOR            8, 26
     STRESS         MINOR            9, 27
     STRESS         MAX-SHR         10, 28
     FORCE          MOMENT-U         3
     FORCE          MOMENT-V         4
     FORCE          SHEAR-XY         6
     FORCE          SHEAR-YZ         7

TRIARG
     STRESS         RADIAL           2
     STRESS         CIRCUM           3
     STRESS         AXIAL            4
     STRESS         SHEAR            5
     FORCE          RADIAL           2, 5, 8
     FORCE          CIRCUM           3, 6, 9
     FORCE          AXIAL            4, 7, 10

TRAPRG
     STRESS         RADIAL           2,  6, 10, 14 ... 22
     STRESS         CIRCUM           3,  7, 11, 15 ... 23
     STRESS         AXIAL            4,  8, 12, 16 ... 24
     STRESS         SHEAR            5,  9, 13, 17 ... 25
     STRESS         SHR-FBRC         6, 10, 14, 18 ... 26
     FORCE          RADIAL           2,  5,  8, 11
     FORCE          CIRCUM           3,  6,  9, 12
     FORCE          AXIAL            4,  7, 10, 13

TORDRG
     STRESS         MEM-T            2,  7, 12
     STRESS         MEM-C            3,  8, 13
     STRESS         FLEX-T           4,  9, 14
     STRESS         FLEX-C           5, 10, 15
     STRESS         SHR-FORC         6, 11, 16
     FORCE          RADIAL           2,  8
     FORCE          CIRCUM           3,  9
     FORCE          AXIAL            4, 10
     FORCE          MOMENT           5, 11
     FORCE          CURV             7, 13

IHEX1, IHEX2
     STRESS         NORM-X           3, 25, 47, 69 ... ETC.
     STRESS         SHEAR-XY         4, 26, 48, 70 ... ETC.
     STRESS         PRINC-A          5, 27, 49, 71 ... ETC.
     STRESS         MEAN             9, 31, 53, 75 ... ETC.
     STRESS         NORM-Y          11, 33, 55, 77 ... ETC.
     STRESS         SHEAR-YZ        12, 34, 56, 78 ... ETC.
     STRESS         PRINC-B         13, 35, 57, 79 ... ETC.
     STRESS         NORM-Z          17, 39, 61, 83 ... ETC.
     STRESS         SHEAR-ZX        18, 40, 62, 84 ... ETC.
     STRESS         PRINC-C         19, 41, 63, 85 ... ETC.
     STRESS         MAX-SHR         10, 32, 54, 76 ... ETC.
     STRESS         OCT-SHR         10, 32, 54, 76 ... ETC.

IHEX3
     STRESS         NORM-X           3, 26, 49, 72 ... 739
     STRESS         SHEAR-XY         4, 27, 50, 73 ... 740
     STRESS         PRINC-A          5, 28, 51, 74 ... 741
     STRESS         MEAN             9, 32, 55, 78 ... 745
     STRESS         NORM-Y          12, 35, 58, 81 ... 748
     STRESS         SHEAR-YZ        13, 36, 59, 82 ... 749
     STRESS         PRINC-B         14, 37, 60, 83 ... 750
     STRESS         NORM-Z          18, 41, 64, 87 ... 754
     STRESS         SHEAR-ZX        19, 42, 65, 88 ... 755
     STRESS         PRINC-C         20, 43, 66, 89 ... 756
     STRESS         MAX-SHR         10, 33, 56, 79 ... 746
     STRESS         OCT-SHR         10, 33, 56, 79 ... 746

TRIAAX, TRAPAX
     STRESS         RADIAL           3, 11, 19
     STRESS         AXIAL            4, 12, 20
     STRESS         CIRCUM           5, 13, 21
     STRESS         MEM-C            6, 14, 22
     STRESS         FLEX-T           7, 15, 23
     STRESS         FLEX-C           8, 16, 24
     FORCE          RADIAL           3,  7, 11
     FORCE          CIRCUM           4,  8, 12
     FORCE          AXIAL            5,  9, 13

QUAD4, TRIA3
     STRESS         NORMAL-X         3, 11
     STRESS         NORMAL-Y         4, 12
     STRESS         SHEAR-XY         5, 13
     STRESS         MAJOR            7, 15
     STRESS         MINOR           18, 16
     STRESS         MAX-SHR          9, 17
     FORCE          FX+FY            2,  3
     FORCE          FXY              4
     FORCE          MX+MY            5,  6
     FORCE          MXY              7
     FORCE          VX+VY            8, 19
     STRESS         NORMAL-1         5, 15, 25, 35
     STRESS         NORMAL-2         6, 16, 26, 36
     STRESS         SHEAR-12         7, 17, 27, 37
     STRESS         SHEAR-1Z        10, 20, 30, 40
     STRESS         SHEAR-2Z        11, 21, 31, 41

   Use output field number(s) to specify component(s) for elements or keywords
   not listed above. See sections 2.3.51 and 2.3.52 of the Programmer's Manual
   for additional element stress and force component definitions.
=PAGE=
SDAMPING - Structural Damping

Description

Selects table which defines damping as a function of frequency in modal
formulation problems.

Format and Example(s)

SDAMPING = n

SDAMPING = 77

Option      Meaning

n           Set identification of a TABDMP1 table (Integer > 0).

Remarks

1. If SDAMPING is not used BHH = [0].
=PAGE=
SDISPLACEMENT - Solution Set Displacement Output Request

Description

Requests form and type of solution set displacement output.

Format and Example(s)

               Ú                      ¿    É      »
SDISPLACEMENT  ³( SORT1, PRINT, REAL )³    º ALL  º
               ³  SORT2  PUNCH  IMAG  ³  = º  n   º
               ³                PHASE ³    º NONE º
               À                      Ù    È      ¼

SDISPLACEMENT  =  ALL

SDISPLACEMENT(SORT2, PUNCH, PHASE)  =  NONE

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each grid point (or mode number). SORT2 is available only in
            transient and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Displacements for all points (modes) will be output.

NONE        Displacements for no points (modes) will be output.

n           Set identification of previously appearing SET card. Only
            displacements of points whose identification numbers appear on
            this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. In a frequency response problem any request for SORT2 causes all output to
   be SORT2.

4. SVECTOR is an alternate form which is entirely equivalent to SDISPLACEMENT.

5. SDISPLACEMENT = NONE allows overriding an overall output request.
=PAGE=
SET - Set Definition Card

Description

1. Lists identification numbers (point or element) for output requests.

2. Lists the frequencies for which output will be printed in frequency
response problems.

Format and Example(s)

1. SET n = {i1[,i2, i3 THRU 14 EXCEPT i5, i6, i7, i8 THRU i9]}

   SET 77 =  5

   SET 88 = 5, 6, 7, 8, 9, 10 THRU 55 EXCEPT 15, 16, 77, 78, 79, 100 THRU 300

   SET 99 = 1 THRU 100000

2. SET n = {r1[, r2, r3, r4]}

   SET 101 = 1.0, 2.0, 3.0

   SET 105 = 1.009, 10.2, 13.4, 14.0, 15.0

Option      Meaning

n           Set identification (Integer > 0). Any set may be redefined by
            reassigning its identification number. Sets inside SUBCASE
            delimiters are local to the SUBCASE.

i1, i2 etc. Element or point identification number at which output is
            requested. (Integer > 0) If no such identification number exists,
            the request is ignored.

i3 THRU i4  Output at set identification numbers i3 through i4 (i4 > i3).

EXCEPT      Set identification numbers following EXCEPT will be deleted from
            output list as long as they are in the range of the set defined by
            the immediately preceding THRU.

r1, r2 etc. Frequencies for output (Real >= 0.0). The nearest solution
            frequency will be output. EXCEPT and THRU cannot be used.

Remarks

1. A SET card may be more than one physical card. A comma (,) at the end of a
   physical card signifies a continuation card. Commas may not end a set.

2. Identification numbers following EXCEPT within the range of the THRU must
   be in ascending order.

3. In the first format, i8 must be greater than i4; that is, the THRU must not
   be within an EXCEPT range.
=PAGE=
SPC - Single-Point Constraint Set Selection

Description

Selects the single-point constraint set to be applied to the structural model.

Format and Example(s)

SPC = n

SPC = 10

Option      Meaning

n           Set identification of a single-point constraint set and hence must
            appear on an SPC, SPC1, SPCADD, SPCAX, SPCS, or SPCS1 card
            (Integer > 0).

Remarks

1. SPC, SPC1, SPCADD, SPCAX, SPCS, or SPCS1 cards will not be used by NASTRAN
   unless selected in Case Control.
=PAGE=
SPCFORCES - Single-Point Forces of Constraint Output Request

Description

Requests form and type of single point force of constraint vector output.

Format and Example(s)

           Ú                        ¿    É      »
SPCFORCES  ³ ( SORT1, PRINT, REAL ) ³    º ALL  º
           ³   SORT2  PUNCH  IMAG   ³  = º  n   º
           ³                 PHASE  ³    º NONE º
           À                        Ù    È      ¼

SPCFORCES = 5

SPCFORCES(SORT2, PUNCH, PRINT, IMAG) = ALL

SPCFORCES(PHASE) = NONE

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of load, frequency,
            or time for each grid point. SORT2 is available only in static
            analysis, transient, and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Single point forces of constraint for all points will be output.
            (SORT1 will only output nonzero values.)

NONE        Single point forces of constraint for no points will be output.

n           Set identification of previously appearing SET card. Only
            single-point forces of constraint for points whose identification
            numbers appear on this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. In static analysis or frequency response problems, any request for SORT2
   output causes all output to be SORT2.

4. A request for SORT2 causes loads (zero and nonzero) to be output.

5. SPCFORCES = NONE allows overriding an overall output request.

6. In heat transfer analysis, SPCFORCE output is the power necessary to
   maintain a grid point at a fixed temperature.
=PAGE=
STRAIN - Element Strain/Curvature Output Request

Description

Requests element strain/curvature output.

Format and Example(s)

       Ú         ¿   É      »
STRAIN ³( PRINT )³ = º ALL  º
       ³  PUNCH  ³   º  n   º
       À         Ù   º NONE º
                     È      ¼

STRAIN (PUNCH)  =  5

STRAIN (PRINT,PUNCH)  =  ALL

Option      Meaning

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

ALL         Strains/curvatures for all elements will be output. See Remark 5.

NONE        Strains/curvatures for no elements will be output.

n           Set identification of previously appearing SET card (Integer > 0).
            Only strains/curvatures for elements whose identification numbers
            appear on this SET card will be output. See Remark 5.

Remarks

1. Element strains/curvatures are output from static analysis (Rigid Format 1)
   only.

2. The output will be in SORT1 format.

3. Both PRINT and PUNCH may be requested.

4. STRAIN = NONE allows overriding an overall output request.

5. Strains/curvatures are computed only for TRIA1, TRIA2, QUAD1, and QUAD2
   elements.

6. If element strains/curvatures in the material coordinate system are
   desired, the parameter STRAIN (see the description of the PARAM bulk data
   card in Section 2.4.2) should be set to be a positive integer. If, in
   addition to element strains/curvatures in the material coordinate system,
   strains/curvatures at the connected grid points are also desired, the
   parameter STRAIN should be set to 0.

7. The format of the two-line output for each element consists of strain in
   the middle surface (line 1) and curvature (line 2).
=PAGE=
STRESS - Element Stress Output Request

Description

Requests form and type of element stress output.

Format and Example(s)

         Ú                                              ¿     É      »
         ³( SORT1       PRINT       EXTREME       REAL )³     º ALL  º
STRESS   ³  SORT2   ,   PUNCH    ,  LAYER     ,   IMAG  ³  =  º  n   º
         ³             NOPRINT                    PHASE ³     º NONE º
         À                                              Ù     È      ¼

STRESS = 5

STRESS = ALL

STRESS(SORT1, PRINT, PUNCH, PHASE) = 15

Option      Meaning

SORT1       Output will be presented as a tabular listing of elements for each
            load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of load, frequency,
            or time for each element type. SORT2 is available only in static
            analysis, transient, and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

NOPRINT     Stresses are calculated and saved on file which is not sent to
            output device.

EXTREME or LAYER  Requests stresses to be calculated at the extreme (top and 
            bottom) fibers of a plate element or, for composites, the stresses
            for each layer. (See Remarks 7 and 8.)

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Stresses for all elements will be output.

n           Set identification of a previously appearing SET card (Integer >
            0). Only stresses for elements whose identification numbers appear
            on this SET card will be output.

NONE        Stresses for no elements will be output.

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. In static analysis or frequency response problems, any request for SORT2
   output causes all output to be SORT2.

4. STRESS is an alternate form and is entirely equivalent to ELSTRESS.

5. STRESS = NONE allows overriding an overall request.

6. If element stresses in the material coordinate system are desired (only for
   TRIA1, TRIA2, QUAD1, and QUAD2 elements and only in Rigid Format 1), the
   parameter STRESS (see the description of the PARAM bulk data card in
   Section 2.4.2) should be set to be a positive integer. If, in addition to
   element stresses in the material coordinate system, stresses at the
   connected grid points are also desired, the parameter STRESS should be set
   to 0.

7. When LAYER is selected, individual layer stresses and/or failure indices
   will be output.

8. The options EXTREME and LAYER are only applicable for the QUAD4 and TRIA3
   elements.
=PAGE=
SUBCASE - Subcase Delimiter

Description

Delimits and identifies a subcase.

Format and Example(s)

SUBCASE n

SUBCASE 101

Option      Meaning

n           Subcase identification number (Integer > 0).

Remarks

1. The subcase identification number, n, must be strictly increasing (that is,
   greater than all previous subcase identification numbers).

2. Plot requests and RANDOM requests refer to n.
=PAGE=
SUBCOM - Combination Subcase Delimiter

Description

Delimits and identifies a combination subcase.

Format and Example(s)

SUBCOM  n

SUBCOM  125

Option      Meaning

n           Subcase identification number (Integer > 2).

Remarks

1. The subcase identification number, n, must be strictly increasing (that is,
   greater than all previous subcase identification numbers).

2. A SUBSEQ card may appear in this subcase.

3. SUBCOM may only be used in statics or inertia relief problems.

4. Output requests above the subcase level will be utilized.

5. Up to 360 SUBCOM cards can be used in one NASTRAN analysis.
=PAGE=
SUBSEQ - Subcase Sequence Coefficients

Description

Gives the coefficients for forming a linear combination of the previous
subcases.

Format and Example(s)

SUBSEQ = R1 [, R2, R3, ..., RN]

SUBSEQ = 1.0, -1.0, 0.0, 2.0

Option      Meaning

R1 to RN    Coefficients of the previously occurring subcases (Real).

Remarks

1. A SUBSEQ card must only appear in a SUBCOM subcase.

2. A SUBSEQ card may be more than one physical card. A comma at the end
   signifies a continuation card.

3. SUBSEQ may only be used in statics or inertia relief problems.

4. A default value of 1.0 is used for all of the coefficients if no SUBSEQ
   card is used.
=PAGE=
SUBTITLE - Output Subtitle

Description

Defines a BCD (alphanumeric) subtitle which will appear on the second heading
line of each page of NASTRAN printer output.

Format and Example(s)

            É              »
SUBTITLE  = º Any BCD data º
            È              ¼

SUBTITLE = NASTRAN PROBLEM NO. 5-1A

Remarks

1. SUBTITLE appearing at the subcase level will title output for that subcase
   only.

2. SUBTITLE appearing before all subcases will title any outputs which are not
   subcase dependent.

3. If no SUBTITLE card is supplied, the subtitle line will be blank.

4. SUBTITLE information is also placed on NASTRAN plotter output as
   applicable.
=PAGE=
SVECTOR - Solution Set Displacement Output Request

Description

Requests form and type of solution set displacement output.

Format and Example(s)

           Ú                      ¿    É      »
           ³( SORT1, PRINT, REAL )³    º ALL  º
SVECTOR    ³  SORT2  PUNCH  IMAG  ³  = º  n   º
           ³                PHASE ³    º NONE º
           À                      Ù    È      ¼

SVECTOR = ALL

SVECTOR(SORT2, PUNCH, PHASE) = NONE

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each grid point (or mode number). SORT2 is available only in
            transient and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Displacements for all points (modes) will be output.

NONE        Displacements for no points (modes) will be output.

n           Set identification of previously appearing SET card. Only
            displacements of points whose identification numbers appear on
            this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. In a frequency response problem any request for SORT2 causes all output to
   be SORT2.

4. SDISPLACEMENT is an alternate form and is entirely equivalent to SVECTOR.

5. SVECTOR = NONE allows overriding an overall output request.
=PAGE=
SVELOCITY - Solution Set Velocity Output Request

Description

Requests form and type of solution set velocity output.

Format and Example(s)

           Ú                      ¿   É      »
           ³( SORT1, PRINT, REAL )³   º ALL  º
SVELOCITY  ³  SORT2  PUNCH  IMAG  ³ = º  n   º
           ³                PHASE ³   º NONE º
           À                      Ù   È      ¼

SVELOCITY = 5

SVELOCITY(SORT2, PUNCH, PRINT, PHASE) = ALL

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each grid point (or mode number). SORT2 is available only in
            transient and frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on frequency response
            problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            frequency response problems.

ALL         Velocity for all solution points (modes) will be output.

NONE        Velocity for no solution points (modes) will be output.

n           Set identification of a previously appearing SET card. Only
            velocities of points whose identification numbers appear on this
            SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. Velocity output is only available for transient and frequency response
   problems.

4. In a frequency response problem any request for SORT2 output causes all
   output to be SORT2.

5. SVELOCITY = NONE allows overriding an overall output request.
=PAGE=
SYM - Symmetry Subcase Delimiter

Description

Delimits and identifies a symmetry subcase.

Format and Example(s)

SYM   n

SYM   123

Option      Meaning

n           Subcase identification number (Integer > 0).

Remarks

1. The subcase identification number, n, must be strictly increasing (that is,
   greater than all previous subcase identification numbers).

2. Plot requests and RANDOM requests should refer to n.

3. Overall output requests will not propagate into a SYM subcase (that is any
   output desired must be requested within the subcase).

4. SYM may only be used in statics or inertia relief.
=PAGE=
SYMCOM - Symmetry Combination Subcase Delimiter

Description

Delimits and identifies a symmetry combination subcase.

Format and Example(s)

SYMCOM n

SYMCOM 123

Option      Meaning

n           Subcase identification number (Integer > 2).

Remarks

1. The subcase identification number, n, must be strictly increasing (that is,
   greater than all previous subcase identification numbers).

2. SYMCOM may only be used in statics or inertia relief problems.

3. Up to 360 SYMCOM cards can be used in one NASTRAN analysis.
=PAGE=
SYMSEQ - Symmetry Sequence Coefficients

Description

Gives the coefficients for combining the symmetry subcases into the total
structure.

Format and Example(s)

SYMSEQ = R1 [, R2, R3 ..., Rn]

SYMSEQ = 1.0, -2.0, 3.0, 4.0

Option      Meaning

R1 to RN    Coefficients of the previously occurring N SYM subcases (Real).

Remarks

1. A SYMSEQ card may only appear in a SYMCOM subcase.

2. The default value for the coefficients is 1.0 if no SYMSEQ card appears.

3. A SYMSEQ card may consist of more than one physical card.

4. SYMSEQ may only be used in statics or inertia relief.
=PAGE=
TEMPERATURE - Thermal Properties Set Selection

Description

Selects the temperature set to be used in either material property calculation
or thermal loading.

Format and Example(s)

             Ú             ¿
             ³(  BOTH     )³
TEMPERATURE  ³   MATERIAL  ³  =  n
             ³   LOAD      ³
             À             Ù

TEMPERATURE (LOAD) = 15

TEMPERATURE (MATERIAL) = 7

TEMPERATURE = 7

Option      Meaning

BOTH        Both options, MATERIAL and LOAD, will use the same temperature
            table.

MATERIAL    The selected temperature table will be used to determine
            temperature-dependent material properties indicated on the MATTi
            type cards.

LOAD        The selected temperature table will be used to determine an
            equivalent static load.

n           Set identification number of TEMP, TEMPD, TEMPP1, TEMPP2, TEMPP3,
            TEMPRB, or TEMPAX cards (Integer > 0).

Remarks

1. Only one temperature-dependent material request may be made in any problem
   and it must be above the subcase level.

2. Thermal loading may only be used in statics, inertia relief, differential
   stiffness, and buckling problems.

3. Temperature-dependent materials may not be used in piecewise linear
   problems.

4. The total load applied will be the sum of external (LOAD), thermal
   (TEMP(LOAD)), element deformation (DEFORM), and constrained displacement
   (SPC) loads.

5. Static, thermal, and element deformation loads should have unique set
   identification numbers.

6. In heat transfer analysis, the TEMP data is used for the following special
   purposes:

   a. The Case Control card TEMP(MATERIAL) will select the initial estimated
      temperature field for nonlinear conductivity and radiation effects. See
      Section 1.8 (APP HEAT, Rigid Formats 1, 3, and 9).

   b. In Rigid Format 3, heat boundary temperatures are defined by the
      specified Case Control card TEMP(MATERIAL). These points are specified
      with SPC data.
=PAGE=
TFL - Transfer Function Set Selection

Description

Selects the transfer function set to be added to the direct input matrices.

Format and Example(s)

TFL = n

TFL = 77

Option      Meaning

n           Set identification of a TF card (Integer > 0).

Remarks

1. Transfer functions will not be used unless selected in the Case Control
   Deck.

2. Transfer functions are supported on dynamics problems only.

3. Transfer functions are simply another form of direct matrix input.
=PAGE=
THERMAL - Temperature Output Request

Description

Requests form and type of temperature vector output.

Format and Example(s)

         Ú             ¿    É      »
THERMAL  ³PRINT , SORT1³  = º ALL  º
         ³PUNCH   SORT2³    º  n   º
         À             Ù    º NONE º
                            È      ¼
THERMAL = 5

THERMAL(PRINT,PUNCH) = ALL

Option      Meaning

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

ALL         Temperatures for all points will be output.

NONE        Temperatures for no points will be output.

n           Set identification of previously appearing SET card. Only
            temperatures of points whose identification numbers appear on this
            SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested. The punched output will consist of
   double field TEMP* Bulk Data cards defining the temperatures at the grid
   points.

2. THERMAL output request is designed for use with the heat transfer option.
   The printed output will have temperature headings and the punched output
   will be TEMP bulk data cards. The SID on a bulk data card will be the
   subcase number (= 1 if no defined subcases). The output format will be
   SORT1 for Static problems and SORT2 for transient problems.

3. An output request for ALL in transient response problems generally produces
   large amounts of printout. An alternative would be to define a SET of
   interest.

4. DISPLACEMENT and VECTOR are alternate forms and are entirely equivalent to
   THERMAL.

5. THERMAL = NONE allows overriding an overall output request.

6. The output format will be SORT1 for Rigid Formats 1 and 3, SORT2 for Rigid
   Format 9.

7. If punched output is desired in Rigid Format 9 for subsequent use in the
   other Rigid Formats, SORT1 format must be selected.
=PAGE=
TITLE - Output Title

Description

Defines a BCD (alphanumeric) title which will appear on the first heading line
of each page of NASTRAN printer output.

Format and Example(s)

          É              »
TITLE  =  º Any BCD data º
          È              ¼

TITLE = **$// ABCDEFGHI .... $

Remarks

1. TITLE appearing at the subcase level will title output for that subcase
   only.

2. TITLE appearing before all subcases will title any outputs which are not
   subcase dependent.

3. If no TITLE card is supplied, the title line will contain data and page
   numbers only.

4. TITLE information is also placed on NASTRAN plotter output as applicable.
=PAGE=
TSTEP - Transient Time Step Set Selection

Description

Selects integration and output time steps for transient problems.

Format and Example(s)

TSTEP = n

TSTEP = 731

Option      Meaning

n           Set identification of a selected TSTEP bulk data card (Integer >
            0).

Remarks

1. A TSTEP card must be selected to execute a transient problem.

2. Only one TSTEP card may have this value of n.
=PAGE=
VECTOR - Displacement Output Request
Description

Requests form and type of displacement vector output.

Format and Example(s)

         Ú                       ¿     É      »
VECTOR   ³ ( SORT1, PRINT, REAL )³     º ALL  º
         ³   SORT2  PUNCH  IMAG  ³  =  º  n   º
         ³                 PHASE ³     º NONE º
         À                       Ù     È      ¼

VECTOR = 5

VECTOR(REAL) = ALL

VECTOR(SORT2, PUNCH, REAL) = ALL

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available on transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each grid point. SORT2 is available only in transient and
            frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on complex eigenvalue or
            frequency response problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            complex eigenvalue or frequency response problems.

ALL         Displacements for all points will be output.

NONE        Displacements for no points will be output.

n           Set identification of a previously appearing SET card. Only
            displacements of points whose identification numbers appear on
            this SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. On a frequency response problem any request for SORT2 causes all output to
   be SORT2.

3. DISPLACEMENT and PRESSURE are alternate forms and are entirely equivalent
   to VECTOR.

4. VECTOR = NONE allows overriding an overall output request.
=PAGE=
VELOCITY - Velocity Output Request

Description

Requests form and type of velocity vector output.

Format and Example(s)

           Ú                        ¿    É      »
VELOCITY   ³ ( SORT1, PRINT, REAL ) ³    º ALL  º
           ³   SORT2  PUNCH  IMAG   ³ =  º  n   º
           ³                 PHASE  ³    º NONE º
           À                        Ù    È      ¼

VELOCITY = 5

VELOCITY(SORT2, PHASE, PUNCH) = ALL

Option      Meaning

SORT1       Output will be presented as a tabular listing of grid points for
            each load, frequency, eigenvalue, or time, depending on the rigid
            format. SORT1 is not available in transient problems (where the
            default is SORT2).

SORT2       Output will be presented as a tabular listing of frequency or time
            for each gridpoint. SORT2 is available only in transient and
            frequency response problems.

PRINT       The printer will be the output device.

PUNCH       The card punch will be the output device.

REAL or IMAG  Requests real or imaginary output on frequency response
            problems.

PHASE       Requests magnitude and phase (0.0 <= phase < 360.0 degrees) on
            frequency response problems.

ALL         Velocity for all solution points will be output.

NONE        Velocity for no solution points will be output.

n           Set identification of a previously appearing SET card. Only
            velocities of points whose identification numbers appear on this
            SET card will be output (Integer > 0).

Remarks

1. Both PRINT and PUNCH may be requested.

2. An output request for ALL in transient and frequency response problems
   generally produces large amounts of printout. An alternative would be to
   define a SET of interest.

3. Velocity output is only available for transient and frequency response
   problems.

4. In a frequency response problem any request for SORT2 output causes all
   output to be SORT2.

5. VELOCITY = NONE allows overriding an overall output request.
=PAGE=
$ - Comment Card

Description

Defines a comment card by specifying a $ in column one with commentary text
appearing in columns 2-80.

Format and Example(s)

   É              »
$  º Any BCD data º
   È              ¼

$---THIS IS AN EXAMPLE OF A COMMENT CARD.

Remarks

1. Unlike other Case Control cards, which are free field, the comment card
   must have the $ in column 1.

